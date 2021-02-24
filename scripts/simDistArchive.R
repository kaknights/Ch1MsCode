##### Efficient Effort Allocation in Line Transect Distance Sampling #####
#####                    for High Density Species                    #####
##Produce simulated distance data and calculations comparing full and optimised methods

library(tidyverse); library(Distance); library(future.apply)

#folder/directory structure outlined in readme.txt

source("scripts/simDistArchiveFunctions.R")

nSims <- 10000

##Create scenarios
parsAll <- expand.grid("Dens" = c(0.005, 0.01, 0.02, 0.05, 0.075, 0.1), 
                       "Cm" = c(1.1, 2, 5, 10), 
                       "sigm" = c(1, 3, 5, 10))

##Produce the list of inputs for the simulation function
ParsCalc <- SimCalc_Func(parsAll$Dens, parsAll$sigm, parsAll$Cm)
SimPars <- ParsCalc[ParsCalc$Opt_Alpha<1, ]
myList <- split(SimPars, seq(nrow(SimPars)))

#writing results from each loop then putting them together with another loop
counter <- 0
nCores <- 30 #adjust depending on availability
#using future.apply plan() function to implement parallel processing
plan(multiprocess, workers = nCores)
for (i in 1:nSims){
  counter <- counter + 1
  #run the simulations
  thisoutput <- future_lapply(myList, simulateDist)
  
  #extract 61 rows of output
  thisoutput.df <- data.frame()
  for (a in 1:nrow(SimPars))
  {thisoutput.df <- rbind(thisoutput.df, as.data.frame(thisoutput[[a]]))}
  
  thisoutput.df$iteration <- counter
  
  #option1
  write.csv(thisoutput.df, paste0("SimOutput/SimOutput", counter, ".csv"))
  
  #option2
  #myresults <- rbind(myresults, thisoutput.df)
  print(counter)
}

counter <- 0
mycompiledoutput <- data.frame()

for (i in 1:nSims){

  counter <- counter + 1
  thiscsv <- read.csv(paste0("SimOutput/SimOutput", i, ".csv"))

  mycompiledoutput <- rbind(mycompiledoutput, thiscsv)
  print(counter)

  }

write.csv(mycompiledoutput, "SimOutput/sim10KcompiledOutput.csv", row.names = FALSE)

#####Creating dataframes from the output (local, not on server)

#write.table(SimPars, "10KSimPars.txt", row.names = FALSE) #saves for future use

data <- merge(SimPars, mycompiledoutput, by = "ScenID")

##### Add calculated columns for: adjusted est Dopt and est Nopt, error and expected N from calculations

master <- cbind(data, 
               "adjDopt" = data$EstDOpt*(data$nObsOpt/data$subSize),
               "adjNopt" = data$nObsOpt/data$EstPOpt,
               "expNopt" = (data$mu/data$W)*(data$Dens*data$Area),
               "expNfull" = (data$mu/data$W)*(data$Dens*data$fullArea),
               "simDopt"= data$NTotOpt/data$Area,
               "simDfull"= data$NTotfull/data$fullArea)

master$newErrDopt  <-  master$simDopt - master$adjDopt
master$newErrDfull  <-  master$simDfull - master$EstDfull
master$newPercErropt <- master$newErrDopt/master$simDopt*100
master$newPercErrfull <- master$newErrDfull/master$simDfull*100

##### From master, create df of 1 record per scenario including var from sims and benefit

##### for loop to perform summarise function on relevant columns in master

Output <- list()
counter <- 0
for (i in c("EstDfull", "adjDopt", "newPercErropt", "newPercErrfull")) {
  counter <- counter + 1
  calcs <- Summarise(master[ ,i], master$ScenID)
  Output[[counter]] <- data.frame(calcs)
}

summdata <- do.call(cbind, Output)
summarydata <- summdata[ , -c(4,7,10)]

names(summarydata) <- c("ScenID", "estDfullmean", "estDfullsd", "estDoptmean", "estDoptsd", 
                        "percerrDOptmean", "percerrDOptsd","percerrDfullmean", "percerrDfullsd")

##### Add columns for benefit 
# Observed benefit - Variance instead of SE
summarydata$obsBenefit <- summarydata$estDfullsd^2/summarydata$estDoptsd^2
# Predicted benefit - variance (instead of SE)
summarydata$predVaropt <- SimPars$Dens^2*(1/((SimPars$mu/SimPars$W)*(SimPars$Dens*SimPars$Area))*
                                                (1+1/(2*SimPars$Opt_Alpha)))
summarydata$predVarfull <- SimPars$Dens^2*(3/(2*((SimPars$mu/SimPars$W)*(SimPars$Dens*SimPars$fullArea))))
summarydata$predBenefit <- summarydata$predVarfull/summarydata$predVaropt

#add Rc to relevant dataframe (Cm/Cd)
Rctest <- aggregate.data.frame((master$Cm/master$Cd), by = list(master$ScenID), mean)
summarydata$Rc <- Rctest[ , 2]

write.table(master, "analysis/10Kmaster.txt", row.names = FALSE)
write.table(summarydata, "analysis/summarydata.txt", row.names = FALSE)
