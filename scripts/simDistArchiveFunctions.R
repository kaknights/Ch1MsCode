#simDistArchive functions

##Function to calculate optimal proportions given the scenario parameters

SimCalc_Func <- function(Dens, sigm, Cm){
  B <- 10000; Cl <- 1
  W <- 2*sigm
  mu <- sqrt(pi*(sigm^2)/2)
  R <-  2*mu*Dens
  Opt_Alpha <-  ifelse(sqrt(Cl/(2*R*Cm))< 1, sqrt(Cl/(2*R*Cm)), 1)
  Opt_L <-  B/(Cl + R*Opt_Alpha*Cm)
  Area <- Opt_L*2*W
  fullLength <- B/(Cl + (R*Cm)) 
  fullArea  <-  fullLength*2*W 
  Cd <- Cl/R
  Ct <- Cm*R
  Rc <- Cm/Cd
  SimCalcData <-  cbind.data.frame(W, mu, R, Opt_Alpha, Opt_L, Area, fullLength, 
                                   fullArea, Cd, Ct, Rc)         
  parsAll <- cbind("ScenID" = 1:nrow(parsAll), parsAll, SimCalcData)
}

##function to generate distance data and analyse
##based on list of parameters created by the function above
simulateDist <- function(listItem) {
  #load relevant parameters from list of parameters
  ScenID <- listItem$ScenID
  Dens <- listItem$Dens
  Area <- listItem$Area
  W <- listItem$W
  sigm <- listItem$sigm
  Opt_Alpha <- listItem$Opt_Alpha
  Opt_L <- listItem$Opt_L
  fullLength <- listItem$fullLength
  fullArea <- listItem$fullArea
  
  ##Opt data creation and model
  #draw total number of individuals in covered area
  N <- rpois(1, Dens*Area)
  distance <- runif(N,0,W) #each individual distance
  obs <- rbernoulli(N, exp(-distance^2/(2*sigm^2))) #detected or not
  
  #create table for distance package, only observed individual distances
  sim_data <- cbind.data.frame(distance, obs,  
                               "Region.Label" = "a", 
                               "Sample.Label" = 1, 
                               "Effort" = Opt_L, 
                               "Area" = Area) 
  Dist_data <- sim_data[sim_data$obs == TRUE, ]
  
  #select subset of measured distances for model
  distSubset <- Dist_data[seq(1, length(Dist_data$distance), 1/Opt_Alpha), ]
  #use selected distances for model - default half-normal
  distModelOpt <- ds(distSubset, adjustment = NULL, dht.group = TRUE)
  
  ##Conventional LTS data creation and analysis
  #draw n individuals in covered area
  N1 <- rpois(1, Dens*fullArea)
  distance1 <- runif(N1,0,W)#each individual distance
  obs1 <- rbernoulli(N1, exp(-distance1^2/(2*sigm^2)))#detected or not
  
  #create table for distance package, only observed individual distances
  sim_data1 <- cbind.data.frame(distance1, obs1,  
                                "Region.Label" = "a", 
                                "Sample.Label" = 1, 
                                "Effort" = fullLength, 
                                "Area" = fullArea) 
  Dist_data1 <- sim_data1[sim_data1$obs1 == TRUE, ]
  #distance model - default half-normal
  distModelFull <- ds(Dist_data1, adjustment = NULL, dht.group = TRUE)
  
  #compile results both methods
  SimDataOut <- cbind.data.frame(ScenID, "NTotOpt" = N, "nObsOpt" = nrow(Dist_data), "subSize"= nrow(distSubset), 
                                 "EstPOpt" = as.numeric(distModelOpt$dht$individuals$average.p),
                                 "EstDOpt" = as.numeric(distModelOpt$dht$individuals$D$Estimate),
                                 "cvEstDOpt" = as.numeric(distModelOpt$dht$individuals$D$cv),
                                 "EstNOpt" = as.numeric(distModelOpt$dht$individuals$N$Estimate),
                                 "cvEstNOpt" = as.numeric(distModelOpt$dht$individuals$N$cv),
                                 "CovdAOpt" = as.numeric(distModelOpt$dht$individuals$bysample$Sample.Area),
                                 "NTotfull" = N1, "nObsfull"= nrow(Dist_data1), 
                                 "EstPfull" = as.numeric(distModelFull$dht$individuals$average.p),
                                 "EstDfull" = as.numeric(distModelFull$dht$individuals$D$Estimate),
                                 "cvEstDfull" = as.numeric(distModelFull$dht$individuals$D$cv),
                                 "EstNfull" = as.numeric(distModelFull$dht$individuals$N$Estimate),
                                 "cvEstNfull" = as.numeric(distModelFull$dht$individuals$N$cv),
                                 "CovdAfull" = as.numeric(distModelFull$dht$individuals$bysample$Sample.Area))
}

##### function to calculate mean and sd for various columns
Summarise <- function(namecol, namegroup){
  
  mean <- aggregate(namecol, list(namegroup), mean)
  sd <- aggregate(namecol, list(namegroup), sd)
  output <- cbind("ScenID" = mean$Group.1, "mean" = mean$x, "sd" = sd$x)
}