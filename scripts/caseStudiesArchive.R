#distance optimisation case studies

#read in 3 datasets; brassica, grevillea seedlings, grevillea adults
brassica <- read.csv("caseStudyData/brassica.csv")
grevSeed <- read.csv("caseStudyData/Grevseed.csv")
grevAd <- read.csv("caseStudyData/GrevAd.csv")

#removing anything beyond 2m perpendicular distance (truncation distance) and 2m transect length (seedlings)
#same but 5m for adults
grevSeed <- grevSeed[ grevSeed$P_Dist<= 200 & grevSeed$Transect_Dist <= 200, ] 
grevAd <- grevAd[ grevAd$P.Dist<= 500 & grevAd$TransectDist <= 500, ]

#dataframe of metrics to calculate optimal values (alpha, length, sample size) 

inputs <- cbind.data.frame("target" = c("brassica", "grevillea(seed)", "grevillea(adult)"),
                           "nDet" = c(134, length(grevSeed$P_Dist), length(grevAd$P.Dist)),
                           "lTot" = c(100, 20, 20), #transect length
                           "Cm" = c(20/60, 20/60, 0.5), #time in mins
                           "B1" = 480, #1 day or 8hrs of survey time,
                           "surveyTime" = c("brassicaTime" = 54.6,"grevSeedTime" = 82.67-(40/60), "grevAdTime" = 37-8.5))
#times need adjusting for the discarded measurements - 8.5mins for grevAds, 40/60 mins for seeds
inputs$Cw <- (inputs$surveyTime - (inputs$Cm*inputs$nDet))/inputs$lTot #this gives mins/m
inputs$E <- inputs$nDet/inputs$lTot 

#write.csv(inputs, "caseStudyData/caseStudyDetails.csv",row.names = FALSE) 

#function to calculate the variables
OptCalc <- function(E, Cm, Cw, B1){
  #cost ratio, optimal prop to measure, predicted and adjusted benefit
  Cd <- Cw/E
  alphaOpt <- sqrt(Cd/(2*Cm))
  Rc <- Cm/Cd
  Ben <- 3/2*((1+Rc)/(1+sqrt(Rc/2))^2)
  AdjustBen <- 0.79*Ben + 0.19/Ben #rounded to account for uncertainty
  
  #length of transect and expected n detected targets with optimisation B1 and B2
  lOpt1 <- B1/(Cw+E*alphaOpt*Cm)
  NtB1 <- E*lOpt1
  N1B1 <- NtB1*alphaOpt

  #length of transect and expected n detected targets with CDS
  CDSlengthB1 <- B1/(Cw + (E*Cm))
  CDSnB1 <- E*CDSlengthB1

 output <- cbind.data.frame(Cd, Rc, alphaOpt, Ben, AdjustBen, 
                            lOpt1, NtB1, N1B1, CDSlengthB1, CDSnB1)
 } 

#loop to run the function on all rows of inputs
outputs <- list()

counter <- 0
for(i in 1:nrow(inputs)){
  counter <- counter+1
  Calcs <-  OptCalc(inputs$E[i], inputs$Cm[i], inputs$Cw[i], inputs$B1[i])
  outputs[[counter]] <- cbind.data.frame("target" = inputs$target[i], 
                                         "Cd" = Calcs$Cd, 
                                         "Rc" = Calcs$Rc,
                                         "alphaOpt" = Calcs$alphaOpt, 
                                         "Ben" = Calcs$Ben,
                                         "AdjustBen" = Calcs$AdjustBen,
                                         "lOpt1" = Calcs$lOpt1, 
                                         "NtB1" = Calcs$NtB1, 
                                         "N1B1" = Calcs$N1B1,
                                         "CDSlengthB1" = Calcs$CDSlengthB1,
                                         "CDSnB1" = Calcs$CDSnB1)
} 

newOutput <- do.call(rbind, outputs) 

myResult <- merge.data.frame(inputs, newOutput, by = "target")

write.csv(myResult, "analysis/caseStudiesTable.csv", row.names = FALSE)
