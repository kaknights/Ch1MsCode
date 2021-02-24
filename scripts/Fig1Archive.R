#Fig 1 (main result) in "Efficient effort allocation in line transect distance sampling..."
summarydata <- read.table("analysis/summarydata.txt", header = TRUE)

#actual benefit = 0.792*x + 0.189/x, where x is the expected benefit.

#creating the predicted benefit line
Cm <- seq(0.1,25, by = 0.1)
Cd <- 1
Rat <- Cm/Cd # creates a sequence of ratios for the prediction line to pass through
predRc <- Rat[Rat>=0.5]
predben <- 3/2*((1+predRc)/(1+sqrt(predRc/2))^2) #benefit as ratio of variances
adjPredBen <- 0.79*predben + 0.19/predben #adjustment applied

origPar <- par()
par(origPar)

png("graphs/Fig1BWlarge.png", width = 1800, height = 1800)
par(mar=c(4,4,2,2),oma=c(1,1,0,0), cex = 5)

plot(summarydata$Rc, summarydata$obsBenefit,
     xlim = c(0.5,25),
     ylim = c(0.85, 2),
     pch = 16,
     las = 1,
     xlab = "Cost ratio (measuring/walking)",
     ylab = "Benefit (var(D)LTS/var(D)opt)"
)
lines(x = predRc,
      y = predben,
      type = "l",
      xlim = c(0.5,25),
      ylim = c(0.85, 2),
      lty = "dashed",
      lwd = 2)
##Add predicted line adjusted by function given by eureqa
lines(x = predRc,
      y = adjPredBen,
      type = "l",
      xlim = c(0.5,25),
      ylim = c(0.85, 2),
      lty = "solid",
      lwd = 2)
legend(x = 12.5, y = 1.2, legend = " observed", col="black", 
       pch = 19, bty = "n", pt.cex = 1.5, cex = 1,  horiz = FALSE)
legend(x = 10.6, y = 1.1, legend = "predicted", 
       lty="dashed", bty = "n", lwd = 3, cex = 1,  horiz = FALSE)
legend(x = 10.6, y = 1.0, legend = "adjusted prediction", 
       lty = 1, bty = "n", lwd = 3, cex = 1,  horiz = FALSE)
dev.off()
