##### Analysis with simulated data 10K runs per scenario
##############

#Read in saved dataframes
master <- read.table("analysis/10Kmaster.txt", header = TRUE)

#data
errorDataRaw1 <- cbind.data.frame("ScenID" = master$ScenID, 
                                  "pcError" = master$newPercErrfull, 
                      "Method" = "LTS")
errorDataRaw2 <- cbind.data.frame("ScenID" = master$ScenID, 
                                  "pcError" =master$newPercErropt, 
                                  "Method" = "Optimised")
errorDataRaw <- rbind.data.frame(errorDataRaw1, errorDataRaw2)

png("graphs/Fig2RawErrorLarge.png", width = 1200, height = 550)
par(mar = c(5,8.5,2,2), cex = 1.5)
boxplot(errorDataRaw$pcError~errorDataRaw$Method,
        ylab = "", 
        xlab = "Percent error in density estimate", 
        horizontal = TRUE, las = 1, cex.lab = 2, cex.axis = 2)
mtext("Method", side = 2, line = 3, las = 1, cex = 3)
dev.off()

