#!/usr/bin/env Rscript 
#Title: Clean Data and Basic Analysis
#Student: Joel Gayle
#University: University of the Cumberlands
#Course name:  Analyzing and Visualizing Data
#Course number: ITS-530-02
#Professor: Dr. Kathy McClure  
#Date: 1/19/2020


#instructions, place the diabetesData.csv file in the working directory
#run the script
#2 output files will be generated: Correlation_Plot.pdf and Density_Plots.pdf in the working directory


#------------ correlation plot -----------
printCorrPlot = function(myData) {
#Open jpeg file
pdf("Correlation_Plot.pdf")
  
res <- cor(myData)
#generate a plot of the data correlation
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#print the correlation of the data 
#res <- cor(myData)
#round(res, 3)

#Close the file
dev.off()
}


#------------ density plots -----------
printDensityPlots = function(myData) {
#Open jpeg file
pdf("Density_Plots.pdf")

#Density plot
par(mfrow=c(3, 3))
colnames <- colnames(myData)
for (i in 2:9) {
  d <- density(myData[,i])
  plot(d, type="n", main=colnames[i])
  polygon(d, col="red", border="gray")
}

#Close the file
dev.off()
}

library(corrplot)

#read in the data from csv file
myData <- read.csv("diabetesData.csv")

#print the correlation plot
printCorrPlot(myData)

#print the density plots
printDensityPlots(myData)
