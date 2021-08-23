## ********************************************************
##
## Script: plot1.R
## Author: Brian Crilly
## Date:   21 August 2021
## Description:
##  This script is part of the second project for the
##  Exploratory Data Analysis course. The purpose of the
##  script is to address the question: "Have total emissions
##  from PM2.5 decreased in the United States from 1999 to
##  2008?" This script uses the PM2.5 data set, and leverages
##  the base plotting system to make a plot showing the total
##  PM2.5 emission from all sources for the years 1999 and 2008.
##
## ********************************************************

# Load the data files (if they exist) and process the data
dataLocation <- "./data/exdata-data-NEI_data/"
PM2_5DataFileName <- "summarySCC_PM25.rds"

if (!(PM2_5DataFileName %in% dir(dataLocation))) {
        stop("Data file does not exist!")
}

PM2_5Data <- readRDS(paste0(dataLocation, PM2_5DataFileName))

# Subset to only 1999 and 2008, and calculate the mean across years
PM2_5Data <- subset(PM2_5Data, PM2_5Data$year %in% c(1999, 2008))
PM2_5Data$year <- as.factor(PM2_5Data$year)
PM2_5Means <- tapply(PM2_5Data$Emissions, PM2_5Data$year, mean)

# Create a boxplot with means to compare data between 1999 and 2008
if (!file.exists("output")){
    dir.create("output")
}

# Store the graph as a '.png' file
png(filename = "./output/plot1.png")

boxplot(log10(Emissions) ~ year, data = PM2_5Data,
        main = "Comparison of PM2.5 Emissions", xlab = "Year")
abline(h = log10(PM2_5Means["1999"]), col = 2, lwd = 2, lty = 2)
abline(h = log10(PM2_5Means["2008"]), col = 3, lwd = 2, lty = 2)
legend("bottomleft", legend = c("1999 Mean Emissions", "2008 Mean Emissions"),
       col = c(2, 3), lwd = 2, lty = 2)

dev.off()