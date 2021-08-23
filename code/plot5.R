## ********************************************************
##
## Script: plot5.R
## Author: Brian Crilly
## Date:   22 August 2021
## Description:
##  This script is part of the second project for the
##  Exploratory Data Analysis course. The purpose of the
##  script is to address the question:
##      "How have emissions from motor vehicle sources
##       changed from 1999–2008 in Baltimore City?"
##  This script uses the PM2.5 data set, and leverages the
##  ggplot plotting system to make a plot showing the PM2.5
##  emission from motor vehicle sources in Baltimore City 
##  for the years 1999 and 2008.
##
## ********************************************************

# Load the data files (if they exist) and process the data
dataLocation <- "./data/exdata-data-NEI_data/"
PM2_5DataFileName <- "summarySCC_PM25.rds"

if (!(PM2_5DataFileName %in% dir(dataLocation))) {
        stop("Data file does not exist!")
}

PM2_5Data <- readRDS(paste0(dataLocation, PM2_5DataFileName))

# Subset pollution data to only the on-road vehicle sources
PM2_5Data <- subset(PM2_5Data, type == "ON-ROAD")

# Include only Baltimore City (fips == "24510")
PM2_5Data <- subset(PM2_5Data, fips == "24510")

# Create a plot with means to compare data between 1999 and 2008
if (!file.exists("output")){
    dir.create("output")
}

# Store the graph as a '.png' file
png(filename = "./output/plot5.png")

boxplot(log10(Emissions) ~ year, PM2_5Data,
        main = "Comparison of Pollutants from Motor Vehicles in Baltimore City",
        xlab = "Year", ylab = "log10(Emissions in Tons)")
abline(h = log10(mean(PM2_5Data[PM2_5Data$year == "1999", ]$Emissions)), col = 2, lwd = 2, lty = 2)
abline(h = log10(mean(PM2_5Data[PM2_5Data$year == "2008", ]$Emissions)), col = 3, lwd = 2, lty = 2)
legend("bottomleft", legend = c("1999 Mean Emissions", "2008 Mean Emissions"),
       col = c(2, 3), lwd = 2, lty = 2, cex = 0.9)

dev.off()