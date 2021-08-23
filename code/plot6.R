## ********************************************************
##
## Script: plot6.R
## Author: Brian Crilly
## Date:   22 August 2021
## Description:
##  This script is part of the second project for the
##  Exploratory Data Analysis course. The purpose of the
##  script is to address the question:
##      "Compare emissions from motor vehicle sources in
##       Baltimore City with emissions from motor vehicle
##       sources in Los Angeles County, California
##       (fips == "06037"). Which city has seen greater
##       changes over time in motor vehicle emissions?"
##  This script uses the PM2.5 data set, and leverages the
##  ggplot plotting system to make a plot showing the PM2.5
##  emission from motor vehicle sources in Baltimore City 
##  for the years 1999 and 2008.
##
## ********************************************************

# Load libraries
library(dplyr)
library(tidyr)

# Load the data files (if they exist) and process the data
dataLocation <- "./data/exdata-data-NEI_data/"
PM2_5DataFileName <- "summarySCC_PM25.rds"
SCCFileName <- "Source_Classification_Code.rds"

if (!(PM2_5DataFileName %in% dir(dataLocation) & SCCFileName %in% dir(dataLocation))) {
        stop("Data file does not exist!")
}

PM2_5Data <- readRDS(paste0(dataLocation, PM2_5DataFileName))
SCCData <- readRDS(paste0(dataLocation, SCCFileName))

# Subset Source Classification Codes to those that include on-road vehicles
SCCData <- subset(SCCData, grepl("^[H|h]ighway Vehicles", SCC.Level.Two))

# Subset pollution data to only the on-road vehicle sources
PM2_5Data <- subset(PM2_5Data, SCC %in% SCCData$SCC)

# Include only Baltimore City (fips == "24510") and Los Angeles (fips == "06037")
PM2_5Data <- subset(PM2_5Data, fips == "24510" | fips == "06037")

# Include only 1999 and 2008
PM2_5Data <- subset(PM2_5Data, year == 1999 | year == 2008)

# Calculate the mean emissions across years and regions
PM2_5Means <- with(PM2_5Data, tapply(Emissions, list(fips, year), mean, na.rm = TRUE))
PM2_5Means <- as.data.frame(PM2_5Means, row.names = c("Los Angeles", "Baltimore"))

# Calculate the change in emissions relative to 1999
PM2_5Means <- mutate(PM2_5Means,
                     c1999 = ((PM2_5Means$`1999` - PM2_5Means$`1999`) / PM2_5Means$`1999`) + 1)
PM2_5Means <- mutate(PM2_5Means,
                     c2008 = ((PM2_5Means$`2008` - PM2_5Means$`1999`) / PM2_5Means$`1999`) + 1)

# Create a plot with means to compare data between 1999 and 2008
if (!file.exists("output")){
    dir.create("output")
}

# Store the graph as a '.png' file
png(filename = "./output/plot6.png")

with(PM2_5Means, plot(rep(1999, 2), c1999, xlim = c(1998, 2009), ylim = c(0, 4), col = c(2, 3),
                      main = "Relative Change in Vehicle Emissions from 1999 to 2008",
                      xlab = "Year", ylab = "Emissions Relative to 1999"))
with(PM2_5Means, points(rep(2008, 2), c2008, col = c(2, 3)))
with(PM2_5Means, segments(rep(1999, 2), c1999, rep(2008, 2), c2008,
                          col = c(2, 3), lwd = 2, lty = 2))
legend("topleft", legend = c("Los Angeles", "Baltimore City"),
       col = c(2, 3), lwd = 2, lty = 2)
dev.off()