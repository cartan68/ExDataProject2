## ********************************************************
##
## Script: plot4.R
## Author: Brian Crilly
## Date:   21 August 2021
## Description:
##  This script is part of the second project for the
##  Exploratory Data Analysis course. The purpose of the
##  script is to address the question:
##      "Across the United States, how have emissions from
##       coal combustion-related sources changed from
##       1999–2008?"
##  This script uses the PM2.5 data set, and leverages the
##  ggplot plotting system to make a plot showing the PM2.5
##  emission from coal combustion sources for the years
##  1999 and 2008.
##
## ********************************************************

# Load libraries
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

# Subset Source Classification Codes to those that include Coal
SCCData <- subset(SCCData, grepl("[C|c]oal", SCC.Level.Three))

# Subset pollution data to only the coal sources
PM2_5Data <- subset(PM2_5Data, SCC %in% SCCData$SCC)

# Separate the data by year and SSC
#PM2_5Data$SCC <- as.factor(PM2_5Data$SCC)
PM2_5Means <- with(PM2_5Data, tapply(Emissions, list(SCC, year), mean))
PM2_5Means <- as.data.frame(PM2_5Means)
PM2_5Means$SCC <- rownames(PM2_5Means)
PM2_5Means <- gather(PM2_5Means, year, emissions, -SCC)

# Create a plot with means to compare data between 1999 and 2008
if (!file.exists("output")){
    dir.create("output")
}

# Store the graph as a '.png' file
png(filename = "./output/plot4.png")

boxplot(log10(emissions) ~ year, PM2_5Means, main = "Comparison of Pollutants from Coal",
        xlab = "Year", ylab = "log10(Emissions in Tons)")
abline(h = log10(mean(PM2_5Means[PM2_5Means$year == "1999", ]$emissions, na.rm = TRUE)),
       col = 2, lwd = 2, lty = 2)
abline(h = log10(mean(PM2_5Means[PM2_5Means$year == "2008", ]$emissions, na.rm = TRUE)),
       col = 3, lwd = 2, lty = 2)
legend("topright", legend = c("1999 Mean Emissions", "2008 Mean Emissions"),
       col = c(2, 3), lwd = 2, lty = 2, cex = 0.65)

dev.off()