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

# Separate the data by year
PM2_5Data$SCC <- as.factor(PM2_5Data$SCC)
PM2_5Data1999 <- subset(PM2_5Data, PM2_5Data$year == 1999)
PM2_5Data2008 <- subset(PM2_5Data, PM2_5Data$year == 2008)

# Calculate the mean across types of pollutant
PM2_5Means1999 <- tapply(PM2_5Data1999$Emissions, PM2_5Data1999$SCC, mean, na.rm = TRUE)
PM2_5Means1999 <- data.frame(SCC = names(PM2_5Means1999), mean = PM2_5Means1999, year = "1999")
PM2_5Means1999 <- subset(PM2_5Means1999, !is.na(mean))
PM2_5Means2008 <- tapply(PM2_5Data2008$Emissions, PM2_5Data2008$SCC, mean, na.rm = TRUE)
PM2_5Means2008 <- data.frame(SCC = names(PM2_5Means2008), mean = PM2_5Means2008, year = "2008")
PM2_5Means2008 <- subset(PM2_5Means2008, !is.na(mean))

PM2_5Means <- rbind(PM2_5Means1999, PM2_5Means2008)
PM2_5Means$year <- as.factor(PM2_5Means$year)

boxplot(log10(mean) ~ year, PM2_5Means, main = "Comparison of Pollutants from Coal",
        xlab = "Year", ylab = "log10(Mean Emissions)")
abline(h = log10(mean(PM2_5Means[PM2_5Means$year == "1999", ]$mean)), col = 2, lwd = 2, lty = 2)
abline(h = log10(mean(PM2_5Means[PM2_5Means$year == "2008", ]$mean)), col = 3, lwd = 2, lty = 2)
legend("bottomleft", legend = c("1999 Mean Emissions", "2008 Mean Emissions"),
       col = c(2, 3), lwd = 2, lty = 2)

# Create a plot with means to compare data between 1999 and 2008
if (!file.exists("output")){
    dir.create("output")
}

# Store the graph as a '.png' file
png(filename = "./output/plot4.png")

boxplot(log10(mean) ~ year, PM2_5Means, main = "Comparison of Pollutants from Coal",
        xlab = "Year", ylab = "log10(Mean Emissions)")
abline(h = log10(mean(PM2_5Means[PM2_5Means$year == "1999", ]$mean)), col = 2, lwd = 2, lty = 2)
abline(h = log10(mean(PM2_5Means[PM2_5Means$year == "2008", ]$mean)), col = 3, lwd = 2, lty = 2)
legend("bottomleft", legend = c("1999 Mean Emissions", "2008 Mean Emissions"),
       col = c(2, 3), lwd = 2, lty = 2, cex = 0.65)

dev.off()