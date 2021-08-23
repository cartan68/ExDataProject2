## ********************************************************
##
## Script: plot3.R
## Author: Brian Crilly
## Date:   21 August 2021
## Description:
##  This script is part of the second project for the
##  Exploratory Data Analysis course. The purpose of the
##  script is to address the question: "Of the four types of
##  sources indicated by the type (point, nonpoint, onroad,
##  nonroad) variable, which of these four sources have seen
##  decreases in emissions from 1999–2008 for Baltimore City?
##  Which have seen increases in emissions from 1999–2008?"
##  This script uses the PM2.5 data set, and leverages the
##  ggplot plotting system to make a plot showing the PM2.5
##  emission from all sources in Baltimore City for the years
##  1999 and 2008.
##
## ********************************************************

# Load libraries
library(ggplot2)
library(tidyr)

# Load the data files (if they exist) and process the data
dataLocation <- "./data/exdata-data-NEI_data/"
PM2_5DataFileName <- "summarySCC_PM25.rds"

if (!(PM2_5DataFileName %in% dir(dataLocation))) {
        stop("Data file does not exist!")
}

PM2_5Data <- readRDS(paste0(dataLocation, PM2_5DataFileName))

# Subset to only Baltimore City
PM2_5Data <- subset(PM2_5Data, PM2_5Data$fips == "24510")

# Separate the data by year and type
PM2_5Means <- with(PM2_5Data, tapply(Emissions, list(type, year), mean))
PM2_5Means <- as.data.frame(PM2_5Means)
PM2_5Means$type <- rownames(PM2_5Means)
PM2_5Means <- gather(PM2_5Means, year, emissions, -type)

# Create a plot with means to compare data between 1999 and 2008
if (!file.exists("output")){
    dir.create("output")
}

# Store the graph as a '.png' file
png(filename = "./output/plot3.png")

plot3 <- ggplot(data = PM2_5Means) +
    geom_point(aes(year, emissions, col = type, size = 2)) +
    geom_segment(aes(x = c(PM2_5Means[year != "2008", ]$year, rep(NA, 4)),
                     y = c(PM2_5Means[year != "2008", ]$emissions, rep(NA, 4)),
                     xend = c(PM2_5Means[year != "1999", ]$year, rep(NA, 4)),
                     yend = c(PM2_5Means[year != "1999", ]$emissions, rep(NA, 4)),
                     col = type), na.rm = TRUE) +
    labs(title = "Comparison of PM2.5 Emissions in Baltimore City from 1999 to 2008", x = "Year",
    y = "Mean Emissions (tons)", subtitle = "Broken Out by Pollution Emission Type",
                     color = "PM2.5 Type") +
    guides(col = "legend", size = "none")

print(plot3)

dev.off()