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
library(ggplot2)

# Load the data files (if they exist) and process the data
dataLocation <- "./data/exdata-data-NEI_data/"
PM2_5DataFileName <- "summarySCC_PM25.rds"

if (!(PM2_5DataFileName %in% dir(dataLocation))) {
        stop("Data file does not exist!")
}

PM2_5Data <- readRDS(paste0(dataLocation, PM2_5DataFileName))

# Subset pollution data to only the on-road vehicle sources
PM2_5Data <- subset(PM2_5Data, type == "ON-ROAD")

# Include only Baltimore City (fips == "24510") and Los Angeles (fips == "06037")
PM2_5Data <- subset(PM2_5Data, fips == "24510" | fips == "06037")

# Calculate the mean emissions across years and regions
PM2_5Means <- with(PM2_5Data, tapply(Emissions, list(fips, year), mean, na.rm = TRUE))
PM2_5Means <- as.data.frame(PM2_5Means, row.names = c("Los Angeles", "Baltimore"))
PM2_5Means$location <- rownames(PM2_5Means)

# Calculate the change in emissions across years relative to 1999
PM2_5Means <- arrange(gather(PM2_5Means, year, emissions, -location), year)
PM2_5Means <- group_by(PM2_5Means, location)
PM2_5Means <- mutate(PM2_5Means, change = ((emissions - emissions[1]) / emissions[1]) +1)

# Create a plot with means to compare data between 1999 and 2008
if (!file.exists("output")){
    dir.create("output")
}

# Store the graph as a '.png' file
png(filename = "./output/plot6.png")

plot6 <- ggplot(data = PM2_5Means) +
    geom_point(aes(year, change, col = location, size = 2)) +
    geom_segment(aes(x = year, y = change, xend = lag(year, 2), yend = lag(change, 2),
                     col = location), na.rm = TRUE) +
    labs(title = "Relative Vehicle Emissions Change for Baltimore and Los Angeles", x = "Year",
         y = "Relative Vehicle Emissions Change",
         subtitle = "Baseline Year is 1999", color = "Location") +
    guides(col = "legend", size = "none")

print(plot6)

dev.off()