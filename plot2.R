################################################################################################################
# This script will produce a column plot by year from 1999 - 2008 of total emissions of PM2.5 from all sources 
# of pollution within Baltimore, MD.  Script will download data, if not already stored on computer.        
# Plot is using the base plotting system in R.                                                                
################################################################################################################

# Download Data Set
if(!file.exists("summarySCC_PM25.rds") | !file.exists("Source_Classification_Code.rds")) {
  library(zip)
  dir.create("EDAWeek4Project")
  fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  filename <- "exdata_data_NEI_data.zip"
  download.file(fileURL, destfile = paste("./EDAWeek4Project/", filename, sep = ""), method = "curl")
  setwd("./EDAWeek4Project")
  unzip(filename)
}

# Read Data
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")

# Load Library
library(dplyr)

# Filter Data Set
balt <- NEI %>%
  group_by(year, fips) %>%
  filter(fips == "24510") %>%
  summarise(Total.Emissions = round(sum(Emissions) / 1000, 2))

# Generate Plot
png("plot2.png")
par(family = "Avenir", cex.main = 1.5, col.lab = "firebrick", col.main = "black", cex.sub = .8)
bp <- barplot(balt$Total.Emissions, names.arg = balt$year, 
              border = "whitesmoke",
              ylim = c(0, 6),
              main = expression("Total Baltimore PM"[2.5] * " Emissions by Year"),
              ylab = expression("PM"[2.5] * " in tons per thousands"),
              col = "orange2")
text(bp, balt$Total.Emissions + 0.3, labels = balt$Total.Emissions, cex = 1)
title(sub = expression("Baltimore, MD PM"[2.5] * " emissions from 1999 to 2008 have decreased by -43.1%"))
dev.off()