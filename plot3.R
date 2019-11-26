################################################################################################################
# This script will produce a column plot by year from 1999 - 2008 of total emissions of PM2.5 by indicated source 
# type of pollution within Baltimore, MD.  Script will download data, if not already stored on computer.        
# Plot is using the ggplot2 system in R.                                                                
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
SCC <- readRDS("Source_Classificgetation_Code.rds")

# Load Library
library(dplyr)
library(ggplot2)

# Filter Data
type <- NEI %>%
  filter(fips == "24510") %>%
  group_by(year, type) %>%
  summarise(Total.Emissions = sum(Emissions)) %>%
  arrange(type, year)

# Generate Plot
png("plot3.png")
g <- ggplot(type, aes(x = as.factor(year), y = Total.Emissions, fill = factor(type)))
g + 
  geom_col() + 
  facet_grid(.~type) +
  geom_text(aes(label = round(Total.Emissions)), size = 2.8, nudge_y = -50, fontface = "bold", color = "white") +
  labs(caption = "From 1999 to 2008, emissions changed -- NON-ROAD: -89%, NONPOINT: -35%, ON-ROAD: -75%, POINT: +16%", title = expression("PM"[2.5]*" Emissions by Type")) +
  xlab("Year") + 
  ylab(expression("PM"[2.5]*" Emissions in Tons")) +
  theme(axis.text.x = element_text(angle = 90, size = 11),
        axis.line = element_blank(),
        text = element_text(family = "Avenir", size = 11),
        legend.position = "none",
        panel.background = element_rect(fill = "whitesmoke", color = NA),
        panel.grid.major = element_line(color = "white", size = .5, linetype = "dashed"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "deepskyblue3"),
        strip.text = element_text(color = "white"))
dev.off()