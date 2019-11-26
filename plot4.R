################################################################################################################
# This script will produce a column plot by year from 1999 - 2008 of total emissions of PM2.5 from coal 
# combustion pollution types within the US.  Script will download data, if not already stored on computer.        
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
SCC <- readRDS("Source_Classification_Code.rds")

# Load Library
library(dplyr)
library(ggplot2)

# Filter Data
## Find coal sources with SCC and subset NEI dataframe
coal <- SCC[grepl("[Cc]oal", SCC$EI.Sector),]
coal.emissions <- NEI[(NEI$SCC %in% coal$SCC), ]
## Matching Sector Type to Data Frame by SCC code
coal.emissions.sub <- coal.emissions
coal.emissions.sub$EI.Sector <- SCC$EI.Sector[match(coal.emissions.sub$SCC, SCC$SCC)]
## Summarize Findings
total.coal.level.sector <- coal.emissions.sub %>%
  select(year, EI.Sector, Emissions) %>%
  group_by(year, EI.Sector) %>%
  summarise(Emissions = sum(Emissions))

## Clean Up Factor Names for Easier Viewing and Simplification
total.coal.level.sector$EI.Sector  <- gsub("-", "", total.coal.level.sector$EI.Sector)
total.coal.level.sector$EI.Sector <- gsub("Fuel Comb", "", total.coal.level.sector$EI.Sector)
total.coal.level.sector$EI.Sector <- gsub("Coal", "", total.coal.level.sector$EI.Sector)

# Generate Plot
png("plot4.png")
ggplot(total.coal.level.sector, aes(x = as.factor(year), y = (Emissions / 1000), fill = EI.Sector)) +
  geom_bar(stat = "identity") +
  labs(caption = "Total Coal Combustion emissions have decresed by -39.9%", title = expression("Total Coal PM"[2.5]*" Emissions")) +
  xlab("Year") + 
  ylab(expression("PM"[2.5]*" Emissions in Tons in Thousands")) +
  theme_minimal(base_family = "Avenir") +
  scale_fill_manual("Coal Sector", values = c("firebrick", "steelblue2", "yellow3"))
dev.off()
