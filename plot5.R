################################################################################################################
# This script will produce a column plot by year from 1999 - 2008 of total emissions of PM2.5 from motor 
# vehicle pollution within the Baltimore, MD.  Script will download data, if not already stored on computer.        
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
## Find Motor Vehicle Sources with SCC and subset within the NEI dataframe
mobile <- SCC[grepl("Mobile - On-Road", SCC$EI.Sector),]
## Match Sector and Detail info to data frame
balt.auto <- NEI[(NEI$SCC %in% mobile$SCC), ]
balt.auto$EI.Sector <- SCC$EI.Sector[match(balt.auto$SCC, SCC$SCC)]
balt.auto <- balt.auto %>%
  filter(fips == "24510") %>%
  group_by(year, EI.Sector) %>%
  select(year, EI.Sector, Emissions)
## Clean up Sector Factors for Simplification and Grouping, then Summarize Findings
balt.auto.totals <- balt.auto %>%
  group_by(year, EI.Sector) %>%
  summarise(Emissions = sum(Emissions)) %>%
  mutate(EI.Sector = gsub("Mobile - On-Road", "", EI.Sector)) %>%
  mutate(EI.Sector = gsub("Vehicles", "", EI.Sector))

# Generate Plot
png("plot5.png")
ggplot(balt.auto.totals, aes(x = as.factor(year), y = Emissions, fill = EI.Sector)) +
  geom_col() +
  labs(caption = "Baltimore vehicle emissions decreased by -74.5%. Biggest decline within Diesel Heavy Duty Vehicles (-81.5%)", 
       title = expression("Total Motor Vehicle PM"[2.5]*" Emissions - Baltimore, MD and by Type")) +
  xlab("Year") + 
  ylab(expression("PM"[2.5]*" Emissions in Tons")) +
  theme_minimal(base_family = "Avenir") +
  scale_fill_manual("", values = c("aquamarine3", "yellow", "orange", "dodgerblue2")) +
  theme(legend.position = "top")
dev.off()