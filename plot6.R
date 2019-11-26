################################################################################################################
# This script will produce a column plot by year from 1999 - 2008 of total emissions of PM2.5 from motor 
# vehical pollution within the Baltimore, MD.  Script will download data, if not already stored on computer.        
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
city.compare <- NEI[(NEI$SCC %in% mobile$SCC), ]
city.compare$EI.Sector <- SCC$EI.Sector[match(city.compare$SCC, SCC$SCC)]
## Cleanup EI.Sector Factors for Simplification and Viewing, then Filter by Baltimore and Los Angeles
city.compare <- city.compare %>%
  filter(fips == "24510" | fips == "06037") %>%
  mutate(EI.Sector = gsub("Mobile - On-Road", "", EI.Sector)) %>%
  mutate(EI.Sector = gsub("Vehicles", "", EI.Sector)) %>%
  mutate(EI.Sector = gsub("Heavy Duty", "", EI.Sector)) %>%
  mutate(EI.Sector = gsub("Light Duty", "", EI.Sector)) %>%
  mutate(fips = gsub("24510", "Baltimore", fips)) %>%
  mutate(fips = gsub("06037", "Los Angeles", fips)) %>%
  select(year, fips, EI.Sector, Emissions) %>%
  group_by(year, fips, EI.Sector) %>%
  summarise(Emissions = sum(Emissions))

# Generate Plot
png("plot6.png")
ggplot(city.compare, aes(x = as.factor(year), y = Emissions, fill = EI.Sector)) +
  facet_grid(fips ~., scales = "free") +
  geom_col() +
  labs(caption = "Los Angeles increased motor vehicle emissions by +4.4%, while Baltimore declined emissions by -74.5%", 
       title = expression("Total Motor Vehicle PM"[2.5]*" Emissions - Baltimore/Los Angeles by Gas")) +
  xlab("Year") + 
  ylab(expression("PM"[2.5]*" Emissions in Tons")) +
  theme_light(base_family = "Avenir") +
  scale_fill_manual("", values = c("coral2", "firebrick")) +
  theme(legend.position = "top",
        strip.background = element_rect(fill = "firebrick4"),
        strip.text = element_text(color = "white"))
dev.off()