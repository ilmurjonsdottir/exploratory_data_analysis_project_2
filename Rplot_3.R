#Load packages
library(dplyr)
library(ggplot2)
library(scales)
library(data.table)

#Setwd
setwd("~/Desktop/R_Programming/Working_Directory/exdata_data_NEI_data")

#Input the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Exploreing the data
head(NEI)

str(NEI)

head(SCC)

str(SCC)

#Question nr 3
nei.baltimore <- NEI %>% filter(fips == "24510") %>% group_by(type, year) %>% summarize(Annual.Total = sum(Emissions));
nei.baltimore$type <- factor(nei.baltimore$type, levels = c("ON-ROAD", "NON-ROAD", "POINT", "NONPOINT")) # Re-order factor levels so they plot in the order we wish
ggplot(nei.baltimore, aes(x = factor(year), y = Annual.Total, fill = type)) + 
        geom_bar(stat = "identity") + 
        facet_grid(. ~ type) + 
        xlab("Year") + 
        ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
        ggtitle(expression("Total Tons of PM"[2.5]*" Emissions in Baltimore by Source Type")) +
        theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        scale_y_continuous(labels = comma) +
        guides(fill = FALSE)