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

#Question nr. 5
scc.vehicles <- SCC[grep("Mobile.*Vehicles", SCC$EI.Sector),  ]; # Pattern match mobile vehicles in SCC description
scc.vehicles.list <- unique(scc.vehicles$SCC); # Create motor vehicle lookup list by SCC
nei.vehicles <- subset(NEI, SCC %in% scc.vehicles.list); # Filter for motor vehicle sources
nei.vehicles <- nei.vehicles %>% filter(fips == "24510") # Filter for Baltimore
nei.vehicles <- merge(x = nei.vehicles, y = scc.vehicles[, c("SCC", "SCC.Level.Two", "SCC.Level.Three")], by = "SCC") # Join in descriptive data on SCC codes
nei.vehicles <- nei.vehicles %>% group_by(year, SCC.Level.Two, SCC.Level.Three) %>% summarize(Annual.Total = sum(Emissions))
nei.vehicles.total <- nei.vehicles %>% group_by(year) %>% summarize(Annual.Total = sum(Annual.Total)) %>% mutate(SCC.Level.Two = "Total")
nei.vehicles <- bind_rows(nei.vehicles, nei.vehicles.total);
nei.vehicles$SCC.Level.Two <- factor(nei.vehicles$SCC.Level.Two, levels = c("Total", "Highway Vehicles - Diesel", "Highway Vehicles - Gasoline"));
ggplot(nei.vehicles, aes(x = factor(year), y = Annual.Total, fill = SCC.Level.Two)) +
        geom_bar(stat = "identity") +
        facet_grid(. ~ SCC.Level.Two) +
        xlab("Year") +
        ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
        ggtitle(expression(atop("Total Tons of PM"[2.5]*" Emissions in Baltimore City", paste("from Motor Vehicle Sources")))) +
        theme(plot.title = element_text(hjust = 0.5)) + # Center the plot title
        theme(plot.margin = unit(c(1,1,1,1), "cm")) + # Adjust plot margins
        scale_fill_brewer(palette = "Set1") +
        guides(fill = FALSE)