---
title: "Exploratory Data Analysis Project 2"
author: "Ceyda"
date: "14 09 2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Loading Packages and Data:

```{r, echo = TRUE, warning=FALSE, message = FALSE}
library(RColorBrewer)
library(dplyr)
library(ggplot2)


NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

Sys.setlocale("LC_TIME", "English")

head(NEI)
head(SCC)
```

### Question 1 (plot1.R)

Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.


```{r, echo = TRUE, warning=FALSE, message = FALSE}
total_emission <- aggregate(Emissions ~ year, NEI, FUN = sum)

coul <- brewer.pal(5, "Set2") 
barplot(
    (height = total_emission$Emissions)/10^6,
    names.arg = total_emission$year,
    col= coul,
    main = "Total PM2.5 Emission Across US States",
    xlab = "Year",
    ylab = "Total PM2.5 Emission (10^6 Tons)"
)

dev.copy(png, file="plot1.png", height=480, width=640)
dev.off()
```

## Question 2 (plot2.R)

Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (\color{red}{\verb|fips == "24510"|}fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.

```{r, echo = TRUE, warning=FALSE, message = FALSE}
total_emission_baltimore <- NEI %>% 
    filter(fips == "24510")
    
total_emission <- aggregate(Emissions ~ year, total_emission_baltimore, FUN = sum)

coul <- brewer.pal(5, "Set2") 
barplot(
    height = total_emission$Emissions,
    names.arg = total_emission$year,
    col= coul,
    main = "Total PM2.5 Emission From Baltimore City Sources",
    xlab = "Year",
    ylab = "Total PM2.5 Emission (in Tons)"
)

dev.copy(png, file="plot2.png", height=480, width=640)
dev.off()

```

### Question 3 (plot3.R)

Of the four types of sources indicated by the \color{red}{\verb|type|}type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

```{r, echo = TRUE, warning=FALSE, message = FALSE}
total_emission_baltimore <- NEI %>% 
    filter(fips == "24510") %>% 
    group_by(year, type) %>% 
    summarise(baltimore_emission = sum(Emissions))

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73")
BaltimoreEmission <- ggplot(data = total_emission_baltimore,
                            aes(x = factor(year), y= baltimore_emission,
                            fill = type, colore = "black")) + 
    geom_bar(stat = "identity") + 
    scale_fill_manual(values=cbPalette) +
    facet_grid(. ~ type) +
    xlab("Year") +
    ylab("Total PM2.5 Emission (in Tons)") +
    ggtitle("Baltimore Emissions by Source Type") +
    theme_bw()

print(BaltimoreEmission)
dev.copy(png, file="plot3.png", height=480, width=640)
dev.off()

```

### Question 4 (plot4.R)

Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

```{r, echo = TRUE, warning=FALSE, message = FALSE}
combustionRelated <- grepl("comb", SCC$SCC.Level.One, ignore.case=TRUE)
coalRelated <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE) 
coalCombustion <- (combustionRelated & coalRelated)
combustionSCC <- SCC[coalCombustion,]$SCC
combustionNEI <- NEI[NEI$SCC %in% combustionSCC,]

coalcomb_emissions_sum <- combustionNEI %>% 
    group_by(year) %>%
    summarise(coalcomb_emission = sum(Emissions))

CoalCombEmission <- ggplot(data = coalcomb_emissions_sum,
                            aes(x = factor(year), y= coalcomb_emission/10^5,
                                fill = year, label = round(coalcomb_emission/10^5))) + 
    geom_bar(stat = "identity") + 
    geom_label(aes(fill = year),colour = "white", fontface = "bold") +
    xlab("Year") +
    ylab("Total PM2.5 Emission (10^5 Tons)") +
    ggtitle("Coal Combustion-related Sources Changed from 1999 to 2008") +
    theme_bw()

print(CoalCombEmission)
dev.copy(png, file="plot4.png", height=480, width=640)
dev.off()

```

### Question 5 (plot5.R)

How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

```{r, echo = TRUE, warning=FALSE, message = FALSE}
motorsources <- SCC[grep("[Vv]ehicle", SCC$EI.Sector), "SCC"]
vehicle_emissions <- NEI %>% 
    filter(SCC %in% motorsources & fips == "24510") %>%
    group_by(year) %>%
    summarise(BaltimoreVehicleEmissions = sum(Emissions))

BaltimoreEmission <- ggplot(data = vehicle_emissions,
                            aes(x = factor(year), y = BaltimoreVehicleEmissions,
                                fill = year, label = round(BaltimoreVehicleEmissions))) + 
    geom_bar(stat = "identity") + 
    geom_label(aes(fill = year),colour = "white", fontface = "bold") +
    xlab("Year") +
    ylab("Total PM2.5 Emission (in Tons)") +
    ggtitle("Baltimore Vehicle Emissions") +
    theme_bw()

print(BaltimoreEmission)

dev.copy(png, file="plot5.png", height=480, width=640)
dev.off()


```

### Question 6 (plot6.R)

Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (\color{red}{\verb|fips == "06037"|}fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?


```{r, echo = TRUE, warning=FALSE, message = FALSE}
vehicles <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
vehiclesSCC <- SCC[vehicles,]$SCC
vehiclesNEI <- NEI[NEI$SCC %in% vehiclesSCC,]

baltimore <- vehiclesNEI %>% 
    filter(fips == "24510") %>% 
    group_by(year) %>% 
    mutate(city = "Baltimore")

los_angeles <- vehiclesNEI %>% 
    filter(fips == "06037") %>% 
    group_by(year) %>% 
    mutate(city = "Los Angeles")

bothcities <- rbind(baltimore,los_angeles)

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73")
BaltimoreLosAngelesEmission <- ggplot(data = bothcities,
                           aes(x = factor(year), y= Emissions,
                               fill = factor(year))) + 
    geom_bar(stat = "identity") + 
    scale_fill_manual(values=cbPalette) +
    facet_grid(scales="free", space="free", .~city) +
    xlab("Year") +
    ylab("Total PM2.5 Emission (Kilo-Tons)") +
    labs(fill="Year") +
    ggtitle("Motor Vehicle Source Emissions in Baltimore and Los Angeles from 1999 to 2008") +
    theme_bw()

print(BaltimoreLosAngelesEmission)
dev.copy(png, file="plot6.png", height=480, width=640)
dev.off()
```
