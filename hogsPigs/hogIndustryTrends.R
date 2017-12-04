setwd('/Users/mackfinkel/Documents/agricultureData/')
library(tidyr)
library(highcharter)

#hog farms by farm size https://quickstats.nass.usda.gov/results/4BA25EA7-5C50-3F50-9AC7-2E5206668DA3
hogsFarms <- read.csv('farms/hogs.csv')
hogsFarmsClean <- hogsFarms[,which(colnames(hogsFarms) == "Year" | colnames(hogsFarms) == "Domain.Category" | colnames(hogsFarms) == "Value")]
hogsFarmsClean$Domain.Category <- as.character(hogsFarmsClean$Domain.Category)
hogsFarmsClean$Domain.Category <- sub("INVENTORY OF HOGS: \\(", "", hogsFarmsClean$Domain.Category)
hogsFarmsClean$Domain.Category <- sub(" HEAD?\\)", "", hogsFarmsClean$Domain.Category)
hogsFarmsClean$Domain.Category <- sub(" TO ", " - ", hogsFarmsClean$Domain.Category)
hogsFarmsClean$Domain.Category <- sub(" OR MORE", "+", hogsFarmsClean$Domain.Category)

# remove superfluous data columns
hogsFarmsClean <- hogsFarmsClean[hogsFarmsClean$Domain.Category != "1,000+" & hogsFarmsClean$Domain.Category != "500+",]

#for each year, sort data based on first number of bin size
yearToSort <- substr(hogsFarmsClean$Domain.Category,0,regexpr("-|[+]",hogsFarmsClean$Domain.Category)-1)
hogsFarmsClean$YearSort <- as.numeric(gsub(",", "", yearToSort))
hogsFarmsClean <- hogsFarmsClean[order(hogsFarmsClean$Year, hogsFarmsClean$YearSort),]

#convert numbers to actual numbers
hogsFarmsClean$Value <- as.numeric(sub(",", "", as.character(hogsFarmsClean$Value)))

#chart hog farms by farm inventory
hogsFarms <- hchart(hogsFarmsClean, "column", hcaes(x = Domain.Category, y = Value, group = Year)) %>% 
  hc_title(
    text = "The amount of small hog farms is decreasing") %>% 
  hc_credits(
    enabled = TRUE,
    text = "Source: Census of Agriculture",
    href = "https://www.agcensus.usda.gov/index.php") %>%
  # Axis
  hc_yAxis(
    title = list(text = "Farms")
  ) %>% 
  hc_xAxis(
    title = list(text = "Farm Size (Hogs on Farm)")
  ) %>% 
  hc_tooltip(shared = TRUE, table = TRUE
  )

#hogs by farm inventory https://quickstats.nass.usda.gov/results/1DC5F644-B1F3-3FF3-9EFC-7D01ACD4953E
hogsInventory <- read.csv('inventory/hogs.csv')
hogsInventoryClean <- hogsInventory[,which(colnames(hogsInventory) == "Year" | colnames(hogsInventory) == "Domain.Category" | colnames(hogsInventory) == "Value")]
hogsInventoryClean$Domain.Category <- as.character(hogsInventoryClean$Domain.Category)
hogsInventoryClean$Domain.Category <- sub("INVENTORY OF HOGS: \\(", "", hogsInventoryClean$Domain.Category)
hogsInventoryClean$Domain.Category <- sub(" HEAD?\\)", "", hogsInventoryClean$Domain.Category)
hogsInventoryClean$Domain.Category <- sub(" TO ", " - ", hogsInventoryClean$Domain.Category)
hogsInventoryClean$Domain.Category <- sub(" OR MORE", "+", hogsInventoryClean$Domain.Category)

#remove superfluous data
hogsInventoryClean <- hogsInventoryClean[hogsInventoryClean$Domain.Category != "1,000+" & hogsInventoryClean$Domain.Category != "500+",]

#for each year, sort data based on first number of bin size
yearToSort <- substr(hogsInventoryClean$Domain.Category,0,regexpr("-|[+]",hogsInventoryClean$Domain.Category)-1)
hogsInventoryClean$YearSort <- as.numeric(sub(",", "", yearToSort))
hogsInventoryClean <- hogsInventoryClean[order(hogsInventoryClean$Year, hogsInventoryClean$YearSort),]

#convert numbers to actual numbers
hogsInventoryClean$Value <- as.numeric(gsub(",", "", as.character(hogsInventoryClean$Value)))

#chart hogs by hog farm size
hogsInventory <- hchart(hogsInventoryClean, "column", hcaes(x = Domain.Category, y = Value, group = Year)) %>% 
  hc_title(
    text = "An increasing majority of hogs is factory farmed") %>% 
  hc_credits(
    enabled = TRUE,
    text = "Source: Census of Agriculture",
    href = "https://www.agcensus.usda.gov/index.php") %>%
  # Axis
  hc_yAxis(
    title = list(text = "Hogs")
  ) %>% 
  hc_xAxis(
    title = list(text = "Farm Size (Hogs on Farm)")
  ) %>% 
  hc_tooltip(shared = TRUE, table = TRUE
  )