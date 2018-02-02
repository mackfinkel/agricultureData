#TRI animal ag waste by year

# download all TRI files from https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-2016
setwd('/Users/mackfinkel/Documents/wasteYears/TRI')

fileNames <- list.files()
TRI <- data.frame()

# load facility data
for (i in 1:length(fileNames)){
  currentTRI <- read.csv(fileNames[i])
  
  # select for animal agriculture
  currentTRI <- currentTRI[substr(as.character(currentTRI$PRIMARY_NAICS), 1, 4) == "3111" | substr(as.character(currentTRI$PRIMARY_NAICS), 1, 4) == "3115" | substr(as.character(currentTRI$PRIMARY_NAICS), 1, 4) == "3116",]
  
  # select variables of interest
  currentTRI <- currentTRI[,c("YEAR", 
                              "PRIMARY_NAICS", 
                              "CHEMICAL", 
                              "UNIT_OF_MEASURE", 
                              "ON.SITE_RELEASE_TOTAL",
                              "X8.6_TREATMENT_ON.SITE", 
                              "PARENT_COMPANY_NAME")]
  TRI <- rbind(TRI, currentTRI)
}

# waste = on site released + treated on site
TRI$waste <- TRI$ON.SITE_RELEASE_TOTAL + TRI$X8.6_TREATMENT_ON.SITE

# only look at facilities that have waste
TRI <- TRI[TRI$waste > 0,]

# sum facility waste by year and parent company
library(plyr)
TRIcleaner <- ddply(TRI, .(YEAR, PARENT_COMPANY_NAME), summarize,
                    waste = sum(waste, na.rm=TRUE))

arrange(TRIcleaner, desc(waste))



#if not one of these, call "other"
TRIcleaner$PARENT_COMPANY_NAME <- as.character(TRIcleaner$PARENT_COMPANY_NAME)
TRIcleaner$PARENT_COMPANY_NAME <- sub("[.]", "", TRIcleaner$PARENT_COMPANY_NAME)

#464 unique parent companies
length(unique(TRIcleaner$PARENT_COMPANY_NAME))

companies <- ddply(TRIcleaner, .(PARENT_COMPANY_NAME), summarize,
                   waste = sum(waste, na.rm=TRUE))
companies <- arrange(companies, desc(waste))

#get names of top 6 companies
companies <- companies$PARENT_COMPANY_NAME[!is.na(companies$PARENT_COMPANY_NAME)][1:6]

# also remove "springdale"
TRIcleaner$PARENT_COMPANY_NAME[grepl("SPRINGDALE", TRIcleaner$PARENT_COMPANY_NAME) | !grepl(gsub(", ", "|", toString(as.character(companies))), TRIcleaner$PARENT_COMPANY_NAME)] <- "OTHER"

TRIcleaner <- ddply(TRIcleaner, .(YEAR, PARENT_COMPANY_NAME), summarize,
                    waste = sum(waste, na.rm=TRUE))

TRIcleaner <- TRIcleaner[TRIcleaner$waste > 0,]

#capitalize 1st words
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#fix capitalization
TRIcleaner$PARENT_COMPANY_NAME <- as.character(sapply(tolower(TRIcleaner$PARENT_COMPANY_NAME), simpleCap))
TRIcleaner$PARENT_COMPANY_NAME <- gsub("Jbs", "JBS", TRIcleaner$PARENT_COMPANY_NAME)
TRIcleaner$PARENT_COMPANY_NAME <- gsub("Usa|Us", "USA", TRIcleaner$PARENT_COMPANY_NAME)

library(highcharter)

#make chart
wastePlot <- highchart() %>%
  hc_add_series(TRIcleaner, type = "area", stacking = "normal", hcaes(x = YEAR, y = round(waste), group = PARENT_COMPANY_NAME)) %>%
  hc_title(text = "Large Growth in Animal Agriculture Waste") %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Waste (lbs)")) %>%
  hc_tooltip(valueDecimals = 0, shape = "rectangle") %>%
  hc_legend(align = "left", verticalAlign = "top", layout = "vertical", floating = "true", x = 65, y = 34) %>%
  hc_credits(enabled = TRUE, text = "Source: Toxics Release Inventory", href = "https://www.epa.gov/toxics-release-inventory-tri-program")


