# farm waste, poverty, and race

setwd('/Users/mackfinkel/Documents/waste/')

#Census data source: Steven Manson, Jonathan Schroeder, David Van Riper, and Steven Ruggles. IPUMS National Historical Geographic Information System: Version 12.0 [Database]. Minneapolis: University of Minnesota. 2017. http://doi.org/10.18128/D050.V12.0
#tri files: https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-2016

#load census data, CPS five year estimates 2011-2015
popData <- read.csv('nhgis0003_csv/nhgis0003_ds215_20155_2015_blck_grp.csv')
popDataClean <- popData[,c("GISJOIN", "STATE", "STATEA", "COUNTY", "COUNTYA", "TRACTA", "BLKGRPA", "ADKJE001", "ADKWE001", "ADKXE002", "ADKXE003", "ADKXE004", "ADKXE005", "ADKXE006", "ADKXE007", "ADKXE008", "ADKXE009", "ADKXE010", "ADNEE001", "ADNEE002", "ADNEE003", "ADNEE004", "ADNEE005", "ADNEE006", "ADNEE007", "ADNEE008", "ADSBE002", "ADSBE017", "ADSBE018", "ADSBE033", "ADSBE034", "ADSBE050", "ADSBE051", "ADSBE066" ,"ADOVE001")]
popDataClean <- popDataClean[-1,]

# load TRI data
setwd('TRI/')
fileNames <- list.files()
TRI <- data.frame()
# load facility data from 2011 - 2015
for (i in 1:length(fileNames)){
  currentTRI <- read.csv(fileNames[i])
  
  # select for animal agriculture
  currentTRI <- currentTRI[substr(as.character(currentTRI$PRIMARY_NAICS), 1, 4) == "3111" | substr(as.character(currentTRI$PRIMARY_NAICS), 1, 4) == "3115" | substr(as.character(currentTRI$PRIMARY_NAICS), 1, 4) == "3116",]
  
  # select variables of interest
  currentTRI <- currentTRI[,c("YEAR", 
                              "FACILITY_NAME", 
                              "STREET_ADDRESS", 
                              "CITY", 
                              "COUNTY", 
                              "ST", 
                              "ZIP", 
                              "LATITUDE", 
                              "LONGITUDE", 
                              "PRIMARY_NAICS", 
                              "CHEMICAL", 
                              "UNIT_OF_MEASURE", 
                              "ON.SITE_RELEASE_TOTAL", 
                              "X5.5.2_LAND_TREATMENT", 
                              "X5.2_STACK_AIR", 
                              "X5.1_FUGITIVE_AIR", 
                              "X5.3_WATER", 
                              "OFF.SITE_RELEASE_TOTAL", 
                              "TOTAL_RELEASES", 
                              "X8.1B_ON.SITE_OTHER_RELEASES",
                              "X8.6_TREATMENT_ON.SITE", 
                              "PARENT_COMPANY_NAME")]
  TRI <- rbind(TRI, currentTRI)
}
# waste = on site released + treated on site
TRI$waste <- TRI$ON.SITE_RELEASE_TOTAL + TRI$X8.6_TREATMENT_ON.SITE

# only look at facilities that have waste
TRI <- TRI[TRI$waste > 0,]

# sum facility waste over 5 year period
library(plyr)
TRIcleaner <- ddply(TRI, .(LATITUDE, LONGITUDE, PARENT_COMPANY_NAME, COUNTY, STREET_ADDRESS, CITY, ST, ZIP), summarize,
                  ON.SITE_RELEASE_TOTAL = sum(ON.SITE_RELEASE_TOTAL, na.rm=TRUE),
                  X5.5.2_LAND_TREATMENT = sum(X5.5.2_LAND_TREATMENT, na.rm=TRUE),
                  X5.2_STACK_AIR = sum(X5.2_STACK_AIR, na.rm=TRUE),
                  X5.1_FUGITIVE_AIR = sum(X5.1_FUGITIVE_AIR, na.rm=TRUE),
                  X5.3_WATER = sum(X5.3_WATER, na.rm=TRUE),
                  TOTAL_RELEASES = sum(TOTAL_RELEASES, na.rm=TRUE),
                  X8.1B_ON.SITE_OTHER_RELEASES = sum(X8.1B_ON.SITE_OTHER_RELEASES, na.rm=TRUE),
                  X8.6_TREATMENT_ON.SITE = sum(X8.6_TREATMENT_ON.SITE, na.rm=TRUE))


library(geosphere)

library(foreign)
# load block group data from census CPS 2015
setwd('/Users/mackfinkel/Documents/waste/')
blckGrp <- read.dbf('nhgis0001_shape/nhgis0001_shapefile_tl2015_us_blck_grp_2015/US_blck_grp_2015.dbf')

# get variables of interest
blckGrp <- blckGrp[,c("INTPTLAT", "INTPTLON", "ALAND", "Shape_Area", "GISJOIN")]

# merge census data
blckPop <- merge(blckGrp, popDataClean, by=c("GISJOIN","GISJOIN"))

# convert longitude and latitude from factors to numbers
blckPop$INTPTLON <- as.numeric(as.character(blckPop$INTPTLON))
blckPop$INTPTLAT <- as.numeric(as.character(blckPop$INTPTLAT))

blckPop$wasteProx <- 0

# cycle through facilities to determine waste and add to block level data
for (i in 1:length(TRIcleaner$ST)){
  print(i)
  
  # determine which facilities are less than 3 miles away from a waste producing facility
  if (is.na(TRIcleaner$LATITUDE[i]) | is.na(TRIcleaner$LONGITUDE[i])) next
  idx <- abs(TRIcleaner$LATITUDE[i] - blckPop$INTPTLAT) < 1 & abs(TRIcleaner$LONGITUDE[i] - blckPop$INTPTLON) < 1
  if (any(idx)==FALSE) next
  dist <- distm(blckPop[idx,c("INTPTLON", "INTPTLAT")], TRIcleaner[i,c("LONGITUDE", "LATITUDE")])
  dist <- (dist/1609.344)
  idx[which(idx == TRUE)] <- dist < 3
  
  # determine this facility's waste
  waste <- (TRIcleaner$ON.SITE_RELEASE_TOTAL[i] + TRIcleaner$X8.6_TREATMENT_ON.SITE[i])
  
  # multiply waste by proximity
  wasteProx <- waste*(3 - dist[dist < 3])
  
  # add this facility's contribution to already calculated waste for this census block group
  blckPop$wasteProx[idx] <- blckPop$wasteProx[idx] + wasteProx
  
}
# blckPop$biggestIdx <- NULL

#total population
blckPop$totalPop <- as.numeric(as.character(blckPop$ADKWE001))
  
#white population
blckPop$whitePct <- 100 * as.numeric(as.character(blckPop$ADKXE002))/blckPop$totalPop

#population in poverty
blckPop$povertyPct <- 100 * (as.numeric(as.character(blckPop$ADNEE002)) + as.numeric(as.character(blckPop$ADNEE003)))/blckPop$totalPop

# aggregate income of population
blckPop$income <- blckPop$ADOVE001
blckPop$ADOVE001 <- NULL

# index which block groups are near waste
blckPop$noWaste <- FALSE
blckPop$noWaste[blckPop$wasteProx == 0] <- TRUE

library(gridExtra)

#income ADOVE001
#convert income to income per person
blckPop$incomePop <- as.numeric(as.character(blckPop$income))/as.numeric(as.character(blckPop$totalPop))

#get income percentile
blckPop$incomePopPctile <- 100*rank(blckPop$incomePop)/length(blckPop$incomePop)

#white percentile
blckPop$whitePctile <- 100*rank(blckPop$whitePct)/length(blckPop$whitePct)

#black percentile
blckPop$blackPctile <- 100*rank(blckPop$blackPct)/length(blckPop$blackPct)

#round percentiles
blckPop$incomeRounded <- round(blckPop$incomePopPctile)
blckPop$whiteRounded <- round(blckPop$whitePctile)
blckPop$blackRounded <- round(blckPop$blackPctile)

# get nearby waste values by income percentile
incomePollution <- ddply(blckPop, .(incomeRounded), summarize,
         wasteProxSum = sum(wasteProx, na.rm=TRUE),
         wasteProxMean = mean(wasteProx, na.rm=TRUE),
         wasteNear = sum(!noWaste, na.rm = TRUE))

# get nearby waste values by white percentile
whitePollution <- ddply(blckPop, .(whiteRounded), summarize,
                         wasteProxSum = sum(wasteProx, na.rm=TRUE),
                         wasteProxMean = mean(wasteProx, na.rm=TRUE),
                        wasteNear = sum(!noWaste, na.rm = TRUE))


# get linear regression fit and 95% CI
modelIncome <- lm(wasteProxSum ~ incomeRounded, data = incomePollution)
incomePollution[,c("median", "0.95LCL", "0.95UCL")] <- predict(modelIncome,incomePollution, interval='confidence', level=0.95)
incomePollution$incomeRounded <- 1:length(incomePollution$incomeRounded)

modelWhite <- lm(wasteProxSum ~ whiteRounded, data = whitePollution)
whitePollution[,c("median", "0.95LCL", "0.95UCL")] <- predict(modelWhite,whitePollution, interval='confidence', level=0.95)
whitePollution$whiteRounded <- 1:length(whitePollution$whiteRounded)

modelIncomeNear <- lm(wasteNear ~ incomeRounded, data = incomePollution)
incomePollution[,c("medianNear", "0.95LCLNear", "0.95UCLNear")] <- predict(modelIncomeNear,incomePollution, interval='confidence', level=0.95)
incomePollution$incomeRounded <- 1:length(incomePollution$incomeRounded)

modelWhiteNear <- lm(wasteNear ~ whiteRounded, data = whitePollution)
whitePollution[,c("medianNear", "0.95LCLNear", "0.95UCLNear")] <- predict(modelWhiteNear,whitePollution, interval='confidence', level=0.95)
whitePollution$whiteRounded <- 1:length(whitePollution$whiteRounded)

#cut 0 off the bottom CI range because there's really no point
incomePollution[incomePollution < 0] <- 0
whitePollution[whitePollution < 0] <- 0

library(highcharter)

#incomehist
highchart() %>% 
  hc_add_series_labels_values(incomePollution$incomeRounded, incomePollution$wasteNear, name = "Nearby facilities", type = "scatter") %>% 
  hc_title(text = "More animal agriculture facilities in poorer areas") %>%
  hc_xAxis(title = list(text = "Community income percentile"), data = incomePollution$incomeRounded) %>%
  hc_yAxis(title = list(text = "Waste-producing facilities")) %>%
  hc_add_series(incomePollution$`0.95UCLNear`, name = "95% CI (high)", type = "line", color = "#868e96")  %>%
  hc_add_series(incomePollution$medianNear, name = "Predicted value", type = "line", color = "#343a40")  %>%
  hc_add_series(incomePollution$`0.95LCLNear`, name = "95% CI (low)", type = "line", color = "#868e96")  %>%
  hc_tooltip(valueDecimals = 0, shape = "rectangle", shared = TRUE, table = TRUE) %>%
  hc_legend(enabled = FALSE) %>%
  hc_credits(enabled = TRUE, text = "Source: CPS & TRI, 2011-2015")

#racehist
highchart() %>% 
  hc_add_series_labels_values(whitePollution$whiteRounded, whitePollution$wasteNear, name = "Nearby facilities", type = "scatter") %>%
  hc_title(text = "More animal agriculture faciltiies in non-white areas") %>%
  hc_xAxis(title = list(text = "Community white percentile"), data = whitePollution$incomeRounded) %>%
  hc_yAxis(title = list(text = "Waste-producing facilities")) %>%
  hc_add_series(whitePollution$`0.95UCLNear`, name = "95% CI (high)", type = "line", color = "#868e96", shape = "diamond")  %>%
  hc_add_series(whitePollution$medianNear, name = "Predicted value", type = "line", color = "#343a40")  %>%
  hc_add_series(whitePollution$`0.95LCLNear`, name = "95% CI (low)", type = "line", color = "#868e96", shape = "diamond")  %>%
  hc_tooltip(valueDecimals = 0, shape = "rectangle", shared = TRUE, table = TRUE) %>%
  hc_legend(enabled = FALSE) %>%
  hc_credits(enabled = TRUE, text = "Source: CPS & TRI, 2011-2015")

#plot race and nearby waste count
highchart() %>%
  hc_title(text = "Varied animal agriculture waste in (non)white areas") %>%
  hc_xAxis(title = list(text = "Community white percentile"), data = whitePollution$whiteRounded) %>%
  hc_yAxis(title = list(text = "Total nearby waste (lbs)")) %>%
  hc_add_series(whitePollution$wasteProxSum, name = "Waste nearby", type = "scatter")  %>%
  hc_tooltip(valueDecimals = 0, shape = "rectangle", shared = TRUE, table = TRUE) %>%
  hc_legend(enabled = FALSE) %>%
  hc_credits(enabled = TRUE, text = "Source: CPS & TRI, 2011-2015")

#plot income and nearby waste count
highchart() %>%
  hc_title(text = "More animal agriculture waste in poorer communities") %>%
  hc_xAxis(title = list(text = "Community income percentile"), data = incomePollution$incomeRounded) %>%
  hc_yAxis(title = list(text = "Total nearby waste (lbs)")) %>%
  hc_add_series(incomePollution$wasteProxSum, name = "Waste nearby", type = "scatter")  %>%
  hc_add_series(incomePollution$`0.95UCL`, name = "95% CI (high)", type = "line", color = "#868e96", shape = "diamond")  %>%
  hc_add_series(incomePollution$median, name = "Predicted value", type = "line", color = "#343a40")  %>%
  hc_add_series(incomePollution$`0.95LCL`, name = "95% CI (low)", type = "line", color = "#868e96", shape = "diamond")  %>%
  hc_tooltip(valueDecimals = 0, shape = "rectangle", shared = TRUE, table = TRUE) %>%
  hc_legend(enabled = FALSE) %>%
  hc_credits(enabled = TRUE, text = "Source: CPS & TRI, 2011-2015")


worseThanDuplin <- blckPop[blckPop$wasteProx >= 635196  & blckPop$incomePopPctile <= 9,]
worseThanDuplin$FIPS <- paste0(worseThanDuplin$STATEA, worseThanDuplin$COUNTYA, worseThanDuplin$TRACTA, worseThanDuplin$BLKGRPA)

#duplin chart
highchart() %>%
  hc_title(text = "Areas with more waste and less income than Duplin") %>%
  hc_xAxis(title = list(text = "Community income percentile")) %>%
  hc_yAxis(title = list(text = "Total nearby waste (log(1+lbs))")) %>%
  hc_add_series_scatter(round(worseThanDuplin$incomePopPctile,1), log1p(worseThanDuplin$wasteProx), name = "waste", type = "scatter",
                        COUNTY = worseThanDuplin$COUNTY, whitePctile = worseThanDuplin$whitePctile, totalPop = worseThanDuplin$totalPop, FIPS = worseThanDuplin$FIPS, STATE = worseThanDuplin$STATE, WASTE = worseThanDuplin$wasteProx)  %>%
  hc_tooltip(valueDecimals = 2, shape = "rectangle",
             headerFormat = "",
             pointFormat = '<i>{point.COUNTY}, {point.STATE}</i></small><br/>Income Percentile: <b>{point.x:.1f}</b><br/>White Percentile: <b>{point.whitePctile:.1f}</b><br/>Population: <b>{point.totalPop}</b><br/>Nearby waste (lbs): <b>{point.WASTE:.0f}</b><br/>FIPS: <b>{point.FIPS}</b>') %>%
  hc_legend(enabled = FALSE) %>%
  hc_credits(enabled = TRUE, text = "Source: CPS & TRI, 2011-2015")

