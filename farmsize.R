# Data from 2012 US Agricultural Census
# https://agcensus.usda.gov/Publications/2012/
library(ggplot2)
library(reshape2)

# cattle data from https://agcensus.usda.gov/Publications/2012/Full_Report/Volume_1,_Chapter_1_US/st99_1_014_016.pdf
herdSize = c("1 - 9","10 - 19","20 - 49","50 - 99", "100 - 199", "200 - 499", "500 - 999", "1000 - 2499", "2500 - 4999", "5000+")


cattleCalves <- data.frame(
  herdSize = factor(herdSize, levels=unique(herdSize)),
  farms =  c(243071, 171675, 222547, 118394, 76729, 52878, 17437, 7582, 1809, 1124),
  number = c(1179625, 2350565, 6907696, 8161882, 10456310, 15851268, 11881862, 11058595, 6121370, 16025441))

totalCattleCalves <- sum(cattleCalves$number)
totalCowFarms <- sum(cattleCalves$farms)
cattleCalves$Farms <- cattleCalves$farms/totalCowFarms*100
cattleCalves$Livestock <- cattleCalves$number/totalCattleCalves*100

cowPlot <- melt(cattleCalves[,c("herdSize", "Farms", "Livestock")])

ggplot(cowPlot, aes(x = herdSize, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  xlab("Animals Per Farm") + ylab("Percentage of Total") +
  ggtitle("Cows and Cow Farms in the U.S.") +
  scale_fill_hue(name="")

# dairy cattle data from https://agcensus.usda.gov/Publications/2012/Full_Report/Volume_1,_Chapter_1_US/st99_1_017_019.pdf
herdSize <- c("1 - 9","10 - 19","20 - 49","50 - 99", "100 - 199", "200 - 499", "500 - 999", "1000+")

dairy <- data.frame(
  herdSize = factor(herdSize, levels=unique(herdSize)),
  farms =  c(16463, 3762, 14107, 15351, 7359, 3712, 1537, 1807),
  number = c(591224, 170092, 1023375, 2117266, 1987432, 2094778, 1835324, 7695658))

totaldairy <- sum(dairy$number)
totalDairyFarms <- sum(dairy$farms)
dairy$Farms <- dairy$farms/totalDairyFarms*100
dairy$Livestock <- dairy$number/totaldairy*100

dairyPlot <- melt(dairy[,c("herdSize", "Farms", "Livestock")])

ggplot(dairyPlot, aes(x = herdSize, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  xlab("Animals Per Farm") + ylab("Percentage of Total") +
  ggtitle("Dairy Cows and Dairy Farms in the U.S.") +
  scale_fill_hue(name="")

# hog and pigs data from https://agcensus.usda.gov/Publications/2012/Full_Report/Volume_1,_Chapter_1_US/st99_1_017_019.pdf
herdSize = c("1 - 24","25 - 49","50 - 99","100 - 199", "200 - 499", "500 - 999", "1000 - 1999", "2000 - 4999", "5000+")

hogPigs <- data.frame(
  herdSize = factor(herdSize, levels=unique(herdSize)),
  farms =  c(41688, 3435, 2161, 1469, 2115, 1977, 2677, 4718, 3006),
  number = c(244250, 116808, 146967, 201460, 683977, 1384921, 3662168, 14867199, 44719035))

totalhogPigs <- sum(hogPigs$number)
totalHogFarms <- sum(hogPigs$farms)
hogPigs$Farms <- hogPigs$farms/totalHogFarms*100
hogPigs$Livestock <- hogPigs$number/totalhogPigs*100

hogPigsPlot <- melt(hogPigs[,c("herdSize", "Farms", "Livestock")])

ggplot(hogPigsPlot, aes(x = herdSize, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  xlab("Animals Per Farm") + ylab("Percentage of Total") +
  ggtitle("Hogs and Pigs and Pork Farms in the U.S.") +
  scale_fill_hue(name="")