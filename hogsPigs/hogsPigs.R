# Data from 2012 US Agricultural Census
# https://agcensus.usda.gov/Publications/2012/
library(ggplot2)
library(reshape2)

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

ggplot(hogPigs, aes(x = herdSize, y = hogPigs$number/1000000)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  xlab("Farm Size (Hogs/Pigs On Farm)") + ylab("Hogs/Pigs (in millions)") +
  ggtitle("Most hogs and pigs are on factory farms")

ggplot(hogPigs, aes(x = herdSize, y = hogPigs$farms/1000)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  xlab("Farm Size (Hogs/Pigs On Farm)") + ylab("Farms (in thousands)") +
  ggtitle("Most hogs and pig farms are small")

ggplot(hogPigsPlot, aes(x = herdSize, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  xlab("Hogs/Pigs On Farm") + ylab("Percent (%)") +
  ggtitle("Most farms are small, but most hogs/pigs are on factory farms") +
  scale_fill_discrete(name="",
                      breaks=c("Farms", "Livestock"),
                      labels=c("Percent of All Farms", "Percent of All Hogs/Pigs"))

# 95.79204% of hogs are on factory farms
# factory farm defined as 1000 hogs/pigs per farm
100*sum(hogPigs$number[7:9])/sum(hogPigs$number)

# 66026785 total hogs on farms
sum(hogPigs$number)

# 63248402 total hogs on factory farms
sum(hogPigs$number[7:9])