#downlowd, convert, import US Census of Agriculture Data into R

#set up
dir.create("raw")
dir.create("rImported")

#all files from USDA FTP quickstats site ftp://ftp.nass.usda.gov/quickstats/
#best way for of downloadable tabular census data I know of
#download to censusData
setwd("raw")
download.file("ftp://ftp.nass.usda.gov/quickstats/qs.census2002.txt.gz", "2002.txt.gz")
download.file("ftp://ftp.nass.usda.gov/quickstats/qs.census2007.txt.gz", "2007.txt.gz")
download.file("ftp://ftp.nass.usda.gov/quickstats/qs.census2012.txt.gz", "2012.txt.gz")

#convert to r
cag2002 <- read.table(gzfile("2002.txt.gz"), sep="\t", header=TRUE)
cag2007 <- read.table(gzfile("2007.txt.gz"), sep="\t", header=TRUE)
cag2012 <- read.table(gzfile("2012.txt.gz"), sep="\t", header=TRUE)

#save
setwd("../rImported")
save(cag2002,file="cag2002.Rda")
save(cag2007,file="cag2007.Rda")
save(cag2012,file="cag2012.Rda")