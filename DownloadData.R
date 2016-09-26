library(foreign)
library(ggplot2)
library(ggmap)
library(sp)
library(RgoogleMaps)

#### TO DO
#1. Turn CITY and COUNTY into meaningful names
#2. Overlay accidents on city maps
#3. Hypothesis: legalization of Marijuana in Colorado reduced the proportion of fatalities due to DD
#4. Need data from years before 2012
#5. Scale by population size


#Download the data files
for(i in 1975:2015){
  dir.create(paste("data/fars",i,sep=""))
  assign(paste("data/fars",i,"/zip.zip", sep=""),file())
  
  if((i >= 1975 & i <= 1993) | (i >= 2001 & i <= 2011)){
    file <- paste("ftp://ftp.nhtsa.dot.gov/fars/",i,"/DBF/FARS",i,".zip", sep="")
  }
  if(i >= 1994 & i <= 2000){
    file <- paste("ftp://ftp.nhtsa.dot.gov/fars/",i,"/DBF/FARSDBF",substr(i,3,4),".zip", sep="")
  }
  if(i == 2012){
    file <- paste("ftp://ftp.nhtsa.dot.gov/fars/",i,"/National/DBF/FARS",i,".zip", sep="")
  }
  if(i >= 2013){
    file <- paste("ftp://ftp.nhtsa.dot.gov/fars/",i,"/National/FARS",i,"NationalDBF.zip", sep="")
  }
  download.file(file, paste("data/fars",i,"/zip.zip", sep=""))
  unzip(paste("data/fars",i,"/zip.zip", sep=""),exdir=paste("data/fars",i,sep=""))
}

