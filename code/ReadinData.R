library(foreign)
library(plyr)
library(dplyr)
library(lubridate)

######################
## READ IN THE DATA ##
######################

path <- "../data"
for(i in 2000:2015){
   #person file
    if(i <= 1982 | i >= 1994){
        assign(paste0("person",i),read.dbf(paste0(path, "/fars",i,"/person.dbf")))
    }
    else{
        assign(paste0("person",i),read.dbf(paste0(path, "/fars",i,"/per",i,".dbf")))
    }
}

###################
## DATA CLEANING ##
###################

## concatenate all the person files, and add a year variable
person <- data.frame()
for(i in 2000:2015){
    text0 <- paste0("person",i,"$YEAR <- rep(",i,", dim(person",i,")[1])")
    eval(parse(text=text0))
    
    text <- paste0("person <- rbind.fill(person, person",i,")")
    eval(parse(text=text))
}

## get only info about drivers
drivers <- person %>% filter(PER_TYP==1)

## get ALC_RES for years prior to 2015 to agree with 2015 values
drivers$ALC_RES[drivers$YEAR < 2015] <- 10*drivers$ALC_RES[drivers$YEAR < 2015]

## get AGE for years prior to 2009, and make match years after that
drivers$AGE[drivers$YEAR <= 2008] <- ifelse(99, 999, drivers$AGE[drivers$YEAR <= 2008])

## add a variable indicating whether the driver was drunk
## drunk driving in south carolina is defined as BAC > .08
drivers <- dplyr::mutate(drivers, drunk=(DRINKING==1 | (ALC_RES >= 800 & ALC_RES <= 940)))

## missing data
#drunk
drivers$drunk <- ifelse((drivers$ALC_RES>940 & drivers$DRINKING %in% c(8,9)),
                        NA,drivers$drunk)
#day
drivers$DAY <- ifelse(drivers$DAY==99, 
                      NA,drivers$DAY)

##compute dates from year and month variables
drivers$DATE <- ymd(paste(drivers$YEAR, drivers$MONTH, drivers$DAY, sep="-"))








q