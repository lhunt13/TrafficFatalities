library(foreign)
library(plyr)
library(dplyr)

## Read in accident and person data
path <- "/Users/lamarhuntiii/Documents/Second Year Classes/Data Science/data"
for(i in 2008:2015){
   #person file
    if(i <= 1982 | i >= 1994){
        assign(paste0("person",i),read.dbf(paste0(path, "/fars",i,"/person.dbf")))
    }
    else{
        assign(paste0("person",i),read.dbf(paste0(path, "/fars",i,"/per",i,".dbf")))
    }
}

## concatenate all the person files, and add a year variable
person <- data.frame()
for(i in 2008:2015){
    text0 <- paste0("person",i,"$YEAR <- rep(",i,", dim(person",i,")[1])")
    eval(parse(text=text0))
    
    text <- paste0("person <- rbind.fill(person, person",i,")")
    eval(parse(text=text))
}

## get only info about drivers
drivers <- person %>% filter(PER_TYP==1)

## get ALC_RES for years prior to 2015 to agree with 2015 values
drivers$ALC_RES[drivers$YEAR < 2015] <- 10*drivers$ALC_RES[drivers$YEAR < 2015]

## get AGE for year 2008, and 2009 to match years after that
drivers$AGE[drivers$YEAR == 2008] <- ifelse(99, 999, drivers$AGE[drivers$YEAR == 2008])

## add a variable indicating whether the driver was drunk
drivers <- dplyr::mutate(drivers, drunk=(DRINKING==1 | (ALC_RES >= 500 & ALC_RES <= 940)))

## missing data
#drunk
drivers$drunk <- ifelse((drivers$ALC_RES>940 & drivers$DRINKING %in% c(8,9)),
                        NA,drivers$drunk)

#age
drivers$AGE <- ifelse(drivers$AGE %in% c(998,999), 
                      NA,drivers$AGE)

#sex
drivers$SEX <- ifelse(drivers$SEX %in% c(8,9),
                      NA,drivers$SEX)

## get proportion of drunk drivers per month, per year, per state for 96 months
## get raw numbers of drunk driving fatalities in colorado
col.drunk <- drivers %>% group_by(STATE, YEAR, MONTH) %>% 
    filter(STATE==8) %>% 
    summarize(prop=mean(drunk, na.rm=T))
plot(1:96,col.drunk$prop,ylim=c(0,1))

us.drunk <- drivers %>% group_by(YEAR, MONTH) %>% 
    summarize(prop=mean(drunk, na.rm=T))
points(1:96,us.drunk$prop, col="red")

for(i in unique(drivers$STATE)){
    drunk.props <- drivers %>% 
        group_by(STATE, YEAR, MONTH) %>% 
        filter(STATE==i) %>% 
        summarize(prop=mean(drunk, na.rm=T))
    plot(1:dim(drunk.props)[1], 
         drunk.props$prop, 
         ylim=c(0,1), 
         main=paste(i))
}












