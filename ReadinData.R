library(foreign)
library(plyr)
library(dplyr)
library(lubridate)

######################
## READ IN THE DATA ##
######################

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

###################
## DATA CLEANING ##
###################

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
## drunk driving in south carolina is defined as BAC > .08
drivers <- dplyr::mutate(drivers, drunk=(DRINKING==1 | (ALC_RES >= 800 & ALC_RES <= 940)))

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
#day
drivers$DAY <- ifelse(drivers$DAY==99, 
                      NA,drivers$DAY)

##compute dates from year and month variables
drivers$DATE <- ymd(paste(drivers$YEAR, drivers$MONTH, drivers$DAY, sep="-"))

##############
## MODELING ##
##############

## get proportion of drunk drivers per month, per year, per state for 96 months
## let the day be the avg day for that month
sc.drunk <- drivers %>% 
            group_by(STATE, YEAR, MONTH) %>% 
            filter(STATE==45) %>%  
            summarize(prop=mean(drunk, na.rm=T), DAY=round(mean(DAY))) 
tx.drunk <- drivers %>%
            group_by(STATE, YEAR, MONTH) %>%
            filter(STATE==48) %>%
            summarize(prop=mean(drunk, na.rm=T), DAY=round(mean(DAY)))

## add a date variable
sc.drunk$DATE <- ymd(paste(sc.drunk$YEAR, sc.drunk$MONTH, sc.drunk$DAY, sep="-"))
sc.drunk$DATE_reg <- sc.drunk$DATE - ymd("2008-1-1")

tx.drunk$DATE <- ymd(paste(tx.drunk$YEAR, tx.drunk$MONTH, tx.drunk$DAY, sep="-"))
tx.drunk$DATE_reg <- tx.drunk$DATE - ymd("2008-1-1")


## fit a linear interrupted time series using, but set intercept at Jan 1st, 2008
c <- ymd("2014-10-1") - ymd("2008-1-1")
model.sc <- lm(log(prop) ~ DATE_reg + (DATE_reg >= c) + DATE_reg*(DATE_reg >= c), 
              data=sc.drunk)
model.tx <- lm(log(prop) ~ DATE_reg + (DATE_reg >= c) + DATE_reg*(DATE_reg >= c), 
               data=tx.drunk)

## plot the data with the fitted line
# southcaroline
par(mfrow=c(1,1))
plot(sc.drunk$DATE, log(sc.drunk$prop), type="l",
     ylab="Log(Proportion)",
     xlab="Time",
     main="South Carolina vs. Texas",
     ylim=c(-2.3,-0.5),
     lwd=2)
lines(sc.drunk$DATE, model.sc$fitted.values, col="red", lwd=2)

# texas
points(tx.drunk$DATE, log(tx.drunk$prop), type="l",
     col="darkorange",lwd=2)
lines(tx.drunk$DATE, model.tx$fitted.values, col="blue",lwd=2)
legend("bottomleft", col=c("black","red","darkorange","blue"), lty=1, lwd=3, 
       legend=c("SC","SC fitted","TX","TX fitted"))

## diagnostic plots
#plot(mmodel2)

#hist(log(sc.drunk$prop[sc.drunk$DATE < ymd("2014-10-1")]))
#hist(sc.drunk$prop[sc.drunk$DATE < ymd("2014-10-1")])








