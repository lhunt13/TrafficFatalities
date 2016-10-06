library(foreign)
library(plyr)
library(dplyr)

## Read in accident and person data
path <- "/Users/lamarhuntiii/Documents/Second Year Classes/Data Science/data"
for(i in 2008:2015){
  #accident file
    if(i <= 1982 | i >= 1994){
        assign(paste0("accident",i),read.dbf(paste0(path, "/fars",i,"/accident.dbf")))
    }
    else{
        assign(paste0("accident",i),read.dbf(paste0(path, "/fars",i,"/acc",i,".dbf")))
    }
  
  #person file
    if(i <= 1982 | i >= 1994){
        assign(paste0("person",i),read.dbf(paste0(path, "/fars",i,"/person.dbf")))
    }
    else{
        assign(paste0("person",i),read.dbf(paste0(path, "/fars",i,"/per",i,".dbf")))
    }
}
## read in the DRIMPAIR data
for(i in 2010:2015){
    assign(paste0("drimpair",i),read.dbf(paste0(path, "/fars",i,"/drimpair.dbf")))
}


## Full join the accident and person rbind.fill
## PER_TYP = 1 indicates Driver
for(i in 2008:2015){
    text <- paste0("d",i," <- full_join(accident",i,", person",i,", by='ST_CASE')")
    eval(parse(text=text))
}

## rbind the dataframes together
accident <- data.frame()
for(i in 2008:2015){
    text <- paste0("accident <- rbind.fill(accident, accident",i,")")
    eval(parse(text=text))
}

person <- data.frame()
for(i in 2008:2015){
    text <- paste0("person <- rbind.fill(person, person",i,")")
    eval(parse(text=text))
}



## get numbers of drivers who are impaired with alcohol
## want people with (PER_TYP == 1) & (DRINKING = ) & (ALC_RES = )
## we can also look at DRUGRES1-3

## get raw numbers of drunk driving fatalities in colorado
accident <- mutate(accident, drunk=(DRUNK_DR > 0))

col.drunk <- accident %>% group_by(STATE, YEAR, MONTH) %>% 
             filter(STATE==8) %>% 
             summarize(mean(drunk)) %>%
             print(n=nrow(.))
plot(1:96,col.drunk$`mean(drunk)`)

us.drunk <- accident %>% group_by(YEAR, MONTH) %>% 
  summarize(mean(drunk))
plot(1:96,us.drunk$`mean(drunk)`)



## tabulate counts by state each year
library(dplyr)
for(i in 1975:2015){
  text <- paste0("table(","accident",i,"$MONTH)")
  print(eval(parse(text=text)))
}

## change all the dates to 4 digits
for(i in 1975:1997){
  text <- paste0("accident",i,"$YEAR <- ifelse(accident",i,"$YEAR==99, NA, ",i,")")
  eval(parse(text=text))
}

## clean up months
accident1975$MONTH <- ifelse(accident1975$MONTH==99, NA, accident1975$MONTH)
accident1977$MONTH <- ifelse(accident1977$MONTH==99, NA, accident1977$MONTH)


## plotting no. fatalities by year for each state
for(i in 1:56){
  y <- numeric(0)
  for(j in 1:41){
    y[j] <- eval(parse(text= paste0("table(accident",j+1974,"$STATE)[",i,"]")))
  }
  plot(1975:2015, y, main=as.character(i), xlab="Year")
}










