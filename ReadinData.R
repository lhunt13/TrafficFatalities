
## Read in accident data
library(foreign)
path <- "/Users/lamarhuntiii/Documents/Second Year Classes/Data Science/data"
for(i in 1983:2015){
  #accident file
  if(i <= 1982 | i >= 1994){
    assign(paste0("accident",i),read.dbf(paste0(path, "/fars",i,"/accident.dbf")))
  }
  else{
    assign(paste0("accident",i),read.dbf(paste0(path, "/fars",i,"/acc",i,".dbf")))
  }
}


## plot counts by state each year
library(dplyr)
for(i in 1975:2015){
  text <- paste0("table(","accident",i,"$MONTH)")
  print(eval(parse(text=text)))
}

#change all the dates to 4 digits
for(i in 1975:1997){
  text <- paste0("accident",i,"$YEAR <- ifelse(accident",i,"$YEAR==99, NA, ",i,")")
  eval(parse(text=text))
}

#clean up months
accident1975$MONTH <- ifelse(accident1975$MONTH==99, NA, accident1975$MONTH)
accident1977$MONTH <- ifelse(accident1977$MONTH==99, NA, accident1977$MONTH)


#


#plotting no. fatalities by year for each state
for(i in 1:56){
  y <- numeric(0)
  for(j in 1:41){
    y[j] <- eval(parse(text= paste0("table(accident",j+1974,"$STATE)[",i,"]")))
  }
  plot(1975:2015, y, main=as.character(i), xlab="Year")
}

#to do:
#weight no. fatalities by some measure of drivers on the road
#




