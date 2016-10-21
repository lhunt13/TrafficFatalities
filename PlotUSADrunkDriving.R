path <- "/Users/lamarhuntiii/Documents/Second Year Classes/Data Science/data"
for(i in 1975:2015){
  #accident file
  if(i <= 1982 | i >= 1994){
    assign(paste0("accident",i),read.dbf(paste0(path, "/fars",i,"/accident.dbf")))
  }
  else{
    assign(paste0("accident",i),read.dbf(paste0(path, "/fars",i,"/acc",i,".dbf")))
  }
}

## concatenate all the person files, and add a year variable
accident <- data.frame()
for(i in 1975:2015){
  text0 <- paste0("accident",i,"$YEAR <- rep(",i,", dim(accident",i,")[1])")
  eval(parse(text=text0))
  
  text <- paste0("accident <- rbind.fill(accident, accident",i,")")
  eval(parse(text=text))
}

## plot monthly dd counts for all USA per month
MonthlyDrunk <- accident %>% filter(YEAR >= 1980) %>%
  group_by(YEAR, MONTH) %>%
  summarise(drunks=sum(DRUNK_DR))
America.drunk <- ts(MonthlyDrunk$drunks, frequency=12, start=c(1980,1))
plot(America.drunk, main="Monthly Counts of Drunk Drivers \n in the U.S.",
     ylab="No. Drunk Drivers")