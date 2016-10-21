#################### Working with Time series object ############################
## convert to ts object and plot
southcarolina <- ts(sc.drunk$prop, frequency=12, start=c(2008,1))
plot(southcarolina,ylim=c(0,1),ylab="Prop. drunk drivers")
boxplot(southcarolina~cycle(southcarolina))
plot(stl(southcarolina, s.window="periodic"))

us.drunk <- drivers %>% group_by(YEAR, MONTH) %>% 
  summarize(prop=mean(drunk, na.rm=T))
usa <- ts(us.drunk$prop, frequency=12, start=c(2008,1))
plot(usa, ylim=c(0,1), ylab="Prop. drunk drivers", col="red", add=T)


for(i in unique(drivers$STATE)){
  drunk.props <- drivers %>% 
    group_by(STATE, YEAR, MONTH) %>% 
    filter(STATE==i) %>% 
    summarize(prop=mean(drunk, na.rm=T))
  drunkts <- ts(drunk.props$prop, frequency=12, start=c(2008,1))
  plot(drunkts, ylim=c(0,1), main=paste0(i))
}

## use a GLM to perform interrupted time series
sc.it <- glm(x ~ month + (month >= 85) + month*(month >= 85),
             data=data.frame(as.data.frame(southcarolina),month=1:96))



################# Explore traffic fatality incidents ######################
## read in accident data
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

table(accident$YEAR)

MonthlyCounts <- accident %>% filter(YEAR >= 1980) %>%
  group_by(YEAR, MONTH) %>%
  summarise(total=n())
America <- ts(MonthlyCounts$total, frequency=12, start=c(1980,1))  
plot(America)
boxplot(America~cycle(America))


## look at drunk driving overall
MonthlyDrunk <- accident %>% filter(YEAR >= 1980) %>%
  group_by(YEAR, MONTH) %>%
  summarise(drunks=sum(DRUNK_DR))
America.drunk <- ts(MonthlyDrunk$drunks, frequency=12, start=c(1980,1))
plot(America.drunk, main="Monthly Counts of Drunk Drivers \n in the U.S.",
     ylab="No. Drunk Drivers")

boxplot(America.drunk~cycle(America.drunk))
plot(stl(America.drunk,s.window="periodic"))

