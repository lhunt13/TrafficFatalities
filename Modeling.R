#################### MODELING ############################
## get proportion of drunk drivers per month, per year, per state for 96 months
## get raw numbers of drunk driving fatalities in colorado
## let the day be the avg day for that month
sc.drunk <- drivers %>% group_by(STATE, YEAR, MONTH) %>% 
  filter(STATE==45) %>%  
  summarize(prop=mean(drunk, na.rm=T), DAY=round(mean(DAY))) 
sc.drunk$DATE <- ymd(paste(sc.drunk$YEAR, sc.drunk$MONTH, sc.drunk$DAY, sep="-"))
plot(sc.drunk$DATE, sc.drunk$prop, type="l")
hist(log(sc.drunk$prop[sc.drunk$DATE < ymd("2014-10-1")]))
hist(sc.drunk$prop[sc.drunk$DATE < ymd("2014-10-1")])

model2 <- glm(log(prop) ~ DATE + (DATE >= ymd("2014-10-1")) + DATE*(DATE >= ymd("2014-10-1")), 
              data=sc.drunk)
with(sc.drunk, plot(DATE, log(prop), type="l"))
lines(sc.drunk$DATE, model2$fitted.values, col="red")

model3 <- glm(log(prop) ~ DATE + (DATE >= ymd("2013-1-1")) + DATE*(DATE >= ymd("2013-1-1")), 
              data=sc.drunk)
with(sc.drunk, plot(DATE, log(prop), type="l"))
lines(sc.drunk$DATE, model3$fitted.values, col="red")