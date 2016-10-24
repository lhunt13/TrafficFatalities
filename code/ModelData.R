##############
## MODELING ##
##############
## get proportion of drunk drivers per month, per year, per state for 96 months
## let the day be the avg day for that month
sc.drunk <- drivers %>% filter(YEAR >= 2008) %>%
  group_by(STATE, YEAR, MONTH) %>% 
  filter(STATE==45) %>%  
  summarize(prop=mean(drunk, na.rm=T), DAY=round(mean(DAY))) 
tx.drunk <- drivers %>% filter(YEAR >= 2008) %>%
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
