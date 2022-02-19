datats <- ts(Wage109[-1],start=1997)

##a)
##data = read.csv(file="Wage109.csv",header = TRUE)
##datats = ts(data[-1],start=1997)
male=datats[,"Males"]
female = datats[,"Females"]

plot( female,col="red", lwd = 2,ylim=c(10,35), main="Evolution of the Hourly Nominal Wage in Saskatchewan(manufacturing workers)",ylab="Hourly Nominal Wage($)",cex.main=0.8)

lines(male,col="blue",lwd=2,lty=2)

legend("topleft", bty="n", c("Male","Female"), col=c("blue","red"), lty=c(2,1),lwd=2)

##b) 
cpi=datats[,"CPI"]
rmale=male/cpi*100
rfemale=female/cpi*100

plot( rfemale,col="red", lwd = 2,ylim=c(10,25), main="Evolution of the Hourly Real Wage in dollar of 2002 in Saskatchewan(manufacturing workers)",ylab= "Hourly real Wage in dollar of 2002)", cex.main=0.8 )

lines(rmale,col="blue",lwd=2,lty=2)

legend("topleft", bty="n", c("Male","Female"), col=c("blue","red"), lty=c(2,1),lwd=2)

##c)
t<-time(male)
lmale=lm(rmale~t)
coemale <- coef(lmale)
trendmale <- coemale[1]+coemale[2]*t

lfemale=lm(rfemale~t)
coefemale <- coef(lfemale)
trendfemale <- coefemale[1]+coefemale[2]*t

plot( trendfemale,col="red", lwd = 2,ylim=c(10,25), main="Linear Trend of the Hourly Real Wage in Saskatchewan(manufacturing workers)",ylab= "Hourly Real Wage in dollar of 2002)",cex.main=0.8)

lines(trendmale,col="blue",lty=2,lwd=2)

legend("topleft", bty="n", c("Male","Female"), col=c("blue","red"), lty=c(2,1),lwd=2)

mean(trendfemale)
mean(trendmale)

##d)
dtmale <- rmale-trendmale
dtfemale <- rfemale-trendfemale

plot(dtmale,dtfemale, pch=21, col=1,bg="red", xlab="Hourly Real Wage of Males in dollar of 2002", ylab="Hourly Real Wage of Females in dollar of 2002",main="Comovement Between the Cyclical Component of Hourly Real Wage of Males and Females in Saskatchewan ", cex.main = 0.6,xy.labels = FALSE,xy.lines = FALSE)

