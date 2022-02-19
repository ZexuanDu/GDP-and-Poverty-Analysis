databts = ts(datab[-1],start=1970)
JOR=databts["JOR"]
HTI = databts["HTI"]
NZL=databts["NZL"]
TCD=databts["TCD"]
JOR -> KindomofJordan
HTI -> Haiti
NZL -> NewZealand
TCD -> Chad

##b)
JOR=databts[,"JOR"]
HTI = databts[,"HTI"]
NZL=databts[,"NZL"]
TCD=databts[,"TCD"]
JOR <- log(JOR)
HTI <- log(HTI)
NZL <- log(NZL)
TCD <- log(TCD)
par(cex=0.75)
plot( HTI,col="red", lwd = 2,ylim=c(6.5,11), main="Evolution of the Real per Capita GDP (log-scale)",ylab="Real per Capita GDP(log-scale)",cex.main=0.8)

lines(TCD,col="darkgreen",lty=2,lwd=2)
lines(JOR,col="darkblue",lty=3,lwd=2)
lines(NZL,col="black",lty=4,lwd=2)

legend("topleft", bty="n", c("Haiti","Chad","Kindom of Jordan","New Zealand"), col=c("red","darkgreen","darkblue","black"), lty=c(1,2,3,4),lwd=2)


##c)
t2 <- time(TCD)
t22 <- t2^2
lHTI <- lm(HTI~t2+t22)
coeHTI <- coef(lHTI)
tHTI <- coeHTI[1]+coeHTI[2]*t2+coeHTI[3]*t22
lTCD <- lm(TCD~t2+t22)
coeTCD <- coef(lTCD)
tTCD <- coeTCD[1]+coeTCD[2]*t2+coeTCD[3]*t22
lJOR <- lm(JOR~t2+t22)
coeJOR <- coef(lJOR)
tJOR <- coeJOR[1]+coeJOR[2]*t2+coeJOR[3]*t22
lNZL <- lm(NZL~t2+t22)
coeNZL <- coef(lNZL)
tNZL <- coeNZL[1]+coeNZL[2]*t2+coeNZL[3]*t22
Haiti <- tHTI
KindomofJordan <- tJOR
Chad <- tTCD
NewZealand <- tNZL
Haiti <- HTI-Haiti
KindomofJordan <- JOR-KindomofJordan
Chad <- TCD-Chad
NewZealand <- NZL-NewZealand
allto <- cbind(Haiti,Chad,KindomofJordan,NewZealand)

plot(allto,lwd=2,main="Cylical Component of the Real per Capita GDP(log-scale)",cex.lab=0.8,cex.main=0.8)

##d)
HTIge<-diff(HTI)/lag(HTI,-1)*100
TCDge<-diff(TCD)/lag(TCD,-1)*100
JORge<-diff(JOR)/lag(JOR,-1)*100
NZLge<-diff(NZL)/lag(NZL,-1)*100
avgHTI=mean(HTIge)
avgTCD=mean(TCDge)
avgJOR=mean(JORge)
avgNZL=mean(NZLge)

plot(window(HTI,end=1970), avgHTI, main="Average Annual Growth Rate vs. Real per Capita GDP(log-scale)", xlab="Real per Capita GDP(log-scale)", ylab="Average Annual Growth Rate(persent)", xy.labels=FALSE, xy.lines=FALSE, pch=21, col=1, bg=1,cex.main=0.75,xlim=c(7,10),ylim=c(-0.1,0.7))

text(window(HTI,end=1970), avgHTI,"Haiti", pos=3, cex=.7)
points(window(TCD,end=1970), avgTCD, col=1, bg=1, pch=21)
points(window(JOR,end=1970), avgJOR ,col=1, bg=1, pch=21)
points(window(NZL,end=1970), avgNZL,col=1, bg=1, pch=21)

text(window(TCD,end=1970), avgTCD,"Chad", pos=3, cex=.7)
text(window(JOR,end=1970), avgJOR,"Kindom of Jordan", pos=3, cex=.7)
text(window(NZL,end=1970), avgNZL,"New Zealand", pos=3, cex=.7)

##e) 
year1 <-window(databts,start=1973,end=1973)
year2 <-window(databts,start=2003,end=2003)
year1<-year1/1000
year2<-year2/1000

hist(year1, breaks=25, xlab="Real per Capita GDP (thousands of international dollars of 1973)", cex=.8, main="Distribution of real per capita GDP Across Country in 1973", col="gray", labels=TRUE)

hist(year2, breaks=25, xlab="Real per Capita GDP (thousands of international dollars of 2003)", cex=.8, main="Distribution of real per capita GDP Across Country in 2003", col="gray", labels=TRUE)

##f) 
year1 <-window(databts,start=1973,end=1973)
year2 <-window(databts,start=2003,end=2003)
year1 <- log(year1)
year2<-log(year2)
hist(year1, breaks=25, xlab="Real per Capita GDP(log-scale)", cex=.8, main="Distribution of real per capita GDP Across Country in 1973", col="gray", labels=TRUE)
hist(year2, breaks=25, xlab="Real per Capita GDP(log-scale)", cex=.8, main="Distribution of real per capita GDP Across Country in 2003", col="gray", labels=TRUE)

