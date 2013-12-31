# Trips analysis
setwd("/home/robin/Data/DfT-NTS-2002-2008/UKDA-5340-spss/spss/spss12/") # where nts spss data is saved

head(trips)
head(tdf)
names(tdf) # Too many of them! reduce them...

house1 <- tdf[which(tdf$h88 == 11 & tdf$psuid == 20102),]
depart <- as.numeric(strtrim(house1$j60[which(house1$j24 == "Home")], width=4))
plot(house1$j60)
plot(depart)

# Departure times for all trips
depart <- as.numeric(strtrim(tdf$j60, width=4))
d <- head(depart)
as.POSIXct(d, origin=) #not working
depart<-sprintf("%04d",depart) 
depart <- strptime(depart, "%H%M")
plot(depart)
hist(depart)
Origin <- tdf$j24
??time

## plot
library(scales)
p <- ggplot(x=depart)
qplot(x=depart, geom="histogram")
ggplot(aes(x=tdf$j60, colour=destination)) + geom_histogram()
p <- ggplot(data=tdf, aes(x=depart, fill=Origin))
p + geom_histogram() + scale_x_datetime(labels = date_format("%H:%M")) #Final works!
p + geom_histogram(binwidth=30*60, aes(y = ..density..)) + 
  scale_x_datetime(labels = date_format("%H:%M"), breaks="2 hour") + 
  geom_density(alpha = 0.2, adjust = 3) + xlab("Departure time") +
  ylab("Density (Gaussian kernel)") + 
  ggtitle("NTS data (N = 354,332), plotted by R. Lovelace") + 
  theme_classic()

ggsave("rush-hour.png", width=8, height=5)

length(which(tdf$j60 ==NA))

head(tdf)
plot(tdf$j14)
head(tdf$j23)
plot(tdf$j23)
length(which(tdf$j23 == "One")) / length(tdf$j23)

# N. trips/person
nrow(tdf) / length(unique(paste(tdf$h96, tdf$psuid, tdf$h88))) 
tdf$uniss <- paste(tdf$h96, tdf$psuid, tdf$h88)
uniss <- as.list(tdf$uniss)
aggs <- aggregate.data.frame(tdf$jdungross, by=list(tdf$uniss), FUN="mean")
head(aggregate.data.frame(tdf$jdungross, by=list(tdf$uniss), FUN="mean"))
freqqs <- data.frame(table(tdf[,c("uniss")]))
aggs$N.trips.week <- freqqs[,2]
aggs$Distance <- aggs$x/10
summary(aggs)
plot(freqqs$Freq, aggs$x)
head(aggs)

p <- ggplot(data=aggs, aes(x=N.trips.week, y=Distance))
p + geom_point(alpha = 0.1) + geom_smooth() +
 ylim(0,50) + xlim(0,50)  +   stat_density2d(aes(fill = ..level..), geom="polygon", alpha=0.5) 
ggsave("dist-trips-nts.png", width = 8, height = 5) # This is wrong! too many trips/wk

cor.test(aggs$N.trips.week, aggs$Distance)


summary(aggs)
length(which(aggs$N.trips.week == 10 | aggs$N.trips.week == 6)) / 31247
length(which(aggs$N.trips.week > 14)) / 31247
hist(aggs$N.trips.week)
qplot(aggs$N.trips.week, geom="histogram", binwidth = 2) + 
  scale_x_continuous(limits=c(0,28), breaks=c(0,7,14,21,28)) + xlab("Trips per week") +
  ylab("Count") + theme_bw()

tdf$uniss2 <- paste(tdf$h96, tdf$psuid, tdf$h88, tdf$i1)
tmany <- tdf[which(tdf$uniss2 %in% aggs2$Group.1[which(aggs2$N.trips.week > 28)]),]
aggs2 <- aggregate.data.frame(tdf$jdungross, by=list(tdf$uniss2), FUN="mean")
freqqs <- data.frame(table(tdf[,c("uniss2")]))
aggs2$N.trips.week <- freqqs[,2]
aggs2$Distance <- aggs2$x/10
head(aggs2)
hist(aggs2$N.trips.week)

tdf.uniqid <- tdf[!duplicated(tdf$uniss2),]
nrow(tdf) / nrow(tdf.uniqid)
  
aggs3 <- merge(x=aggs2, y=tdf.uniqid[,c("uniss2","jdungross", "j36")], by.x="Group.1", 
               by.y="uniss2", all=T)
aggs3 <- merge(x=aggs3, y=tdf.uniqid[,c("uniss2","jdungross", "j36")], by.x="Group.1", 
               by.y="uniss2", all=T) # adding employment status

summary(aggs3)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(aggs3$N.trips.week)
aggs3$Dist.miles <- cut(aggs3$jdungross/10, breaks=c(0,2,5,10,20,50,100))
qplot(data = aggs3, x=N.trips.week, geom="histogram", binwidth = 2, fill=Dist.miles) + 
  scale_x_continuous(limits=c(0,28), breaks=c(0,7,14,21,28)) + xlab("Trips per week (one way)") +
  ylab("Count") + theme_bw() # This is very cool, and exactly what I wanted
length(which(aggs3$N.trips.week >= 9 & aggs3$N.trips.week <= 11)) / nrow(aggs3)

ggsave("n.trips.hist.png", width=8, height=5)
# Now make proportional
aggs3$Dist.miles <- cut(aggs3$jdungross/10, breaks=c(0,2,5,10,20,50,100))
qplot(data = aggs3, x=N.trips.week, geom="histogram", binwidth = 1, fill=Dist.miles,
      position = "fill") + 
  scale_x_continuous(limits=c(0,28), breaks=c(0,7,14,21,28)) + xlab("Trips per week (one way)") +
  ylab("Proportion (by distance bin)") + theme_bw() # This is very cool, and exactly what I wanted
ggsave("n.trips.hist.prop.png", width=8, height=5)
length(which(aggs3$N.trips.week >14 | aggs3$N.trips.week < 6 )) 
length(which(aggs3$N.trips.week >14 | aggs3$N.trips.week < 4 )) / nrow(aggs3)
summary(aggs3$N.trips.week)
sd(aggs3$N.trips.week)

### true relationship btwn ntrips and distance
head(aggs3)
aggs3$dbin <- cut(aggs3$Distance, breaks=c(0,2,5,10,20,30,40,60,200)*1.6/1.4 )
distable <- aggregate(aggs3$N.trips.week, by=list(aggs3$dbin), FUN = mean)
t(distable)
plot(distable)
write.csv(t(distable), "~/Dropbox/Thesis/Tables/distable.csv")

dv <- c(1,3.5,7.5,15,25,40,60, 80, 120)
dv <- dv * 1.6 
dv <- data.frame(d = dv, f = distable$x)
dv
mod1 <- lm(f~d, data=dv)
summary(mod1)
dv$pred <- predict(mod1, dv)
dv
plot(dv)


qplot(data=distable, x=Group.1, y= x) + geom_line(aes(group=1), fill=NA) +
  xlab("Route distance bin (miles)") + ylab("1 way trips per week") + theme_bw()
ggsave("~/Dropbox/Thesis/Figures/ntrips~dist.png")


aggs3$N.trips.r <- aggs3$N.trips.week + runif(n=nrow(aggs3), min=-0.5, max=0.5 )

#replot scatter
library(zoo)
rmeans <- rollmean(x=aggs3$Distance, k=aggs3$N.trips.week, ) # failed
p <- ggplot(data=aggs3, aes(x=N.trips.week, y=Distance))
p + geom_point(alpha = 0.1) + geom_smooth() +
  ylim(0,30) + xlim(0,25)  + ylab("Distance (miles)") +
  stat_density2d(aes(fill = ..level..), geom="polygon", alpha=0.5,na.rm=T, se=0.1) 
ggsave("dist-trips-nts.png", width = 8, height = 5) # This is wrong! too many trips/wk
ggsave()
cor.test(aggs3$Distance, aggs3$N.trips.week)
mean(aggs3$Distance)
mean(tdf$jdungross)