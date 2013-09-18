# Analysis of msim results for thesis
# First load eresults
load("/media/AA5EAF5C5EAF2055/vul-meth/etsimY/eresults.RData")
intall[[1]] <- intall[[1]][,-c(14:15)]
# add zone number
for(i in 1:length(intall)){
  intall[[i]]$zone <- i
}

allindividuals <- do.call(rbind, intall)
head(allindividuals)
head(meanEt)
Et.1 <- (aggregate(x=allindividuals$Et.1, by=list(allindividuals$zone), FUN=mean))
Et.0 <- aggregate(x=allindividuals$Et, by=list(allindividuals$zone), FUN=mean)
plot(Et.0[,2], Et.1[,2])
cor(Et.0[,2], Et.1[,2])

### Individual-level analysis
summary(allindividuals$nssec8)
indenergy <- aggregate(allindividuals$Et.1, by=list(allindividuals$nssec8), FUN=mean)
indenergy$distance <- aggregate(allindividuals$dis, by=list(allindividuals$nssec8), FUN=mean)[,2]
indenergy$efficiency <- indenergy$x / indenergy$distance
write.csv(indenergy, "indenergy.csv")

meip <- which.max(soy$top) # most energy intensive place
soy@data[meip,]
meip <- allindividuals[which(allindividuals$zone == meip),]
indenergy <- aggregate(meip$Et.1, by=list(meip$nssec8), FUN=mean)
indenergy$distance <- aggregate(meip$dis, by=list(meip$nssec8), FUN=mean)[,2]
indenergy$efficiency <- indenergy$x / indenergy$distance
write.csv(indenergy, "indenergy-unequal.csv")

meip <- which.min(soy$top) # least energy intensive place
meip <- allindividuals[which(allindividuals$zone == meip),]
indenergy <- aggregate(meip$Et.1, by=list(meip$nssec8), FUN=mean)
indenergy$distance <- aggregate(meip$dis, by=list(meip$nssec8), FUN=mean)[,2]
indenergy$efficiency <- indenergy$x / indenergy$distance
write.csv(indenergy, "indenergy-equal.csv")

summary(allindividuals$EI)
summary(allindividuals)
### Plotting for soyo msoas
x = c("ggplot2", "sp", "rgeos", "mapproj", "rgdal", "maptools")
lapply(x, require, character.only=T) # the R packages we'll be using

soy <- readOGR("/home/robin/Dropbox/vul-meth/geodat/soyo-msoa/", "soyo-buses")
soy <- SpatialPolygonsDataFrame(Sr=gSimplify(soy, tol=100), data=soy@data)
plot(soy)
head(soy@data)
soy <- soy[order(soy$NAME),]
soy@data <- cbind(soy@data, meanEt)
soy$id <- as.character(soy$NAME)

fsoy <- fortify(soy, region="id")
fsoy <- join(fsoy, soy@data)
fsoy[,1:2] <- fsoy[,1:2]/1000
head(fsoy)

soyo <- readOGR("/home/robin/Data/Nationaltravel/districts/", "soyo")
names(soyo)[1] <- "id"
soyo$id <- as.character(soyo$id)
soyo <- SpatialPolygonsDataFrame(Sr=gSimplify(soyo, tol=100), data=soyo@data)
fsoyo <- fortify(soyo, region="id")
fsoyo <- join(fsoyo,soyo@data[,c(1:10)])
head(fsoyo)
fsoyo[,1:2] <- fsoyo[,1:2] / 1000
nsoyo <- data.frame(x = coordinates(soyo)[,1]/1000,
                    y = coordinates(soyo)[,2]/1000, 
                    name = soyo$ZONE_LABEL)

install.packages("ggmap")
library(ggmap)
ggmap(get_map("Sheffield"), )

ggplot() + 
  geom_polygon(data=fsoy, aes(x=long, y=lat, group=group, fill = meanEt/2), alpha=1) +
  scale_fill_continuous(low="green", high="red", name="Etrp (MJ)")  + coord_fixed() + # This works
  theme_classic() + geom_path(data=fsoy, aes(x=long, y=lat, group=group), 
                              colour="grey", size = 0.1) +
  xlab("Easting (km)") + ylab("Northing (km)") +
  geom_text(data=nsoyo, aes(x=x,y=y, label=name), size = 3) + geom_path(data=fsoyo, aes(x=long, y=lat, group=group))
ggsave("~/Dropbox/Thesis/Figures/soyoen.png", width=7, height = 5)
soy@data[grepl("008", soy$id), ]
fsoy[grepl("008", fsoy$id), ]

write.csv(head(allindividuals[,c(1,2,3,13,17,18)]), "Dropbox/Thesis/Tables/indout-carensize.csv")

### Analysing the % energy used by to 20% of commuters
a1 <- subset(allindividuals, zone == i, Et)
atop <- sum(a1$Et[a1$Et > quantile(a1$Et, probs=0.8)])
atop/sum(a1)

# now put that in a loop
soy$top20 <- soy$youngshare  <- soy$pric <- soy$pricc<- 0
for(i in 1:173){
  a1 <- subset(allindividuals, zone == i)
  atop <- sum(a1$Et[a1$Et > quantile(a1$Et, probs=0.8)])
  soy$top[i] <- atop/sum(a1$Et)
  soy$youngshare[i] <- nrow(a1[which(a1$dis > 20 & a1$mode == "Car (p)" & a1$age < 30 ),])
  soy$pric[i] <- sum(a1$Et[grepl("higher", a1$nssec8)]) / sum(a1$Et)
  soy$ppric[i] <- length(a1$Et[grepl("higher", a1$nssec8)]) / nrow(a1)
}

### plot again
head(soy@data)

fsoy <- fortify(soy, region="id")
fsoy <- join(fsoy, soy@data)
fsoy[,1:2] <- fsoy[,1:2]/1000
head(fsoy)
fsoy$prictime <- fsoy$pric/fsoy$ppric

ggplot() + 
  geom_polygon(data=fsoy, aes(x=long, y=lat, group=group, fill = prictime), alpha=1) +
  scale_fill_continuous(limits = c(1.4, 4),low="green", high="red", 
                        name="Ratio of energy use \nby top class:average")  + 
  coord_fixed() + # This works
  theme_classic() + geom_path(data=fsoy, aes(x=long, y=lat, group=group), 
                              colour="grey", size = 0.1) +
  xlab("Easting (km)") + ylab("Northing (km)") +
  geom_text(data=nsoyo, aes(x=x,y=y, label=name), size = 3) + geom_path(data=fsoyo, aes(x=long, y=lat, group=group))

ggsave("~/Dropbox/Thesis/Figures/topprop.png", width=7, height = 5)

ggplot() + 
  geom_polygon(data=fsoy, aes(x=long, y=lat, group=group, fill = top), alpha=1) +
  scale_fill_continuous(low="green", high="red", 
                        name="Proportion of \nenergy used by \ntop 20%")  + 
  coord_fixed() + # This works
  theme_classic() + geom_path(data=fsoy, aes(x=long, y=lat, group=group), 
                              colour="grey", size = 0.1) +
  xlab("Easting (km)") + ylab("Northing (km)") +
  geom_text(data=nsoyo, aes(x=x,y=y, label=name), size = 3) + 
  geom_path(data=fsoyo, aes(x=long, y=lat, group=group))
ggsave("~/Dropbox/Thesis/Figures/prop20.png", width=7, height = 5)

fsoy[grepl("008", fsoy$id), ]

# and % used by different distances
length(which(allindividuals$dis > 30)) / nrow(allindividuals)
sum(allindividuals$Et.1[which(allindividuals$dis > 30)]) / sum(allindividuals$Et.1)

### Shift to telecommuting - for chapter 8 !!! save in reproducible!
head(soy@data)
names(allindividuals)
a1 <- subset(allindividuals, zone == i, Et)
tele <- which(grepl("manage|higher", allindividuals$nssec8))
atop/sum(a1)
length(tele) / nrow(allindividuals)
summary(allindividuals$EI.1); summary(allindividuals$EI)
allindividuals$Et.tele <- allindividuals$Et.1
allindividuals$Et.tele[tele] <- allindividuals$Et.1[tele] * 0.66667
summary(allindividuals$Et.tele) / summary(allindividuals$Et.1)
# now put that in a loop
soy$top20 <- soy$youngshare  <- soy$pric <- soy$pricc<- 0

soy$pertele <- 1 # set-up telecommuting e. save var
soy$propcom <- 0.05
for(i in 1:173){
  a1 <- subset(allindividuals, zone == i)
#   soy$pertele[i] <- 1 - sum(a1$Et.tele) / sum(a1$Et.1) # original approximation
summary(a1$nssec8)
  soy$pertele[i] <- sum(a1$Et.1)
  tele <- which(grepl("manage|higher", a1$nssec8))
  probcom <- 1/exp(5.3 - 0.022 * a1$dis[tele])
  tele <- sample(tele, size=0.10*length(tele), prob=probcom, replace=F)
  soy$pertele[i] <- (soy$pertele[i] - sum(a1$Et.1[tele]))/nrow(a1)
  soy$propcom[i] <- length(tele)/nrow(a1) # proportion who now telecommute
}
# test probabilities work:
plot(a1$dis[tele], probcom)
1 - summary(soy$pertele/soy$meanEt)
summary(soy$propcom)

fsoy <- fortify(soy, region="id")
fsoy <- join(fsoy, soy@data)
fsoy[,1:2] <- fsoy[,1:2]/1000
head(fsoy)
fsoy$prictime <- fsoy$pric/fsoy$ppric

ggplot() + 
  geom_polygon(data=fsoy, aes(x=long, y=lat, group=group, fill = (1 - pertele/meanEt)*100), alpha=1) +
  scale_fill_continuous(low="green", high="red", 
                        name="Energy saved\n by telework (%)")  + 
  coord_fixed() + # This works
  theme_classic() + geom_path(data=fsoy, aes(x=long, y=lat, group=group), 
                              colour="grey", size = 0.1) +
  xlab("Easting (km)") + ylab("Northing (km)") +
  geom_text(data=nsoyo, aes(x=x,y=y, label=name), size = 3) + geom_path(data=fsoyo, aes(x=long, y=lat, group=group))
ggsave("~/Dropbox/Thesis/Figures/telesave.png", width=7, height = 5)
# what's the link between energy saved and average e. use?
plot(soy$meanEt, 1 - soy$pertele/soy$meanEt )
cor.test(soy$meanEt, 1 - soy$pertele/soy$meanEt ) # quite low, 0.12
plot(soy$propcom, 1 - soy$pertele/soy$meanEt)

cor.test(soy$propcom, 1 - soy$pertele/soy$meanEt ) # quite low, 0.12


# link with income?
soy$income <- read.csv("~/Dropbox/Thesis/Reproducible/income-msoa.csv", header=F)[,c(16)]
head(incomesoyo)
plot(soy$income, 1 - soy$pertele/soy$meanEt)
cor.test(soy$income, 1 - soy$pertele/soy$meanEt)

cor.test(soy$meandis, 1 - soy$pertele/soy$meanEt)

# re-add correct vars
names(Und.df)
names(allindividuals)
allindividuals2 <- join(allindividuals[,1:3], Und.df, by="pidp")
names(allindividuals2)

head(allindividuals[,1:6])
head(allindividuals2[,1:6]) # looks right - good to join

allindividuals2 <- cbind(allindividuals, allindividuals2[,6:31])

summary(allindividuals2$a_lkmove)

