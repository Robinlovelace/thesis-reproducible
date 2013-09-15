# england.R - loads english commuting data and plots it
# (run before netherlands.R and nl-en-compare.R for comparison of energy costs)
setwd("~/Dropbox/Thesis/Reproducible/esubnat/")
x = c("ggplot2", "sp", "rgeos", "mapproj", "rgdal", "maptools")
lapply(x, require, character.only=T) # the R packages we'll be using

setwd("~/Data/Nationaltravel/ST121-STwards/")
wards <-  readOGR(".", layer="wardsst121uk" )
object.size(wards)/1000000 # woa - 300 MB - too large
wards@data <- wards@data[,c(1:102)]
object.size(wards)/1000000 # woa - still too large
wards <- SpatialPolygonsDataFrame(Sr= gSimplify(wards, tol=2000, topologyPreserve=T), data= wards@data)
### nb: use large tol=values (e.g. 2000) for plotting all England; smaller (e.g. 500 for regions)
object.size(wards)/1000000 # 
plot(wards[wards$ZONE_CODE=="00AAFA" ,], col="red")
plot(wards[grepl("00A", wards$ZONE_CODE) ,], col="red")

# bbox(wards)
# bbox(o)
# plot(wards[grepl("00", wards$ZONE_CODE) ,], col="red", xlim=matrix(c(300,200,400,400), 2,2))

?sp:::plot.SpatialPolygons 
head(wards@data[,1:10])
grepl("00AAF", wards$ZONE_CODE)
names(wards)

setwd("~/Dropbox/Thesis/Reproducible/esubnat/")
wards@data[1:10,1:5] # take a look at the first 5 columns of data: integer counts
wards$abbed <- abbreviate(wards$ZONE_LABEL, minlength=2) # create abbreviated area names
wards@data[1:5,c("ZONE_LABEL", "abbed")] # check the abbreviation worked
# plot(bbox(wards)) # not working - should know...
# text(coordinates(wards), labels=wards$abbed, col="red") # add our region names: in the right place?

dbreaks <- c(2,5,10,20,30,40,60)
dbreaks2 <- c("all", 0, dbreaks)
# Correct labels for the data...
# Other than 002 it's an 11 by 9 matrix
modenames <- c("all", "metro", "train", "bus", "moto", "car", "carp", "taxi", "bike", "walk", "other")
dbreaks <- c(2,5,10,20,30,40,60) # these are the distance breaks in the data
dbreaks2 <- c("all", 0, dbreaks) # correct labels to use with our data
newnames <- paste(rep(modenames, times=9), rep(dbreaks2, each=11), sep=".")
wards@data$mfh <- wards@data$ST1210002 #
wards@data <- wards@data[,-4] # remove the original mainly working from home column: gets in the way
names(wards@data)[3:101] <- newnames
# ecosts <- read.csv("~/Dropbox/Thesis/Tables/final-e-justpres.csv") # just reformat
ecosts <- c(0,0.57,0.77,2.1,1.7,3,0,3,0.09,0.13,0) # these are the direct energy costs (MJ/KM) for each mode
names(ecosts) <- c("All", "Tram", "Train", "Bus", "Moto", "Car.d", "Car.p", "Taxi", "Cycle", "Walk", "Other")

md <- read.csv("~/Dropbox/Thesis/Reproducible/energy-nat/dbxs.csv", header=T, ) # the distances associated with each distance bin
plot(rep(c(0,0,1,3.5,7.5,15,25,35,50,100),11), t(md)) # visualise them: make sense?

mdv <- as.vector(as.matrix(md[,2:10])) # rearange the distances data
edcosts <- rep(ecosts, 9) * mdv # multiply the energy costs by average distances fo each of 9 modes
names(edcosts) <- newnames
wards@data$other.60 <- 0
ecs <- sweep(x=wards@data[,3:101], MARGIN=2, edcosts, '*' ) 
barplot(colSums((ecs)[seq(6,by=11,length.out=9)]))

paste(names(ecs),"en", sep=".")
names(ecs) <- paste("moden",names(ecs), sep=".")
names(ecs)
wards@data <- cbind(wards@data, ecs)
wards@data$ET <- rowSums(ecs)
wards@data$EAV <- wards$ET / wards$all.all
plot(wards[grepl("00A", wards$ZONE_CODE),], col = rowSums(ecs))
names(wards@data)
range(wards@data$EAV)
summary(wards@data$EAV)
save(wards, file="wards.RData")

# Geographically subsetting
library(raster)
clip.extent <- as(extent(300000, 400000, 300000, 500000), "SpatialPolygons")
rm(clip.extent)
clip.extent2 # another way?
proj4string(clip.extent) <- proj4string(wards)
gI <- gIntersects(wards, clip.extent, byid=T  )
la.hfd <- wards[which(gI),]
plot(la.hfd)

# convert to ggplot2, just for plotting
la <- fortify(wards, region="ZONE_LABEL")
head(la)
object.size(la)
head(la)
# save(la,file="la.RData")
load("la.RData")
load("wards.RData")
gortex <- coordinates(wards)/1000 
gortex <- as.data.frame(cbind(gortex))
gortex$V3 <- as.character(abbreviate(wards$ZONE_LABEL))
head(gortex)

object.size(la.all)/10^6
la.all$ <- la
la <- la.all[1:10000,  ]

p <- ggplot(data=la, aes( x=long/1000, y=lat/1000))
p + geom_polygon(data=la,aes(x=long/1000, y=lat/1000, fill=ET/all.all, group=group)) + 
  scale_fill_continuous(low="green", high="red", name="Etrp (MJ)",
                        limits=c(10,80)) + coord_fixed() +
  xlab("Easting (km)") + ylab("Northing (km)") 

# now by mode...
ggsave("~/Dropbox/Thesis/Figures/goren.png", scale=1, width= 5.5, height=5.5)

summary(la) 
  la$all.all
# Load-in distance boxes
mdis <- read.csv("~/1PhD/Core data/dboxes.csv", header=T)
head(mdis)
mdis <- mdis[-1,]
mdis$mode <- row.names(mdis)
mdis <- melt(mdis)
mdis$b.max <- rep(c(dbreaks, 250), each=10)
mdis$b.min <- rep(c(0,dbreaks), each=10)
head(mdis)
mdis$Avd <- mdis$value/mdis$b.max
qplot(data=mdis, x=b.min, y=Avd, geom="line", colour=mode) # OK but too many modes...
mdis$avbox <- (mdis$b.max + mdis$b.min)/2
mdis$value <- mdis$value *1.4
mdis$avbox[mdis$avbox> 60] <- 70
qplot(data = mdis[which(mdis$mode %in% c("bus", "card", "cycle", 
            "train", "walk")),], x=avbox, y=value, geom="line", colour=mode) + 
  xlab("Euclidean distance bin average (km)") +
  ylab("Average route distance (km)") +
  xlim(c(0,70))+ scale_x_continuous(breaks = c(0,2,5,10,20,30,40,60)) 
ggsave("~/Dropbox/Thesis/Figures/big-route-dis.png")
