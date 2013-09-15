#plotalll
load("wards.RData")
library(rgeos)
load("wards.RData")
miniwards <- SpatialPolygonsDataFrame(Sr= gSimplify(wards, tol=5, topologyPreserve=T), data= wards@data)
object.size(miniwards) / object.size(wards) # only 0.83 in tol=800, 0.93 in tol=5
plot(wards[grepl("00A", wards$ZONE_CODE) ,], col="red")
plot(miniwards[grepl("00A", wards$ZONE_CODE) ,], col="red") 
head(wards@data)
la <- fortify(miniwards, region="ZONE_LABEL") # 500,000 odd rows in tol=5 - minutes to run

head(la)
wtrains <- grepl("moden.train.[0-9]", names(wards), perl=T)
names(wards)
names(wards)[wtrains]
head(wards@data[wtrains])
head(wards@data)
wards$ET.train <- rowSums(wards@data[wtrains])
wards$id <- as.character(wards$ZONE_LABEL)

la <- join(la, wards@data[c("id", "EAV", "ET", "ET.train")], match="first")
head(la)

object.size(la)/1000000
names(la) # 33 Mb for 400 tol
# la <- la[c(1:8, 15, 16)]
object.size(la)/1000000 # 30 MB with tol=800 # 33 MB with tol=400
save(la, file="lamini.RData")
head(la)

### fromla.mini
load("lamini.RData")
head(la)
p <- ggplot(data=la, aes( x=long/1000, y=lat/1000))
p + geom_polygon(data=la,aes(x=long/1000, y=lat/1000, fill=EAV, group=group)) + 
  scale_fill_continuous(low="green", high="red", name="Etrp (MJ)",
                        limits=c(10,80)) + coord_fixed() +
  xlab("Easting (km)") + ylab("Northing (km)") # looking good - finally plotted it all!
ggsave("engplotall-tol100.png")
### Add gors
setwd("../energy-nat/EN-Data/") # navigate to the correct working directory
library(rgdal)
gors <- readOGR(".", layer="GOR_st121" ) # load the data!
gors$abbed <- abbreviate(gors$ZONE_LABEL, minlength=2) # create abbreviated area names
gors@data[1:5,c("ZONE_LABEL", "abbed")] # check the abbreviation worked
plot(gors) # results in a plot of England
text(coordinates(gors), labels=gors$abbed, col="red") # add our region names: in the right place?

fgor <- fortify(gors, region="ZONE_LABEL")
fgor <- merge(fgor, gors@data[,c(1,2,3,8,ncol(gors@data))], 
              by.x = "id", by.y="ZONE_LABEL")
head(fgor)
gortex <- coordinates(gors)/1000 
gortex <- as.data.frame(cbind(gortex))
gortex$V3 <- gors$abbed
head(gortex)
class(gortex$V1)

ggplot() + geom_polygon(data=la,aes(x=long/1000, y=lat/1000, fill=EAV, group=group)) + 
  scale_fill_continuous(low="green", high="red", name="Etrp (MJ)",
                        limits=c(10,80)) + coord_fixed() +
  xlab("Easting (km)") + ylab("Northing (km)") + # looking good - finally plotted it all!
  geom_path(data=fgor,aes(x=long/1000, y=lat/1000,group=group)) +
  geom_text(data=gortex, aes(x=V1, y=V2, label=V3), colour="blue")

ggsave("engplot-compare.png", width=8, height = 8)

## Add cities
library(maps)
data(world.cities)
plot(world.cities$long, world.cities$lat)
head(world.cities)
UKcs <- (world.cities[world.cities$country.etc == "UK",])
plot(UKcs$long, UKcs$lat)
UKcs <- UKcs[UKcs$pop > 100000,]
UKcs <- SpatialPointsDataFrame(coords=UKcs[,c("long", "lat")], proj4string=CRS("+init=epsg:4326" ), 
                              data=UKcs)
plot(UKcs)
head(coordinates(UKcs))
proj4string(UKcs)
plot(gors)
coordinates(gors)
points(UKcs)
UKcssp <- spTransform(x=UKcs, CRS("+init=epsg:27700")) # sloppy typing error...
head(coordinates(UKcssp))
plot((UKcssp))
plot(gors) ; points(UKcssp)

UKcssp$pname <- NA
UKcssp$pname[UKcs@data$pop > 200000] <- UKcssp$name[UKcs@data$pop > 200000]
head(UKcssp@data)
UKcssp$paname <- abbreviate(UKcssp$pname, minlength=3)
text(coordinates(UKcssp), labels=UKcssp$paname)
# remove extraneous points
UKcssp <- UKcssp[!UKcssp$lat > 600000,]
excludeme <- grepl("Belf|Cardi|Swanse|Newpor", UKcssp$name)
plot(UKcssp)
UKcssp <- UKcssp[!excludeme,]
plot(UKcssp)

ggplot() + geom_polygon(data=la,aes(x=long/1000, y=lat/1000, fill=EAV, group=group)) + 
  scale_fill_continuous(low="green", high="red", name="Etrp (MJ)",
                        limits=c(10,80)) + coord_equal() +  #coord_map() +
  xlab("Easting (km)") + ylab("Northing (km)") + # looking good - finally plotted it all!
  geom_path(data=fgor,aes(x=long/1000, y=lat/1000,group=group)) +
  geom_text(data=gortex, aes(x=V1, y=V2, label=V3), colour="blue") +
  geom_point(aes(x=coordinates(UKcssp)[,1]/1000, y=coordinates(UKcssp)[,2]/1000)) +
  # geom_text(aes(x=coordinates(UKcssp)[,1]/1000, y=coordinates(UKcssp)[,2]/1000, label=UKcssp$paname )) +
  xlim(bbox(wards)[1,]/1000) + ylim(bbox(wards)[2,]/1000)

ggsave("~/Dropbox/Thesis/Figures/engplotnm.png", width=8, height = 8)

plot(wards[which.min(wards$EAV),])
wards[which.max(wards$EAV),]
1451/2139


### Plot for trains
load("lamini.RData") ## 1 million-odd
load("wards.RData")
object.size(wards)
head(wards@data)

wtrains <- grepl("moden.train.[0-9]", names(wards), perl=T)
names(wards)
names(wards)[wtrains]
head(wards@data[wtrains])
head(wards@data)
wards$ET.train <- rowSums(wards@data[wtrains])
wards$id <- as.character(wards$ZONE_LABEL)
library(plyr)
wards2 <- wards@data[,c("id", "ET.train", "all.all", "ET", "EAV")]
str(la)
?join
la <- join(la, wards2, by="id", match="first")
# la <- merge(la, wards@data[,c("id", "ET.train", "all.all")])
head(la)
la$Train.energy.use <- la$ET.train / la$ET * 100
la$ET.trainqs <- cut(la$ET.train/1000, breaks= quantile(la$ET.train/1000, na.rm=T))
levels(la$ET.trainqs) <- paste("q",1:4, sep="")

engout <- readOGR("/home/robin/Data/Nationaltravel/Just-england/", "England_boundary")
rails <- readOGR("/home/robin/gis-data/Strategicvectordata/gb_south-20091211/", "railway_polyline")
railn <- readOGR("/home/robin/gis-data/Strategicvectordata/north/", "railway_polyline")
gI <- gIntersects(railn, engout, byid=T)
railn <- railn[which(gI),]
# plot(railn[which(gI),])
gI <- gIntersects(rails, engout, byid=T)
rails <- rails[which(gI),]
rfn <- fortify(railn)
rfs <- fortify(rails)
summary(la$Train.energy.use)
fengout <- fortify(engout)

ggplot() + geom_polygon(data=la[which(la$Train.energy.use > 3),],
                        aes(x=long/1000, y=lat/1000, fill=Train.energy.use, group=group)) + 
  scale_fill_continuous(low="green", high="red", name = "% energy \n use by train") + coord_equal() +  #coord_map() +
  xlab("Easting (km)") + ylab("Northing (km)") + # looking good - finally plotted it all!
#   geom_path(data=fgor,aes(x=long/1000, y=lat/1000,group=group)) +
#   geom_text(data=gortex, aes(x=V1, y=V2, label=V3), colour="blue") +
#   geom_point(aes(x=coordinates(UKcssp)[,1]/1000, y=coordinates(UKcssp)[,2]/1000)) +
#   geom_text(aes(x=coordinates(UKcssp)[,1]/1000, y=coordinates(UKcssp)[,2]/1000, label=UKcssp$paname )) +
  geom_path(data=rfn, aes(x=long/1000, y=lat/1000,group=group)) +
  geom_path(data=rfs, aes(x=long/1000, y=lat/1000,group=group)) +
  geom_path(data=fengout,aes(x=long/1000, y=lat/1000, group=group)) +
  xlim(bbox(wards)[1,]/1000) + ylim(bbox(wards)[2,]/1000)   +
  theme_classic()+
  theme(legend.position = c(0.2,0.6))

ggsave("~/Dropbox/Thesis/Figures/trainen.png", , width=8, height = 8)




