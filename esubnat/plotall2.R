#plotalll
load("wards.RData")
library(rgeos)
miniwards <- SpatialPolygonsDataFrame(Sr= gSimplify(wards, tol=5, topologyPreserve=T), data= wards@data)
object.size(miniwards) / object.size(wards) # only 0.83 in tol=800, 0.93 in tol=5
plot(wards[grepl("00A", wards$ZONE_CODE) ,], col="red")
plot(miniwards[grepl("00A", wards$ZONE_CODE) ,], col="red") 

head(wards@data)
la <- fortify(miniwards, region="ZONE_LABEL")
la <- merge(la, wards@data[c("ZONE_LABEL", "EAV", "ET")], 
            by.x = "id", by.y="ZONE_LABEL")
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

ggsave("engplot.png", width=8, height = 8)

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
load("la.RData") ## 1 million-odd
load("wards.RData")

wtrains <- grepl("moden.train.[0-9]", names(wards), perl=T)
names(wards)
names(wards)[wtrains]
head(wards@data[wtrains])
wards$ET.train <- rowSums(wards@data[wtrains])
head(wards$ET.train)
wards$ET.train <- wards$ET.train / wards$all.all
wards$id <- as.character(wards$ZONE_LABEL)
la <- join(la, wards@data[c("id", "ET.train")])
head(la)
la$ET.trainqs <- cut(la$ET.train/1000, breaks= quantile(la$ET.train/1000, na.rm=T))
levels(la$ET.trainqs) <- paste("q",1:4, sep="")
la$ET.trainlog <- log10(la$ET.train)
la$ET.trainqs <- cut(la$ET.train/1000, breaks= quantile(la$ET.train/1000, na.rm=T, probs=c(0.7,0.8,0.9,1)))
levels(la$ET.trainqs) 
la$Train.energy.use <- la$ET.trainqs
levels(la$Train.energy.use) <- c("high", "very high", "highest")

ggplot() + geom_polygon(data=la[-which(is.na(la$ET.trainqs)),],
                        aes(x=long/1000, y=lat/1000, fill=Train.energy.use, group=group)) + 
  scale_fill_manual(values=c("yellow", "orange", "red")) + coord_equal() +  #coord_map() +
  xlab("Easting (km)") + ylab("Northing (km)") + # looking good - finally plotted it all!
  geom_path(data=fgor,aes(x=long/1000, y=lat/1000,group=group)) +
#   geom_text(data=gortex, aes(x=V1, y=V2, label=V3), colour="blue") +
#   geom_point(aes(x=coordinates(UKcssp)[,1]/1000, y=coordinates(UKcssp)[,2]/1000)) +
#   geom_text(aes(x=coordinates(UKcssp)[,1]/1000, y=coordinates(UKcssp)[,2]/1000, label=UKcssp$paname )) +
  xlim(bbox(wards)[1,]/1000) + ylim(bbox(wards)[2,]/1000)

ggsave("~/Dropbox/Thesis/Figures/trainen.png")

### train layer
setwd("/media/AA5EAF5C5EAF2055/GIS/GIS-uk-data/Strategicvectordata/gb_south-20091211")
rails <- readOGR(".", "railway_polyline")
plot(railn)
head(rails@data)
setwd("/media/AA5EAF5C5EAF2055/GIS/GIS-uk-data/Strategicvectordata/north/")
railn <- readOGR(".", "railway_polyline")
railt <- spRbind(railn, rails)
railt <- fortify(railn)
head(railt)
qplot(data=railt, x=long, y=lat, group=group, geom="path") +
  geom_path(data=fgor,aes(x=long, y=lat,group=group)) 
  
  
railt2 <-  fortify(rails)

gI <- gIntersects(railn, gors, byid=T)
railn <- railn[which(gI),]
engout <- gUnaryUnion(gors) # works, but there are still objects left
engout <- unionSpatialPolygons(gors, IDs=rep(1,nrow(gors@data)))
plot(engout)

setwd("/home/robin/Data/Nationaltravel/Just-england")
engout <- readOGR(".", "England_boundary")
gI <- gIntersects(railn, engout, byid=T)
railn <- railn[which(gI),]
plot(railn)

gI <- gIntersects(rails, engout, byid=T)
rails <- rails[which(gI),]
plot(rails)

railt <- fortify(railn)
head(railt)
railt2 <-  fortify(rails)
qplot(data=railt, x=long, y=lat, group=group, geom="path") +
  geom_path(data=fgor,aes(x=long, y=lat,group=group)) 

ggplot() + geom_polygon(data=la[-which(is.na(la$ET.trainqs)),],
                        aes(x=long/1000, y=lat/1000, fill=Train.energy.use, group=group)) + 
  scale_fill_manual(values=c("yellow", "orange", "red")) + coord_equal() +  #coord_map() +
  xlab("Easting (km)") + ylab("Northing (km)") + # looking good - finally plotted it all!
#   geom_path(data=fgor,aes(x=long/1000, y=lat/1000,group=group)) +
  #   geom_text(data=gortex, aes(x=V1, y=V2, label=V3), colour="blue") +
  #   geom_point(aes(x=coordinates(UKcssp)[,1]/1000, y=coordinates(UKcssp)[,2]/1000)) +
  #   geom_text(aes(x=coordinates(UKcssp)[,1]/1000, y=coordinates(UKcssp)[,2]/1000, label=UKcssp$paname )) +
  geom_path(data=railt, aes(x=long/1000, y=lat/1000, group=group)) +
  geom_path(data=railt2, aes(x=long/1000, y=lat/1000, group=group)) +
  xlim(bbox(wards)[1,]/1000) + ylim(bbox(wards)[2,]/1000)

### Plot as perc. train use
la <- join(la, wards@data[c("id", "all.all")])


