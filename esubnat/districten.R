# dismap
x = c("ggplot2", "sp", "rgeos", "mapproj", "rgdal", "maptools")
lapply(x, require, character.only=T) # the R packages we'll be using
search()

diss <- readOGR("/home/robin/Data/Nationaltravel/dislas/", "districts121")
s1 <- object.size(diss) # 50 MB for only 308 polygons
diss <- SpatialPolygonsDataFrame(Sr= gSimplify(diss, tol=500, topologyPreserve=T), data= diss@data)
object.size(diss)/s1 # shrunk to only 8% of it's original size!
plot(diss) # much faster
dis <- diss # save orginal to repeat original code

diss <- readOGR("/home/robin/Data/Nationaltravel/dislas/", "dismode_la_eng")
s1 <- object.size(diss) # 50 MB for only 308 polygons
diss <- SpatialPolygonsDataFrame(Sr= gSimplify(diss, tol=500, topologyPreserve=T), data= diss@data)
object.size(diss)/s1 # shrunk to only 8% of it's original size!
plot(diss) # much faster

row.names(diss) <- as.character(1001:1046)
spChFIDs(diss, x=as.character(1000:nrow(diss@data)+1000))
d <- spRbind(diss, dis)
plot(d)
# plot(gUnion(diss, dis, byid=T))
# d <- gUnion(diss, dis) # these failed
diss <- d
object.size(diss) # Mb: awesome
rm(d,dis)

dbreaks <- c(2,5,10,20,30,40,60)
dbreaks2 <- c("all", 0, dbreaks)
modenames <- c("all", "metro", "train", "bus", "moto", "car", "carp", "taxi", "bike", "walk", "other")
dbreaks <- c(2,5,10,20,30,40,60) # these are the distance breaks in the data
dbreaks2 <- c("all", 0, dbreaks) # correct labels to use with our data
newnames <- paste(rep(modenames, times=9), rep(dbreaks2, each=11), sep=".")
diss@data$mfh <- diss@data$ST1210002 #
diss@data <- diss@data[,-4] # remove the original mainly working from home column: gets in the way
names(diss@data)[3:101] <- newnames

ecosts <- c(0,0.57,0.77,2.1,1.7,3,0,3,0.09,0.13,0) # these are the direct energy costs (MJ/KM) for each mode
names(ecosts) <- c("All", "Tram", "Train", "Bus", "Moto", "Car.d", "Car.p", "Taxi", "Cycle", "Walk", "Other")

md <- read.csv("~/Dropbox/Thesis/Reproducible/energy-nat/dbxs.csv", header=T, ) # the distances
mdv <- as.vector(as.matrix(md[,2:10])) # rearange the distances data
edcosts <- rep(ecosts, 9) * mdv # multiply the energy costs by average distances fo each of 9 modes
names(edcosts) <- newnames
# diss@data$other.60 <- 0
ecs <- sweep(x=diss@data[,3:101], MARGIN=2, edcosts, '*' ) 
barplot(colSums((ecs)[seq(6,by=11,length.out=9)]))
paste(names(ecs),"en", sep=".")
names(ecs) <- paste("moden",names(ecs), sep=".")
names(ecs)
diss@data <- cbind(diss@data, ecs)
diss@data$ET <- rowSums(ecs)
diss@data$EAV <- diss$ET / diss$all.all
diss$abbed <- abbreviate(diss$ZONE_LABEL, minlength=2) # create abbreviated area names
diss$id <- 1:nrow(diss@data)

la <- fortify(diss, region="id")
la <- join(la, diss@data, by="id", match="first")
head(la)

object.size(la)/1000000
names(la) # 33 Mb for 400 tol
p <- ggplot(data=la, aes( x=long/1000, y=lat/1000))
p + geom_polygon(data=la,aes(x=long/1000, y=lat/1000, fill=ET/all.all, group=group)) + 
  scale_fill_continuous(low="green", high="red", name="Etrp (MJ)") + coord_fixed() +
  xlab("Easting (km)") + ylab("Northing (km)") + 
  geom_path(data=fgor, aes(x=long/1000, y=lat/1000, group=group)) + theme_classic()
ggsave("~/Dropbox/Thesis/Figures/districten.png", width=8, height = 8)
diss@data[diss$EAV <25,c("ZONE_LABEL", "EAV")]
diss@data[diss$EAV >54,c("ZONE_LABEL", "EAV")]
sd(diss@data$EAV)
sd(gors@data$ET/gors$all.all)

ogrDrivers()
writeOGR(wards, dsn=".", layer="wardens", driver="ESRI Shapefile")
writeOGR(diss, dsn=".", layer="districtens", driver="ESRI Shapefile")

