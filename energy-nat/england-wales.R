# wales vs UK
x = c("ggplot2", "sp", "rgeos", "mapproj", "rgdal", "maptools")
lapply(x, require, character.only=T) # the R packages we'll be using
load("../esubnat/.RData")
w <- readOGR("/home/robin/Data/Nationaltravel/", "wales_121")
w@data
plot(w)
w <- SpatialPolygonsDataFrame(gSimplify(w, tol=1000), data=w@data)
row.names(w) <- "2"
load("engout.RData")
row.names(engout) <- "1"

edat <- read.csv("/home/robin/Data/Nationaltravel/england_121.csv")
engout <- SpatialPolygonsDataFrame(engout, data=edat)
engout@data
w@data
names(engout)
names(w)
names(engout) <- names(w)

ew <- rbind.SpatialPolygonsDataFrame(engout,w)
plot(ew)
ew@data[,4] <- NULL
names(ew@data)
ecs <- sweep(x=ew@data[,3:101], 2, edcosts, "*")
paste(names(ecs),"en", sep=".")
names(ecs) <- paste("moden",names(ecs), sep=".")
names(ecs)
ew@data <- cbind(ew@data, ecs)
ew@data$ET <- rowSums(ecs)
ew@data$EAV <- ew$ET / ew$ST1210001
ew$id <- as.character(ew$ZONE_LABEL)
few <- fortify(ew, region="id")
few <- join(few, ew@data, by="id", match="first")
head(few)

ggplot(few, aes(x=long/1000, y=lat/1000, fill=EAV, group=group)) + geom_polygon() +
  scale_fill_continuous(low="green", high="red", name="Etrp (MJ)") + coord_fixed() +
  xlab("Easting (km)") + ylab("Northing (km)") + theme_classic()  #+ theme_minimal()
ggsave("~/Dropbox/Thesis/Figures/ew.png")
ew$EAV[2]/ew$EAV[1]
