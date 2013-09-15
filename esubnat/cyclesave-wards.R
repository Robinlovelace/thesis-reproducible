head(yatwards@data) # raw data
# names(edcosts) <- names(yatwards@data[1,3:101]) # proper e.use/trp (mj) names
edcosts

# Select YatH subset
ci <- yatwards[which(yatwards$abbed == "YaTH"),]
plot(ci)
proj4string(ci) <- proj4string(miniwards)
gi <- gIntersects(miniwards, ci, byid=T)
yatwards <- miniwards[which(gi),]
plot(yatwards)

# different approach to intersection
ysss <- gIntersection(miniwards, ci)
plot(ysss)

cycols <-  which(grepl("bike", names(edcosts)))

carcols <- which(grepl("car[.]", names(edcosts)))
tail(yatwards@data[,carcols+2])
edcosts[carcols]
# number of car trips replaced:
crep <- sweep(yatwards@data[,carcols+2], 2, 
              STATS=c(0,0.5,0.3,0.05,0.01,0,0,0,0) ,"*") 
yatwards$cysave <- rowSums(sweep(crep, 2, edcosts[carcols], "*") ) / 
  yatwards$all.all
yatwards$cysavet <- yatwards$cysave - rowSums(sweep(crep, 2, edcosts[cycols], "*")) / 
  yatwards$all.all # total energy saving (after e.use bikes subtracted)
yatwards$cysave.prop <- yatwards$cysave / (yatwards$ET / yatwards$all.all) * 100
yatwards$cysave.propt <- yatwards$cysavet / (yatwards$ET / yatwards$all.all) * 100
summary(yatwards$cysave.prop) / summary(yatwards$cysave.propt)
summary(yatwards$cysave.prop) ; summary(yatwards$cysave.propt)
sum(yatwards$cysave.propt * yatwards$all.all) / sum(yatwards$all.all) # total prop e. use

names(yatwards)
yatwards$id <- 1:nrow(yatwards@data)

la <- fortify(yatwards, region="id")
la <- join(la, yatwards@data, by="id", match="first")
head(la)

p <- ggplot(data=la, aes( x=long/1000, y=lat/1000))
p <- ggplot()
p + geom_polygon(data=la,aes(x=long/1000, y=lat/1000, fill=cysave.propt, group=group)) + 
  scale_fill_continuous(low="green", high="red", name="Etrp (MJ)") + coord_fixed() +
  xlab("Easting (km)") + ylab("Northing (km)") + theme_classic()
ggsave("cysave.png") # good, but need more bells + whistles

# Bells + whistles

# yath outline
yout <- (fgor[grepl(pattern="York", fgor$id),])
head(yout) 
p + geom_path(data=yout, aes(x=long/1000, y=lat/1000, group=group)) + theme_classic()  +
  geom_point(aes(x=coordinates(UKcssp)[,1]/1000, y=coordinates(UKcssp)[,2]/1000)) # next element to add

UKcs <- (world.cities[world.cities$country.etc == "UK",])
plot(UKcs$long, UKcs$lat)
UKcs <- UKcs[UKcs$pop > 50000,]
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
UKcssp <- UKcssp[which(UKcssp$lat > 380000 & UKcssp$lat < 510000),]
UKcssp <- UKcssp[which(UKcssp$long > 400000 & UKcssp$long < 550000),]

ggplot() + geom_polygon(data=la, aes(x=long/1000, y=lat/1000, fill=cysave.propt, group=group)) + 
  scale_fill_continuous(low="green", high="red", name="Energy\nsaving (%)") + coord_fixed() +
  xlab("Easting (km)") + ylab("Northing (km)") + 
#   geom_path(data=yout, aes(x=long/1000, y=lat/1000, group=group))   +
  geom_path(data=la,aes(x=long/1000, y=lat/1000, color=cysave.propt, group=group)) + 
  scale_color_continuous(low="green", high="red", name="Energy\nsaving (%)") +
  geom_point(aes(x=coordinates(UKcssp)[,1]/1000, y=coordinates(UKcssp)[,2]/1000), shape=1, size=7/3*2) + 
  geom_point(aes(x=coordinates(UKcssp)[,1]/1000, y=coordinates(UKcssp)[,2]/1000, size = UKcssp$pop), shape=4) +
  scale_shape(solid=F) + scale_size_continuous(name="Population") +
  theme_classic() # next element to add

ggsave("~/Dropbox/Thesis/Figures/cysave.png") # good
