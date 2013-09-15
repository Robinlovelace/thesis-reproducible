#plotalll
miniwards <- wards
load("wards.RData")
head(wards@data)
miniwards@data <- wards@data
la <- fortify(miniwards, region="ZONE_LABEL")
la <- merge(la, wards@data, 
            by.x = "id", by.y="ZONE_LABEL")
object.size(la)
la <- la[c(1:8)]
object.size(la)
names(la)
la <- la[c(1:8, 15, 16)]
object.size(la)
save(la, file="lamini.RData")
head(la)

### fromla.mini
load("lamini.RData")
p <- ggplot(data=la, aes( x=long/1000, y=lat/1000))
p + geom_polygon(data=la,aes(x=long/1000, y=lat/1000, fill=ET/all.all, group=group)) + 
  scale_fill_continuous(low="green", high="red", name="Etrp (MJ)",
                        limits=c(10,80)) + coord_fixed() +
  xlab("Easting (km)") + ylab("Northing (km)") 