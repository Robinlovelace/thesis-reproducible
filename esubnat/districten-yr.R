# # dismap
# x = c("ggplot2", "sp", "rgeos", "mapproj", "rgdal", "maptools")
# lapply(x, require, character.only=T) # the R packages we'll be using
# search()
# 
# diss <- readOGR("/home/robin/Data/Nationaltravel/dislas/", "districts121")
# s1 <- object.size(diss) # 50 MB for only 308 polygons
# diss <- SpatialPolygonsDataFrame(Sr= gSimplify(diss, tol=500, topologyPreserve=T), data= diss@data)
# object.size(diss)/s1 # shrunk to only 8% of it's original size!
# plot(diss) # much faster
# dis <- diss # save orginal to repeat original code
# 
# diss <- readOGR("/home/robin/Data/Nationaltravel/dislas/", "dismode_la_eng")
# s1 <- object.size(diss) # 50 MB for only 308 polygons
# diss <- SpatialPolygonsDataFrame(Sr= gSimplify(diss, tol=500, topologyPreserve=T), data= diss@data)
# object.size(diss)/s1 # shrunk to only 8% of it's original size!
# plot(diss) # much faster
# 
# row.names(diss) <- as.character(1001:1046)
# spChFIDs(diss, x=as.character(1000:nrow(diss@data)+1000))
# d <- spRbind(diss, dis)
# plot(d)
# # plot(gUnion(diss, dis, byid=T))
# # d <- gUnion(diss, dis) # these failed
# diss <- d
# object.size(diss) # Mb: awesome
# rm(d,dis)
# 
# dbreaks <- c(2,5,10,20,30,40,60)
# dbreaks2 <- c("all", 0, dbreaks)
# modenames <- c("all", "metro", "train", "bus", "moto", "car", "carp", "taxi", "bike", "walk", "other")
# dbreaks <- c(2,5,10,20,30,40,60) # these are the distance breaks in the data
# dbreaks2 <- c("all", 0, dbreaks) # correct labels to use with our data
# newnames <- paste(rep(modenames, times=9), rep(dbreaks2, each=11), sep=".")
# diss@data$mfh <- diss@data$ST1210002 #
# diss@data <- diss@data[,-4] # remove the original mainly working from home column: gets in the way
# names(diss@data)[3:101] <- newnames
# 
# ecosts <- c(0,0.57,0.77,2.1,1.7,3,0,3,0.09,0.13,0) # these are the direct energy costs (MJ/KM) for each mode
# names(ecosts) <- c("All", "Tram", "Train", "Bus", "Moto", "Car.d", "Car.p", "Taxi", "Cycle", "Walk", "Other")
# 
# md <- read.csv("~/Dropbox/Thesis/Reproducible/energy-nat/dbxs.csv", header=T, ) # the distances
# mdv <- as.vector(as.matrix(md[,2:10])) # rearange the distances data
# edcosts <- rep(ecosts, 9) * mdv # multiply the energy costs by average distances for each of 9 modes
# names(edcosts) <- newnames
# # diss@data$other.60 <- 0
# ecs <- sweep(x=diss@data[,3:101], MARGIN=2, edcosts, '*' ) 
# barplot(colSums((ecs)[seq(6,by=11,length.out=9)]))
# paste(names(ecs),"en", sep=".")
# names(ecs) <- paste("moden",names(ecs), sep=".")
# names(ecs)
# diss@data <- cbind(diss@data, ecs)
# diss@data$ET <- rowSums(ecs)
# diss@data$EAV <- diss$ET / diss$all.all
# diss$abbed <- abbreviate(diss$ZONE_LABEL, minlength=2) # create abbreviated area names
# diss$id <- 1:nrow(diss@data)

head(ecs[13:15])
names(ecs)[12:22] # 11 modes of transport including 'all'
eyr <- c( 7.2,
          7.6,
          7.4,
          7.3,
          7.0,
          6.9,
          6.5,
          4.3)
eyr <- rep(c(0,eyr), times=1, each=11)
length(ecs)
cbind(names(ecs), eyr) # changes in frequency match changes in dist.
eyr <- eyr * 44 # Number of trips/yr
summary(eyr)
names(diss@data)[101:(100+99)]
eyr <- sweep(diss@data[101:(100+99)], MARGIN=2, eyr, '*')
head(eyr[,12:18])
ETyr <- rowSums(eyr) / diss$all.all
summary(ETyr / (365*3.6)) # energy costs/person/yr (kWh)

# load pops
list.files()
pops <- read.csv("pops-la.csv")
summary(pops[,c(3,5)])
pops$ZONE_CODE <- pops$Code
names(diss@data)
pops <- join(diss@data[c("ZONE_CODE", "all.all")], pops[c("ZONE_CODE", "All.ages")], )
head(pops)
pops$All.ages <- as.numeric(as.character(pops$All.ages))
pops$All.ages <- pops$All.ages * 1000
summary(pops$all.all/ pops$All.ages)
diss$ETyr <- ETyr
diss$ETyr.tot <- rowSums(eyr)
diss$kWhcommuterd <- ETyr / (365*3.6)
diss$kWhpd <- diss$kWhcommuterd * (pops$all.all/ pops$All.ages) # Average kWh/p/d
summary(diss$kWhpd )


diss@data[which(grepl("Barn",diss$ZONE_LABEL)),1:10]
### Comparison with total energy at LA level
toten <- read.csv("totenergy-la.csv")
head(toten[,2:6])
toten[which(grepl("Birming", toten$X)),] # definitely works...
toten$ZONE_LABEL <- toten[,2]
summary(toten[which(toten$ZONE_LABEL %in% diss$ZONE_LABEL),1:5]) # Yes! good match
toten <- join(diss@data[1:2], toten)
toten[which(grepl("Birming", toten$ZONE_LABEL)),] # test - works
nrow(toten) ; nrow(diss)
toten$Total.final.energy.consumption.at.regional.and.local.authority.level..2003.in.GWh..experimental. <- NULL
head(toten[1:5])
summary(toten[1:5]) # All factors
toten[1:5,1:5]
as.character(toten[1:5,5]) # it works
# toten[3:32] <- apply(toten[3:32], MARGIN=2, FUN='as.numeric(as.character)') # This fails
# summary(as.character(toten$X.1))
# head(as.character(toten$X.1))
# summary(as.numeric(as.character(toten$X.1))) # not working
# levels(as.factor(toten$X.1))

for(i in 3:ncol(toten)){
  toten[,i] <- as.numeric(gsub(",", "", as.character(toten[,i])))
}

toten[which(grepl("Birming", toten$ZONE_LABEL)),] # test - previous command broke it
summary(toten[1:5])
diss$toten <- toten$X.25 # energy use in GWh/yr
summary(toten[32] / toten$X.25)
diss$ttrans <- toten[,32]
toten[which(grepl("Birming", toten$ZONE_LABEL)),]
toten[which(grepl("Birming", toten$ZONE_LABEL)),32] # All transport
toten[which(grepl("Birming", toten$ZONE_LABEL)),14:18] # All transport
diss$rtrans <- toten[,14]

plot(diss$ET, diss$rtrans)
cor.test(diss$ETyr.tot, diss$rtrans)
cor.test(diss$ETyr.tot, diss$ttrans)
cor.test(diss$ETyr.tot, diss$toten)
cor(diss@data[,c(303:ncol(diss@data))], use="complete.obs")
cor(diss$ET, diss$ETyr.tot)
names(diss)

summary((diss$ETyr.tot/3.6) / (diss$toten * 1000000) )
summary((diss$ETyr.tot/3.6) / (diss$ttrans * 1000000) )

diss$prop.energy <- ((diss$ETyr.tot/3.6) / (diss$toten * 1000000) )
diss$prop.trans.en <- ((diss$ETyr.tot/3.6) / (diss$ttrans * 1000000) )

la <- fortify(diss, region="id")
la <- join(la, diss@data, by="id", match="first")
head(la)

object.size(la)/1000000
names(la) # 33 Mb for 400 tol
p <- ggplot(data=la, aes( x=long/1000, y=lat/1000))
p + geom_polygon(data=la,aes(x=long/1000, y=lat/1000, fill=prop.trans.en * 100, group=group)) + 
  scale_fill_continuous(low="green", high="red", name="Proportion\nof transport\nenergy use (%)") + coord_fixed() +
  xlab("Easting (km)") + ylab("Northing (km)") + 
  geom_path(data=fgor, aes(x=long/1000, y=lat/1000, group=group)) + theme_classic()

 ggsave("~/Dropbox/Thesis/Figures/prop-trans-energy.png", width=8, height = 8)

p <- ggplot(data=la, aes( x=long/1000, y=lat/1000))
p + geom_polygon(data=la,aes(x=long/1000, y=lat/1000, fill=prop.energy * 100, group=group)) + 
  scale_fill_continuous(low="green", high="red", name="Proportion\nof total\nenergy use (%)") + coord_fixed() +
  xlab("Easting (km)") + ylab("Northing (km)") + 
  geom_path(data=fgor, aes(x=long/1000, y=lat/1000, group=group)) + theme_classic()

ggsave("~/Dropbox/Thesis/Figures/prop-total-energy.png", width=8, height = 8)

sum(diss$ETyr.tot) / 1000000000 # MJ, GJ, TJ, PJ, EJ
sum(diss$ETyr.tot) / (1000000000 * 3.6) # TWh
sum(diss$ETyr.tot/3.6) / sum(diss$toten * 1000000, na.rm=T)
sum(diss$ETyr.tot/3.6) / sum(diss$ttrans * 1000000, na.rm=T)

reg.e.use <- c(83470.9,
215321.6,
181226.6,
136224.7,
155993.4,
167920.4,
175749.0,
246220.2,
135247.6)
sum(diss$ETyr.tot/3.6) / sum(reg.e.use * 1000000, na.rm=T)
which(is.na(diss$ttrans))
nrow(diss)

diss@data[which(diss$prop.energy > 0.09),c("ZONE_LABEL", "prop.energy")]
diss@data[which(diss$prop.energy < 0.01),c("ZONE_LABEL", "prop.energy")]

### Comparisons with CO2 data

head(diss@data[,1:10])
co2la <- read.csv("co2-ems-la.csv")
head(co2la[,1:5])
example(rename)
co2la <- rename(co2la, c("LAD10CD" = "ZONE_CODE"))
summary(co2la$ZONE_CODE)
summary(diss$ZONE_CODE)
co2la <- join(diss@data[,1:3], co2la)
nrow(co2la) ; nrow(diss@data)
head(co2la)

plot(co2la$Transport.Total, diss$ET)
cor.test(co2la$Transport.Total, diss$ET)

cor(diss$ET, co2la$B..Industry.and.Commercial.Gas,  use= "complete.obs")
cor(diss$ET, co2la$I..Road.Transport..A.roads.,  use= "complete.obs")
corme <- data.frame(ET = diss$ET, EAV = diss$car.all, Aroads = co2la$I..Road.Transport..A.roads.,
                    Mways = co2la$J..Road.Transport..Motorways., minor.roads = co2la$K..Road.Transport..Minor.roads., 
                    Trans.total = co2la$Transport.Total)
summary(co2la$)
round(cor(corme, use="complete.obs"), digits=2)
write.csv(round(cor(corme, use="complete.obs"), digits=2), "~/Dropbox/Thesis/Tables/co2-cors.csv")

# accounting for pops
plot(co2la$Transport.Total/diss$all.all, diss$EAV)
cor.test(co2la$Transport.Total/diss$all.all, diss$EAV)

plot(co2la$Per.Capita.Emissions..t., diss$EAV)
cor.test(co2la$Per.Capita.Emissions..t., diss$EAV)



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

# ggsave("~/Dropbox/Thesis/Figures/districten.png", width=8, height = 8)
diss@data[diss$EAV <25,c("ZONE_LABEL", "EAV")]
diss@data[diss$EAV >54,c("ZONE_LABEL", "EAV")]
sd(diss@data$EAV)
sd(gors@data$ET/gors$all.all)

ogrDrivers()
writeOGR(wards, dsn=".", layer="wardens", driver="ESRI Shapefile")
writeOGR(diss, dsn=".", layer="districtens", driver="ESRI Shapefile")

