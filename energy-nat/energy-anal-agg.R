# Aim: load aggregate data and calculated energy costs at the national level
x = c("ggplot2", "sp", "maptools", "rgeos", "mapproj", "rgdal")
lapply(x, require, character.only=T) # the R packages we'll be using

setwd("~/Data/Nationaltravel/gor/")
gors <- readOGR(".", layer="GOR_st121-esmaller" )
gors$abbed <- abbreviate(gors$ZONE_LABEL, minlength=2)
setwd("~/Dropbox/Thesis/Reproducible/energy-nat")
class(gors)
head(gors@data)
plot(gors)
la.en <- SpatialPolygonsDataFrame(Sr=gSimplify(gors, tol=1000, topologyPreserve=T), data=gors@data )  
class(la.en)
plot(la.en)

dbreaks <- c(2,5,10,20,30,40,60)
dbreaks2 <- c("all", 0, dbreaks)
# Correct labels for the data...
# Other than 002 it's an 11 by 9 matrix
modenames <- c("all", "metro", "train", "bus", "moto", "car", "carp", "taxi", "bike", "walk", "other")
newnames <- paste(rep(modenames, times=9), rep(dbreaks2, each=11), sep=".")
la.en@data$mfh <- la.en@data$ST1210002
la.en@data <- la.en@data[,-4]
names(la.en@data)[3:101] <- newnames
ecosts <- read.csv("~/Dropbox/Thesis/Tables/final-e-justpres.csv") # just reformat
ecosts <- c(0,0.57,0.77,2.1,1.7,3,0,3,0.09,0.13,0)
names(ecosts) <- c("All", "Tram", "Train", "Bus", "Moto", "Car.d", "Car.p", "Taxi", "Cycle", "Walk", "Other")

mdis <- read.csv("~/1PhD/Core data/dboxes.csv", header=T)
mdis <- mdis[-1,] * 1.4 # very Important - adding circuity here!!!
md <- mdis[c(6,5,4,3,1,2,10,7,8,9),] # for nl-en-compare
md <- rbind(0,md)
md[is.na(md)] <- 0
md <- cbind(0,md)

mdv <- as.vector(as.matrix(md))
edcosts <- rep(ecosts, 9) * mdv
ecs <- sweep(x=la.en@data[,3:101], MARGIN=2,edcosts, '*' )
names(ecs)[seq(2,by=11,length.out=9)] # all metro 
ecs.mode <- cbind(rowSums(ecs[,seq(2,by=11,length.out=9)]))
ecs.mode <- data.frame(array(dim = c(9,11)))
for(i in 1:11){
  ecs.mode[,i] <- cbind(rowSums(ecs[,seq(from=i,by=11,length.out=9)]))
}
paste(names(ecs),"en", sep=".")
names(ecs.mode) <- paste("moden",modenames, sep=".")
la.en@data <- cbind(la.en@data, ecs.mode)
la.en@data$ET <- rowSums(ecs)
plot(la.en, col = rowSums(ecs))
head(la.en@data)
class(la.en@data)
for(i in 1:ncol(la.en@data)){
  la.en@data[,i] <- as.numeric(la.en@data[,i])
}
writeOGR(obj=la.en, dsn=".", layer="gorens", driver="ESRI Shapefile")

# convert to ggplot2, just for plotting
la <- fortify(la.en, region="ZONE_LABEL")
la <- merge(la, la.en@data[,c(1,2,3,8,ncol(la.en@data))], 
            by.x = "id", by.y="ZONE_LABEL")
head(la)
gortex <- coordinates(gors)/1000 
gortex <- as.data.frame(cbind(gortex))
gortex$V3 <- gors$abbed
head(gortex)
class(gortex$V1)

p <- ggplot(data=la, aes( x=long/1000, y=lat/1000))
p + geom_polygon(data=la,aes(x=long/1000, y=lat/1000, fill=ET/all.all, group=group)) + 
  geom_path(data=la,aes(x=long/1000, y=lat/1000,group=group)) +
  scale_fill_continuous(low="green", high="red", name="Etrp (MJ)",
                        limits=c(20,55)) + coord_fixed() +
  xlab("Easting (km)") + ylab("Northing (km)") +
  geom_text(data=gortex, aes(x=V1, y=V2, label=V3), colour="blue") #+ theme_minimal()

# now by mode...
ggsave("~/Dropbox/Thesis/Figures/goren.png", scale=1, width= 5.5, height=5.5)

summary(la) / la$all.al
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

??ticks