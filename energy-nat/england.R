# england.R - loads english commuting data and plots it
# (run before netherlands.R and nl-en-compare.R for comparison of energy costs)
x = c("ggplot2", "sp", "rgeos", "mapproj", "rgdal", "maptools")
lapply(x, require, character.only=T) # the R packages we'll be using

setwd("EN-Data/")
gors <- readOGR(".", layer="GOR_st121" )

setwd("../")
getwd() # check we're in the correct directory: energy-nat
class(gors) # check we've got the right R object: SpatialPolygonsDataFrame
gors@data[,1:5] # take a look at the first 5 columns of data: integer counts
gors$abbed <- abbreviate(gors$ZONE_LABEL, minlength=2) # create abbreviated area names
gors@data[1:5,c("ZONE_LABEL", "abbed")] # check the abbreviation worked
plot(gors) # results in a plot of England
text(coordinates(gors), labels=gors$abbed, col="red") # add our region names: in the right place?


dbreaks <- c(2,5,10,20,30,40,60)
dbreaks2 <- c("all", 0, dbreaks)
# Correct labels for the data...
# Other than 002 it's an 11 by 9 matrix
modenames <- c("all", "metro", "train", "bus", "moto", "car", "carp", "taxi", "bike", "walk", "other")
newnames <- paste(rep(modenames, times=9), rep(dbreaks2, each=11), sep=".")
gors@data$mfh <- gors@data$ST1210002 #
gors@data <- gors@data[,-4] # remove the original mainly working from home column: gets in the way
names(gors@data)[3:101] <- newnames
# ecosts <- read.csv("~/Dropbox/Thesis/Tables/final-e-justpres.csv") # just reformat
ecosts <- c(0,0.57,0.77,2.1,1.7,3,0,3,0.09,0.13,0)
ecosts <- c(0,0.57,0.77,2.1,1.7,3,0,3,0.09,0.13,0) # these are the direct energy costs (MJ/KM) for each mode
names(ecosts) <- c("All", "Tram", "Train", "Bus", "Moto", "Car.d", "Car.p", "Taxi", "Cycle", "Walk", "Other")

mdis <- read.csv("~/1PhD/Core data/dboxes.csv", header=T)
mdis <- mdis[-1,] * 1.4 # very Important - adding circuity here!!!
md <- mdis[c(6,5,4,3,1,2,10,7,8,9),] # for nl-en-compare
md <- read.csv("dbxs.csv", header=T, ) # the distances associated with each distance bin
plot(rep(c(0,0,1,3.5,7.5,15,25,35,50,100),11), t(md)) # visualise them: make sense?

mdv <- as.vector(as.matrix(md[,2:10])) # rearange the distances data
edcosts <- rep(ecosts, 9) * mdv # multiply the energy costs by average distances fo each of 9 modes
ecs <- sweep(x=gors@data[,3:101], MARGIN=2, edcosts, '*' ) # multiply the count data in each cell by their respective energy cost, for every zone
(ecs)[seq(6,by=11,length.out=9)] # test for one mode: cars (for all zones)
ecs.mode <- cbind(rowSums(ecs[,seq(2,by=11,length.out=9)]))
ecs.mode <- data.frame(array(dim = c(9,11)))
for(i in 1:11){
  ecs.mode[,i] <- cbind(rowSums(ecs[,seq(from=i,by=11,length.out=9)]))
}
paste(names(ecs),"en", sep=".")
names(ecs.mode) <- paste("moden",modenames, sep=".")
gors@data <- cbind(gors@data, ecs.mode)
gors@data$ET <- rowSums(ecs)
plot(gors, col = rowSums(ecs))

head(gors@data)
class(gors@data)
names(gors)
for(i in 3:100){
  gors@data[,i] <- as.numeric(gors@data[,i])
}
writeOGR(obj=gors, dsn=".", layer="gorens", driver="ESRI Shapefile")

# convert to ggplot2, just for plotting
la <- fortify(gors, region="ZONE_LABEL")
la <- merge(la, gors@data[,c(1,2,3,8,ncol(gors@data))], 
            by.x = "id", by.y="ZONE_LABEL")
head(la)
gortex <- coordinates(gors)/1000 
gortex <- as.data.frame(cbind(gortex))
gortex$V3 <- as.character(gors$abbed)
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

# saving the files to .shps
setwd("~/Desktop/gorens/")
writeOGR(obj=miniwards, dsn=".", layer="wards", driver="ESRI Shapefile")
writeOGR(obj=c, dsn=".", layer="counties", driver="ESRI Shapefile")

