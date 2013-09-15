# National energy map
nm <- read.csv("Mobiliteit__vervoerw_210213113510-dist-modes-regions.csv",
               sep = ";", header=T) # data too messed up - process in calc
nm <- read.csv("2011netherlands.csv", sep=";")
head(nm)
agrep(pattern=" (PV)", x=nm[,1], )
sub(pattern=" (PV)", replacement="", x=as.character(nm[,1])) # also don't work... calc
class(nm[,1])

nm <- read.csv("2011netherlands.csv", sep=";")
head(nm)
names(nm) <- gsub(pattern="Auto..passagier.", replacement="Car.p", x=names(nm))
names(nm) <- gsub(pattern="Auto..bestuurder.", replacement="Car.d", x=names(nm))
names(nm) <- gsub(pattern="Trein", replacement="Train", x=names(nm))
names(nm) <- gsub("Brom..snorfiets", "Moto", x=names(nm))
names(nm) <- gsub(names(nm[8]), "Bicycle", x=names(nm))
names(nm) <- gsub(names(nm[9]), "Walk", x=names(nm))
names(nm) <- gsub(names(nm[10]), "Other", x=names(nm))
names(nm)
# Now is where you add the energy estimates in 

library(rgeos)
library(rgdal)
regs <- readOGR(dsn=".", layer="Provinces")
plot(regs) # distant islands present
??area; 
gArea(regs, byid=T)
plot(regs[which(regs$code_hasc=="NL.LI" )])
# r2 <- regs[regs$code_hasc=="NL.LI",] # didn't work
r2 <- regs[2:nrow(regs@data),] # this did!
plot(r2)
head(r2@data) # remove excess cols
r2@data <- r2@data[,-c(1,3,4,5,6,7,8,9,10,11,12,14,15:18,20:ncol(r2@data))]

# match-up names
r2@data
nm[,1]
nm <- nm[-2,]
names(nm)
code2 <- strtrim(nm[,1], width=2)
nm$c2 <- toupper(code2)
c2 <- strsplit(as.character(r2$code_hasc), split="[.]" )
r2@data
r2@data$c2 <- unlist(c2)[seq(2,24,2)]
nm$Vervoerwijzen
nm$c2
r2$c2 ; r2$name

# plotr2: is it ok?
fr2 <- fortify(r2)
fr2 <- join(fr2,r2@data)
head(fr2)
ggplot() + geom_point(data=fr2, aes(x=long, y=lat)) + geom_text(aes(x=coordinates(r2)[,1],
          y = coordinates(r2)[,2], label = r2$c2 ))

merge(y=nm, x=r2@data, by="c2")
unmatched <- merge(y=nm, x=r2@data, by="c2")[,1]
unmatched
nm[which(nm$c2 %in% unmatched == F),1:3]
duplicated(regs@data$c2)
nm[,c(1,29)]
nm[10,29] # Says ZU, but should be ZH
nm[10,29] <- "ZH"
nm[9,29] # NO but should be NH, North Holland
nm[9,29] <- "NH"
nm$c2 <- sub("NO", "NB",  nm$c2)
nm[,c(1,29)]
head(nm) # still contains superfluous units
nm <- nm[-1,]
head(nm) # missing times + distances for key modes
class(nm$Car.d) # but still factors
for(i in 3:(ncol(nm)-1)){
  nm[,i] <- as.numeric(as.character(nm[,i]))
}
cbind(nm$Vervoerwijzen, nm$c2)
class(nm$Car.d) # numeric
natdis <- as.numeric(as.character(read.csv("2011netherlands.csv", sep=";")[2,12:19]))
(nm)[,12:19] # zeros must be replaced by nat. averages
for(i in 1:8){
  nm[which(nm[,i+11] == 0),i+11] <- natdis[i]
}
head(nm)
# now re-match
?merge # sort = T is default!!! hence wrong join
r2@data <- merge(x=r2@data, y=nm, by="c2", sort=F) # worked a treat

# re-classifying numerical cols
class(r2$Car.d)
as.numeric(r2$Car.d)
as.numeric(as.character(r2$Car.d))


rscale <- cut(r2$Car.d, breaks=c(0,0.25,0.3,0.9))
plot(r2, col=rscale) 
plot(coordinates(r2))
text(x=coordinates(r2[,1]), y=coordinates(r2[,2]), labels=r2$c2, )

# Now look at n. vertices
fr2 <- fortify(r2) # 2000 vertices
r3 <- gSimplify(r2, tol=0.01, topologyPreserve=T)
nrow(fortify(r3))
plot(r3)
r3 <- SpatialPolygonsDataFrame(r3, data=r2@data)
proj4string(r3)
r3 <- spTransform(r3, CRS("+init=epsg:27700"))

# Now ggplot it 
r3$id <- 1:length(r2$name)
fr3 <- fortify(r2)
head(fr3)
library(plyr)
fr3 <- join(x=fr3, y=r3@data)
head(fr3)
p <- ggplot(data = fr3, aes(x=long, y=lat, fill=Car.d))
p + geom_polygon(aes(group=group)) + geom_path(aes(group=group)) + 
  geom_text(data = r3@data ,aes(x=coordinates(r3)[,1],
                    y=coordinates(r3)[,2], label=r3$name)  )
# Now calculate the energy costs

names(fr3)
ecosts <- c(0,0.57,0.77,2.1,1.7,3,0,3,0.09,0.13,0)
ecosts <- c(3,0,)
# ecosts <- read.csv(textConnection("Bicycle,0.09,0.541,0.05,-,0.7
#                    Bus (local),2.1,0.85,0.15,0.30,3.4
#                    Car (small),2.5,0.99,0.38,0.30,4.1
#                    Car (average),3.0,1.21,0.56,0.30,5.1
#                    Car (large),3.9,1.58,0.87,0.30,6.7
#                    Coach,0.43,0.17,0.08,0.30,1.0
#                    Motorbike,1.7,0.70,0.33,0.30,3.1
#                    Train ,0.77,0.31,-,-,1.1
#                    Tram,0.57,0.38,-,-,1.0
#                    Walking,0.13,0.75,-,-,0.9"))
names(r3@data)
# r3@data[,7:14] <- (r3@data[,7:14]) * 2 # !!! Massively important: stops you dividing e. us by 2!
?sweep # commented out above: better to divide by proportion of total ttw for each area. totall != 0.5 always
r3$Totaal.vervoerwijzen <- as.numeric(as.character(r3$Totaal.vervoerwijzen))
1/r3$Totaal.vervoerwijzen
summary(1/r3$Totaal.vervoerwijzen) # Shows that 2 is a good estimate, but out by an average of 3.7%
example(sweep)
r3@data[,7:14] <- sweep(r3@data[,7:14], MARGIN=1, STATS=r3$Totaal.vervoerwijzen, FUN="/" )
rowSums(sweep(r3@data[,7:14], MARGIN=1, STATS=r3$Totaal.vervoerwijzen, FUN="/" ))
ec <- c(3,0,0.77,0.57,1.7,0.09,0.13,0)
names(ec) <- names(fr3[,14:21])
ec

ecosts <- r3@data[,7:14] # set-up correctly dimensioned df
for(i in 1:nrow(r3@data)){
  ecosts[i,] <- r3@data[i,7:14] * r3@data[i,16:23] * ec
}
head(r3@data)
r3@data$etot <- rowSums(ecosts)

# Now re-plot with total energy costs/trip estimated (MJ)
r3$id <- 1:length(r3$name)
fr3 <- fortify(r3, region="c2")
head(fr3)
names(fr3)[7] <- "c2"
fr3 <- join(x=fr3, y=r3@data, by="c2")
head(fr3)
fr3$lat <- fr3$lat / 1000
fr3$long <- fr3$long / 1000
head(fr3)
# if analysis alread done... load(".RData")
p <- ggplot(data = fr3, aes(x=long, y=lat))
p + geom_polygon(aes(group=group, fill=etot)) + geom_path(aes(group=group)) + 
  geom_text(data = r3@data ,aes(x=coordinates(r3)[,1]/1000,
                                y=coordinates(r3)[,2]/1000, label=r3$c2), color = "blue") +
  scale_fill_continuous(low="green", high="red", name="Etrp (MJ)",
                        limits=c(20,55)) + coord_fixed() +
  xlab("Easting (km)") + ylab("Northing (km)") #+ theme_minimal()

ggsave("~/Dropbox/Thesis/Figures/neth1.png", height=5.5, width=5.5)

# This looks good. Next challenge: include all distances + make scales the same.
head(nm) # missing times + distances for key modes
natdis <- read.csv("2011netherlands.csv", sep=";")[2,12:19]
head(nm)[,7:14]
