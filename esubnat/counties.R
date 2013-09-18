### English counties map, based on existing 'esubnat' project
counties <- readOGR("/home/robin/Data/Nationaltravel/eng-counties/", "output")
lass <- readOGR("/home/robin/Data/Nationaltravel/dislas/", "dismode_la_eng")
row.names(lass) <- as.character(42:(nrow(lass@data)+41))
c <- spRbind(lass, counties)
object.sizes()
c <- SpatialPolygonsDataFrame(gSimplify(c, tol=500), data=c@data)
object.sizes()
plot(c)

rm(counties, lass)
# cs <- sample(1:nrow(lass), size=nrow(c), replace=T) # delme
# c@data[,3:102] <- read.dbf("/home/robin/Data/Nationaltravel/dislas/dismode_la_eng.dbf")[cs,3:102] # delme

c@data$mfh <- c@data$ST1210002 #
c@data <- c@data[,-4] # remove the original mainly working from home column: gets in the way
names(c@data)[3:101] <- newnames

ecs <- sweep(x=c@data[,3:101], MARGIN=2, edcosts, '*' ) 
names(ecs) <- paste("moden",names(ecs), sep=".")
c@data <- cbind(c@data, ecs)
c@data$ET <- rowSums(ecs)
c@data$EAV <- c$ET / c$all.all
c$abbed <- abbreviate(c$ZONE_LABEL, minlength=2, strict=T) # create abbreviated area names
c$id <- 1:nrow(c@data)

la <- fortify(c, region="id")
la <- join(la, c@data, by="id", match="first")

# making text boxes
ct <- data.frame(coordinates(c), 
                 text = c$abbed)

p <- ggplot(data=la, aes( x=long/1000, y=lat/1000))
p + geom_polygon(data=la,aes(x=long/1000, y=lat/1000, fill=ET/all.all, group=group)) + 
  scale_fill_continuous(low="green", high="red", name="Etrp (MJ)") + coord_fixed() +
  xlab("Easting (km)") + ylab("Northing (km)") + 
  geom_text(data=ct, aes(x=X1/1000, y=X2/1000, label=text), size= I(3), family="mono") +
  geom_path(data=fgor, aes(x=long/1000, y=lat/1000, group=group)) + theme_classic()
ggsave("~/Dropbox/Thesis/Figures/countyen.png", width=8, height=8)

### Analysis
c@data[c$EAV < 25,c("ZONE_LABEL", "EAV")]
c@data[c$EAV > 45,c("ZONE_LABEL", "EAV")]
mean(c$EAV)
c$EAV[grepl("Kent|Essex|Bedf", x=c$ZONE_LABEL)]
mean(c$EAV[grepl("Kent|Essex|Bedf", x=c$ZONE_LABEL)])
??distance
gDistance(spgeom1=c[grepl("Rutl", x=c$ZONE_LABEL),], spgeom2=c[grepl("Inner", x=c$ZONE_LABEL),])

### Bath-plot
la$Baths <- cut(la$ET/ (la$all.all * 3.6 * 5), 
#                 breaks=c(0.5, 1, 1.5, 2, 2.5, 3)
                breaks=c(0,1,2,3)
                )
summary(la$Baths)
p + geom_polygon(data=la,aes(x=long/1000, y=lat/1000, fill=Baths, group=group)) + 
  scale_fill_brewer() + coord_fixed() +
  xlab("Easting (km)") + ylab("Northing (km)") + 
  geom_text(data=ct, aes(x=X1/1000, y=X2/1000, label=text), size= I(3), family="mono") +
  geom_path(data=fgor, aes(x=long/1000, y=lat/1000, group=group)) + theme_classic()
ggsave("~/Dropbox/Conferences/Nottingham-FOSS4G-17-21st-Sept-2013/baths-full.png", width=8, height=8)
