# Simplify polygons
setwd("~/Data/Nationaltravel/gor/")
gors <- readOGR(".", "GOR_st121")
head(gors@polygons)
fgors <- fortify(gors) # 800,000 polys
gors1 <- gSimplify(gors, tol=1, topologyPreserve=T)
fgors1 <- fortify(gors1)
gors50 <- gSimplify(gors, tol=50, topologyPreserve=T) 
fgors50 <- fortify(gors50)
gors500 <- gSimplify(gors, tol=500, topologyPreserve=T) 
fgors500 <- fortify(gors500)
gors5k <- gSimplify(gors, tol=5000, topologyPreserve=T) 
fgors5k <- fortify(gors5k) # That's 1/4 of the orginal size!
plot(gors5k)

fgors <- fortify(gors, region="ZONE_CODE") # 800,000 rows: too many!

for(i in 1:length(gors@polygons)){
  for(j in 1:length(gors@polygons[[i]]@Polygons)){
    temp <- as.data.frame(gors@polygons[[i]]@Polygons[[j]]@coords)
    names(temp) <- c("x", "y")
    temp2 <- dp(temp, 0.01)
    gors@polygons[[i]]@Polygons[[j]]@coords <- as.matrix(cbind(temp2$x, temp2$y))
  }
}

fgors <- fortify(gors, region="ZONE_CODE") # 800,000 rows: too many!
p <- ggplot(fgors, aes(x=long, y=lat, group=group))
p + geom_polygon()

setwd("~/Data/Nationaltravel/gor/")
gors <- readOGR(".", "GOR_st121-esmaller")
fgors <- fortify(gors, region="ZONE_CODE") # 800,000 rows: too many!
p <- ggplot(fgors, aes(x=long, y=lat, group=group))
p + geom_polygon()
