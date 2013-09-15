# Aim: load aggregate data and calculated energy costs at the national level
x = c("ggplot2", "sp", "maptools", "rgeos", "mapproj", "rgdal")
lapply(x, require, character.only=T) # the R packages we'll be using

la.en <- readOGR(".", layer="dismode_la_eng" )
head(la.en@data)
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
md <- mdis[c(7,6,5,4,2,3,11,8,9,10),]
md <- rbind(0,md)
md[is.na(md)] <- 0
md <- cbind(0,md)
mdv <- as.vector(as.matrix(md))
edcosts <- rep(ecosts, 9) * mdv
la.en@data[1:3,3:101] * edcosts
ecs <- sweep(x=la.en@data[,3:101], MARGIN=2,edcosts, '*' )
la.en@data$ET <- rowSums(ecs)
plot(la.en, col = rowSums(ecs))
head(la.en@data)
la.en@data <- la.en@data[,c(1,2,3,8,ncol(la.en@data))]

# convert to ggplot2, just for plotting
la <- fortify(la.en, region="ZONE_CODE")
la <- merge(la, la.en@data)
head(la)
p <- ggplot(data=la, x=long, y=long) # This just takes too long... must simplify first

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
qplot(data = mdis, x=b.min, y=value, geom="line", colour=mode) + xlab("Bin minimum (km)") +
  ylab("Average distance travelled (km)")