# Aim: load aggregate data and calculated energy costs at the national level
x = c("ggplot2", "sp", "maptools", "rgeos", "mapproj", "rgdal")
lapply(x, require, character.only=T) # the R packages we'll be using

la.en <- readOGR(".", layer="dismode_la_eng" )
head(la.en@data)
dbreaks <- c(2,5,10,20,30,40,60)
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

mdis <- read.csv("~/1PhD/Core data/dboxes.csv", header=T)
head(mdis)
mdis <- mdis[which(row.names(mdis) %in% c("card", "bus", "carp", "train", "metro") ),]
mdis$mode <- row.names(mdis)
mdis <- melt(mdis)
mdis$b.max <- rep(c(dbreaks, 250), each=5)
mdis$b.min <- rep(c(0,dbreaks), each=5)
head(mdis)
mdis$Avd <- mdis$value/mdis$b.max
p2 <- qplot(data=mdis, x=b.min, y=Avd, geom="line", colour=mode) +
  ylab("Av. dis. / bin max.") + xlab("Bin minimum (km)") + 
  theme(legend.position=c(0.5, 0.2),  
  legend.background = element_rect(fill = "white", colour = NA), 
        legend.direction="horizontal")  # Nice
p1 <- qplot(data = mdis, x=b.min, y=value, geom="line", colour=mode) + xlab("") +
  ylab("Av. dis. (km)") + theme(legend.position="none")
multiplot(p1, p2)
ggsave("dboxes.png", height=4, width=5)

# Non-motorised (strange)
mdis <- read.csv("~/1PhD/Core data/dboxes.csv", header=T)
head(mdis)
mdis <- mdis[which(row.names(mdis) %in% c("walk", "cycle")),]
mdis$mode <- row.names(mdis)
mdis <- melt(mdis)
mdis$b.max <- rep(c(dbreaks, 250), each=2)
mdis$b.min <- rep(c(0,dbreaks), each=2)
head(mdis)
mdis$Avd <- mdis$value/mdis$b.max
p2 <- qplot(data=mdis, x=b.min, y=Avd, geom="line", colour=mode) +
  ylab("Av. dis. / bin max.") + xlab("Bin minimum (km)") + 
  theme(legend.position=c(0.5, 0.2),  
        legend.background = element_rect(fill = "white", colour = NA), 
        legend.direction="horizontal") + xlim(c(0,10))  # Nice
p1 <- qplot(data = mdis, x=b.min, y=value, geom="line", colour=mode) + xlab("") +
  ylab("Av. dis. (km)") + theme(legend.position="none") + xlim(c(0,10))
multiplot(p1, p2) + ylim(c(0,10))
ggsave("dboxes.png", height=4, width=5)
