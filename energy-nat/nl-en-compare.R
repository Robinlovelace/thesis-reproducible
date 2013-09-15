# NL-EN comparisons
# Idea: create an rmarkdown of this to act as a tutorial! 
gors$label <- gortex$V3
gors$ZONE_CODE <- gortex$V3
gors$EAV <- gors$ET/gors$all.all
gors@data[,c(1:5,115,114, 116)]
summary(gors$EAV)
sum(gors$ET) / sum(gors$all.all) # EN population-weighted average, lower than average of GORs 23.9

r3@data
r3$Totaal.vervoerwijzen <- as.numeric(as.character(r3$Totaal.vervoerwijzen))
sum(r3$Totaal.vervoerwijzen * r3$etot)/((mean(r3$Totaal.vervoerwijzen))*12) # NL av, higher than av4 areas
r3$Totaal.vervoerwijzen; r3$etot; 
r3@data[,which(names(r3) == "etot")]
r3@data[,"etot"] # Same way of doing it, way faster!
r3$pops <- c(2415, 1180, 3530, 2583, 482, 642, 1968, 576, 1143, 1105, 356, 378  )
r3@data[,c("name", "Vervoerwijzen", "Totaal.vervoerwijzen", "etot", "pops")]

# Yes but what are the pops of NL? (in 1000s)
# r3$pops <- c(490, 395, 646, 2000, 574, 1132, 2415, 2606, 1113, 1180, 380, 3530)
sum(r3$pops)
summary(r3$etot)
summary(gors$EAV)
mean(r3$etot) / mean(gors$EAV)
40.65/34.67
r3$etot * r3$pops
sum(gors$ET) / sum(gors$all.all) # En pop weighted
sum(r3$etot * r3$pops) / sum(r3$pops) # NL pop-weighted av. also lower, due to cities
 
(sum(r3$etot * r3$pops) / sum(r3$pops)) / (sum(gors$ET) / sum(gors$all.all))
(sum(gors$ET) / sum(gors$all.all)) / (sum(r3$etot * r3$pops) / sum(r3$pops))
msplit.nl <- r3@data[,7:14]
for(i in 1:8){
  msplit.nl[,i] <- r3@data[,i+6] * r3$pops
}
m.nl <- colSums(msplit.nl)
names(m.nl)[4] <- "Metro"

# why?
cbind(gors$ZONE_CODE, gors$all.all, gors$EAV)


# m.nl <- m.nl/sum(m.nl) * 100
m.nl <- m.nl * 500
m.nl <- data.frame(tot = m.nl, prop=m.nl/sum(m.nl) * 100) ; m.nl$mode <- row.names(m.nl)
m.nl <- m.nl[-c(3,5,8),]
p <- ggplot(m.nl, aes(x="", y=prop, fill=mode)) 
p+geom_bar(aes(x=mode)) 
p+geom_bar() +  coord_polar(theta="y")
m.nl$Country <- "Netherlands"

head(gors@data)
m.en <- colSums(gors@data[,c(5,6,8,9,11,12,4)]) / sum(gors$all.all)
m.en <- colSums(gors@data[,c(4:13)])
m.en <- data.frame(tot = m.en, prop = m.en/sum(m.en) *100)
wones <- c(4,6,8,9,11,12) - 3
m.en <- m.en[wones,]
m.en$mode <- c("Metro", "Bus", "Car.d", "Car.p", "Bicycle", "Walk")
m.en$Country <- "England"

m.all <- rbind(m.en, m.nl)
head(m.all)
# p <- ggplot(m.all, aes(y = prop, fill = mode))
# p + geom_bar(aes(x=mode), stat="identity") + facet_grid(facets= . ~ Country) + 
#   scale_fill_brewer(type="qual", palette=7) +
#   ylab("% of commuter trips")
# ggsave("~/Dropbox/Thesis/Figures/envsnl-modesplits.png", width=8, height=4) # old, see below

# Distances
names(gors)[4:15]
md # mean distances, saved from "english analysis"
md <- md[,-1]
md[2,] * 1:9 # Just checking vector multiplication
md[2,] * gors@data[1,seq(from=4, by=11, length.out=9)] # how it's done
t.en <- colSums(gors@data[,3:101])
md
md$mdtotald <- 0
for(i in 1:11){
  md[i,"mdtotald"] <- sum(md[i,1:9] * colSums(gors@data[,seq(from=4, by=11, length.out=9) + i-2]))
}
md

md$dist <- md$mdtotald / t.en[1:11]
md$pdist <- md$mdtotald / sum(md$mdtotald)
md
barplot(md$mdtotald, col=1:11)
md$tdist <- md$mdtotald ; md <- md[,-10]
md2 <- md[c(3,4,6,7,9,10,2),]
md2
md2$Country <- "England"
sum(md$tdist) / sum(gors$all.all) # Total distance by all modes: 10 km

plot(c(mdis[1,] * gors@data[1,seq(from=20, by=11, length.out=8)]),mdis[1,])

# Distance NL
names(r3)
# md.nl <- data.frame(dist = colSums(r3@data[,16:23]) / nrow(r3@data)) # old
md.nl <- data.frame(dist = natdis)
ptrips <- as.numeric(as.matrix(read.csv("NL-Data/2011netherlands.csv", sep=";")[2,3:10]))/0.49
md.nl$tdist <- ptrips * md.nl$dist
md.nl$pdist <- md.nl$tdist / sum(md.nl$tdist )

row.names(md.nl) <- names(r3)[16:23]
# as.numeric(as.matrix(read.csv("NL-Data/2011netherlands.csv", sep=";")[2,3:10]))/0.49
# natdis <- as.matrix(read.csv("NL-Data/2011netherlands.csv", sep=";")[2,12:19])
md.nl # v. useful infor: compare w. UK???
# md.nl$tdist <- md.nl[,1] * colSums(r3@data[,7:14]) #old
# md.nl$pdist <- md.nl$tdist / sum(md.nl$tdist)
md.nl$Country <- "Netherlands"
qplot(data=md.nl, x=row.names(md.nl), y=pdist)
# md.nl$ptrips <- colSums(r3@data[,7:14]) / nrow(r3@data)
md.nl$ptrips <- ptrips
sum(md.nl$dist * md.nl$ptrips) 

md.all <- rbind(md2[,c("dist", "pdist", "tdist", "Country")], 
                md.nl[-which(row.names(md.nl) == "Moto.1" | row.names(md.nl) == "Other.1"),
                      c("dist", "pdist", "tdist", "Country")])
md.all
md.all$mode = c("Train", "Bus", "Car.d", "Car.p", "Bicycle", "Walk", "Metro", 
                "Car.d", "Car.p", "Train", "Metro", "Bicycle", "Walk")
md.all

qplot(data=md.all[-c(1,10),], x=mode, y=dist, fill=mode, geom="bar") + facet_grid(facets= . ~ Country) + 
  scale_fill_brewer(type="qual", palette=7) +
  ylab("Distance of commute (km)")
ggsave("~/Dropbox/Thesis/Figures/avdist-nl-en.png", width=8, height=4)
# Distance analysis
md.all[8,1]/ md.all[3,1]

colMeans(r3@data[,7:14])
md.all$ptrips <- 0
md.all$ptrips[8:13] <- ptrips[c(1,2,3,4,6,7)]
# md.all$ptrips[1:7] <- m.en$prop/100
md.all
# av dists
sum(md.all$dist[1:7] * md.all$ptrips[1:7]) # 14.5 av dist uk
md.all$ptrips <- as.numeric(md.all$ptrips)
sum(md.all$dist[8:13] * md.all$ptrips[8:13])
md.all$dist[md.all$mode == "Car.d"][2] / md.all$dist[md.all$mode == "Car.d"][1]
qplot(data=md.all[-c(1,10),], x=mode, y=ptrips, fill=mode, geom="bar") + facet_grid(facets= . ~ Country) + 
  scale_fill_brewer(type="qual", palette=7) +
  ylab("% of commuter trips")
ggsave("~/Dropbox/Thesis/Figures/envsnl-modesplits.png", width=8, height=4)


# popdens vs etot
gors$area <- gArea(gors, byid=T) / 1000000
r3$area <- gArea(r3, byid=T) / 1000000
r3$popdens <- r3$pops * 1000 / r3$area
gors$popdens <- gors$all.all * 2 / gors$area
r3$EAV <- r3$etot

pden.all <- rbind(r3@data[,c("popdens", "EAV")], gors@data[,c("popdens", "EAV")])
pden.all$Country <- c(rep("Netherlands", 12), rep("England", 9))
qplot(data=pden.all, x=popdens, y=EAV, color=Country) + ylab("Energy costs of commuting (MJ/trip)") +
  xlab("Population density (ppl/sqkm)") + geom_abline(intercept=34.49,slope=0, colour="red") + 
  geom_abline(intercept=37.49,slope=0, colour="blue") 
# Add av. line and names
pden.all$names <- c(r3$code_hasc, gors$ZONE_CODE)
pden.all$pav <- c(rep(37.5, 12), rep(34.5,9))
pden.all$Average <- c(rep("NL", 12), rep("EN", 9))
pden.all$Name <- c(r3$c2, gors$ZONE_CODE)
pden.all
cor.test(pden.all$popdens, pden.all$EAV)

ggplot(pden.all) + geom_point(aes(x=popdens, y=EAV, color=Country, shape=Country)) + 
  geom_line(aes(x=popdens, y=pav, linetype=Average)) + ylab("Energy costs of commuting (MJ/trip)") +
  xlab("Population density (ppl/sqkm)")
ggsave("~/Dropbox/Thesis/Figures/epdensnl.png", )