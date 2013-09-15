#propcycle
# compare proportion cycling in cam and scenarios
miniwards$ZONE_LABEL[which(grepl("Cam",miniwards$ZONE_LABEL))]
which(grepl("Cam",miniwards$ZONE_LABEL))
miniwards@data[919,c(cycols+2)] / miniwards$all.all[919]
which.max(miniwards$bike.0)
which.max(miniwards$bike.all / miniwards$all.all)
miniwards@data[2707,c(cycols+2)] / miniwards$all.all[2707]
miniwards@data[2707,c(cycols+2,2)]

miniwards@data[2709,c(cycols+2)] / miniwards$all.all[2709]
miniwards@data[2709,c(2, cycols+2)]
# which wards have v. high rate of cycling?
miniwards@data[which(miniwards$bike.all / miniwards$all.all > 0.2),c(2, cycols+2)]
mhb <- miniwards@data[,1:102]
head(crep)
mhb[,carcols+2] <- mhb[,carcols+2] - sweep(mhb[,carcols+2], 2, 
      STATS=c(0,0.5,0.3,0.05,0.01,0,0,0,0) ,"*")
head(mhb[,carcols+2])
summary(mhb$car.all / mhb$all.all)
mhb$car.all <- rowSums(mhb[,carcols+2]) - mhb$car.all
summary(mhb$car.all / mhb$all.all)

mhb[,cycols+2] <- miniwards@data[,cycols+2] + sweep(miniwards@data[,carcols+2], 2, 
                                         STATS=c(0,0.5,0.3,0.05,0.01,0,0,0,0) ,"*")
summary(mhb$bike.all)
mhb$bike.all <- rowSums(mhb[,cycols+2]) - mhb$bike.all
summary(mhb$bike.all)
summary(miniwards$bike.all/miniwards$all.all)
summary(mhb$bike.all/mhb$all.all)

# proportion of pop travelling > 5 km
names(mhb)
head(mhb[which(grepl("[.]0|\\<2\\>",names(mhb)))]) # total 0 to 2 km
sum(mhb[which(grepl("[.]0|\\<2\\>",names(mhb)))]) / sum(mhb$all.all)
sum(mhb$all.0) / sum(mhb$all.all)
sum(mhb$all.2) / sum(mhb$all.all)