# cyclesave - general script for calculating energy/trp savings by cycling
head(gors@data) # raw data
# names(edcosts) <- names(gors@data[1,3:101]) # proper e.use/trp (mj) names
edcosts
cycols <-  which(grepl("bike", names(edcosts)))
carcols <- which(grepl("car[.]", names(edcosts)))
gors@data[,carcols+2]
edcosts[carcols]
# number of car trips replaced:
crep <- sweep(gors@data[,carcols+2], 2, 
              STATS=c(0,0.5,0.3,0.05,0.01,0,0,0,0) ,"*") 
gors$cysave <- rowSums(sweep(crep, 2, edcosts[carcols], "*") ) / 
  gors$all.all
gors$cysavet <- gors$cysave - rowSums(sweep(crep, 2, edcosts[cycols], "*")) / 
  gors$all.all # total energy saving (after e.use bikes subtracted)
gors$cysave.prop <- gors$cysave / (gors$ET / gors$all.all) * 100
gors$cysave.propt <- gors$cysavet / (gors$ET / gors$all.all) * 100
summary(gors$cysave.prop) / summary(gors$cysave.propt)
summary(gors$cysave.prop) ; summary(gors$cysave.propt)
sum(gors$cysave.propt * gors$all.all) / sum(gors$all.all) # total prop e. use
cbind(gors$cysave.propt, gors$abbed)

### Ward-level
ggplot()


cols0 <- which(grepl("[.]0", names(edcosts))) # the shortest trips
gors@data[,cols0]
sum(gors$all.0) / sum(gors$all.all)
sum(gors$car.0) / sum(gors$all.all)

sweep()