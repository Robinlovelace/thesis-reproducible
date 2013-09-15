plot(gArea(wards, byid=T), wards$EAV)
plot(wards$all.all / (gArea(wards, byid=T)/1000000), wards$EAV)
cor.test(wards$all.all / (gArea(wards, byid=T)/1000000), wards$EAV)

