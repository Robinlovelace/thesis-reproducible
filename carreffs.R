# carrefs - reproducible
crf <- read.csv("Carreffs.csv")
head(crf)
mean(crf$L.100.km.Urban..Cold.) / mean(crf$L.100.km.Extra.Urban)
cbind(crf,(crf$L.100.km.Urban..Cold.) / (crf$L.100.km.Extra.Urban))

# crf <- crf[,c(1,2,3,5,7,8)]
crf$Urban <- crf$L.100.km.Urban..Cold.
crf$Extra.urban <- crf$L.100.km.Extra.Urban
attach(crf)
crf$Type <- paste(Model, Description)
crf$Make <- Model
class(L.100.km.Combined)


crfm <- melt(crf, id.vars=c("Make", "Description", "Engine.Capacity"),
             measure.vars = c("Urban", "Extra.urban"), value.name= "L.100km", 
             variable.name = "Cycle")
head(crfm)
qplot(data=crfm, x=Engine.Capacity, y=L.100km, shape = Cycle, colour= Make) +
  geom_point(size=3) +
  geom_text(data=crfm[which(crfm$Cycle=="Extra.urban"),], aes(y = Extra.urban * 1.3, 
                                                              label=Description), colour="black",
            angle = 90, alpha=0.5) + ylab("Energy use (l/100 km)")+ xlab("Engine size (cubic cm)")
ggsave("carplot-urb.png", width=6)


ggplot(crf, aes(x=Engine.Capacity, color=Green.Rating), colour=black) +
  geom_point(aes(y=L.100.km.Urban..Cold.), shape = 5) +
  geom_point(aes(y=L.100.km.Extra.Urban), shape = 6)