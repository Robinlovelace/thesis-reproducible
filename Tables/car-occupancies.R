carocc <- read.csv("car-occupancies.csv")
head(carocc)
qplot(data=carocc, x=Year, y=Occupancy, colour=Region, shape=Region) + geom_smooth(method="lm", se=F)  +
  theme_classic() + theme(legend.position = c(0.8,0.8))  
ggsave("~/Dropbox/Thesis/Figures/carocc.png", height=5, width=5)