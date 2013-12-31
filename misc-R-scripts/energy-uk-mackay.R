# Energy in 1 kwh/p/d
1000 / 45 # twh
22.222 * 3.6 #PJ

E <- read.csv("~/Dropbox/Thesis/Tables/energy-agg.csv")
names(E)[2] <- "Source"
names(E)[3] <- "Ef"

qplot(data=E, x=Mode, y=Ef, geom="bar", fill=Source) +
  ylab("Energy use (PJ/yr)") + 
         theme_classic() 
ggsave("direct-en.png", height=4, width=6)
       