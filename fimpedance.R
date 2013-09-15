x <- seq(0,10,by=0.1)
walk <- 0.486*exp(-1.683*x)
cycle <- 0.402*exp(-0.203*x)
cycle.shopping <- 0.343*exp(-0.514*x)
plot(x,walk)
lines(x,cycle)

df <- data.frame(x,walk,cycle,cycle.shopping)
dm <- melt(df, id.vars="x")
head(dm)
qplot(data=dm, x=x, y=value, colour=variable, geom="line") + theme_classic() + 
  scale_color_discrete(name="Impedance") + opts(legend.position = c(0.6,0.8)) +
  xlab("Distance (km)") + ylab("% travel")
ggsave("impedance.png",width=5, height=3)
getwd()