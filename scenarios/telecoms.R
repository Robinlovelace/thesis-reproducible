### Shift to telecommuting - for chapter 8 !!! save in reproducible!
head(soy@data)
names(allindividuals)
a1 <- subset(allindividuals, zone == i, Et)
tele <- which(grepl("manage|higher", allindividuals$nssec8))
atop/sum(a1)
length(tele) / nrow(allindividuals)
summary(allindividuals$EI.1); summary(allindividuals$EI)
allindividuals$Et.tele <- allindividuals$Et.1
allindividuals$Et.tele[tele] <- allindividuals$Et.1[tele] * 0.66667
summary(allindividuals$Et.tele) / summary(allindividuals$Et.1)
# now put that in a loop
soy$top20 <- soy$youngshare  <- soy$pric <- soy$pricc<- 0

soy$pertele <- 1 # set-up telecommuting e. save var
soy$propcom <- 0.05
for(i in 1:173){
  a1 <- subset(allindividuals, zone == i)
  #   soy$pertele[i] <- 1 - sum(a1$Et.tele) / sum(a1$Et.1) # original approximation
  summary(a1$nssec8)
  soy$pertele[i] <- sum(a1$Et.1)
  tele <- which(grepl("manage|higher", a1$nssec8))
  probcom <- 1/exp(5.3 - 0.022 * a1$dis[tele])
  tele <- sample(tele, size=0.10*length(tele), prob=probcom, replace=F)
  soy$pertele[i] <- (soy$pertele[i] - sum(a1$Et.1[tele]))/nrow(a1)
  soy$propcom[i] <- length(tele)/nrow(a1) # proportion who now telecommute
}
# test probabilities work:
plot(a1$dis[tele], probcom)
1 - summary(soy$pertele/soy$meanEt)
summary(soy$propcom)

fsoy <- fortify(soy, region="id")
fsoy <- join(fsoy, soy@data)
fsoy[,1:2] <- fsoy[,1:2]/1000
head(fsoy)
fsoy$prictime <- fsoy$pric/fsoy$ppric

ggplot() + 
  geom_polygon(data=fsoy, aes(x=long, y=lat, group=group, fill = (1 - pertele/meanEt)*100), alpha=1) +
  scale_fill_continuous(low="green", high="red", 
                        name="Energy saved\n by telework (%)")  + 
  coord_fixed() + # This works
  theme_classic() + geom_path(data=fsoy, aes(x=long, y=lat, group=group), 
                              colour="grey", size = 0.1) +
  xlab("Easting (km)") + ylab("Northing (km)") +
  geom_text(data=nsoyo, aes(x=x,y=y, label=name), size = 3) + geom_path(data=fsoyo, aes(x=long, y=lat, group=group))
ggsave("~/Dropbox/Thesis/Figures/telesave.png", width=7, height = 5)
# what's the link between energy saved and average e. use?
plot(soy$meanEt, 1 - soy$pertele/soy$meanEt )
cor.test(soy$meanEt, 1 - soy$pertele/soy$meanEt ) # quite low, 0.12
plot(soy$propcom, 1 - soy$pertele/soy$meanEt)

cor.test(soy$propcom, 1 - soy$pertele/soy$meanEt ) # quite low, 0.12


# link with income?
soy$income <- read.csv("~/Dropbox/Thesis/Reproducible/income-msoa.csv", header=F)[,c(16)]
head(incomesoyo)
plot(soy$income, 1 - soy$pertele/soy$meanEt)
cor.test(soy$income, 1 - soy$pertele/soy$meanEt)

cor.test(soy$meandis, 1 - soy$pertele/soy$meanEt)