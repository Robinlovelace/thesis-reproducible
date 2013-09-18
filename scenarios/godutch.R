### Go dutch individual scenario - for chapter 8 !!! save in reproducible!
nts <- read.spss("/home/robin/Data/DfT-NTS-2002-2008/UKDA-5340-spss/spss/spss12/individual.sav")
summary(nts$i272)
rides <- ifelse(nts$i272 == "Yes", 1, 0)
summary(rides)
mr <- glm(rides ~ nts$i6a, family = binomial)
summary(mr)
rides.pred <- predict(mr, newdata=nts$i6a)
summary(rides.pred)
plot(mr$coefficients)

# binomial model seems overkill - go simpler with aggregate
mr <- aggregate(rides, by=list(nts$i6a), FUN=mean)
mr$age <- c(2,7.5,13,18,25,35,45,55,65,80)
mr <- mr[2:10,]
plot(mr)
qplot(data=mr, x=Group.1, y=x, )
qplot(data = mr, x=age, y = x)
smod <- lm(data = mr, x ~ age)
mr$predicted <- predict(smod, mr)
qplot(data=mr, x=age, y=x) + geom_line(x=mr$age, y=mr$predicted) + ylab("prob. ridden a bike") +
  theme_bw()
ggsave("~/Dropbox/Thesis/Figures/priden.png")

test.frame <- data.frame(age = allindividuals$age)
allindividuals2$pbike <-  (predict(smod, newdata = allindividuals))
allindividuals2$pbike[which(allindividuals2$pbike < 0 )] <- 0.0001
allindividuals2$pbike[which(allindividuals2$dis > 10 )] <- 0.0001
pcycle <- 0.402*exp(-0.203*allindividuals2$dis)
allindividuals2$pbike <- allindividuals2$pbike * pcycle
allindividuals2$pbike[which(allindividuals2$mode != "Car (d)")] = 0
allindividuals2$biker = 0
allindividuals2$biker[sample(1:nrow(allindividuals2), size=nrow(allindividuals2) * 0.08, 
                             prob = allindividuals2$pbike)] <- 1
allindividuals2$cysave <- 0 # new variable where we can save e.savings
allindividuals2$cysave[which(allindividuals2$biker == 1)] <-
  allindividuals2$Et.1[which(allindividuals2$biker == 1)] - 
  allindividuals2$dis[which(allindividuals2$biker == 1)] * 0.09
summary(allindividuals2$cysave)
sum(allindividuals2$cysave) / sum(allindividuals2$Et.1) # 

summary(allind)

pcycle.simple <- pcycle # new simple variable
pcycle.simple[which(allindividuals2$dis < 3)] = 0.5
pcycle.simple[which(allindividuals$dis >=3 & allindividuals$dis < 7)] = 0.3
pcycle.simple[which(allindividuals$dis >=7 & allindividuals$dis <= 14)] = 0.05
pcycle.simple[which(allindividuals$dis >=14 & allindividuals$dis <= 29)] = 0.01
pcycle.simple[which(allindividuals$dis >=29)] = 0
unique(pcycle.simple)

pcycle.simple[which(allindividuals2$mode != "Car (d)")] = 0

allindividuals2$biker.siimple <- 0
allindividuals2$biker.siimple[sample(1:nrow(allindividuals2), size=nrow(allindividuals2) * 0.08, 
                             prob = pcycle.simple)] <- 1
allindividuals2$cysave.simple <- 0 # new variable where we can save e.savings
allindividuals2$cysave.simple[which(allindividuals2$biker.siimple == 1)] <-
  allindividuals2$Et.1[which(allindividuals2$biker.siimple == 1)] - 
  allindividuals2$dis[which(allindividuals2$biker.siimple == 1)] * 0.09

sum(allindividuals2$cysave) / sum(allindividuals2$Et.1) # 
sum(allindividuals2$cysave.simple) / sum(allindividuals2$Et.1) # 

soy$cysave = 0
soy$cysave.simple = 0

for(i in 1:173){
  a1 <- subset(allindividuals2, zone == i)
  soy$cysave[i] <- sum(a1$cysave)
  soy$cysave.simple[i] <- sum(a1$cysave.simple)
}
summary(soy$cysave)
summary(soy$cysave / soy$sumEt)

# test probabilities work:

fsoy <- fortify(soy, region="id")
fsoy <- join(fsoy, soy@data)
fsoy[,1:2] <- fsoy[,1:2]/1000
head(fsoy)
fsoy$perc.cy.ind <- fsoy$cysave/fsoy$sumEt * 100
fsoy$perc.cy.agg <- fsoy$cysave.simple/fsoy$sumEt * 100

ggplot() + 
  geom_polygon(data=fsoy, aes(x=long, y=lat, group=group, fill = perc.cy.ind), alpha=1) +
  scale_fill_continuous(low="green", high="red", 
                        name="Energy saved\nby 'go Dutch'\n(ind. version) (%)")  + 
  coord_fixed() + # This works
  theme_classic() + geom_path(data=fsoy, aes(x=long, y=lat, group=group), 
                              colour="grey", size = 0.1) +
  xlab("Easting (km)") + ylab("Northing (km)") +
  geom_text(data=nsoyo, aes(x=x,y=y, label=name), size = 3) + geom_path(data=fsoyo, aes(x=long, y=lat, group=group))
ggsave("~/Dropbox/Thesis/Figures/cysave-ind.png", width=7, height = 5)

ggplot() + # aggregate implementation
  geom_polygon(data=fsoy, aes(x=long, y=lat, group=group, fill = perc.cy.agg), alpha=1) +
  scale_fill_continuous(low="green", high="red", 
                        name="Energy saved\nby 'go Dutch'\n(agg. version) (%)")  + 
  coord_fixed() + # This works
  theme_classic() + geom_path(data=fsoy, aes(x=long, y=lat, group=group), 
                              colour="grey", size = 0.1) +
  xlab("Easting (km)") + ylab("Northing (km)") +
  geom_text(data=nsoyo, aes(x=x,y=y, label=name), size = 3) + geom_path(data=fsoyo, aes(x=long, y=lat, group=group))
ggsave("~/Dropbox/Thesis/Figures/cysave-agg.png", width=7, height = 5)

ggplot() + # aggregate implementation
  geom_polygon(data=fsoy, aes(x=long, y=lat, group=group, fill = perc.cy.agg - perc.cy.ind), alpha=1) +
  scale_fill_continuous(low="white", high="black", 
                        name="Difference\n(agg - ind)\n(percentage pts)")  + 
  coord_fixed() + # This works
  theme_classic() + geom_path(data=fsoy, aes(x=long, y=lat, group=group), 
                              colour="grey", size = 0.1) +
  xlab("Easting (km)") + ylab("Northing (km)") +
  geom_text(data=nsoyo, aes(x=x,y=y, label=name), size = 3) + geom_path(data=fsoyo, aes(x=long, y=lat, group=group))
ggsave("~/Dropbox/Thesis/Figures/cysave-diff.png", width=7, height = 5)

# what's the link between energy saved and average e. use?
plot(soy$meanEt, soy$cysave)
cor.test(soy$meanEt, soy$cysave) # quite low, 0.12
plot(soy$propcom, 1 - soy$pertele/soy$meanEt)

cor.test(soy$propcom, 1 - soy$pertele/soy$meanEt ) # quite low, 0.12


# link with income?
soy$income <- read.csv("~/Dropbox/Thesis/Reproducible/income-msoa.csv", header=F)[,c(16)]
head(incomesoyo)
plot(soy$income, 1 - soy$pertele/soy$meanEt)
cor.test(soy$income, 1 - soy$pertele/soy$meanEt)
cor.test(soy$meandis, 1 - soy$pertele/soy$meanEt)

### Comparison of scens
tele <- which(grepl("manage|higher", allindividuals$nssec8))
probcom <- 1/exp(5.3 - 0.022 * allindividuals$dis[tele])
tele <- sample(tele, size=0.10*length(tele), prob=probcom, replace=F)

cychange <- allindividuals2[which(allindividuals2$biker.siimple == 1),] # those who shift to bike
nrow(cychange) / length(which(allindividuals2$mode == "Car (d)")) 
telechange <- allindividuals2[tele,]
length(which(tele %in% which(allindividuals2$biker.siimple == 1)))
length(which(which(allindividuals2$biker.siimple == 1) %in% tele))
nrow(cychange) / nrow(allindividuals)
nrow(telechange) / nrow(allindividuals)

summary(allindividuals2$income) ;summary(cychange$income) ;  mean(telechange$income)
summary(allindividuals2$Et.1) ; summary(cychange$Et.1) ;summary(telechange$Et.1) 
summary(allindividuals2$age) ; summary(cychange$age) ;summary(telechange$age) 
summary(allindividuals2$dis) ; summary(cychange$dis) ;summary(telechange$dis) 
