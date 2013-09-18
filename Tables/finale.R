setwd("~/Dropbox/Thesis/Tables/")
ecs <- read.csv("final-e-justpres.csv")[,1:5]
head(ecs)
ecs <- ecs[-1,]
plot(ecs)

ecs <- melt(ecs, id.vars=c("Mode...Attribute"), 
            measure.vars=c("Ev", "Eg", "Ef", "Efp")
              )
ecs <- melt(ecs)
head(ecs)
summary(ecs)
ecs$value <- as.numeric(ecs$value)
ecs$variable <- factor(ecs$variable, levels=c("Ef", "Efp", "Ev", "Eg"))
names(ecs)[1] <- "Mode"
names(ecs)[2] <- "Component"

qplot(data=ecs, x= Mode, y = value, geom="bar", stat="identity", fill = Component)

qplot(data=ecs, x= Mode, y = value, geom="bar", stat="identity", fill = Component,
      order = as.numeric(ecs$Component)) +
  ylab("Energy costs (MJ/pkm)")  +
  theme_classic()+  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave("~/Dropbox/Thesis/Figures/final-e.png", height = 4, width = 6)

## Efficiencies over time
efs <- read.csv("~/Dropbox/Thesis/Tables/efficiency-uk-usa-time.csv")
head(efs)
names(efs)[1] <- c("Year")
efs[,4] <- as.numeric(efs[,4])
names(efs)[4] <- c("MJ.per.km")
names(efs)[5] <- "Nation"
qplot(data=efs, x=Year, y=MJ.per.km, colour=Nation) + ylim(0,6) + ylab("Energy use (MJ/vkm)") +
  geom_line() + scale_color_brewer(palette = 6, type="qual")
getwd()
ggsave("natefficall.png", width=8, height=6)

qplot(data=efs[which(efs$Nation == "UK (DfT)"),], x=Year, y=MJ.per.km) + 
  ylab("Energy use (MJ/vkm)") + scale_x_continuous(breaks=c(2000:2012))
ggsave("~/Dropbox/Thesis/Figures/natefic.png", width=8, height=6)

### The energy use of future vehicles
fvs <- read.csv("~/Dropbox/Thesis/Tables/alternative-vehicles.csv", sep="\t")
head(fvs)
names(fvs) <- c("Technology", "Energy.source", "Energy.use")
p = ggplot(fvs, aes(x=Technology, y=Energy.use, fill=Energy.source)) + geom_bar() +
  theme(axis.text.x = element_text(angle =90, color = "black")) + ylab("Energy use (MJ/vkm)") +
  scale_fill_brewer(type="qual", palette=6)
ggsave("~/Dropbox/Thesis/Figures/altves.png", width=6)