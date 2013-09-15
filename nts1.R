library(foreign)
setwd("/home/robin/Data/DfT-NTS-2002-2008/UKDA-5340-spss/spss/spss12/") # where nts spss data is saved
df <- read.spss("individual.sav")
df <- as.data.frame(df)
nrow(df)

plot(df$i310[-which(df$i310 == "DNA")])
summary(df$i310)
levels(df$i310)
exclude <- levels(df$i310)[8:11]
fh <- df$i310[-which(df$i310 %in% exclude)]
fh <- factor(fh)
plot(fh)
qplot(fh)
levels(fh)
lss <- c("> 3/wk", "1-2/wk", "wk-3/month", "1-2/month", "month-3/yr", "1-2/yr", "< 1/yr")
levels(fh) <- lss
fh
strtrim(levels(fh), width=12 )
summary(fh) / length(fh)
sum(summary(fh)/length(fh))
sum(summary(fh)[3:7]/length(fh))


qplot(fh) + xlab("Frequency of wfh (times per week, month or year)") + theme_bw()
ggsave("freq-nts.png", width=6, height=3)
length(fh) / nrow(df)

## counting NAs
which(df[1:1000,] == "NA" | df[1:1000,] == "No answers"  | df[1:1000,] == "Multipunched") 
which(df[1:2,] != "nn")
which(df == "NA" | df == "No answers"  | df == "Multipunched") 
length(which(df == "NA" | df == "No answers"  | df == "Multipunched")) / (nrow(df) * ncol(df))

################### Household level
hdf <- read.spss("household.sav")
hdf <- as.data.frame(hdf)
nrow(hdf)

#### trip level

tdf <- read.spss("trips-commuting.sav")
tdf <- data.frame(tdf)
head(tdf[,1:7])

summary(tdf$jdungross)
plot(tdf$jdungross)
summary(tdf$jd)

trips <- read.spss("trips.sav")
trips <- data.frame(trips)
summary(trips$j26)
summary(trips$j23)
207 / 214

stages <- data.frame(read.spss("stage.sav"))
nrow(stages) / nrow(trips)

vs <- data.frame(read.spss("vehicle.sav"))


### Are trips and tdf the same?
tdf[which(tdf$psuid == 20102 & tdf$h88 == 81 & tdf$j3 == "Jrny 1"),1:21]
trips[which(trips$psuid == 20102 &trips$h88 == 81 & trips$j3 == "Jrny 1"),1:21]