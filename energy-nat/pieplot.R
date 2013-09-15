# sectors pieplot attempt
pienl <- read.csv("Netherlands/gdp.csv")
pienl
??pie
p <- ggplot(data=pienl,aes(x="", y=Percentage, fill=Sector))
p + geom_bar( width=1, stat="identity") + facet_grid(. ~ Country) +
  coord_polar(theta="y") + xlab("") + ylab("")
ggsave("~/Dropbox/Thesis/Figures/gpd-uk-nl.png", width = 4.5, height=3)