suppressMessages(library(dplyr))
library(tidyr)
library(maps)
library(ggplot2)
library(ggthemes)

setwd('~/repos/Plot-Abundance-Map')

abundance <- read.csv("./data/relative_abundance.tsv", sep = "\t")

metadata <- read.csv("./data/metadata.csv")

abundance <- inner_join(abundance, metadata)

abundance <- abundance %>% filter(IslandID != "CAR")

gg <- ggplot()

wrld <- map_data("world")
state.list <- c("hawaii", "samoa")
map("state", state.list)

xlims = c(-190, -135)
ylims = c(-30, 30)

p <- ggplot()
p <- p + theme(panel.background = element_rect(fill =NA),
               panel.border = element_rect(colour = "#000000",
                                           size = 1,
                                           linetype = "solid",
                                           fill = NA),
               axis.title = element_blank(),
               axis.ticks.x = element_blank(),
               axis.text.x = element_blank(),
               axis.text.y = element_text(),
               axis.ticks.y = element_line(),
               legend.position="bottom",
               legend.background = element_rect(fill="white", colour = "black"),
               legend.key = element_rect(fill=NA))

#Draws the map and assigns background color for continents
p <-p + geom_polygon( data=wrld, aes(x=long, y=lat, group = group), colour="#4d4d4d", fill="#4d4d4d")#,colour="black", fill="black" )

#Plots negative stations
neg_map <- p + geom_point( data=abundance %>% filter(SumRelativeAbundance == 0),
                           shape = 21,
                           colour="black",
                           fill="black",
                           size = 0.5,
                           aes(x=Longitude, y=Latitude)
)

#Add positive stations sized by rel_abundance

p <-  neg_map + geom_point( data=abundance %>%  
                     filter(SumRelativeAbundance > 0),
                     shape=21, 
                     colour="#b21616", 
                     fill="#e84646",
                     aes(x=Longitude, y=Latitude, size=SumRelativeAbundance)
                     )
# create facetted plot by Gene
p <- p + facet_wrap(~Gene) + coord_quickmap() + theme_map()

p <- p + coord_fixed(xlim = xlims, ylim = ylims)

p

## Save figure
ggsave(file="./results/abundance_map.png", plot=p, width=14, height=8)
