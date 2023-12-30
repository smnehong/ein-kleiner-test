library(dplyr, exclude = c("filter", "lag"))
library(tidyverse)

favorites <- read.csv("lieblinge.csv",TRUE,",")

ggplot(favorites, aes(Lieblingsfarbe, Alter))+
    coord_flip()+
    stat_boxplot(geom='errorbar', width=0.8)+
    geom_boxplot()+
    labs(x="Alter", y="Lieblingsfarbe")
