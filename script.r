library(tidyverse)

favorites <- read.csv("lieblinge.csv", TRUE, ",")

library(forcats)
favorites$Lieblingstier <- fct_other(favorites$Lieblingstier, keep = c("Hund", "Katze"), other_level = "Andere")

ggplot(favorites, aes(Lieblingsfarbe, Alter)) +
    stat_boxplot(geom = "errorbar", width = 0.75) +
    coord_flip() +
    geom_boxplot() +
    labs(x = "Lieblingsfarbe", y = "Alter")

ggplot(favorites, aes(Lieblingstier, Alter)) +
    stat_boxplot(geom = "errorbar", width = 0.75) +
    coord_flip() +
    geom_boxplot() +
    labs(x = "Lieblingstier", y = "Alter")

library(ggmosaic)

ggplot(favorites) +
    geom_mosaic(aes(product(Lieblingsfarbe, Lieblingstier), fill = Lieblingsfarbe)) +
    scale_fill_manual(values = c("lightblue3", "khaki", "palegreen3", "palevioletred")) +
    labs(x = "Lieblingstier", y = "Lieblingsfarbe")
