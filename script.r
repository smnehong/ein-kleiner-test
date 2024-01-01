Mode <- function(x) {
    ux <- unique(x)
    fx <- tabulate(match(x, ux))
    ux[fx == max(fx)]
}

# Phi-Streuungsmaß für nominale Merkmale
phi <- function(x) {
    ux <- unique(x)
    rfx <- tabulate(match(x, ux)) / length(x)
    pmin <- 2 * (1 - max(rfx))
    i <- seq_along(ux)
    pmax <- sum(abs(rfx[i] - 1 / length(ux)))
    pmin / (pmin + pmax)
}

chisq <- function(x, y) {
    n <- min(length(x), length(y))
    ux <- unique(x)
    uy <- unique(y)
    fxy <- table(x, y)
    sum <- 0
    for (i in seq_along(ux)) {
        for (j in seq_along(uy)) {
            sum <- sum + fxy[i, j]^2 / (sum(fxy[i, ]) * sum(fxy[, j]))
        }
    }
    n * (sum - 1)
}

cramer <- function(x, y) {
    ux <- unique(x)
    uy <- unique(y)
    n <- min(length(x), length(y))
    sqrt(chisq(x, y) / (n * (min(length(ux), length(uy)) - 1)))
}

library(tidyverse)

favorites <- read.csv("lieblinge.csv", TRUE, ",")

cat("lieblinge.csv", "\n")

cat("\nMerkmal: Alter", "\n")
mean <- mean(favorites$Alter)
cat("Arithmetisches Mittel: ", mean, "\n")
sd <- sd(favorites$Alter)
cat("Standardabweichung: ", sd, "\n")
cat("Median: ", median(favorites$Alter), "\n")
q <- quantile(favorites$Alter, probs = c(0.25, 0.75), type = 2)
cat("Quartilsdifferenz: ", q[2] - q[1], "\n")
cat("Modus: ", Mode(favorites$Alter), "\n")
cat("Spannweite: ", max(favorites$Alter) - min(favorites$Alter), "\n")
i <- seq_along(favorites$Alter)
sk1 <- 1 / (length(favorites$Alter) - 1) * sum((favorites$Alter[i] - mean)^3) # TODO fix
cat("Schiefekoeffizent 1: ", sk1, "\n")
cat("Variationskoeffizent: ", sd / mean, "\n")

cat("\nMerkmal: Lieblingsfarbe", "\n")
cat("Modus: ", as.character(Mode(favorites$Lieblingsfarbe)), "\n")
cat("Phi-Streuungsmaß: ", phi(favorites$Lieblingsfarbe), "\n")

cat("\nMerkmal: Lieblingstier", "\n")
cat("Modus: ", as.character(Mode(favorites$Lieblingstier)), "\n")
cat("Phi-Streuungsmaß: ", phi(favorites$Lieblingstier), "\n")

cat("\nKontingenz: ", "\n")
print(table(favorites$Lieblingsfarbe, favorites$Lieblingstier),
    zero.print = "."
)
cat("X^2-Koeffizient: ", chisq(
    favorites$Lieblingsfarbe,
    favorites$Lieblingstier
), "\n")
cat("Cramérs Kontingenzindex: ", cramer(favorites$Lieblingsfarbe, favorites$Lieblingstier), "\n")

library(ggmosaic)
library(forcats)
favorites$Lieblingstier_alle <- favorites$Lieblingstier
favorites$Lieblingstier <- fct_other(favorites$Lieblingstier,
    keep = c("Hund", "Katze"), other_level = "Andere"
)

ggplot(favorites, aes(Alter)) +
    geom_histogram(aes(y = after_stat(density)), breaks = c(17, 18, 19, 20, 22, 24, 26, 30, 36)) +
    geom_vline(aes(xintercept = mean(Alter)), linetype = "dashed", linewidth = 1) +
    geom_vline(aes(xintercept = median(Alter)), colour = "darkgray", linetype = "dashed", linewidth = 1) +
    geom_vline(aes(xintercept = Mode(Alter)), color = "grey", linetype = "dashed", linewidth = 1) +
    labs(x = "Alter", y = "empirische Dichte")

ggplot(favorites, aes(Lieblingstier_alle)) +
    geom_bar() +
    labs(x = "Lieblingstier", y = "absolute Häufigkeit")
ggplot(favorites, aes(Lieblingsfarbe)) +
    geom_bar() +
    labs(x = "Lieblingsfarbe", y = "absolute Häufigkeit")

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

ggplot(favorites) +
    geom_mosaic(aes(product(Lieblingsfarbe, Lieblingstier),
        fill = Lieblingsfarbe
    )) +
    scale_fill_manual(values = c(
        "lightblue3", "khaki", "palegreen3",
        "palevioletred"
    )) +
    labs(x = "Lieblingstier", y = "Lieblingsfarbe")
