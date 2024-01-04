library(tidyverse)
library(ggplot2)
library(ggmosaic)

desc.mode <- function(x) {
  ux <- unique(x)
  fx <- tabulate(match(x, ux))
  ux[fx == max(fx)]
}

# Phi-Streuungsmaß für nominale Merkmale
desc.phi <- function(x) {
  ux <- unique(x)
  rfx <- tabulate(match(x, ux)) / length(x)
  pmin <- 2 * (1 - max(rfx))
  i <- seq_along(ux)
  pmax <- sum(abs(rfx[i] - 1 / length(ux)))
  pmin / (pmin + pmax)
}

desc.chisq <- function(x, y) {
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

desc.cramer <- function(x, y) {
  ux <- unique(x)
  uy <- unique(y)
  n <- min(length(x), length(y))
  sqrt(desc.chisq(x, y) / (n * (min(length(ux), length(uy)) - 1)))
}

#
draw.histogram <- function(df, factor, label, breaks = NULL) {
  ggplot(df, aes(factor)) +
    geom_histogram(aes(y = after_stat(density)), breaks = breaks) +
    geom_vline(aes(xintercept = mean(factor)),
      linetype = "dashed",
      linewidth = 1
    ) +
    geom_vline(aes(xintercept = median(factor)),
      colour = "darkgray",
      linetype = "dashed", linewidth = 1
    ) +
    geom_vline(aes(xintercept = desc.mode(factor)),
      color = "grey",
      linetype = "dashed", linewidth = 1
    ) +
    labs(
      x = label, y = "empirische Dichte",
      title = paste("Histogramm von ", label)
    )
}

draw.barchart <- function(df, factor, label) {
  ggplot(df, aes(factor)) +
    geom_bar() +
    labs(
      x = label, y = "absolute Häufigkeit",
      title = paste("Stabdiagramm von ", label)
    )
}

draw.boxplots <- function(
    df, factor_numerical, factor_categorial,
    label_numerical, label_categorial) {
  ggplot(df, aes(factor_categorial, factor_numerical)) +
    stat_boxplot(geom = "errorbar", width = 0.75) +
    coord_flip() +
    geom_boxplot() +
    labs(
      x = label_categorial, y = label_numerical,
      title = paste(
        "Boxplots von ", label_numerical, " zu ",
        label_categorial
      )
    )
}

draw.mosaic <- function(
    df, mapping, label_x, label_y,
    manual_colors = NULL) {
  ggplot(df) +
    geom_mosaic(mapping) +
    scale_fill_manual(values = manual_colors) +
    labs(
      x = label_x, y = label_y,
      title = paste("Mosaikplot von ", label_x, " und ", label_y)
    )
}
