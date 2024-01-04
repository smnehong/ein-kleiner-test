if (!exists("desc.mode", mode = "function")) source("library.r")

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
cat("Modus: ", desc.mode(favorites$Alter), "\n")
cat("Spannweite: ", max(favorites$Alter) - min(favorites$Alter), "\n")
i <- seq_along(favorites$Alter)
sk1 <- (1 / (length(favorites$Alter) - 1) * sum((favorites$Alter[i] - mean)^3)
) / sd^3
cat("Schiefekoeffizent 1: ", sk1, "\n")
cat("Variationskoeffizent: ", sd / mean, "\n")

cat("\nMerkmal: Lieblingsfarbe", "\n")
cat("Modus: ", as.character(desc.mode(favorites$Lieblingsfarbe)), "\n")
cat("Phi-Streuungsmaß: ", desc.phi(favorites$Lieblingsfarbe), "\n")

cat("\nMerkmal: Lieblingstier", "\n")
cat("Modus: ", as.character(desc.mode(favorites$Lieblingstier)), "\n")
cat("Phi-Streuungsmaß: ", desc.phi(favorites$Lieblingstier), "\n")

cat("\nKontingenz: ", "\n")
print(table(favorites$Lieblingsfarbe, favorites$Lieblingstier),
  zero.print = "."
)
cat("X^2-Koeffizient: ", desc.chisq(
  favorites$Lieblingsfarbe,
  favorites$Lieblingstier
), "\n")
cat("Cramérs Kontingenzindex: ", desc.cramer(
  favorites$Lieblingsfarbe,
  favorites$Lieblingstier
), "\n")

draw.histogram(favorites,
  factor = favorites$Alter, label = "Alter",
  breaks = c(17, 18, 19, 20, 22, 24, 26, 30, 36)
)

draw.barchart(favorites,
  factor = favorites$Lieblingstier,
  label = "Lieblingstier"
)

draw.barchart(favorites,
  factor = favorites$Lieblingsfarbe,
  label = "Lieblingsfarbe"
)

draw.boxplots(favorites,
  factor_numerical = favorites$Alter,
  factor_categorial = favorites$Lieblingsfarbe,
  label_numerical = "Alter",
  label_categorial = "Lieblingsfarbe"
)

library(forcats)

favorites$Lieblingstier <- fct_other(favorites$Lieblingstier,
  keep = c("Hund", "Katze"), other_level = "Andere"
)

draw.boxplots(favorites,
  factor_numerical = favorites$Alter,
  factor_categorial = favorites$Lieblingstier,
  label_numerical = "Alter",
  label_categorial = "Lieblingstier"
)

draw.mosaic(favorites,
  mapping = aes(product(Lieblingsfarbe, Lieblingstier),
    fill = Lieblingsfarbe
  ),
  label_x = "Lieblingstier",
  label_y = "Lieblingsfarbe",
  manual_colors = c("lightblue3", "khaki", "palegreen3", "palevioletred")
)
