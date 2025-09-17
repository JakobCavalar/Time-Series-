rm(list = ls())
setwd("C:/Users/lfstat-caja/Zeitreihen Einarbeitung/Simulation/ERgebnisse BV4/Produktion")
library(readxl)

# Vektor mit den Jahren als Zeichenketten
years <- 84:61

# Schleife zum Einlesen der Dateien
for (year in years) {
  assign(paste0("Prod", year), 
         read_excel(paste0("PROB.BAU_P", year, ".IL.XLSx")))
}

Produktion <- data.frame(matrix(nrow = 84, ncol = 0))

# Schleife über die Jahre
for (year in years) {
  prod_obj <- get(paste0("Prod", year))
  
  Produktion[[as.character(year)]] <- prod_obj[13:96, 6]
}

# Kontrolle
head(Produktion)


