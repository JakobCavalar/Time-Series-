# Set Up -----------------------------------------------------------------------
setwd("C:/Jakob/Uni Bamberg/Masterarbeit/Zeitreihen Einarbeitung/R/Testdaten")
rm(list = ls())
#install.packages("")
pacman::p_load(ranger,astsa,ggplot2,RJDemetra,tidyverse,tseries,forecast,openxlsx,
               dplyr,viridis,zoo,bayesplot)

# Testdaten Funktion -----------------------------------------------------------
simulate_time_series <- function(n = 200, 
                                 trend_slope = 0.15, trend_amplitude = 15,
                                 cycle_period = 80, cycle_amplitude = 1, 
                                 season_period = 12, season_amplitude = 5, 
                                 noise_sd = 0.5, 
                                 outlier_prob = 0.012, outlier_magnitude = 12,
                                 shift_prob = 0.008,   # Wahrscheinlichkeit eines permanenten Shifts
                                 shift_magnitude = 5, # Stärke des permanenten Shifts
                                 seed = 1) 
  {
  set.seed(seed)
  # Zeitachse
  time <- 1:n
  date <- seq.Date(from = as.Date("2010-01-01"), length.out = n, by = "month")
  
  # 1. Trend-Konjunktur-Komponente
  trend <- 100 + trend_slope * time + trend_amplitude * sin(2 * pi * time / cycle_period)
  
  # 2. Saisonale Komponente
  seasonality <- season_amplitude * sin(2 * pi * time / season_period)
  
  # 3. Stochastische Komponente
  noise <- rnorm(n, mean = 0, sd = noise_sd)
  
  # 4. Ausreißer
  outliers <- rbinom(n, 1, outlier_prob) * runif(n, -outlier_magnitude, (1/3)*outlier_magnitude)
  outliers[abs(outliers) < 5.5] <- 0
  outliers[38] <- -20
  for (i in 1:n) {
    if (outliers[i] > 7.5 | outliers[i] < -7.5){
      outliers[i+1] = (1/1.18) * outliers[i]
      outliers[i+2] = (1/1.2) * outliers[i+1]
      outliers[i+3] = (1/1.3) * outliers[i+2]
      outliers[i+4] = (1/1.5) * outliers[i+3]
      outliers[i+5] = (1/3) * outliers[i+4]
      outliers[i+6] = (1/4) * outliers[i+5]
      outliers[i+7] = (1/5) * outliers[i+6]
    } else {
      outliers[i] = outliers[i]
    }
    outliers <- outliers[1:n]
  }
outliers_indicator <- numeric(n)
for (i in 2:n){
    if (abs(outliers[i]) > abs(outliers[i-1])){
      outliers_indicator[i] <-  1
    } else{
      outliers_indicator[i] <-  0
    }
  }
  
  # 5. Permanent Shift
  shifts <- rep(0, n)
  if (shift_prob > 0) {
    shift_events <- rbinom(n, 1, shift_prob) * runif(n, -shift_magnitude, shift_magnitude)
    for (i in 2:n) {
      shifts[i] <- shifts[i-1] + shift_events[i]  # Kumulativer Shift
    }
  }
  shift_indicator <- c(0, as.numeric(diff(shifts) != 0))
  
  
  
  # 6. Zusammensetzen der Zeitreihe
  X_t <- trend + seasonality + noise # + outliers + shifts
  
  
  # 7. Data Frame erstellen
  data <- data.frame(
    time = time,
    date = date,
    X_t = X_t,
    trend = trend,
    seasonality = seasonality,
    noise = noise,
    outliers = outliers,
    outliers_ind = outliers_indicator,
    shifts = shifts,
    shift_ind = shift_indicator
  )
  data$controll <- data$X_t - (data$trend + data$seasonality + data$noise +
                    data$shifts + data$outliers)
  return(data)
}
ts_data <- simulate_time_series(n = 200, trend_slope = 0.2, trend_amplitude = 10, cycle_period = 80, cycle_amplitude = 1, 
                                season_period = 12, season_amplitude = 5, noise_sd = 0.6, outlier_prob = 0.01, 
                                outlier_magnitude = 10, shift_prob = 0.006, shift_magnitude = 8, seed = 1234511)
plot(ts_data$X_t)

# ZRA Funktionen ---------------------------------------------------------------
# SEATS Methode
SEATS <- function(Daten,Variable_Zeitreihe,Variable_Datum,Frequenz,Format,Anteil,
                     Ymin = NULL, Ymax = NULL, Modell_out = "Modellseats"){
  # Anteil - Zahl zwsichen 1 und unendlich die bestimmt wie viel Prozent des Datensatzes
  #          als Trainingsdaten verwendet werden. Bei 1 werden alle vorhandenen Daten
  #          zum trainieren verwendet.
  
  # Daten  - Ein Datensatz in Matrix oder Dataframe Format.
  
  # Format - Format in der das Datum im Datensatz vorliegt z.B. "%d %b %y".
  
  # Frequenz - Frequenz in der Daten gemessen wurden und saisonale Muster wiederkehren.
  #            Bei monatlichen Messungen Format = 12, bei vierteljährlichen = 4.
  
  # Variable_Datum - Spalte in der das Datum im Datensatz hinterlegt ist.
  
  # Variable_Zeitreihe - Spalte in der die Variable von interesse hinterlegt ist.
  
  # Ymin/Ymax - Paramterer zum Anpassen des Graphen
  
  
  if (all(class(Daten[,Variable_Datum]) == "Date") == TRUE){
    Datum <- Daten[,Variable_Datum]
  }
  else {
    Datum <- as.Date(paste("01",Daten[,Variable_Datum]), format = Format)
  }
  Ende <- round(length(Daten[,Variable_Zeitreihe])/Anteil,0)
  Testdaten <- ts(Daten[1:Ende,Variable_Zeitreihe],
                  frequency = Frequenz)
  Validdaten <- ts(Daten[(Ende+1):length(Daten[,Variable_Zeitreihe]),Variable_Zeitreihe],
                   frequency = Frequenz)
  Modell <- tramoseats(Testdaten, spec = "RSAfull")
  assign(Modell_out, Modell, envir = .GlobalEnv) 
  Prediction <- Modell[["regarima"]][["forecast"]][,"fcst"]
  Länge_Predictions <- length(Prediction)
  MSE <- round(mean((as.numeric(Prediction)-Validdaten[1:Länge_Predictions])^2),2)
  #Plot <- plot(Modell, type_chart = "sa-trend")
  #if (!is.null(Modell$decomposition)) {
  #  Plot_Dec <- plot(Modell$decomposition)
  #}
  #Plot_Pred <- plot(y = Validdaten[1:Länge_Predictions],
                    #x = Datum[(Ende+1):((Ende+1)+Länge_Predictions)],
                    #col = "green3", xlab= "Datum",
                    #ylab = Variable_Zeitreihe, type = "l",
                    #main = "Vorhersage der Zeitreihe mit X13-ARIMA",
                    #sub = paste0("MSE: ",format(MSE, big.mark = ",")),
                    #ylim = c(Ymin,Ymax)) %>%
    #lines(x = Datum[(Ende+1):((Ende+1)+Länge_Predictions)], y= Prediction,
          #col = "red4", lwd = 1, type = "l")
  #Plot
  #Plot_Dec
  #Plot_Pred
  print("Job Done")
}
SEATS(Daten = ts_data, Variable_Zeitreihe = "X_t", Variable_Datum = "date",
         Frequenz = 12 ,Anteil =  1, Format = "%d %b %y",
         Ymin = 35, Ymax = 55)

#SEATS_TS 
SEATS_TS <- function(Daten,Variable_Zeitreihe, Anteil,
                     Modell_out = "Modellseats"){
  
  # Anteil - Zahl zwsichen 1 und unendlich die bestimmt wie viel Prozent des Datensatzes
  #          als Trainingsdaten verwendet werden. Bei 1 werden alle vorhandenen Daten
  #          zum trainieren verwendet.
  
  # Daten  - Eine Zeitreihe in form ts.
  
  # Variable_Zeitreihe - Spalte in der die Variable von interesse hinterlegt ist.
  
  Modell <- tramoseats(Daten[,Variable_Zeitreihe], spec = "RSAfull")
  assign(Modell_out, Modell, envir = .GlobalEnv) 
  print("Job Done")
}

#X13 Methode
X13ARIMA <- function(Daten,Variable_Zeitreihe,Variable_Datum,Frequenz,Format,Anteil,
                     Ymin = NULL, Ymax = NULL, Modell_out = "Modellx13"){
  # Anteil - Zahl zwsichen 1 und unendlich die bestimmt wie viel Prozent des Datensatzes
  #          als Trainingsdaten verwendet werden. Bei 1 werden alle vorhandenen Daten
  #          zum trainieren verwendet.
  
  # Daten  - Ein Datensatz in Matrix oder Dataframe Format.
  
  # Format - Format in der das Datum im Datensatz vorliegt z.B. "%d %b %y".
  
  # Frequenz - Frequenz in der Daten gemessen wurden und saisonale Muster wiederkehren.
  #            Bei monatlichen Messungen Format = 12, bei vierteljährlichen = 4.
  
  # Variable_Datum - Spalte in der das Datum im Datensatz hinterlegt ist.
  
  # Variable_Zeitreihe - Spalte in der die Variable von interesse hinterlegt ist.
  
  # Ymin/Ymax - Paramterer zum Anpassen des Graphen
  

  if (all(class(Daten[,Variable_Datum]) == "Date") == TRUE){
    Datum <- Daten[,Variable_Datum]
  }
  else {
    Datum <- as.Date(paste("01",Daten[,Variable_Datum]), format = Format)
  }
  Ende <- round(length(Daten[,Variable_Zeitreihe])/Anteil,0)
  Testdaten <- ts(Daten[1:Ende,Variable_Zeitreihe],
                  frequency = Frequenz)
  Validdaten <- ts(Daten[(Ende+1):length(Daten[,Variable_Zeitreihe]),Variable_Zeitreihe],
                   frequency = Frequenz)
  Modell <- x13(Testdaten, spec = "RSA5c")
  assign(Modell_out, Modell, envir = .GlobalEnv)
  Prediction <- Modell[["regarima"]][["forecast"]][,"fcst"]
  Länge_Predictions <- length(Prediction)
  MSE <- round(mean((as.numeric(Prediction)-Validdaten[1:Länge_Predictions])^2),2)
  #Plot <- plot(Modell, type_chart = "sa-trend")
  #Plot_Dec <- plot(Modell$decomposition)
  #Plot_Pred <- plot(y = Validdaten[1:Länge_Predictions],
                    #x = Datum[(Ende+1):((Ende+1)+Länge_Predictions)],
                    #col = "green3", xlab= "Datum",
                    #ylab = Variable_Zeitreihe, type = "l",
                    #main = "Vorhersage der Zeitreihe mit X13-ARIMA",
                    #sub = paste0("MSE: ",format(MSE, big.mark = ",")),
                    #ylim = c(Ymin,Ymax)) %>%
                    #lines(x = Datum[(Ende+1):((Ende+1)+Länge_Predictions)],
                    #y= Prediction, col = "red4", lwd = 1, type = "l")
  #Plot
  #Plot_Dec
  #Plot_Pred
  print("Job Done")
}
X13ARIMA(Daten = ts_data[1:104,], Variable_Zeitreihe = "X_t", Variable_Datum = "date",
         Frequenz = 12 ,Anteil =  1, Format = "%d %b %y",
         Ymin = 35, Ymax = 55, Modell_out = "ModellX13")

#X13_TS
X13ARIMA_TS <- function(Daten,Variable_Zeitreihe, Anteil,
                     Modell_out = "Modellseats"){
  
  # Anteil - Zahl zwsichen 1 und unendlich die bestimmt wie viel Prozent des Datensatzes
  #          als Trainingsdaten verwendet werden. Bei 1 werden alle vorhandenen Daten
  #          zum trainieren verwendet.
  
  # Daten  - Eine Zeitreihe in form ts.
  
  # Variable_Zeitreihe - Spalte in der die Variable von interesse hinterlegt ist.
  
  Modell <- x13(Daten[,Variable_Zeitreihe], spec = "RSA5c")
  assign(Modell_out, Modell, envir = .GlobalEnv) 
  print("Job Done")
}

#BV4 Methode



# Summe Quadrierter Revisionen -------------------------------------------------
Summe_quadrierter_Revisisonen <- function(Daten, Bereich_Untergrenze, Bereich_Obergrenze, Methode = X13ARIMA,
                                          Variable_Zeitreihe, Variable_Datum, Frequenz, Anteil, Format, Output = "SQR"){
  
  if (Bereich_Untergrenze < 36) {
    stop("Fehler: Die Untergrenze darf nicht kleiner als 36 sein.")
  }
  
  if (Bereich_Untergrenze > Bereich_Obergrenze) {
    stop("Fehler: Die Untergrenze darf nicht größer als die Obergrenze sein.")
  }
  
  erlaubte_Methoden <- c("X13ARIMA", "X13ARIMA_TS", "SEATS", "SEATS_TS", "BV4")
  if (!deparse(substitute(Methode)) %in% erlaubte_Methoden) {
    stop("Fehler: Die Methode muss eine der folgenden Funktionen sein: X13ARIMA, SEATS oder BV4.")
  }
  
  if (class(Daten[[Variable_Zeitreihe]]) != "numeric") {
    stop("Fehler: Variable_Zeitreihe nicht im korrekten Format (numeric).")
  }
  
  if (class(Daten[[Variable_Datum]]) != "Date") {
    stop("Fehler: Variable_Datum nicht im korrekten Format (Date) oder nocht so codiert,
          dass damit gearbeitet werden kann. Bitte bearbeiten und erneut versuchen.")
  }
  
  Bereich <- seq(Bereich_Untergrenze,Bereich_Obergrenze,1)
  Revision_outer <- numeric(length(Bereich))
  for (i in Bereich){
    Revision_inner <- numeric(12)
    for (k in 1:12){
      Methode(Daten = Daten[1:(i+k-1),], Variable_Zeitreihe = Variable_Zeitreihe, Variable_Datum = Variable_Datum, # Modell Zum Zeitpunkt k
              Frequenz = Frequenz, Anteil = Anteil, Format = Format, Modell_out = "real")
      Methode(Daten = Daten[1:(i+k-2),], Variable_Zeitreihe = Variable_Zeitreihe, Variable_Datum = Variable_Datum, # Modell Zum Zeitpunkt k-1
              Frequenz = Frequenz, Anteil = Anteil, Format = Format, Modell_out = "back")
      Revision_inner[k] <- (real[["final"]][["series"]][,"t"][(i-1)] - back[["final"]][["series"]][,"t"][(i-1)])^2
      print(k)
    }
    Summe_k <- sum(Revision_inner)
    Methode(Daten = Daten[1:(i+11),], Variable_Zeitreihe = Variable_Zeitreihe, Variable_Datum = Variable_Datum, # Modell Zum Zeitpunkt k=12
             Frequenz = Frequenz, Anteil = Anteil, Format = Format, Modell_out = "last")
    Entwicklung_last <- (last[["final"]][["series"]][,"t"][(i-1)]) ^2
    Revision_outer[i-(Bereich[1]-1)] <- (Summe_k / Entwicklung_last)*100
    print(i)
  }
  SQR <- mean(Revision_outer)
  assign(Output, SQR, envir = .GlobalEnv)
}
Summe_quadrierter_Revisisonen(Daten = ts_data, Bereich_Untergrenze = 148, Bereich_Obergrenze = 152, Methode = X13ARIMA,
                              Variable_Zeitreihe = "X_t", Variable_Datum = "date", # Modell zum Zeitpunkt k = 12
                              Frequenz = 12, Anteil = 1, Output = "SQR1")

# Errors Symmetrischer Filterbereich ------------------------------------------------------------------
Errors_Symmetrischer_Filterbreich <- function(Daten, Bereich_Obergrenze, Methode = X13ARIMA,
                            Variable_Zeitreihe, Variable_Datum, Frequenz = 12,
                            Anteil, Format, Output = "Errors"){
  
  if (Bereich_Obergrenze > length(Daten[[Variable_Zeitreihe]]) - 20) {
    stop("Fehler: Die Obergrenze liegt außerhalb der Daten.")
  }
  
  erlaubte_Methoden <- c("X13ARIMA", "SEATS", "BV4")
  if (!deparse(substitute(Methode)) %in% erlaubte_Methoden) {
    stop("Fehler: Die Methode muss eine der folgenden Funktionen sein: X13ARIMA, SEATS oder BV4.")
  }
  
  if (class(Daten[[Variable_Zeitreihe]]) != "numeric") {
    stop("Fehler: Variable_Zeitreihe nicht im korrekten Format (numeric).")
  }
  
  if (class(Daten[[Variable_Datum]]) != "Date") {
    stop("Fehler: Variable_Datum nicht im korrekten Format (Date) oder nocht so codiert,
          dass damit gearbeitet werden kann. Bitte bearbeiten und erneut versuchen.")
  }
  
  Methode(Daten = Daten[1:Bereich_Obergrenze,], Variable_Zeitreihe = Variable_Zeitreihe, Variable_Datum = Variable_Datum, # Modell Zum Zeitpunkt k
            Frequenz = Frequenz, Anteil = Anteil, Format = Format, Modell_out = "Modell_MSE_Sym")

  Error <- data.frame(x = Modell_MSE_Sym[["final"]][["series"]][30:(Bereich_Obergrenze - 30),"i"])
  Errors <- ggplot(Error, aes(x = x)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
    geom_density(color = "red", linewidth = 1)
  assign(Output[1], Errors, envir = .GlobalEnv)
  
  Mean_Error <- mean(Error$x)
  assign(Output[2], Mean_Error, envir = .GlobalEnv)
}
Errors_Symmetrischer_Filterbreich(Daten = ts_data, Bereich_Obergrenze = 160, Methode = X13ARIMA,
                Variable_Zeitreihe = "X_t", Variable_Datum = "date", Frequenz = 12,
                Anteil = 1, Output = c("Errors_Sym","Mean_Error_Sym"))


# MSE Predictions --------------------------------------------------------------
MSE_Prediction <- function(Daten, Bereich_Untergrenze, Bereich_Obergrenze, Methode = X13ARIMA,
                            Variable_Zeitreihe, Variable_Datum, Frequenz = 12,
                            Anteil, Format, Output = "MSE_Pred"){
  
  if (Bereich_Obergrenze > length(Daten[[Variable_Zeitreihe]])) {
    stop("Fehler: Die Obergrenze liegt außerhalb der Daten.")
  }
  
  if (Bereich_Untergrenze < 36) {
    stop("Fehler: Die Untergrenze darf nicht kleiner als 36 sein.")
  }
  
  if (Bereich_Untergrenze > Bereich_Obergrenze) {
    stop("Fehler: Die Untergrenze darf nicht größer als die Obergrenze sein.")
  }
  
  erlaubte_Methoden <- c("X13ARIMA", "SEATS", "BV4")
  if (!deparse(substitute(Methode)) %in% erlaubte_Methoden) {
    stop("Fehler: Die Methode muss eine der folgenden Funktionen sein: X13ARIMA, SEATS oder BV4.")
  }
  
  if (class(Daten[[Variable_Zeitreihe]]) != "numeric") {
    stop("Fehler: Variable_Zeitreihe nicht im korrekten Format (numeric).")
  }
  
  if (class(Daten[[Variable_Datum]]) != "Date") {
    stop("Fehler: Variable_Datum nicht im korrekten Format (Date) oder nocht so codiert,
          dass damit gearbeitet werden kann. Bitte bearbeiten und erneut versuchen.")
  }
  
  Bereich <- seq(Bereich_Untergrenze,Bereich_Obergrenze,1)
  X_wahr <- matrix(rep(0,12*length(Bereich)), nrow = 12, ncol = length(Bereich))
  X_schätz <- matrix(rep(0,12*length(Bereich)), nrow = 12, ncol = length(Bereich))
  
  for (i in Bereich) {
    idx <- i - (Bereich[1]-1)
    X_wahr[,idx] <- Daten[[Variable_Zeitreihe]][(i+1):(i+12)]
    Methode(Daten = Daten[1:i,], Variable_Zeitreihe = Variable_Zeitreihe, Variable_Datum = Variable_Datum, # Modell Zum Zeitpunkt k
            Frequenz = Frequenz, Anteil = Anteil, Format = Format, Modell_out = "Modell_MSE_Pred")
    X_schätz[,idx] <- Modell_MSE_Pred[["final"]][["forecasts"]][,"y_f"]
  }
  
  # MSE der Predictions nach länge der Vorhersage in die zukunft
  Predictions_Matrix <-  (X_wahr-X_schätz)
  MSE_Predicitons <-  mean((X_wahr-X_schätz)^2)
  MSE_Pred_1Step <- mean((X_wahr[1,]-X_schätz[1,])^2)
  MSE_Pred_12Step <- mean((X_wahr[12,]-X_schätz[12,])^2)
  assign(Output[1], Predictions_Matrix, envir = .GlobalEnv)
  
  MSE <- numeric(12)
  for (i in 1:12) {
    MSE[i] <- mean((X_wahr[i,]-X_schätz[i,])^2)
  }
  
  MSE_Pred <- data.frame("Pred" = paste0(1:12, " Step Prediction"), MSE = MSE)
  MSE_Pred[13,1] <- "MSE Total"
  MSE_Pred[13,2] <- mean(MSE_Pred[1:12,2])
  assign(Output[2], MSE_Pred, envir = .GlobalEnv)
  
  # Plot mit Predictions
  Bereich1 <- seq(from = 1, to = length(Bereich), by = 1)
  df_wahr <- as.data.frame(X_wahr[, Bereich1])
  df_schätz <- as.data.frame(X_schätz[, Bereich1])
  df_wahr$time <- Daten$time[1:12]
  df_schätz$time <- Daten$time[1:12]
  df_wahr_long <- pivot_longer(df_wahr, cols = -time, names_to = "Variable", values_to = "Wahr")
  df_schätz_long <- pivot_longer(df_schätz, cols = -time, names_to = "Variable", values_to = "Schätz")
  
  df_combined <- left_join(df_wahr_long, df_schätz_long, by = c("time", "Variable"))
  df_combined$Variable <- factor(df_combined$Variable, 
                                 levels = paste0("V", sort(as.numeric(gsub("V", "", unique(df_combined$Variable))))))
  
  Preds <- ggplot(df_combined, aes(x = time)) +
    geom_line(aes(y = Wahr), color = "grey", lwd = 1, alpha = 0.8) +
    geom_line(aes(y = Schätz), color = "blue", lwd = 1, alpha = 0.7) +
    facet_wrap(~Variable, scales = "free_y") +
    labs(title = "Wahr vs Schätz pro Variable", y = "Wert") +
    theme_minimal()
  assign(Output[3], Preds, envir = .GlobalEnv)
}
MSE_Prediction(Daten = ts_data, Bereich_Untergrenze = 132, Bereich_Obergrenze = 140, Methode = X13ARIMA,
                Variable_Zeitreihe = "X_t", Variable_Datum = "date", Frequenz = 12,
                Anteil = 1, Format, Output = c("MSE_Prediction_Matrix","MSE_Preds", "Preds_Plot"))


# MSE TKK Aktuell -----------------------------------------------------
MSE_TKK_aktuell <- function(Daten, Bereich_Untergrenze, Bereich_Obergrenze, Methode = X13ARIMA,
                        Variable_Zeitreihe, Variable_Datum, Variable_Trend, Frequenz = 12,
                        Anteil, Format, Output = "MSE1"){
  
  if (Bereich_Obergrenze > length(Daten[[Variable_Zeitreihe]])) {
    stop("Fehler: Die Obergrenze liegt außerhalb der Daten.")
  }
  
  if (Bereich_Untergrenze < 36) {
    stop("Fehler: Die Untergrenze darf nicht kleiner als 36 sein.")
  }
  
  if (Bereich_Untergrenze > Bereich_Obergrenze) {
    stop("Fehler: Die Untergrenze darf nicht größer als die Obergrenze sein.")
  }
  
  erlaubte_Methoden <- c("X13ARIMA", "SEATS", "BV4")
  if (!deparse(substitute(Methode)) %in% erlaubte_Methoden) {
    stop("Fehler: Die Methode muss eine der folgenden Funktionen sein: X13ARIMA, SEATS oder BV4.")
  }
  
  if (class(Daten[[Variable_Zeitreihe]]) != "numeric") {
    stop("Fehler: Variable_Zeitreihe nicht im korrekten Format (numeric).")
  }
  
  if (class(Daten[[Variable_Trend]]) != "numeric") {
    stop("Fehler: Variable_Trend nicht im korrekten Format (numeric).")
  }
  
  if (class(Daten[[Variable_Datum]]) != "Date") {
    stop("Fehler: Variable_Datum nicht im korrekten Format (Date) oder nocht so codiert,
          dass damit gearbeitet werden kann. Bitte bearbeiten und erneut versuchen.")
  }
  
  Bereich <- seq(Bereich_Untergrenze,Bereich_Obergrenze,1)
  TKK_wahr <- numeric(length(Bereich))
  TKK_schätz <- numeric(length(Bereich))
  
  for (i in Bereich) {
    idx <- i - (Bereich[1]-1)
    TKK_wahr[idx] <- Daten[[Variable_Trend]][i]
    Methode(Daten = Daten[1:i,], Variable_Zeitreihe = Variable_Zeitreihe, Variable_Datum = Variable_Datum, # Modell Zum Zeitpunkt k
            Frequenz = Frequenz, Anteil = Anteil, Format = Format, Modell_out = "Modell_MSE1_TKK")
    TKK_schätz[idx] <- Modell_MSE1_TKK[["final"]][["series"]][,"t"][i]
  }
  
MSE <-  mean((TKK_wahr-TKK_schätz)^2)
assign(Output, MSE, envir = .GlobalEnv)
}
MSE_TKK_aktuell(Daten = ts_data, Bereich_Untergrenze = 90, Bereich_Obergrenze = 97, Methode = X13ARIMA,
                        Variable_Zeitreihe = "X_t", Variable_Datum = "date", Variable_Trend = "trend", Frequenz = 12,
                        Anteil = 1, Format, Output = "MSE1_TKK")
  
# MSE TKK vor 4 Perioden -------------------------------------------------------
MSE_TKK_vor4 <- function(Daten, Bereich_Untergrenze, Bereich_Obergrenze, Methode = X13ARIMA,
                        Variable_Zeitreihe, Variable_Datum, Variable_Trend, Frequenz = 12,
                        Anteil, Format, Output = "MSE4"){
  
  if (Bereich_Obergrenze > length(Daten[[Variable_Zeitreihe]])) {
    stop("Fehler: Die Obergrenze liegt außerhalb der Daten.")
  }
  
  if (Bereich_Untergrenze < 36) {
    stop("Fehler: Die Untergrenze darf nicht kleiner als 36 sein.")
  }
  
  if (Bereich_Untergrenze > Bereich_Obergrenze) {
    stop("Fehler: Die Untergrenze darf nicht größer als die Obergrenze sein.")
  }
  
  erlaubte_Methoden <- c("X13ARIMA", "SEATS", "BV4")
  if (!deparse(substitute(Methode)) %in% erlaubte_Methoden) {
    stop("Fehler: Die Methode muss eine der folgenden Funktionen sein: X13ARIMA, SEATS oder BV4.")
  }
  
  if (class(Daten[[Variable_Zeitreihe]]) != "numeric") {
    stop("Fehler: Variable_Zeitreihe nicht im korrekten Format (numeric).")
  }
  
  if (class(Daten[[Variable_Trend]]) != "numeric") {
    stop("Fehler: Variable_Trend nicht im korrekten Format (numeric).")
  }
  
  if (class(Daten[[Variable_Datum]]) != "Date") {
    stop("Fehler: Variable_Datum nicht im korrekten Format (Date) oder nocht so codiert,
          dass damit gearbeitet werden kann. Bitte bearbeiten und erneut versuchen.")
  }
  
  Bereich <- seq(Bereich_Untergrenze,Bereich_Obergrenze,1)
  TKK_wahr <- numeric(length(Bereich))
  TKK_schätz <- numeric(length(Bereich))
  
  for (i in Bereich) {
    idx <- i - (Bereich[1]-1)
    TKK_wahr[idx] <- Daten[[Variable_Trend]][i-4]
    Methode(Daten = Daten[1:i,], Variable_Zeitreihe = Variable_Zeitreihe, Variable_Datum = Variable_Datum, # Modell Zum Zeitpunkt k
            Frequenz = Frequenz, Anteil = Anteil, Format = Format, Modell_out = "Modell_MSE1")
    TKK_schätz[idx] <- Modell_MSE1[["final"]][["series"]][,"t"][i-4]
  }
  
  MSE <-  mean((TKK_wahr-TKK_schätz)^2)
  assign(Output, MSE, envir = .GlobalEnv)
}
MSE_TKK_vor4(Daten = ts_data, Bereich_Untergrenze = 90, Bereich_Obergrenze = 97, Methode = X13ARIMA,
            Variable_Zeitreihe = "X_t", Variable_Datum = "date", Variable_Trend = "trend", Frequenz = 12,
            Anteil = 1, Format, Output = "MSE4")

# Treffsicherheit -----------------------------------------------------
Treffsicherheit_j<- function(Daten, Bereich_Untergrenze, Bereich_Obergrenze, Methode = X13ARIMA,
                             Variable_Zeitreihe, Variable_Datum, Variable_Trend, Frequenz = 12,
                             Anteil, Format, Verschärfung = 0.5, Output = "Treff_j"){
  
  if (Bereich_Obergrenze > length(Daten[[Variable_Zeitreihe]])) {
    stop("Fehler: Die Obergrenze liegt außerhalb der Daten.")
  }
  
  if (Bereich_Untergrenze < 36) {
    stop("Fehler: Die Untergrenze darf nicht kleiner als 36 sein.")
  }
  
  if (Bereich_Untergrenze > Bereich_Obergrenze) {
    stop("Fehler: Die Untergrenze darf nicht größer als die Obergrenze sein.")
  }
  
  erlaubte_Methoden <- c("X13ARIMA", "SEATS", "BV4")
  if (!deparse(substitute(Methode)) %in% erlaubte_Methoden) {
    stop("Fehler: Die Methode muss eine der folgenden Funktionen sein: X13ARIMA, SEATS oder BV4.")
  }
  
  if (class(Daten[[Variable_Zeitreihe]]) != "numeric") {
    stop("Fehler: Variable_Zeitreihe nicht im korrekten Format (numeric).")
  }
  
  if (class(Daten[[Variable_Trend]]) != "numeric") {
    stop("Fehler: Variable_Trend nicht im korrekten Format (numeric).")
  }
  
  if (class(Daten[[Variable_Datum]]) != "Date") {
    stop("Fehler: Variable_Datum nicht im korrekten Format (Date) oder nicht so codiert,
          dass damit gearbeitet werden kann. Bitte bearbeiten und erneut versuchen.")
  }
  
Reihe1 <- c(seq(from = Bereich_Untergrenze, to = Bereich_Obergrenze, by = 1))
Entwicklung_Wahr <- numeric(length(Reihe1))
Entwicklung_Schätzung_J <- numeric(length(Reihe1))
S <- numeric(length(Reihe1))
for (i in Reihe1){
  Methode(Daten = Daten[1:i,], Variable_Zeitreihe = Variable_Zeitreihe, Variable_Datum = Variable_Datum,
           Frequenz = Frequenz, Anteil = Anteil, Modell_out = "Modell_Treff")
  
  idx <- i - (Reihe1[1] - 1)
  
  Entwicklung_Schätzung_J[idx] <- Modell_Treff[["final"]][["series"]][,"t"][i] -
    Modell_Treff[["final"]][["series"]][,"t"][i-1]
  
  Entwicklung_Wahr[idx] <- Daten[[Variable_Trend]][i] - Daten[[Variable_Trend]][i-1]
  
  if ((Entwicklung_Wahr[idx] >= 0 & Entwicklung_Schätzung_J[idx] >= 0) |
      (Entwicklung_Wahr[idx] < 0 & Entwicklung_Schätzung_J[idx] < 0)){
    S[idx] <- 0.05 * mean(Daten[[Variable_Zeitreihe]])
  } else {
    S[idx] <- (0.05 * mean(Daten[[Variable_Zeitreihe]])) * (1 - Verschärfung * ((1 - ((Entwicklung_Schätzung_J[idx] - Entwicklung_Wahr[idx]) /
                                                           (0.05 * mean(Daten[[Variable_Zeitreihe]])))^2)^2))
  }
}
Trend_Konjunktur_Entwicklung_J <- (1-((Entwicklung_Schätzung_J-Entwicklung_Wahr)/S)^2)^2
Index <- seq(1:length(Entwicklung_Schätzung_J))
Frame1 <- data.frame(Index = Index, TKE = Trend_Konjunktur_Entwicklung_J)
Beste_Bewertung <- Frame1$Index[which.max(Frame1$TKE)]
Beste_Schätzung_Konjunktur_Entwicklung_J<- max(Frame1$TKE)
Treffsicherheit_J <- mean(Trend_Konjunktur_Entwicklung_J)
assign(Output, Treffsicherheit_J, envir = .GlobalEnv)
}
Treffsicherheit_j(Daten = ts_data, Bereich_Untergrenze = 90, Bereich_Obergrenze = 97, Methode = X13ARIMA,
                             Variable_Zeitreihe = "X_t", Variable_Datum = "date", Variable_Trend = "trend", Frequenz = 12,
                             Anteil = 1, Verschärfung = 0.8, Output = "Treff_j")

# Treffsicherheit Goldrian (Referenzwert) --------------------------------------
Treffsicherheit_g<- function(Daten, Bereich_Untergrenze, Bereich_Obergrenze, Methode = X13ARIMA,
                             Variable_Zeitreihe, Variable_Datum, Variable_Trend, Frequenz = 12,
                             Anteil, Format, Verschärfung = 0.5, Output = "Treff_g"){
  
  if (Bereich_Untergrenze < 36) {
    stop("Fehler: Die Untergrenze darf nicht kleiner als 36 sein.")
  }
  
  if (Bereich_Obergrenze > length(Daten[[Variable_Zeitreihe]])) {
    stop("Fehler: Die Obergrenze liegt außerhalb der Daten.")
  }
  
  if (Bereich_Untergrenze > Bereich_Obergrenze) {
    stop("Fehler: Die Untergrenze darf nicht größer als die Obergrenze sein.")
  }
  
  erlaubte_Methoden <- c("X13ARIMA", "SEATS", "BV4")
  if (!deparse(substitute(Methode)) %in% erlaubte_Methoden) {
    stop("Fehler: Die Methode muss eine der folgenden Funktionen sein: X13ARIMA, SEATS oder BV4.")
  }
  
  if (class(Daten[[Variable_Zeitreihe]]) != "numeric") {
    stop("Fehler: Variable_Zeitreihe nicht im korrekten Format (numeric).")
  }
  
  if (class(Daten[[Variable_Trend]]) != "numeric") {
    stop("Fehler: Variable_Trend nicht im korrekten Format (numeric).")
  }
  
  if (class(Daten[[Variable_Datum]]) != "Date") {
    stop("Fehler: Variable_Datum nicht im korrekten Format (Date) oder nicht so codiert,
          dass damit gearbeitet werden kann. Bitte bearbeiten und erneut versuchen.")
  }
  
  Reihe1 <- c(seq(from = Bereich_Untergrenze, to = Bereich_Obergrenze, by = 1))
  
  Methode(Daten = Daten, Variable_Zeitreihe = "X_t", Variable_Datum = "date",
           Frequenz = 12, Anteil = 1, Modell_out = "Modell")
  Entwicklung_Referenz <- numeric(length(Reihe1))
  Entwicklung_Schätzung_g <- numeric(length(Reihe1))
  S <- numeric(length(Reihe1))
  for (i in Reihe1){
    Methode(Daten = Daten[1:i,], Variable_Zeitreihe = Variable_Zeitreihe, Variable_Datum = Variable_Datum,
             Frequenz = Frequenz, Anteil = Anteil, Modell_out = "Modell_Treff")
    
    idx <- i - (Reihe1[1] - 1)
    
    Entwicklung_Schätzung_g[idx] <- Modell_Treff[["final"]][["series"]][,"t"][i] -
      Modell_Treff[["final"]][["series"]][,"t"][i-1]
    Entwicklung_Referenz[idx] <- Modell[["final"]][["series"]][,"t"][i] -
      Modell[["final"]][["series"]][,"t"][i-1]
    
    if ((Entwicklung_Referenz[idx] >= 0 & Entwicklung_Schätzung_g[idx] >= 0) |
        (Entwicklung_Referenz[idx] < 0 & Entwicklung_Schätzung_g[idx] < 0)){
      S[idx] <- 0.05 * mean(Daten[[Variable_Zeitreihe]])
    } else {
      S[idx] <- (0.05 * mean(Daten[[Variable_Zeitreihe]])) * (1 - Verschärfung * ((1 - ((Entwicklung_Schätzung_g[idx] - Entwicklung_Referenz[idx]) /
                                                                             (0.05 *mean(Daten[[Variable_Zeitreihe]])))^2)^2))
    }
  }
  Trend_Konjunktur_Entwicklung_g <- (1-((Entwicklung_Schätzung_g-Entwicklung_Referenz)/S)^2)^2
  Index <- seq(1:length(Entwicklung_Schätzung_g))
  Frame1 <- data.frame(Index = Index, TKE = Trend_Konjunktur_Entwicklung_g)
  Beste_Bewertung <- Frame1$Index[which.max(Frame1$TKE)]
  Beste_Schätzung_Konjunktur_Entwicklung_g<- max(Frame1$TKE)
  Treffsicherheit_g <- mean(Trend_Konjunktur_Entwicklung_g)
  assign(Output, Treffsicherheit_g, envir = .GlobalEnv)
}
Treffsicherheit_g(Daten = ts_data, Bereich_Untergrenze = 90, Bereich_Obergrenze = 97, Methode = X13ARIMA,
                  Variable_Zeitreihe = "X_t", Variable_Datum = "date", Variable_Trend = "trend", Frequenz = 12,
                  Anteil = 1, Output = "Treff_g")

# Plots ------------------------------------------------------------------------
# Revisions Plot
Revisions_Plot <- function(Daten, Methode = X13ARIMA, Untergrenze, Obergrenze, Sprünge = 2,
                           Variable_Zeitreihe, Variable_Datum, Variable_Trend,
                           Frequenz, Format, Plot_out = "Rev_plot"){
  
  if (Untergrenze < 36) {
    stop("Fehler: Die Untergrenze darf nicht kleiner als 36 sein.")
  }
  
  if (Obergrenze < Untergrenze) {
    stop("Fehler: Die Obergrenze darf nicht kleiner als die Untergrenze sein.")
  }
  
  if ((Obergrenze - Untergrenze) / Sprünge > 13) {
    stop("Fehler: Es können nicht mehr als 15 Linien geplottet werden.
          Bitte Grenzen oder Sprünge anpassen.")
  }
  
  erlaubte_Methoden <- c("X13ARIMA", "SEATS", "BV4")
  if (!deparse(substitute(Methode)) %in% erlaubte_Methoden) {
    stop("Fehler: Die Methode muss eine der folgenden Funktionen sein: X13ARIMA, SEATS oder BV4.")
  }
  
  if (class(Daten[[Variable_Zeitreihe]]) != "numeric") {
    stop("Fehler: Variable_Zeitreihe nicht im korrekten Format (numeric).")
  }
  
  if (class(Daten[[Variable_Trend]]) != "numeric") {
    stop("Fehler: Variable_Trend nicht im korrekten Format (numeric).")
  }
  
  if (class(Daten[[Variable_Datum]]) != "Date") {
    stop("Fehler: Variable_Datum nicht im korrekten Format (Date) oder nocht so codiert,
          dass damit gearbeitet werden kann. Bitte bearbeiten und erneut versuchen.")
  }
  
  Werte <- c(seq(from = Untergrenze, to = Obergrenze, by = Sprünge))
  
  Frame_list <- lapply(Werte, function(n) {
    Methode(Daten = Daten[1:n,], Variable_Zeitreihe = Variable_Zeitreihe, Variable_Datum = Variable_Datum,
            Frequenz = Frequenz, Anteil = 1, Format = Format, Modell_out = "Modell")
    Trend_konjunktur <- Modell[["final"]][["series"]][,"t"]
    data.frame(
      TKgeschätzt = Trend_konjunktur,
      Datum = Daten[[Variable_Datum]][1:n],
      run = rep(n, n)
    )
  })
  Frame_combined <- bind_rows(Frame_list)
  Frame <- data.frame(
    Reihe = Daten[[Variable_Zeitreihe]][1:(Obergrenze+5)],
    TK = Daten[[Variable_Trend]][1:(Obergrenze+5)],
    Datum = Daten[[Variable_Datum]][1:(Obergrenze+5)],
    run = Obergrenze + 6
  )
  
  Plot <- ggplot() +
    geom_line(data = Frame_combined, aes(x = Datum, y = TKgeschätzt, group = run, color = as.factor(run)),
              alpha = 0.9, lwd = 1) +
    geom_line(data = Frame, aes(x = Datum, y = Reihe, color = "Zeitreihe"), lwd = 1, alpha = 0.7) +
    geom_line(data = Frame, aes(x = Datum, y = TK, color = "TKK"), lwd = 1, alpha = 0.6) +
    scale_color_manual(
      name = "Anzahl Beobachtungen",
      values = c(
        "Zeitreihe" = "black",
        "TKK" = "blue2",
        setNames(c("red3","green4","purple3","magenta","cyan","darkorange","chartreuse","turquoise",
                   "purple","orchid1","aquamarine", "yellow","hotpink" ,
                   "lightgreen" , "salmon", "indianred", "deepskyblue", "darkolivegreen1"),
                 as.character(sort(unique(Frame_combined$run))))
      )
    ) +
    theme_minimal() +
    ylab("Wert der Zeitreihe")+
    ggtitle(paste0("Revision bei ", deparse(substitute(Methode))))+
    labs(subtitle = "Schätzung der Trend-Konjunktur-Komponente in Abhängigkeit der Anzahl an Beobachtungen", 
         caption = "") #+
  #coord_cartesian(xlim = as.Date(c("2022-01-01", "2025-01-01")), ylim = c(13000,22000))
  assign(Plot_out, Plot, envir = .GlobalEnv)
}
Revisions_Plot(Daten = ts_data, Methode = X13ARIMA, Untergrenze = 38, Obergrenze = 50, Sprünge = 5,
               Variable_Zeitreihe = "X_t", Variable_Datum = "date", Variable_Trend = "trend",
               Frequenz = 12, Plot_out = "Rev_Plot")




