# Qualitätsmaße erstellen

# Revision grafisch Darstellen
# Outlier Wert 144, TK Wende Wert 138, Season Wende Wert 141
Werte <- c(seq(from = 36, to = 74, by = 2))
for (n in Werte) {
  X13ARIMA(Daten = ts_data[1:n,], Variable_Zeitreihe = "X_t", Variable_Datum = "date",
           Frequenz = 12, Anteil = 1)
  Trend_konjunktur <- Modellx13[["final"]][["series"]][,"t"]
  assign(paste0("Frame", n),
         data.frame(TKgeschätzt = Trend_konjunktur,
                    Datum = ts_data$date[1:n],
                    run = rep(n, n)))
}
Frame_list <- lapply(Werte, function(n) get(paste0("Frame", n)))
Frame_combined <- bind_rows(Frame_list)
Frame <- data.frame(Reihe = ts_data$X_t[1:155], TK = ts_data$trend[1:155],
                    Datum = ts_data$date[1:155], run = 156)
ggplot() +
  geom_line(data = Frame_combined, aes(x = Datum, y = TKgeschätzt, group = run, color = as.factor(run)),
            alpha = 0.8, lwd = 0.7) +
  geom_line(data = Frame, aes(x = Datum, y = Reihe, color = "ZR original"), lwd = 1.2, alpha = 0.15) +
  geom_line(data = Frame, aes(x = Datum, y = TK, color = "TK original"), lwd = 1.5, alpha = 0.3) +
  scale_color_manual(
    name = "Anzahl Beobachtungen",
    values = c(
      "ZR original" = "greenWerte <- c(seq(from = 139, to = 151, by = 2))",
      "TK original" = "red3",
      setNames(c("purple","orange","turquoise","salmon","lightgreen","lightblue1","pink1"),
               as.character(sort(unique(Frame_combined$run))))
    )
  ) +
  theme_minimal() +
  ylab("Wert der Zeitreihe")+
  ggtitle("Revision")+
  labs(subtitle = "Schätzung der Trend-Konjunktur-Komponente in Abhängigkeit der Anzahl an Beobachtungen", 
       caption = "") +
  coord_cartesian(xlim = as.Date(c("2017-01-01", "2022-08-01")), ylim = c(15,40))


# Shift Idee grafisch darstellen
Shift <- 5  
Shift_Schätz <- Trend_konjunktur[(1+Shift):length(Trend_konjunktur)]
Shift_Trend <- ts_data$trend[1:length(Shift_Schätz)]
MSE_shift <- mean((Shift_Schätz -Shift_Trend)^2)
Frame <- data.frame(Trend_konjunktur = Trend_konjunktur[1:length(Shift_Schätz)],
                    date = ts_data$date[1:length(Shift_Schätz)],
                    Reihe = ts_data$X_t[1:length(Shift_Schätz)],Shift_Schätz = Shift_Schätz)
Plot_Shift <- ggplot(ts_data, aes(x = date)) +
  geom_line(data = Frame, aes(y = Reihe, color = "Zeitreihe"), lwd = 0.7, alpha = 0.5) +
  geom_line(aes(y = trend, color = "Trend-Konjunktur"), lwd = 1.4, alpha = 0.5) +
  geom_line(data = Frame, aes(y = Trend_konjunktur, color = "TK geschätzt"), lwd = 0.7, alpha = 0.8) +
  geom_line(data = Frame, aes(y = Shift_Schätz, color = "TK geschätzt shift"),
            linetype = "dashed",lwd = 0.7, alpha = 0.8) +
  ggtitle("Trend-Konjunktur-Komponente mit x13ARIMA") +
  xlab("Zeit") + ylab("Wert der Zeitreihe") +
  scale_color_manual(values = c("Zeitreihe" = "green2", "Trend-Konjunktur" = "blue1", "TK geschätzt" = "red1",
                                "TK geschätzt shift" = "red4")) +
  theme_minimal() +
  labs(subtitle = paste0("MSE: ", round(MSE,2), ";  MSE nach Shift: ",round(MSE_shift,2),
                         ";  Seed: ", Seed), color = "Legende",
       caption = Parameter) +
  coord_cartesian(ylim = c(10,55))
Plot_Shift

# MSE 
MSE <- mean((Trend_konjunktur-ts_data$trend)^2)

# MSE last X
Anfang <- length(Trend_konjunktur) - 5
Error <- numeric(length(Trend_konjunktur)-(Anfang-1))
for (i in Anfang:length(Trend_konjunktur)){
  Error[i-(Anfang-1)] <- Trend_konjunktur[i]-ts_data$trend[i]
}
MSE_lastX <- mean(Error^2)

# MSE nach bestem Shift
Shift_Sequenz <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
MSE_shift <- numeric(length(Shift_Sequenz))
for(i in Shift_Sequenz){
  if (i >= 0){
    Shift_Schätz <- Trend_konjunktur[(1+i):length(Trend_konjunktur)]
    Shift_Trend <- ts_data$trend[1:length(Shift_Schätz)]
    MSE_shift[6+i] <- mean((Shift_Schätz -Shift_Trend)^2)
  }
  else {
    Shift_Trend <- ts_data$trend[abs(-1+i):length(ts_data$trend)]
    Shift_Schätz <- Trend_konjunktur[1:length(Shift_Trend)]
    MSE_shift[6+i] <- mean((Shift_Schätz -Shift_Trend)^2)
  }
}
Frame_Shift <- data.frame(Index = Shift_Sequenz, MSE_shift = MSE_shift)
Shiftind <-  which.min(MSE_shift)
Shift <- Frame_Shift$Index[Shiftind]
MSE_shift <- min(MSE_shift)

# Varianz der Schätzungen
Reihe <- c(seq(from = 144, to = 154, by = 1))
Revisionen_Vorläufig <- numeric(length(Reihe))
for (i in Reihe){
  X13ARIMA(Daten = ts_data[1:i,], Variable_Zeitreihe = "X_t", Variable_Datum = "date",
           Frequenz = 12, Anteil = 1,Modell_out = "Modell")
  Schätzungen_Vorläufig[(i-(Reihe[1]-1))] <- Modell[["final"]][["series"]][,"t"][Reihe[1]]
}
Varianz_Schätzungen <- var(Schätzungen_Vorläufig)

# Treffsicherheit aktuelle Konjunkturentwicklung in X nachfolgenden Perioden (Jakob)
Reihe1 <- c(seq(from = 150, to = 161, by = 1))
Entwicklung_Wahr <- numeric(length(Reihe1))
Entwicklung_Schätzung_J <- numeric(length(Reihe1))
S <- numeric(length(Reihe1))
for (i in Reihe1){
  X13ARIMA(Daten = ts_data[1:i,], Variable_Zeitreihe = "X_t", Variable_Datum = "date",
           Frequenz = 12, Anteil = 1, Modell_out = "Modell")
  
  idx <- i - (Reihe1[1] - 1)
  
  Entwicklung_Schätzung_J[idx] <- Modell[["final"]][["series"]][,"t"][i] -
    Modell[["final"]][["series"]][,"t"][i-1]
  
  Entwicklung_Wahr[idx] <- ts_data$trend[i] - ts_data$trend[i-1]
  
  if ((Entwicklung_Wahr[idx] >= 0 & Entwicklung_Schätzung_J[idx] >= 0) |
      (Entwicklung_Wahr[idx] < 0 & Entwicklung_Schätzung_J[idx] < 0)){
    S[idx] <- 0.05 * mean(ts_data$X_t)
  } else {
    S[idx] <- (0.05 * mean(ts_data$X_t)) * (0.5 * ((1 - ((Entwicklung_Schätzung_J[idx] - Entwicklung_Wahr[idx]) /
                                                           (0.05 * mean(ts_data$X_t)))^2)^2))
  }
}
Trend_Konjunktur_Entwicklung_J <- (1-((Entwicklung_Schätzung_J-Entwicklung_Wahr)/S)^2)^2
Index <- seq(1:length(Entwicklung_Schätzung_J))
Frame1 <- data.frame(Index = Index, TKE = Trend_Konjunktur_Entwicklung_J)
Beste_Bewertung <- Frame1$Index[which.max(Frame1$TKE)]
Beste_Schätzung_Konjunktur_Entwicklung_J<- max(Frame1$TKE)
Treffsicherheit_J <- mean(Trend_Konjunktur_Entwicklung_J)

# Treffsicherheit aktuelle Konjunkturentwicklung in X nachfolgenden Perioden (Goldrian)
Reihe1 <- c(seq(from = 150, to = 161, by = 1))
X13ARIMA(Daten = ts_data, Variable_Zeitreihe = "X_t", Variable_Datum = "date",
         Frequenz = 12, Anteil = 1, Modell_out = "Modell")
Entwicklung_Referenz <- numeric(length(Reihe1))
Entwicklung_Schätzung_G <- numeric(length(Reihe1))
S <- numeric(length(Reihe1))
for (i in Reihe1){
  idx <- i - (Reihe1[1] - 1)
  X13ARIMA(Daten = ts_data[1:i,], Variable_Zeitreihe = "X_t", Variable_Datum = "date",
           Frequenz = 12, Anteil = 1, Modell_out = "Modell1")
  # Schätzung aus aktuellem Modell
  Entwicklung_Schätzung_G[idx] <- Modell1[["final"]][["series"]][,"t"][i] -
    Modell1[["final"]][["series"]][,"t"][i-1]
  # Referenz aus vollständigem Modell (vor der Schleife berechnen!)
  Entwicklung_Referenz[idx] <- Modell[["final"]][["series"]][,"t"][i] -
    Modell[["final"]][["series"]][,"t"][i-1]
  if ((Entwicklung_Referenz[idx] >= 0 & Entwicklung_Schätzung_G[idx] >= 0) |
      (Entwicklung_Referenz[idx] < 0 & Entwicklung_Schätzung_G[idx] < 0)) {
    S[idx] <- 0.05 * mean(ts_data$X_t)
  } else {
    S[idx] <- (0.05 * mean(ts_data$X_t)) * (0.5 * ((1 - ((Entwicklung_Schätzung_G[idx] - Entwicklung_Referenz[idx]) /
                                                        (0.05 * mean(ts_data$X_t)))^2)^2))
  }
}

Trend_Konjunktur_Entwicklung_G <- (1 - ((Entwicklung_Schätzung_G - Entwicklung_Referenz) / S)^2)^2
Frame1 <- data.frame(Index = seq_along(Entwicklung_Schätzung_G), TKE = Trend_Konjunktur_Entwicklung_G)
Beste_Bewertung <- Frame1$Index[which.max(Frame1$TKE)]
Beste_Schätzung_Konjunktur_Entwicklung <- max(Frame1$TKE)
Treffsicherheit_G <- mean(Trend_Konjunktur_Entwicklung_G)


# 














# Durchschnittliche Treffsicherheit der Schätzungen in X nachfolgenden Perioden
Reihe1 <- c(seq(from = (length(ts_data$trend)-10), to = length(ts_data$trend), by = 1))
X13ARIMA(Daten = ts_data[1:i,], Variable_Zeitreihe = "X_t", Variable_Datum = "date",
         Frequenz = 12, Anteil = 1, Modell_out = "Modell")
Revisionen_Final <- Modell[["final"]][["series"]][,"t"][Reihe1[1]]
Revisionen_Wahr <- ts_data$trend[Reihe1[1]]
Revisionen_Vorläufig <- numeric(length(Reihe1))
for (i in Reihe1){
  X13ARIMA(Daten = ts_data[1:i,], Variable_Zeitreihe = "X_t", Variable_Datum = "date",
           Frequenz = 12, Anteil = 1, Modell_out = "Modell")
  Revisionen_Vorläufig[(i-(Reihe1[1]-1))] <- Modell[["final"]][["series"]][,"t"][Reihe1[1]]
}
Durschnittliche_Revisionen <- sum(((Revisionen_Vorläufig - Revisionen_Wahr) / Revisionen_Wahr)^2) /
  length(Revisionen_Vorläufig)

# Summe quadrierte Revisionen
#48 = length(ts_data$X_t)
Bereich <- seq(138,149,1)
Revision_outer <- numeric(length(Bereich))
for (i in Bereich){
  Revision_inner <- numeric(12)
  for (k in 1:12){
    X13ARIMA(Daten = ts_data[1:(i+k),], Variable_Zeitreihe = "X_t", Variable_Datum = "date", # Modell Zum Zeitpunkt k
             Frequenz = 12, Anteil = 1, Modell_out = "real")
    X13ARIMA(Daten = ts_data[1:(i+k-1),], Variable_Zeitreihe = "X_t", Variable_Datum = "date", # Modell zum Zeitpunkt k-1
             Frequenz = 12, Anteil = 1, Modell_out = "back")
    Revision_inner[k] <- (real[["final"]][["series"]][,"t"][i] - back[["final"]][["series"]][,"t"][i])^2
    print(k)
  }
  Summe_k <- sum(Revision_inner)
  #print(Revision_inner)
  X13ARIMA(Daten = ts_data[1:(i+12),], Variable_Zeitreihe = "X_t", Variable_Datum = "date", # Modell zum Zeitpunkt k = 12
           Frequenz = 12, Anteil = 1, Modell_out = "last")
  Entwicklung_last <- (last[["final"]][["series"]][,"t"][i])^2
  Revision_outer[i-(Bereich[1]-1)] <- (Summe_k / Entwicklung_last)*100
  print(i)
}
Summe_quadrierte_Revisionen <- sum(Revision_outer) / 12

