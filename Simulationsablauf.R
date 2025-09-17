# 1. Simuliere eine Testzeitreihe ----------------------------------------------
ts_data <- simulate_time_series(n = 200, trend_slope = 0.2, trend_amplitude = 10, cycle_period = 80, cycle_amplitude = 1, 
                                season_period = 12, season_amplitude = 5, noise_sd = 0.6, outlier_prob = 0.01, 
                                outlier_magnitude = 10, shift_prob = 0.006, shift_magnitude = 8, seed = 1234511)
# Parameter übertragen
Parameter <- "Parameter: n = 200, trend_slope = 0.2, trend_amplitude = 10, cycle_period = 80, cycle_amplitude = 1, season_period = 12, season_amplitude = 5,
              noise_sd = 0.6, outlier_prob = 0.015, outlier_magnitude = 10, shift_prob = 0.006, shift_magnitude = 8, seed = 1234511"
# Seed übertragen
Seed <- "1234511"

# 2. Plot der Zeitreihe --------------------------------------------------------
ggplot(ts_data, aes(x = date, y = X_t)) +
  geom_line(color = "blue") +
  geom_point(data = ts_data[ts_data$outliers_ind != 0,], aes(x = date, y = X_t), color = "red", size = 3) +
  geom_point(data = ts_data[ts_data$shift_ind != 0,], aes(x = date, y = X_t), color = "green", size = 3) +
  ggtitle("Simulierte Zeitreihe mit Trend-Konjunktur, Saison, Kalendereffekten und Ausreißern") +
  xlab("Zeit") + ylab("X_t") +
  theme_minimal()
ggplot(ts_data, aes(x = date, y = trend)) +
  geom_line(color = "blue") +
  ggtitle("Simulierte Zeitreihe mit Trend-Konjunktur, Saison, Kalendereffekten und Ausreißern") +
  xlab("Zeit") + ylab("X_t") +
  theme_minimal()

# 3. Exportieren in CSV --------------------------------------------------------
write.xlsx(ts_data, "C:/Users/lfstat-caja/Zeitreihen Einarbeitung/R/Testdaten/DatenOutlier")

# 4. Predicten der Trend-Konjunktur-Komponente mit x13 -------------------------
X13ARIMA(Daten = ts_data, Variable_Zeitreihe = "X_t", Variable_Datum = "date",
         Frequenz = 12 ,Anteil =  1,
         Ymin = 35, Ymax = 55)
Trend_konjunktur <- Modellx13[["final"]][["series"]][,"t"]
Reihe <- Modellx13[["final"]][["series"]][,"y"]
Frame <- data.frame(Trend_konjunktur = Trend_konjunktur, date = ts_data$date,
                    Reihe = Reihe, Trend = ts_data$trend)
Plot_x13ARIMA <- ggplot(ts_data, aes(x = date)) +
  geom_line(data = Frame, aes(y = Reihe, color = "Zeitreihe"), lwd = 0.7, alpha = 0.5) +
  geom_line(aes(y = trend, color = "Trend-Konjunktur"), lwd = 1.4, alpha = 0.5) +
  geom_line(data = Frame, aes(y = Trend_konjunktur, color = "TK geschätzt"), lwd = 0.7, alpha = 0.8) +
  ggtitle("Trend-Konjunktur-Komponente mit x13ARIMA") +
  xlab("Zeit") + ylab("Wert der Zeitreihe") +
  scale_color_manual(values = c("Zeitreihe" = "green3", "Trend-Konjunktur" = "blue", "TK geschätzt" = "red")) +
  theme_minimal() +
  labs(#subtitle = paste0("MSE: ", round(MSE,2), ";  MSE letzte 3 Beobachtungen: ",
                         #round(MSE_lastX,2), ";  MSE mit Shift um ",Shift,": ",
                         #round(MSE_shift,2), ";  Seed: ", Seed),
       color = "Legende",
       caption = Parameter)
Plot_x13ARIMA

# 5. Predicten der Trend-Konjunktur-Komponente mit SEATS -----------------------
SEATS(Daten = ts_data, Variable_Zeitreihe = "X_t", Variable_Datum = "date",
         Frequenz = 12 ,Anteil =  1,
         Ymin = 35, Ymax = 55)
Trend_konjunktur <- Modell[["final"]][["series"]][,"t"]
Reihe <- Modell[["final"]][["series"]][,"y"]
Frame <- data.frame(Trend_konjunktur = Trend_konjunktur, date = ts_data$date,
                    Reihe = Reihe)
Plot_SEATS <- ggplot(ts_data, aes(x = date)) +
  geom_line(data = Frame, aes(y = Reihe, color = "Zeitreihe"), lwd = 0.7, alpha = 0.5) +
  geom_line(aes(y = trend, color = "Trend-Konjunktur"), lwd = 1.4, alpha = 0.5) +
  geom_line(data = Frame, aes(y = Trend_konjunktur, color = "TK geschätzt"), lwd = 0.7, alpha = 0.8) +
  ggtitle("Trend-Konjunktur-Komponente mit tramoSEATS") +
  xlab("Zeit") + ylab("Wert der Zeitreihe") +
  scale_color_manual(values = c("Zeitreihe" = "green3", "Trend-Konjunktur" = "blue", "TK geschätzt" = "red")) +
  theme_minimal() +
  labs(subtitle = paste0("MSE: ", round(MSE,2), ";  MSE letzte 3 Beobachtungen: ",
                         round(MSE_lastX,2), ";  MSE mit Shift um ",Shift,": ",
                         round(MSE_shift,2), ";  Seed: ", Seed), 
       color = "Legende",
       caption = Parameter)
Plot_SEATS

# 6. Predicten der Trend-Konjunktur-Komponente mit BV4.1 -----------------------
# Prediction findet in BV4.1 statt wird hier nur geplottet
BV4.1 <- read.xlsx("Für R4.xlsx")
Trend_konjunktur <-  BV4.1$TKK
Frame <- data.frame(Trend_konjunktur = Trend_konjunktur, date = ts_data$date,
                Reihe = Reihe)
Plot_BV4.1 <- ggplot(ts_data, aes(x = date)) +
  geom_line(data = Frame, aes(y = Reihe, color = "Zeitreihe"), lwd = 0.7, alpha = 0.5) +
  geom_line(aes(y = trend, color = "Trend-Konjunktur"), lwd = 1.4, alpha = 0.5) +
  geom_line(data = Frame, aes(y = Trend_konjunktur, color = "TK geschätzt"), lwd = 0.7, alpha = 0.8) +
  ggtitle("Trend-Konjunktur-Komponente mit BV4.1") +
  xlab("Zeit") + ylab("Wert der Zeitreihe") +
  scale_color_manual(values = c("Zeitreihe" = "green3", "Trend-Konjunktur" = "blue", "TK geschätzt" = "red")) +
  theme_minimal() +
  labs(subtitle = aste0("MSE: ", round(MSE,2), ";  MSE letzte 3 Beobachtungen: ",
                        round(MSE_lastX,2), ";  MSE mit Shift um ",Shift,": ",
                        round(MSE_shift,2), ";  Seed: ", Seed), 
       color = "Legende",
       caption = Parameter)
Plot_BV4.1

# 7. Prediction mit JD - x13-RSA5c (nicht identisch mit x13-RSA5c aus RJDemetra)
x13.SA5c <- read.xlsx("RES6.xlsx")
Trend_konjunktur <-  x13.SA5c$Trend[1:200]
Frame <- data.frame(Trend_konjunktur = Trend_konjunktur, date = ts_data$date,
                    Reihe = Reihe)
Plot_x13.SA5c <- ggplot(ts_data, aes(x = date)) +
  geom_line(data = Frame, aes(y = Reihe, color = "Zeitreihe"), lwd = 0.7, alpha = 0.5) +
  geom_line(aes(y = trend, color = "Trend-Konjunktur"), lwd = 1.4, alpha = 0.5) +
  geom_line(data = Frame, aes(y = Trend_konjunktur, color = "TK geschätzt"), lwd = 0.7, alpha = 0.8) +
  ggtitle("Trend-Konjunktur-Komponente mit x13.SA5c") +
  xlab("Zeit") + ylab("Wert der Zeitreihe") +
  scale_color_manual(values = c("Zeitreihe" = "green3", "Trend-Konjunktur" = "blue", "TK geschätzt" = "red")) +
  theme_minimal() +
  labs(subtitle = paste0("MSE: ", round(MSE,2), ";  MSE letzte 3 Beobachtungen: ",
                        round(MSE_lastX,2), ";  MSE mit Shift um ",Shift,": ",
                        round(MSE_shift,2), ";  Seed: ", Seed), 
       color = "Legende",
       caption = Parameter)
Plot_x13.SA5c





