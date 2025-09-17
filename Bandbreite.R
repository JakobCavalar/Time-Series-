# Gleitende Mittel berechnen (na.rm = TRUE, damit an Rändern NA stehen bleiben)
ts_data$ma_3  <- rollmean(ts_data$X_t, k = 5, fill = NA, align = "center")
ts_data$ma_13 <- rollmean(ts_data$X_t, k = 13, fill = NA, align = "center")
ts_data$ma_30 <- rollmean(ts_data$X_t, k = 40, fill = NA, align = "center")

# Bereichsindikator bleibt, um später wieder die vertikale Trennlinie bei 170 zu setzen
ts_data$bereich <- ifelse(ts_data$time <= 170, "Modell", "Vorhersage")

# Plot
ggplot(ts_data, aes(x = date)) +
    # Originale Zeitreihe
    geom_line(aes(y = X_t, color = factor("Zeitreihe", levels = c("Zeitreihe", "Trend-Konjunktur", "Gleitendes Mittel (5)", "Gleitendes Mittel (13)", "Gleitendes Mittel (30)"))), alpha = 0.8, lwd = 0.9) +
    
    # Trend-Konjunktur-Komponente
    geom_line(aes(y = trend, color = "Trend-Konjunktur"), lwd = 0.9, alpha = 0.8) +
    
    # Gleitende Mittel
    geom_line(aes(y = ma_3, color = "Gleitendes Mittel (5)"), linetype = "solid", lwd = 0.7) +
    geom_line(aes(y = ma_13, color = "Gleitendes Mittel (13)"), linetype = "solid", lwd = 0.7) +
    geom_line(aes(y = ma_30, color = "Gleitendes Mittel (40)"), linetype = "solid", lwd = 0.7) +
    
    # Farben und Achsen
    scale_color_manual(values = c("Zeitreihe" = "black",
                                  "Trend-Konjunktur" = "blue",
                                  "Gleitendes Mittel (5)" = "red3",
                                  "Gleitendes Mittel (13)" = "green4",
                                  "Gleitendes Mittel (40)" = "purple")) +
    theme_minimal() +
    ylab("Wert der Zeitreihe") +
    xlab("Datum") +
    ggtitle("Schätzung der Trend-Konjunktur-Komponente") +
    labs(subtitle = "Modellierung der TKK abhängig von der Bandbreite",
         color = "Legende") +
    coord_cartesian(ylim = c(0, 70), xlim = c(min(ts_data$date), ts_data$date[170]))
  
  
#-------------- mit Pred

# Gleitende Mittel auf Originaldaten
ts_data$ma_5  <- rollmean(ts_data$X_t, k = 5, fill = NA, align = "center")
ts_data$ma_13 <- rollmean(ts_data$X_t, k = 13, fill = NA, align = "center")
ts_data$ma_30 <- rollmean(ts_data$X_t, k = 30, fill = NA, align = "center")

# Lineares Modell bis Beobachtung 170
model_naiv <- lm(X_t ~ time, data = ts_data[1:170, ])

# Vorhersage für alle Zeitpunkte
ts_data$pred_naiv <- predict(model_naiv, newdata = ts_data)

# Kombinierte Zeitreihe: Original bis 170, Vorhersage ab 171
ts_data$X_t_kombiniert <- ifelse(ts_data$time <= 170, ts_data$X_t, ts_data$pred_naiv)

# Gleitende Mittel auf kombinierten Werten
ts_data$ma_5_p  <- rollmean(ts_data$X_t_kombiniert, k = 5, fill = NA, align = "center")
ts_data$ma_13_p <- rollmean(ts_data$X_t_kombiniert, k = 13, fill = NA, align = "center")
ts_data$ma_30_p <- rollmean(ts_data$X_t_kombiniert, k = 30, fill = NA, align = "center")

# Bereichsindikator
ts_data$bereich <- ifelse(ts_data$time <= 170, "Modell", "Vorhersage")

# Plot
ggplot(ts_data, aes(x = date)) +
  # Originale Zeitreihe
  geom_line(aes(y = X_t, color = factor("Zeitreihe", levels = c("Zeitreihe", "Trend-Konjunktur", "Gleitendes Mittel (5)", "Gleitendes Mittel (13)", "Gleitendes Mittel (30)"))), alpha = 0.8, lwd = 0.8) +
  
  # Trend-Konjunktur-Komponente
  geom_line(aes(y = trend, color = "Trend-Konjunktur"), lwd = 0.8, alpha = 0.8) +
  
  # Gleitende Mittel im Modellbereich (durchgezogen)
  geom_line(data = subset(ts_data, bereich == "Modell"),
            aes(y = ma_5, color = "Gleitendes Mittel (5)"), lwd = 0.9) +
  geom_line(data = subset(ts_data, bereich == "Modell"),
            aes(y = ma_13, color = "Gleitendes Mittel (13)"), lwd = 0.9) +
  geom_line(data = subset(ts_data, bereich == "Modell"),
            aes(y = ma_30, color = "Gleitendes Mittel (30)"), lwd = 0.9) +
  
  # Gleitende Mittel im Vorhersagebereich (gestrichelt)
  geom_line(data = subset(ts_data, bereich == "Vorhersage"),
            aes(y = ma_5_p, color = "Gleitendes Mittel (5)"), linetype = "dashed", lwd = 0.9) +
  geom_line(data = subset(ts_data, bereich == "Vorhersage"),
            aes(y = ma_13_p, color = "Gleitendes Mittel (13)"), linetype = "dashed", lwd = 0.9) +
  geom_line(data = subset(ts_data, bereich == "Vorhersage"),
            aes(y = ma_30_p, color = "Gleitendes Mittel (30)"), linetype = "dashed", lwd = 0.9) +
  
  # Farben und Achsen
  scale_color_manual(values = c("Zeitreihe" = "black",
                                "Trend-Konjunktur" = "blue",
                                "Gleitendes Mittel (5)" = "red3",
                                "Gleitendes Mittel (13)" = "green4",
                                "Gleitendes Mittel (30)" = "purple")) +
  theme_minimal() +
  ylab("Wert der Zeitreihe") +
  xlab("Datum") +
  ggtitle("Schätzung und Prognose der Trend-Konjunktur-Komponente") +
  labs(subtitle = "Modellierung und Vorhersage der TKK mit gleitenden Mitteln abhängig von der Bandbreite",
       color = "Legende") +
  coord_cartesian(ylim = c(0, 70)) +
  geom_vline(xintercept = ts_data$date[170], linetype = "dotted", color = "grey")
