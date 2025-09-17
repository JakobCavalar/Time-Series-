# OLS-Modelle nur bis Beobachtung 170
model_deg1 <- lm(X_t ~ time, data = ts_data[1:170, ])
model_deg2 <- lm(X_t ~ poly(time, 3, raw = TRUE), data = ts_data[1:170, ])
model_deg3 <- lm(X_t ~ poly(time, 12, raw = TRUE), data = ts_data[1:170, ])

# Vorhersage über die ganze Zeitreihe
ts_data$fit_deg1 <- predict(model_deg1, newdata = ts_data)
ts_data$fit_deg2 <- predict(model_deg2, newdata = ts_data)
ts_data$fit_deg3 <- predict(model_deg3, newdata = ts_data)

# Bereichsindikator: bis 170 = Modell, ab 171 = Vorhersage
ts_data$bereich <- ifelse(ts_data$time <= 170, "Modell", "Vorhersage")

# Plot
ggplot(ts_data, aes(x = date)) +
  # Originale Zeitreihe
  geom_line(aes(y = X_t, color = factor("Zeitreihe", levels = c("Zeitreihe", "Trend-Konjunktur", "Polynomgrad (1)", "Polynomgrad (3)", "Polynomgrad (12)"))), alpha = 0.8, lwd = 0.8) +
  
  # Trend-Konjunktur-Komponente
  geom_line(aes(y = trend, color = "Trend-Konjunktur"), lwd = 0.8, alpha = 0.8) +
  
  # OLS-Schätzungen im Modellbereich (bis 170)
  geom_line(data = subset(ts_data, bereich == "Modell"),
            aes(y = fit_deg1, color = "Polynomgrad (1)"), linetype = "solid", lwd = 0.9) +
  geom_line(data = subset(ts_data, bereich == "Modell"),
            aes(y = fit_deg2, color = "Polynomgrad (3)"), linetype = "solid", lwd = 0.9) +
  geom_line(data = subset(ts_data, bereich == "Modell"),
            aes(y = fit_deg3, color = "Polynomgrad (12)"), linetype = "solid", lwd = 0.9) +
  
  # Vorhersagebereich ab 171 (gestrichelt)
  geom_line(data = subset(ts_data, bereich == "Vorhersage"),
            aes(y = fit_deg1, color = "Polynomgrad (1)"), linetype = "dashed", lwd = 0.9) +
  geom_line(data = subset(ts_data, bereich == "Vorhersage"),
            aes(y = fit_deg2, color = "Polynomgrad (3)"), linetype = "dashed", lwd = 0.9) +
  geom_line(data = subset(ts_data, bereich == "Vorhersage"),
            aes(y = fit_deg3, color = "Polynomgrad (12)"), linetype = "dashed", lwd = 0.9) +
  
  # Farben und Achsen
  scale_color_manual(values = c("Zeitreihe" = "black",
                                "Trend-Konjunktur" = "blue",
                                "Polynomgrad (1)" = "red3",
                                "Polynomgrad (3)" = "green4",
                                "Polynomgrad (12)" = "purple")) +
  theme_minimal() +
  ylab("Wert der Zeitreihe") +
  xlab("Datum") +
  ggtitle("Schätzung und Prognose der Trend-Konjunktur-Komponente") +
  labs(subtitle = "Modellierung und Vorhersage der TKK mit OLS abhängig vom Polynomgrad",
       color = "Legende") +
  coord_cartesian(ylim = c(0, 70)) +
  geom_vline(xintercept = ts_data$date[170], linetype = "dotted", color = "grey")
