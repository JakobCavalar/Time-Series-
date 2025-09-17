# ------------ Funktionen ----------------------
fourier_lm <- function(data, value_col, time_col, n_freq = 4) {
  # Spektrum berechnen
  SPECS <- spec.pgram(ts_data[[value_col]], log = "no", plot = FALSE)
  
  # Frequenzen und Spektraldichten sortieren (ohne DC-Anteil bei Frequenz 0)
  freq_spec <- data.frame(freq = SPECS$freq, spec = SPECS$spec)
  freq_spec <- freq_spec[freq_spec$freq <= 0.03, ]
  freq_spec <- freq_spec[order(-freq_spec$spec), ]
  
  # Die n höchsten Frequenzen extrahieren
  top_freqs <- head(freq_spec$freq, n_freq)
  
  # Designmatrix mit Fourier-Komponenten erzeugen
  X_list <- lapply(top_freqs, function(f) {
    cbind(
      sin(2 * pi * f * ts_data[[time_col]]),
      cos(2 * pi * f * ts_data[[time_col]])
    )
  })
  
  # Zu einer Matrix zusammenfügen
  X_mat <- do.call(cbind, X_list)
  colnames(X_mat) <- unlist(lapply(1:length(top_freqs), function(i) {
    c(paste0("sin_", i), paste0("cos_", i))
  }))
  # Formel für lm aufstellen
  df <- data.frame(y = ts_data[[value_col]], X_mat)
  colnames(df) <- c("y", paste0("V", 1:ncol(X_mat)))
  formel <- as.formula(paste("y ~ ", paste(colnames(df)[-1], collapse = " + ")))
  
  # Lineares Modell schätzen
  fit <- lm(formel, data = df)
  
  # Rückgabe: Fit, Top-Frequenzen, Designmatrix
  list(
    fit = fit,
    top_freqs = top_freqs,
    design_matrix = X_mat
  )
}

predict_fourier_lm <- function(model, newdata, time_col) {
  # Designmatrix aus den gespeicherten Frequenzen nachbauen
  X_list <- lapply(model$top_freqs, function(f) {
    cbind(
      sin(2 * pi * f * newdata[[time_col]]),
      cos(2 * pi * f * newdata[[time_col]])
    )
  })
  
  X_mat <- do.call(cbind, X_list)
  colnames(X_mat) <- model$X_names
  
  # Vorhersage berechnen
  y_hat <- cbind(1, X_mat) %*% coef(model$fit)
  as.numeric(y_hat)
}

# ------ Plot --------------------------------

# LOESS-Modelle nur bis Beobachtung 170
model_f1 <- fourier_lm(ts_data[1:170], "X_t", "time", 1)
model_f3 <- fourier_lm(ts_data[1:170], "X_t", "time", 3)
model_f10 <- fourier_lm(ts_data[1:170], "X_t", "time", 7)

# Vorhersage über die ganze Zeitreihe
ts_data$fit_1 <- predict_fourier_lm(model_f1, newdata = ts_data, "time")
ts_data$fit_3 <- predict_fourier_lm(model_f3, newdata = ts_data, "time")
ts_data$fit_10 <- predict_fourier_lm(model_f10, newdata = ts_data, "time")

# Bereichsindikator: bis 170 = Modell, ab 171 = Vorhersage
ts_data$bereich <- ifelse(ts_data$time <= 170, "Modell", "Vorhersage")

# Farben und Levels definieren
farben <- c("Zeitreihe" = "black",
            "Trend-Konjunktur" = "blue",
            "Harmonische (1)" = "red3",
            "Harmonische (2)" = "green4",
            "Harmonische (4)" = "purple")

levels_legende <- c("Zeitreihe", "Trend-Konjunktur", "Harmonische (1)", "Harmonische (2)", "Harmonische (4)")

# Plot
ggplot(ts_data, aes(x = date)) +
  # Originale Zeitreihe
  geom_line(aes(y = X_t, color = factor("Zeitreihe", levels = levels_legende)), alpha = 0.8, lwd = 0.8) +
  
  # Trend-Konjunktur-Komponente
  geom_line(aes(y = trend, color = "Trend-Konjunktur"), lwd = 0.8, alpha = 0.8) +
  
  # Fourier-Schätzungen im Modellbereich (bis 170) - durchgezogen
  geom_line(data = subset(ts_data, bereich == "Modell"),
            aes(y = fit_1, color = "Harmonische (1)"), linetype = "solid", lwd = 0.9) +
  geom_line(data = subset(ts_data, bereich == "Modell"),
            aes(y = fit_3, color = "Harmonische (2)"), linetype = "solid", lwd = 0.9) +
  geom_line(data = subset(ts_data, bereich == "Modell"),
            aes(y = fit_10, color = "Harmonische (4)"), linetype = "solid", lwd = 0.9) +
  
  # Fourier-Vorhersagen ab 171 - gestrichelt
  geom_line(data = subset(ts_data, bereich == "Vorhersage"),
            aes(y = fit_1, color = "Harmonische (1)"), linetype = "dashed", lwd = 0.9) +
  geom_line(data = subset(ts_data, bereich == "Vorhersage"),
            aes(y = fit_3, color = "Harmonische (2)"), linetype = "dashed", lwd = 0.9) +
  geom_line(data = subset(ts_data, bereich == "Vorhersage"),
            aes(y = fit_10, color = "Harmonische (4)"), linetype = "dashed", lwd = 0.9) +
  
  # Farben, Achsen und Legende
  scale_color_manual(values = farben) +
  theme_minimal() +
  ylab("Wert der Zeitreihe") +
  xlab("Datum") +
  ggtitle("Schätzung und Prognose der Trend-Konjunktur-Komponente") +
  labs(subtitle = "Modellierung und Vorhersage der TKK mit Signalextraktion abhängig von der Anzahl an Frequenzen",
       color = "Legende") +
  coord_cartesian(ylim = c(0, 70)) +
  geom_vline(xintercept = ts_data$date[170], linetype = "dotted", color = "grey")



