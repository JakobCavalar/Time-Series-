simulate_time_series_umsatz <- function(n = 200, 
                                 trend_slope = 0.18, trend_curvature = 1.01,
                                 cycle_period = 60, cycle_amplitude = 2, 
                                 season_period = 4, season_amplitude = 3,
                                 year_end_spike_amplitude = 17,
                                 noise_sd = 1.3, trend_curvature2 = 1.01, 
                                 outlier_prob = 0.01, outlier_magnitude = 8,
                                 shift_prob = 0.006, shift_magnitude = 5,
                                 seed = 1) 
{
  set.seed(seed)
  time <- 1:n
  date <- seq.Date(from = as.Date("2020-01-01"), length.out = n, by = "month")

  # 1. Stetig steigende Trend-Konjunktur-Komponente
  trend <- 120 + trend_slope * time^trend_curvature + (0.1 * (time^4)^trend_curvature2) +
    1/1.5 * (cycle_amplitude * sin(2 * pi * time / cycle_period))
  
#  # 1. Stückweise Trend-Konjunktur-Komponente mit Trendbruch ab 2024
#  trend <- numeric(n)
#  breakpoint <- which(date == as.Date("2024-01-01"))[1]
  
#  # Vor 2024: flacher Anstieg
#  trend[1:(breakpoint - 1)] <- 120 + trend_slope * time[1:(breakpoint - 1)]^trend_curvature + 
#    1/1.5 * (cycle_amplitude * sin(2 * pi * time[1:(breakpoint - 1)] / cycle_period))
  
#  # Ab 2024: steiler Anstieg (mit z.B. stärkerer Steigung und anderer Krümmung)
#  trend_slope_2 <- 0.6
#  trend_curvature_2 <- 1.2
#  trend[breakpoint:n] <- trend[breakpoint - 1] + 
#    trend_slope_2 * (time[breakpoint:n] - time[breakpoint])^trend_curvature_2 + 
#    1/1.2 * (cycle_amplitude * sin(2 * pi * time[breakpoint:n] / cycle_period))
  
  # 2. Saisonale Komponente (3 Peaks pro Jahr, einer davon besonders am Jahresende)
  month_in_year <- (time - 1) %% 12 + 1
  seasonality <- numeric(n)
  for (i in 1:n) {
    m <- month_in_year[i]
    if (m %in% 1:3) {
      seasonality[i] <- season_amplitude * sin(pi * (m - 1) / 4)
    } else if (m %in% 4:6) {
      seasonality[i] <- season_amplitude * sin(pi * (m - 4) / 3)
    } else if (m %in% 7:12) {
      seasonality[i] <- season_amplitude * sin(pi * (m - 7) / 5)
    }
  }
  
  
  # 2a. Jahresend-Spike (z.B. Dezember jedes Jahr = jeder 12. Monat)
  sep_spike <- ifelse(month_in_year == 10, (1/10)*year_end_spike_amplitude, 0)
  oct_spike <- ifelse(month_in_year == 10, (1/2.2)*year_end_spike_amplitude, 0)
  nov_spike <- ifelse(month_in_year == 11, (1/1.4)*year_end_spike_amplitude, 0)
  dez_spike <- ifelse(month_in_year == 12, year_end_spike_amplitude, 0)
  jan_spike <- ifelse(month_in_year == 1, -(1/5)*year_end_spike_amplitude, 0)
  feb_spike <- ifelse(month_in_year == 2, -(1/5)*year_end_spike_amplitude, 0)
  year_end_spike <- sep_spike + oct_spike + nov_spike + dez_spike + jan_spike + feb_spike
  
  # 3. Stochastische Komponente
  noise <- rnorm(n, mean = 0, sd = noise_sd)
  
  # 4. Ausreißer
  outliers <- rbinom(n, 1, outlier_prob) * runif(n, -outlier_magnitude, outlier_magnitude)
  outliers[abs(outliers) < 5] <- 0
  
  for (i in 1:(n - 7)) {
    if (outliers[i] > 5 | outliers[i] < -5){
      outliers[i+1] = (1/1.2) * outliers[i]
      outliers[i+2] = (1/1.3) * outliers[i+1]
      outliers[i+3] = (1/1.4) * outliers[i+2]
      outliers[i+4] = (1/2) * outliers[i+3]
      outliers[i+5] = (1/3) * outliers[i+4]
      outliers[i+6] = (1/4) * outliers[i+5]
      outliers[i+7] = (1/5) * outliers[i+6]
    }
  }
  outliers <- outliers[1:n]
  
  outliers_indicator <- numeric(n)
  for (i in 2:n){
    outliers_indicator[i] <- as.numeric(abs(outliers[i]) > abs(outliers[i-1]))
  }
  
  # 5. Permanente Shifts
  shifts <- rep(0, n)
  if (shift_prob > 0) {
    shift_events <- rbinom(n, 1, shift_prob) * runif(n, -shift_magnitude, shift_magnitude)
    shift_events[2] <- -7 
    for (i in 2:n) {
      shifts[i] <- shifts[i-1] + shift_events[i]
    }
  }
  shift_indicator <- c(0, as.numeric(diff(shifts) != 0))
  
  # 6. Zusammensetzen der Zeitreihe
  X_t <- trend + seasonality + year_end_spike + noise + outliers + shifts
  
  # 7. Rückgabe als Data Frame
  data <- data.frame(
    time = time,
    date = date,
    X_t = X_t,
    trend = trend,
    seasonality = seasonality + year_end_spike,
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
ts_data <- simulate_time_series_umsatz( n = 61, 
                                        trend_slope = 0.1, trend_curvature = 0.3,
                                        cycle_period = 60, cycle_amplitude = 2, 
                                        season_period = 4, season_amplitude = 9,
                                        year_end_spike_amplitude = 45,
                                        noise_sd = 3, trend_curvature2 = 0.335,
                                        outlier_prob = 0.0001, outlier_magnitude = 5,
                                        shift_prob = 0.0006, shift_magnitude = 5,
                                        seed = 1)

ggplot(ts_data, aes(x = date, y = X_t)) +
  geom_line(color = "blue") +
  geom_line(aes(y = trend), color = "red4") +
  geom_point(data = ts_data[ts_data$outliers_ind != 0,], aes(x = date, y = X_t), color = "red", size = 3) +
  geom_point(data = ts_data[ts_data$shift_ind != 0,], aes(x = date, y = X_t), color = "green", size = 3) +
  ggtitle("Simulierte Zeitreihe mit Trend-Konjunktur, Saison, Kalendereffekten und Ausreißern") +
  xlab("Zeit") + ylab("X_t") +
  theme_minimal() 

