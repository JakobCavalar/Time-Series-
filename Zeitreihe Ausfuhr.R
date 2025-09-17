simulate_time_series_ausfuhr <- function(n = 140, 
                                 trend_slope = 0.2, trend_amplitude = 15,
                                 cycle_period = 80, cycle_amplitude = 6, 
                                 season_period = 4, season_amplitude = 8, 
                                 noise_sd = 2.4, 
                                 outlier_prob = 0.025, outlier_magnitude = 18,
                                 shift_prob = 0.01,   # Wahrscheinlichkeit eines permanenten Shifts
                                 shift_magnitude = 8, # Stärke des permanenten Shifts
                                 seed = 1) 
{
  set.seed(seed)
  time <- 1:n
  date <- seq.Date(from = as.Date("2017-07-01"), length.out = n, by = "month")
  
  # 1. Trend-Konjunktur-Komponente (leicht gekrümmt)
  trend <- 13000 + trend_slope * time^1.03 + trend_amplitude * sin(2 * pi * time / cycle_period)
  
  # 2. Saisonale Komponente (~3 Zyklen pro Jahr)
  seasonality <- season_amplitude * sin(2 * pi * time / season_period)
  
  # 3. Stochastische Komponente
  noise <- rnorm(n, mean = 0, sd = noise_sd)
  
  # 4. Ausreißer
  outliers <- rbinom(n, 1, outlier_prob) * runif(n, -outlier_magnitude, (1/3)*outlier_magnitude)
  outliers[abs(outliers) < 4] <- 0
  outliers[34] <- -4000 
  
  for (i in 1:(n - 3)) {
    if (outliers[i] > 3800 | outliers[i] < -3800){
      outliers[i+1] = (1/1.4) * outliers[i]
      outliers[i+2] = (1/10) * outliers[i+1]
      outliers[i+3] = (1/10) * outliers[i+2]
    }
  }
  outliers <- outliers[1:n]
  
  outliers_indicator <- numeric(n)
  for (i in 2:n){
    outliers_indicator[i] <- as.numeric(abs(outliers[i]) > abs(outliers[i-1]))
  }
  
  # 5. Permanent Shift
  shifts <- numeric(n)
    if (shift_prob > 0) {
      shift_events <- rbinom(n, 1, shift_prob) * runif(n, -shift_magnitude, shift_magnitude)
      shift_events[47] <- -600
      shift_events[72] <- 1300
      shift_events[82] <- 200
      for (i in 2:n) {
        shifts[i] <- shifts[i-1] + shift_events[i]  # Kumulativer Shift
      }
    }
  shift_indicator <- c(0, as.numeric(diff(shifts) != 0))
  
  # 6. Zusammensetzen der Zeitreihe
  X_t <- trend + seasonality + noise + outliers + shifts
  
  # 7. Data Frame
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

ts_data <- simulate_time_series_ausfuhr(n = 97, 
                                        trend_slope = 60, trend_amplitude = 2300,
                                        cycle_period = 55, cycle_amplitude = 400, 
                                        season_period = 4, season_amplitude = 1400, 
                                        noise_sd = 780, 
                                        outlier_prob = 0, outlier_magnitude = 1000,
                                        shift_prob = 0.000001, shift_magnitude = 600,
                                        seed = 4)

ggplot(ts_data, aes(x = date, y = X_t)) +
  geom_line(color = "blue") +
  geom_line(aes(y = trend), color = "red4") +
  geom_point(data = ts_data[ts_data$outliers_ind != 0,], aes(x = date, y = X_t), color = "red", size = 3) +
  geom_point(data = ts_data[ts_data$shift_ind != 0,], aes(x = date, y = X_t), color = "green", size = 3) +
  ggtitle("Simulierte Zeitreihe mit Trend-Konjunktur, Saison, Kalendereffekten und Ausreißern") +
  xlab("Zeit") + ylab("X_t") +
  theme_minimal() +
  coord_cartesian(ylim=c(9000,22000))

ts_data <- ts_data[19:91,]

ggplot(ts_data, aes(x = date, y = X_t)) +
  geom_line(color = "blue") +
  geom_line(aes(y = trend), color = "red4") +
  geom_point(data = ts_data[ts_data$outliers_ind != 0,], aes(x = date, y = X_t), color = "red", size = 3) +
  geom_point(data = ts_data[ts_data$shift_ind != 0,], aes(x = date, y = X_t), color = "green", size = 3) +
  ggtitle("Simulierte Ausfuhr mit Trend-Konjunktur und Ausreißern") +
  xlab("Zeit") + ylab("X_t") +
  theme_minimal() +
  ylim(c(8000,23000))
