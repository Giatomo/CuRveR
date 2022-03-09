# Absolute Error
absolute_error <- function(values, fitted_values) {
  abs(values - fitted_values)
}

# Square Error
square_error <- function(values, fitted_values) {
  (values - fitted_values)^2
}

# Mean Absolute Error
mae <- function(values, fitted_values) {
  mean(absolute_error(values, fitted_values))
}

# Mean Square Error
mse <- function(values, fitted_values) {
  mean(square_error(values, fitted_values))
}

# Mean Square Reduced Error
msre <- function(values, fitted_values) {
  mse(values, fitted_values) / (sd(values)^2)
}

# Root Mean Square Error
rmsq <- function(values, fitted_values) {
  sqrt(mse(values, fitted_values))
}

# Relative Root Mean Square Error
rrmsq <- function(values, fitted_values) {
  100 * rmsq(values, fitted_values) / mean(values)
}

# Relative Mean Absolute Error
rmae <- function(values, fitted_values) {
  100 * mae(values, fitted_values) / mean(values)
}

# Standardised Root Mean Square Error
srmsq <- function(values, fitted_values) {
  rmsq(values, fitted_values) / sd(values)
}

mean_deviation <- function(values) {
  values - mean(values)
}

variance_explained <- function(values, fitted_values) {
  100 * (1 - (sum(square_error(values, fitted_values)) / sum(mean_deviation(values)^2)))
}


legate_mccabe <- function(values, fitted_values) {
  100 * (1 - (sum(absolute_error(values, fitted_values)) / sum(abs(mean_deviation(values)))))
}

willmott_index_agreement <- function(values, fitted_values) {
  e1 <- legate_mccabe(values, fitted_values)
  if (e1 < 0) {
    return(100 * ((sum(abs(mean_deviation(values))) / sum(absolute_error(values, fitted_values))) - 1))
  }
  else {
    return(e1)
  }
}

pearson_product_moment_r <- function(values, fitted_values) {
  sum(mean_deviation(values) * mean_deviation(fitted_values)) / sqrt(sum((mean_deviation(values)^2) * (mean_deviation(fitted_values)^2)))
}
