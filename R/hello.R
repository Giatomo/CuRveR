#' @importFrom stats lm median rnorm sd
NULL



#' @export
fit_richard <- function(y, t, method = "LAD", model = "richard") {
  lin <- lm(y ~ t)
  r_estimate <- lin$coefficients[2]
  sign <- sign(r_estimate)
  max_y <- max(y)
  min_y <- min(y)


  s_boundary <- max(as.numeric(t))

  estimates <- list()
  estimates$p_max <- rnorm(100, mean = max_y, sd = 0.01 * (max_y - min_y))
  estimates$p_min <- rnorm(100, mean = min_y, sd = 0.01 * (max_y - min_y))

  estimates$r_max <- sign * 10^rnorm(100, mean = log(abs(r_estimate), base = 10), sd = 1)
  estimates$s <- rnorm(100, mean = max(t) / 2, sd = 0.05 * max(t))


  suggestions <- cbind(estimates$p_max, estimates$p_min, estimates$r_max, estimates$s)

  model_fun <- switch(model,
    "richard" = richard,
    "linear"  = linear,
  )

  lower <- switch(model,
    "richard" = c(
      p_max = max_y - 0.05 * (max_y - min_y),
      p_min = 0,
      r_max = min(0, r_estimate * 100),
      s = 0
    ),
    "linear" = c(
      a = 0,
      b = 0
    ),
  )

  upper <- switch(model,
    "richard" = c(
      p_max = max_y + 0.05 * (max_y - min_y),
      p_min = min_y + 0.05 * (max_y - min_y),
      r_max = max(0, r_estimate * 100),
      s = s_boundary
    )
  )

  fitness_fun <- switch(method,
    "LAD" = \(p) -sum(abs(y - do.call(model, append(list(t), p)))),
    "OLS" = \(p) -sum((y - do.call(model, append(list(t), p)))^2),
  )


  fit <- GA::de(
    type = "real-valued",
    fitness = fitness_fun,
    suggestions = suggestions,
    lower = lower,
    upper = c(
      p_max = max_y + 0.05 * (max_y - min_y),
      p_min = min_y + 0.05 * (max_y - min_y),
      r_max = max(0, r_estimate * 100),
      s     = s_boundary
    ),
    names = c(
      "p_max",
      "p_min",
      "r_max",
      "s"
    ),
    popSize = 100,
    monitor = FALSE,
    optim = FALSE,
    maxiter = 100
  )


  return(fit@solution[1, ])

  # })
}
