ModelABC <- R6::R6Class("ModelABC", list(

  # Data
  x = NULL,
  y = NULL,

  # Start
  start = NULL,

  # Constraints
  lower = NULL,
  upper = NULL,

  # Optimization & Predictions
  estimated = NULL,

  # Optimize model parameter using selected optimizer
  optimize = function(optimizer) {
    stopifnot(is.environment(optimizer))
    self$estimated <- optimizer$optimize(self)
    names(self$estimated) <- self$parameter
  },
  predict = function(x = self$x) {
    return(do.call(self$equation, append(list(x), self$estimated)))
  }
))


#' Richard equation
#'
#' Return the population at time t
#'
#' @export
#' @param t     Float : Time
#' @param p_max Float : Population Maximum (Upper Asymptote)
#' @param p_min Float : Population Minimum (Lower Asymptote)
#' @param r_min Float : Maximum Growth/Death rate (positive for growth|negative fo death)
#' @param s     Float : Shift (Time at which r_max occurs)
#' @return The population at time \code{t} Given by \deqn{P(t) = p_{min} + \frac{p_{max}-p_{min}}{1 + e^{4r_{max}.(t-s)/p_{min}- p_{max}}}}
#'
RichardModel <- R6::R6Class("RichardModel",
  inherit = ModelABC,
  list(

    # Equation
    equation = function(x, p_max, p_min, r_max, s) {
      p_min + (p_max - p_min) / (1 + exp(4 * r_max * (x - s) / (p_min - p_max)))
    },
    parameter = c("p_max", "p_min", "r_max", "s"),

    # INIT
    initialize = function(x, y, min_bound_perc = 0.10, max_bound_perc = 0.10, r_bound_tresh = 100, n_values_for_estimate = 1) {
      if (is_tibble(x) && dim(x)[2] == 1) {
        x <- x[[1]]
      }
      if (is_tibble(y) && dim(y)[2] == 1) {
        y <- y[[1]]
      }


      if (all(sapply(x, is.numeric))) {
        x <- as.numeric(x)
      }
      if (all(sapply(y, is.numeric))) {
        y <- as.numeric(y)
      }

      # Check input
      stopifnot(is.numeric(x))
      stopifnot(is.numeric(y))

      self$x <- x
      self$y <- y


      tibble(x = x, y = y) |> group_by(y) |> summarise(x = median(x)) -> data
      num_diff <- finite_diff_5pt_cent(data$x, data$y)
      
      print(order(self$y, decreasing=TRUE)[1:n_values_for_estimate])
      print(order(self$y, decreasing=FALSE)[1:n_values_for_estimate])
      max_est = mean(order(self$y, decreasing=TRUE)[1:n_values_for_estimate])
      min_est = mean(order(self$y, decreasing=FALSE)[1:n_values_for_estimate])


      # Estimate starting value for the optimizer
      self$start <- list(
        p_max = max_est,
        p_min = min_est,
        r_max = num_diff[which.max(abs(num_diff))],
        s     = x[which.max(abs(finite_diff_5pt_cent(x, y)))]
      )

      difference <- self$start$p_max - self$start$p_min

      # Estimate boundaries
      self$lower <- list(
        p_max = self$start$p_max - difference * max_bound_perc,
        p_min = self$start$p_min - difference * min_bound_perc,
        r_max = min(0, self$start$r_max * r_bound_tresh),
        s     = min(x)
      )

      self$upper <- list(
        p_max = self$start$p_max + difference * max_bound_perc,
        p_min = self$start$p_min + difference * min_bound_perc,
        r_max = max(0, self$start$r_max * r_bound_tresh),
        s     = max(x)
      )

    },
    predict = function(x = self$x) {
    return(do.call(self$equation, append(list(x), self$estimated)))
  }
  )
)
