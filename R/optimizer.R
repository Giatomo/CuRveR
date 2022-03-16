#' @export
DeOptimizer <- R6::R6Class("DeOptimizer", list(
  fitness_fun = NULL,
  initialize = function(fitness_fun) {
    self$fitness_fun <- fitness_fun
  },
  optimize = function(model) {
    fit_fun <- self$fitness_fun(model)
    print(as.numeric(model$lower))
    print(as.numeric(model$upper))

    fit <- GA::de(
      type = "real-valued",
      fitness = fit_fun,
      lower = as.numeric(model$lower),
      upper = as.numeric(model$upper),
      names = model$parameters,
      popSize = 100,
      monitor = FALSE,
      optim = FALSE,
      maxiter = 100
    )

    solution <- fit@solution[1, ]
    names(solution) <- model$parameters
    return(solution)
  }
))

#' @export
NelderMeadOptimizer <- R6::R6Class("NelderMeadOptimizer", list(
  fitness_fun = NULL,
  initialize = function(fitness_fun) {
    self$fitness_fun <- fitness_fun
  },
  optimize = function(model) {
    fit_fun <- self$fitness_fun(model)

    fit <- optim(as.numeric(model$start),
      method = "Nelder-Mead"
    )

    fit <- GA::de(
      type = "real-valued",
      fitness = fit_fun,
      lower = as.numeric(model$lower),
      upper = as.numeric(model$upper),
      names = model$parameters,
      popSize = 100,
      monitor = FALSE,
      optim = FALSE,
      maxiter = 100
    )

    solution <- fit@solution[1, ]
    names(solution) <- model$parameters
    return(solution)
  }
))

