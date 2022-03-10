#' @export
least_absolute_deviation <- function(model) {
  \(parameters) {
    -sum(abs(model$y - do.call(model$equation, append(list(model$x), parameters))))
  }
}

#' @export
ordinary_least_squares <- function(model) {
  \(parameters) {
    -sum((model$y - do.call(model$equation, append(list(model$x), parameters)))^2)
  }
}

#' @export
tukey_bisquare <- function(model, scaling_method = "aad", c = 9) {
  \(parameters) {
    scaling <- switch(scaling_method,
      "aad" = mean(abs(model$y - mean(model$y))),
      "mad" = median(abs(model$y - mean(model$y))),
      "sd"  = sd(model$y)
    )

    r <- model$y - do.call(model$equation, append(list(model$x), parameters))
    z <- r / scaling
    p_z <- ((c^6) - (((c^2) - (z^2))^3)) / 6
    p_z[abs(z) >= c] <- 0
    return(-sum(p_z))
  }
}
#' @export
huber <- function(model, scaling_method = "aad", c = 9) {
  \(parameters) {
    scaling <- switch(scaling_method,
      "aad" = mean(abs(model$y - mean(model$y))),
      "mad" = median(abs(model$y - mean(model$y))),
      "sd"  = sd(model$y)
    )

    r <- model$y - do.call(model$equation, append(list(model$x), parameters))
    z <- r / scaling
    p_z <- (z^2) / 2
    p_z[abs(z) >= c] <- ((c * abs(z)) - ((c^2) / 2))[abs(z) >= c]
    return(-sum(p_z))
  }
}

#' @export
andrew <- function(model, scaling_method = "aad") {
  \(parameters) {
    scaling <- switch(scaling_method,
      "aad" = mean(abs(model$y - mean(model$y))),
      "mad" = median(abs(model$y - mean(model$y))),
      "sd"  = sd(model$y)
    )

    r <- model$y - do.call(model$equation, append(list(model$x), parameters))
    z <- r / scaling
    p_z <- 1 - cos(z)
    p_z[abs(z) > pi] <- 0
    return(-sum(p_z))
  }
}
#' @export
jaeckel <- function(model) {
  \(parameters) {
    r <- abs(model$y - do.call(model$equation, append(list(model$x), parameters)))
    r <- sort(r)
    n <- length(r)
    ranks <- 1:n

    return(-sum(abs(ranks - ((n + 1) / 2)) * r))
  }
}
