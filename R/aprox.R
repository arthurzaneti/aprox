#'Numerical approximation of defined integrals.
#'
#'A function for calculating approximations of defined integrals with various
#'different methods. The methods available are explained in the details section.
#'
#'@param func The function that is gonna be integrated. The function needs to
#'return a single number as input and receive a single number as input.
#'@param n_points An integer for the number of points that are gonna be used
#'for the  approximation. More points will generate a better approximation but
#'will be heavier computationally
#'@param interval A length 2 numeric vector that is the interval to integrate
#'
#'@param method The method used for approximation, the available methods are described in the details section
#'
#'@return A Numeric. The area bellow the function func in the interval provided.

#'@details
#' The integration methods available are:
#'    \itemize{
#'            \item "m" : Midpoint method
#'            \item "l" : Left endpoint method
#'            \item "r" : Right endpoint method
#'            \item "t" : Trapezoidal method
#'            \item "s" : Simpsons method
#'            }
#'@examples

#'aprox(function(x) x^2, n_points = 100, interval = c(0,1))
#'aprox(function(x) sqrt(x^x + 2), n_points = 100, interval = c(sqrt(2), pi), method = "m")
#'aprox(function(x) exp(x^2), n_points = 1000, interval = c(2,4), method = "t" )
#'aprox(function(x) exp(x^2), n_points = 1000, interval = c(4,2))

#'@export

#_______________________________________________________________________________


aprox<- function(func, n_points, interval, method = "s"){
  check(func, n_points, interval)
  if(method == "s" && n_points %% 2 !=0){
    warning("The number of points needs to be even for the Simpson method,
              the result was calculated using", n_points - 1, " points instead
            of", n_points," points")
    n_points <- n_points - 1
  }

  x <- seq(from = interval[1], to = interval[2], length.out = n_points)
  delta_x <- (interval[2] - interval[1])/n_points
  y_sum <- 0

  if(method == "m"){
    for (i in 1:(n_points-1)){
      y_sum <- y_sum + func((x[i] + x[i+1])/2)
    }
    return(delta_x * y_sum)
  }

  if(method == "r"){
    for (i in 2:n_points){
      y_sum <- y_sum + func(x[i])
    }
    return(delta_x * y_sum)
  }

  if(method == "l"){
    for (i in 1:(n_points-1)){
      y_sum <- y_sum + func(x[i])
    }
    return(delta_x * y_sum)
  }

  if(method == "t"){
    for (i in 1:(n_points-1)){
      y_sum <- y_sum + (func(x[i]) + func(x[i+1]))/2
    }
    return(delta_x * y_sum)
  }

  if(method == "s"){
    for (i in 1:n_points){
      if(i == 1 || i == n_points){
        y_sum <- y_sum + func(x[i])/3

      }else if(i %% 2 == 0){
        y_sum <- y_sum + 2 * func(x[i])/3

      }else {
        y_sum <- y_sum + 4 * func(x[i])/3
      }
    }
    return(delta_x * y_sum)
  }
  return()
}

#_______________________________________________________________________________


check <- function(func, n_points, interval){
  if(!inherits(func, "function")){
    print("The value as the first argument is not a function")
  }else if(!inherits(n_points, "numeric")){
    print("The value sent for the number of points is not a number")
  }else if(length(n_points) != 1){
    print("The value sent for the nubmer of points is a vector, it should be an
          integer")
  }else if(n_points <= 0){
    print("The value sent for the number of points needs to be a positive
          integer")
  }else if(!inherits(interval,"numeric")){
    print("The value sent for the interval is not a pair of numbers")
  }else{
    return (T)
  }
  return (F)
}


