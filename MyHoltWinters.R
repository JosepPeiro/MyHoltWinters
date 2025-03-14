# Author: Josep Peiró Ramos
# Student at Universitat de València

# Alternative algorithm for HoltWinters, with a equivalent but not
# equal behaviour as the base function from stats R package.

# HoltWinters algorithms is a function for forecasting in a single
# time serie.
# It has 3 parameters, alpha, beta and gamma, wich describe
# respectively the dependency with the previous observed value,
# the dependence with the tendency and the dependency of the observed
# periodicity

# We should take into account that for this algorithm we need to ignore
# the values of the whole first period of the training data for
# allowing it to warm up, so we won't have an estimation for those
# firts values

# In this algorithm you can set initial parameters, even though
# they will be used as initial point for the converging algorithm,
# that works for reaching an optimum combination which minimizes the
# error with respect the real data.
# The three parameters must be between 0 and 1. And are only used if
# optimum is set to FALSE.

MyHoltWinters <- function(x, alpha = TRUE, beta = TRUE, gamma = TRUE,
                          optimum = FALSE){
  # Main function
  # Receive values, calculate the coefficients for optimizing the error
  # and return an object (list), with all the components of this analysis
  
  # Inputs:
  ## x: Time serie -> Input data
  ## alpha: numeric or boolean -> Parameter related with previous value
  ## beta: numeric or boolean -> Parameter related with tendency
  ## gamma: numeric or boolean -> Parameter related with seasonality
  ## optimum: boolean -> If we are interested in global optimum or just local
  
  
  if (!is.ts(x)){ # Identify input data as time serie
    print("The input vector must be a time serie")
    return()
  }
  
  # If initial parameters are not given, initialize them to 0.5
  initial = rep(0.5,3)
  if (is.numeric(alpha)){
    initial[1] = alpha
  }
  if (is.numeric(beta)){
    initial[2] = beta
  }
  if (is.numeric(gamma)){
    initial[3] = gamma
  }
  
  p <- frequency(x) # Frecuency of the time serie
  
  if (optimum){
    # If optimum = TRUE, a deeper reseach for good parameters is done
    optimo <- optimizer(x)
  }else{
    # Else we start from a initial point and arrive to local optimum
    optimo <- optim(initial, fun.optim, data = c(x, p), method = "L-BFGS-B", 
                    lower = c(0.00, 0.00, 0.00), upper = c(1, 1, 1))
  }
  
  # We make an estimation of the serie based on the calculated parameters
  ajuste <- fun.ajuste(x, p, optimo$par[1], optimo$par[2], optimo$par[3])
  # Results shown
  plot(x) # Data
  lines(ts(c(rep(NA, 2 * p), ajuste$valfitted), # Adjust
           start = start(x), frequency = frequency(x)),
        type = "l", lty=2, col = "red") 
  
  names(optimo$par) <- c('alpha', 'beta', 'gamma')
  
  # We create an object with all the information and what we want to return
  solution <- list()
  solution$x <- x # Data
  solution$adjust <- ajuste$valfitted # Predicted values as a vector
  solution$fitted <- ajuste$fitted # Each term for the prediction as matrix
  solution$parameters <- optimo$par # Every parameter joined in a vector
  solution$aplha <- optimo$par[1] # Alpha
  solution$beta <- optimo$par[2] # Beta
  solution$gamma <- optimo$par[3] # Gamma
  solution$coefficients <- ajuste$coef # Coefficients for the prediction
  solution$SSE <- ajuste$SE # Error of the prediction during training
  
  return(solution)
}

  
fun.ajuste <- function(serie, freq, alpha, beta, gamma){
  # Function for adjusting a time serie based on certain parameters,
  # ad calculating how this new serie approximates to real values
  
  # Inputs:
  ## serie: Time serie -> Input data
  ## freq: integer -> Frequency of time serie
  ## alpha: numeric or boolean -> Parameter related with previous value
  ## beta: numeric or boolean -> Parameter related with tendency
  ## gamma: numeric or boolean -> Parameter related with seasonality

  
  n <- length(serie) # Length time serie
  valfitted <- c() # Storing predicted values
  
  # Based of the theoretic component that sustains this algorithm
  # Initialize components for HoltWinters formula
  L0 <- sum(serie[1:freq + freq]) / freq
  T0 <- (sum(serie[1:freq + freq]) - sum(serie[1:freq])) / (freq ^ 2)
  S0 <- (serie[1:freq + freq] - L0)
  
  fitted <- matrix(ncol = 4) # Storing components
  
  for (i in (2 * freq + 1):n){ # Period for warming up the algorithm
    
    S0ind <- i %% freq # Term in a season
    if (S0ind == 0){S0ind <- freq} # From 1 to freq (not 0 allowed)

    pred <- L0 + T0 + S0[S0ind] # Prediction based on components
    valfitted <- c(valfitted, pred) # Store prediction
    fitted <- rbind(fitted, c(pred, L0, T0, S0[S0ind])) # Store components
    
    # Update components
    L1 <- alpha * (serie[i] - S0[S0ind]) + (1 - alpha) * (L0 + T0)
    T1 <- beta * (L1 - L0) + (1 - beta) * T0
    S1 <- gamma * (serie[i] - L1) + (1 - gamma) * S0[S0ind]
    
    L0 <- L1
    T0 <- T1
    S0[S0ind] <- S1
  }
  
  # Add names and structure to fitted components
  colnames(fitted) <- c("Xhat", "Level", "Tendency", "Season")
  fitted <- ts(fitted[-1,], start = start(lag(serie, k = -2 * freq)), frequency = freq)
  
  SE <- sum((serie[(2 * freq + 1):n] - valfitted) ^ 2) # Calculate error
  coefficients <- c(L0, T0, S0) # Final coefficients
  names(coefficients) <- c('Level', 'Tendency', paste('Season', 1:freq))
  
  ajust <- list(valfitted = valfitted, SE = SE, coef = coefficients, fitted = fitted)
  return(ajust) # Return an object with everything
}


fun.optim <- function(vect, data){
  # Function used as measure of error for optimizing
  # Receives data and parameters and returns the error produced by them
  # Important: the structure is compatible with optim function
  # from stats R package
  
  # Inputs:
  ## vect: array -> Vector with 3 parameters: alpha, beta, gamma in that
  ##                order
  ## data: array -> Vector containing input data and the frequency in the
  ##                last position
  
  n <- length(data)
  aj <- fun.ajuste(data[1:(n - 1)], data[n], vect[1], vect[2], vect[3])
  return(aj$SE)
}


optimizer <- function(x){
  # Function used if we try to avoid local optimums at training or data
  # This functions still doesn't guarantee the global optimum but carry
  # out a deeper research for finding a fine tunning of the three parameter
  # It iterates between several combinations of parameters and improve
  # them in order to find the best result.
  
  # This functions comes from the necessity of a deeper reseach of good
  # parameters as we could observe how optim function from stats R package
  # tends very often to local and/or incorrect and/or not desidered
  # optimums.

  # Inputs:
  ## x: Time serie -> Input data
  
  
  p <- frequency(x) # Frequency of data
  best_value <- Inf # Initialize best error value
  best_par <- NULL # Initialize best combination of parameters
  for (a in seq(0.1, 0.9, by = 0.1)){ # Values for alpha
    for (b in seq(0.1, 0.9, by = 0.1)){ # Values for beta
      for (c in seq(0.1, 0.9, by = 0.1)){ # Values for gamma
        # Optimizate from every combination of parameters
        optimo <- optim(c(a,b,c), fun.optim, data = c(x, p), method = "L-BFGS-B", 
                        lower = c(0, 0, 0), upper = c(1, 1, 1)) 
        
        # If a new better solution is found, store it as the best
        if (optimo$value < best_value){
          best_value <- optimo$value
          best_par <- optimo$par
        }
      }
    }
  }
  best_solution <- list(value = best_value,
                        par = best_par)
  return(best_solution) # Store and return the best solution
}