MyHoltWinters <- function(x, alpha = TRUE, beta = TRUE, gamma = TRUE){
  if (!is.ts(x)){
    print("The input vector must be a time serie")
    return()
  }
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
  
  solution <- list()
  p <- frequency(x)
  optimo <- optim(initial, fun.optim, data = c(x, p), method = "L-BFGS-B", 
                  lower = c(0.00, 0.00, 0.00), upper = c(0.999, 0.999, 0.999))
  
  ajuste <- fun.ajuste(x, p, optimo$par[1], optimo$par[2], optimo$par[3])
  plot(x)
  lines(ts(c(rep(NA, 2 * p), ajuste$valfitted),
           start = start(x), frequency = frequency(x)),
        type = "l", lty=2, col = "red")
  
  names(optimo$par) <- c('alpha', 'beta', 'gamma')
  
  solution$x <- x
  solution$adjust <- ajuste$valfitted
  solution$fitted <- ajuste$fitted
  solution$parameters <- optimo$par
  solution$aplha <- optimo$par[1]
  solution$beta <- optimo$par[2]
  solution$gamma <- optimo$par[3]
  solution$coefficients <- ajuste$coef
  solution$SSE <- ajuste$SE
  
  return(solution)
}

  
fun.ajuste <- function(serie, freq, alpha, beta, gamma){
  
  n <- length(serie)
  valfitted <- c()
  
  L0 <- sum(serie[1:freq + freq]) / freq
  T0 <- (sum(serie[1:freq + freq]) - sum(serie[1:freq])) / (freq ^ 2)
  S0 <- (serie[1:freq + freq] - L0)
  
  fitted <- matrix(ncol = 4)
  
  for (i in (2 * freq + 1):n){
    
    S0ind <- i %% freq
    if (S0ind == 0){S0ind <- freq}

    pred <- L0 + T0 + S0[S0ind]
    valfitted <- c(valfitted, pred)
    fitted <- rbind(fitted, c(pred, L0, T0, S0[S0ind]))
    
    L1 <- alpha * (serie[i] - S0[S0ind]) + (1 - alpha) * (L0 + T0)
    T1 <- beta * (L1 - L0) + (1 - beta) * T0
    S1 <- gamma * (serie[i] - L1) + (1 - gamma) * S0[S0ind]
    
    L0 <- L1
    T0 <- T1
    S0[S0ind] <- S1
  }
  
  colnames(fitted) <- c("Xhat", "Level", "Tendency", "Season")
  fitted <- ts(fitted[-1,], start = start(lag(serie, k = -2 * freq)), frequency = freq)
               
  SE <- sum((serie[(2 * freq + 1):n] - valfitted) ^ 2)
  coefficients <- c(L0, T0, S0)
  names(coefficients) <- c('Level', 'Tendency', paste('Season', 1:freq))
  
  ajust <- list(valfitted = valfitted, SE = SE, coef = coefficients,
                fitted = fitted)
  return(ajust)
}

fun.optim <- function(vect, data){
  #print(vect)
  n <- length(data)
  aj <- fun.ajuste(data[1:(n - 1)], data[n], vect[1], vect[2], vect[3])
  return(aj$SE)
}
  
optimizer <- function(x){
  p <- frequency(x)
  value_vector <- c()
  best_value <- Inf
  par_matrix <- matrix(ncol = 3)
  best_par <- NULL
  for (a in seq(0.1, 0.9, by = 0.1)){
    for (b in seq(0.1, 0.9, by = 0.1)){
      for (c in seq(0.1, 0.9, by = 0.1)){
        optimo <- optim(c(a,b,c), fun.optim, data = c(x, p), method = "L-BFGS-B", 
                        lower = c(0.001, 0.001, 0.001), upper = c(0.999, 0.999, 0.999))
        par_matrix <- rbind(par_matrix, optimo$par)
        value_vector <- c(value_vector, optimo$value)
        if (optimo$value < best_value){
          best_value <- optimo$value
          best_par <- optimo$par
        }
      }
    }
  }
  best_solution <- list(value = best_value,
                        par = best_par)
  return(best_solution)
}