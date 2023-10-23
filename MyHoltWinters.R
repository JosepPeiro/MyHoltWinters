rm(list = ls())
set.seed(99183992)
# First we simulate a time serie that will be used for proves
sequence_math <- 10 * (cos(seq(0,12 * pi, pi/6))) + sqrt(seq(0,12 * pi, pi/6))
sequence_sim <- sequence_math + rnorm(length(sequence_math), 0, 2)
st_simulated <- ts(sequence_sim, frequency = 12, start = c(2000, 1))
#plot(st_simulated)

#Also the data from the exam of last year
ferrocarril <- c(
  139733896, 144482139, 123561582, 145152679,
  144482993, 146521012, 125713176, 149226028,
  145885551, 142603051, 121745633, 145120639,
  143638419, 144004197, 122310123, 148672623,
  141285387, 144252125, 124124600, 149670808,
  140193460, 144167136, 128486754, 149125431,
  140008498, 149341300, 130352920, 150039240,
  148622967, 151486750, 134747548, 158497166,
  150246047, 163228902, 140311097, 165269379,
  161457841, 160348073, 139321325, 165980376
)


MyHoltWinters <- function(x, alpha = TRUE, beta = TRUE, gamma = TRUE){
  if (!is.ts(x)){
    print("The input vector must be a time serie")
    return()
  }
  
  fun.ajuste <- function(serie, freq, alfa, beta, gamma){
    
    n <- length(serie)
    valfitted <- c()
    
    L0 <- sum(serie[1:freq + freq]) / freq
    T0 <- (sum(serie[1:freq + freq]) - sum(serie[1:freq])) / (freq ^ 2)
    S0 <- (serie[1:freq + freq] - L0)
    
    for (i in (2 * freq + 1):n){
      
      S0ind <- i %% freq
      if (S0ind == 0){S0ind <- freq}

      pred <- L0 + T0 + S0[S0ind]
      valfitted <- c(valfitted, pred)
      
      L1 <- alpha * (serie[i] - S0[S0ind]) + (1 - alpha) * (L0 + T0)
      T1 <- beta * (L1 - L0) + (1 - beta) * T0
      S1 <- gamma * (serie[i] - L1) + (1 - gamma) * S0[S0ind]
      
      L0 <- L1
      T0 <- T1
      S0[S0ind] <- S1
    }
    
    SE <- sum((serie[(2 * freq + 1):n] - valfitted) ^ 2)
    ajust <- list(valfitted = valfitted, SE = SE)
    return(ajust)
    
  }
  fun.optim <- function(vect, data){
    print(vect)
    n <- length(data)
    aj <- fun.ajuste(data[1:(n - 1)], data[n], vect[1], vect[2], vect[3])
    print(aj$SE)
    return(aj$SE)
  }
  
  p <- frequency(x)

  optimo <- optim(c(0.5,0.5,0.5), fun.optim, data = c(x, p), method = "L-BFGS-B", 
                  lower = c(0, 0, 0), upper = c(1, 1, 1))
  
  print(optimo)
}

#MyHoltWinters(ts(ferrocarril))
MyHoltWinters(st_simulated)

# mod<-HoltWinters(st_simulated)
# mod$coefficients
# mod$alpha
# mod$beta
# mod$gamma
# predict(mod, 5)
# plot(mod)
# freq = 12
