rm(list = ls())
set.seed(99183992)
# First we simulate a time serie that will be used for proves
sequence_math <- 10 * (cos(seq(0,12 * pi, pi/6))) + sqrt(seq(0,12 * pi, pi/6))
sequence_sim <- sequence_math + rnorm(length(sequence_math), 0, 2)
st_simulated <- ts(sequence_sim, frequency = 12, start = c(2000, 1))
plot(st_simulated)

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
  #We'll give inital values to alpha beta and gamma because it's difficult to
  #calculate their real value
  alpha = 0.5
  beta = 0.5
  gamma = 0.5
  
  #Attributes of the input
  fr = frequency(x)
  n = length(x)
  
  #The L0 has to be the last data we train, not the first
  L0 = x[fr + 1]
  #For the initial tendence we will use the first observation and the first in
  #the second periode, divided by the frequency
  T0 = (x[fr + 1] - x[1]) / fr
}

MyHoltWinters(st_simulated)
mod<-HoltWinters(st_simulated)
mod$coefficients
mod$alpha
mod$beta
mod$gamma
predict(mod, 5)
plot(mod)
