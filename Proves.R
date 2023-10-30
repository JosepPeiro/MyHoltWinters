rm(list = ls())
set.seed(99183992)

source("MyHoltWinters.R")

# First we simulate a time serie that will be used for proves
sequence_math <- 10 * (cos(seq(0,24 * pi, pi/6))) + seq(0,24 * pi, pi/6) + 10 * sin(seq(0, pi, pi / 24 / 6))
sequence_sim <- sequence_math + rnorm(length(sequence_math), 0, 2)
st_simulated <- ts(sequence_sim, frequency = 12, start = c(2000, 1))

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

ferro<-ts(ferrocarril, frequency = 4)


mod<-HoltWinters(st_simulated)
mod$coefficients
mod$alpha
mod$beta
mod$gamma

# Bucle Ok
# Comparar con Holtwinters Ok
# Representar Ok


Mymodelo <- MyHoltWinters(st_simulated)
Mymodelo$coefficients
Mymodelo$parameters

Mymodelo.par <- MyHoltWinters(st_simulated, mod$alpha,mod$beta,mod$gamma)
Mymodelo.par$coefficients
Mymodelo.par$parameters

#Quitar los c primerso valores
#Comparar con los parametros de holtwinters
#Mirar la otra serie Ok
st_simulated
colnames(mod$fitted)
Mymodelo$fitted

