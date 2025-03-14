# Author: Josep Peiró Ramos
# Student at Universitat de València

# Case study of the development of a HoltWinters algorithm with
# the same theorical base as the original one from the stats R package,
# but programmed as a optional exercise for a better understanding of
# how it works.

# In this script there is a study of how this alternative algorithm
# performs compared with the original one

rm(list = ls())
set.seed(99183992)

source("MyHoltWinters.R")

# First we simulate a time serie that will be used for proves
sequence_math <- 10 * (cos(seq(0,24 * pi, pi/6))) + seq(0,24 * pi, pi/6) + 10 * sin(seq(0, pi, pi / 24 / 6))
sequence_sim <- sequence_math + rnorm(length(sequence_math), 0, 2)
st_simulated <- ts(sequence_sim, frequency = 12, start = c(2000, 1))

#Also the study case data
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

# Here we observe the solution and the parameters that
# the original HoltWinter algorithm from Rbase (stats package) returns
mod<-HoltWinters(st_simulated)
mod$coefficients
mod$alpha
mod$beta
mod$gamma
plot(mod)

# Bucle Ok
# Comparar con Holtwinters Ok
# Representar Ok

#Quitar los c primeros valores Ok
#Comparar con los parametros de holtwinters -> No pasa nada, vuelve a los que ya tenia Ok
#Mirar la otra serie Ok

# Now we see the result of our HoltWinters program
Mymodelo <- MyHoltWinters(st_simulated)
Mymodelo$coefficients
Mymodelo$parameters
# It can be seen that the result is as accurate as the base HoltWinters

# Now we try to want to see what happens if we initiate our algorythm
# with the parameters that the base HoltWinters calculate
Mymodelo.par <- MyHoltWinters(st_simulated, mod$alpha,mod$beta,mod$gamma)
Mymodelo.par$parameters
# And as we can see, the final result is equivalent to our model,
# that means that the issue with differents parameters isn't due to
# different initializations, it's due to the fact that they converge
# in differents directions

# Error metrics for comparing algorythms: mape, rmse
mape_Mymodelo <- 100*mean(abs(st_simulated-Mymodelo$fitted[,1])/st_simulated)
mape_mod <- 100*mean(abs(st_simulated-mod$fitted[,1])/st_simulated)
rmse_Mymodelo <- sqrt(mean((st_simulated-Mymodelo$fitted[,1])^2))
rmse_mod <- sqrt(mean((st_simulated-mod$fitted[,1])^2))

# As both predictions are not the same length, we'll cut first part of the
# base prediction so, they become more comparable:
st_red <- window(mod$fitted, start = c(start(mod$fitted)[1] + 1,start(mod$fitted)[2]))
mape_mod_red <- 100*mean(abs(st_simulated-st_red[,1])/st_simulated)
rmse_mod_red <- sqrt(mean((st_simulated-st_red[,1])^2))

mape_Mymodelo
mape_mod
mape_mod_red

rmse_Mymodelo
rmse_mod
rmse_mod_red


###############################################
###############Ferrocarril#####################
###############################################

# We'll do the same for the case study dataset
# A single time serie about railroads, the interpretations
# of this data is unknown 

# We'll compare both models and see how they perform
modfer<-HoltWinters(ferro)
plot(modfer)
modfer$alpha
modfer$beta
modfer$gamma

Mymodelofer <- MyHoltWinters(ferro)
Mymodelofer$parameters

Mymodelo.par.fer <- MyHoltWinters(ferro, modfer$alpha,modfer$beta,modfer$gamma)
Mymodelo.par$parameters

# In this case, the adaptations to training data looks the same confident
# in both approaches.
# And we could see how the behaviour or parameters are dependent of their
# initializations
# A deeper research in this direction can be done, but it's visual that
# out algorithm can replace the base one as they perform roughly equivalent

# Now this is different,  but equivalent method from our
# algorithm for converging
best_Mymodelo <- MyHoltWinters(ferro, optimum = T)
best_Mymodelo$aplha
best_Mymodelo$beta
best_Mymodelo$gamma

# It is more computationally expensive. And although the
# theorical component is deeper and more robust, there is not a visual
# improvement in this study case, as it converges to the same parameters.
# However, a better study can be done to find if there can be a different
# behaviour in different dataset

mape_best_Mymodelo <- 100*mean(abs(ferro-best_Mymodelo$fitted[,1])/ferro)
mape_best_Mymodelo
rmse_best_Mymodelo <- sqrt(mean((ferro-best_Mymodelo$fitted[,1])^2))
rmse_best_Mymodelo