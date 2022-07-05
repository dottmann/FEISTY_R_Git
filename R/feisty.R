
## Script name: FEISTY
##
##
## Authors: Daniel Ottmann 
## Email: daniel.ottmann.riera@gmail.com
##
## Date created: March 2022
## Last update:  April 2022
##
## ---------------------------
##
## Readme:
##
## This script and produces the FEISTY output
##
## ---------------------------


#####################################################################
feisty <- function(param, result) {
  
  # Load functions:
  source("R/feisty_deriv.R") 
  source("R/calcEncounter.R") 
  source("R/calcNu.R") 
  
  #
  # Init
  #
  y0 <- param$y0
  t <- seq(0, param$tEnd, by = 1)
  
  #
  # Run:
  #
  out <- ode(y = y0, times = t, func = feisty_deriv, parms = param, method = "ode23")
  
  #
  # Construct output:
  #
  result <- list()
  result$y <- out[, -1]
  result$R <- out[, param$ixR + 1] # resource
  result$B <- out[, param$ixFish + 1] # Other groups
  result$t <- t
  result$Yield <- t(t(result$B) * param$mortF)

  encounter <- calcEncounter(out[nrow(out), -1], param)
  result$f <- encounter$f
  result$mortpred <- encounter$mortpred
  result$Eavail <- encounter$Eavail
  result$mort <- result$mortpred[param$ixFish] + param$mort0 + param$mortF
  
  Nu <- calcNu(result$Eavail, result$mort, param)
  result$v <- Nu$v
  result$nu <- Nu$nu

  return(result)
}


#                                          END OF SCRIPT
#############################################################################################################