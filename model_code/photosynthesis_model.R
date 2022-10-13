# photosynthesis_model.R
## a simple photosynthesis model based on Farquhar et al. (1980) and designed to
## run a sensitivity analysis of phi psii

## primary function is called photosynthesis_model
## this sources functions from the "functions" folder
## model parameters:

# libraries
# install.packages('R.utils')
library(R.utils)

# load necessary functions
sourceDirectory('functions', modifiedOnly = FALSE)

photosynthesis_model <- function(){
  
  patm <- calc_patm(z)
  par <- calc_par(paro, z)
  vpd <- calc_vpd(tg_c, z, vpdo)
  ca <- cao * 1e-6 * patm
  
  ci <- 0.7 * ca
  gammastar <- calc_gammastar_pa(temperature_c, elevation)
  km <- calc_km_pa(temperature_c, elevation)
  mc = (ci - gammastar) / (ci + km)
  ac = vcmax * mc # rubisco-limited photosynthesis
  
  
  aj = (j/4) * m
  
  a = pmin(ac, aj)
  
}