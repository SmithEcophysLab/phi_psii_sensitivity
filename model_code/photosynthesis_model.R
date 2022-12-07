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

photosynthesis_model <- function(elevation_m = 0, ca_ppm = 420, temperature_c = 25, par = 400,
                                 tmean = 25,
                                 vcmax25 = 100, jmax25 = 200,
                                 phi_psii = 0.6895, # rate at 25C from Bernacchi
                                 e_partitioning_coef = 4, 
                                 absorbance = 0.85, 
                                 photosystem_partitioning_coef = 0.5,
                                 theta = 0.85,
                                 phi_psii_tresp = 'no',
                                 b_tresp = 0.0474,
                                 c_tresp = 0.000859){
  
  topt_posch <- b_tresp / (2 * c_tresp)
  model_avg_phi_psii <- 0.75
  
  a_tresp <- model_avg_phi_psii - ((b_tresp * topt_posch) - (c_tresp * topt_posch^2)) # a, given the assumption that model average phi psii is at the temperature optimum estimated from the data
  
  patm_pa <- calc_patm(elevation_m) # atmospheric pressure (Pa)
  ca_pa <- ca_ppm * 1e-6 * patm_pa # atmospheric co2 (Pa)
  
  vcmax <- vcmax25 * calc_vcmax_tresp_mult(tleaf = temperature_c, tmean = tmean, tref = 25)
  ci_pa <- 0.7 * ca_pa # intercellular co2 (Pa)
  gammastar_pa <- calc_gammastar_pa(temperature_c, elevation_m) # co2 compensation point (Pa)
  km_pa <- calc_km_pa(temperature_c, elevation_m) # michaelis-menten coefficient for rubisco (Pa)
  mc = (ci_pa - gammastar_pa) / (ci_pa + km_pa) # 
  ac = (vcmax * mc)  # rubisco-limited photosynthesis
  
  if(phi_psii_tresp == "yes"){
    # Bernacchi et al. (2003) temperature response
    phi_psii = a_tresp + (b_tresp * temperature_c) - (c_tresp * temperature_c * temperature_c)
  }else{
    phi_psii
  }
  
  jmax <- jmax25 * calc_jmax_tresp_mult(tleaf = temperature_c, tmean = tmean, tref = 25)
  m <- (ci_pa - gammastar_pa) / (ci_pa + (2 * gammastar_pa))
  psii_light <- absorbance * photosystem_partitioning_coef * par # light getting to psii
  j_a <- theta
  j_b <- -(phi_psii * psii_light + jmax) 
  j_c <- phi_psii * psii_light * jmax
  j <- (-j_b - sqrt(j_b^2 - 4 * j_a * j_c)) / (2 * j_a)
  aj <- ((j/e_partitioning_coef) * m)  # rubp regeneration-limited photosyntehsis
  
  a <- pmin(ac, aj) - (0.015 * vcmax)
  
  results <- data.frame("elevation_m" = elevation_m,
                        "ca_ppm" = ca_ppm,
                        "temperature_c" = temperature_c,
                        "par" = par,
                        "vcmax" = vcmax,
                        "jmax" = jmax,
                        "phi_psii" = phi_psii,
                        "e_partitioning_coef" = e_partitioning_coef, 
                        "absorbance" = e_partitioning_coef, 
                        "photosystem_partitioning_coef" = photosystem_partitioning_coef,
                        "theta" = theta,
                        "patm_pa" = patm_pa,
                        "ca_pa" = ca_pa,
                        "ci_pa" = ci_pa,
                        "gammastar_pa" = gammastar_pa,
                        "km_pa" = km_pa,
                        "mc" = mc,
                        "ac" = ac,
                        "m" = m,
                        "psii_light" = psii_light,
                        "j_a" = j_a,
                        "j_b" = j_b,
                        "j_c" = j_c,
                        "j" = j,
                        "aj" = aj,
                        "a" = a)
  
  results
  
}
