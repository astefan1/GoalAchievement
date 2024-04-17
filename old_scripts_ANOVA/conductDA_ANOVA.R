# ==============================================================================
# Monte Carlo Simulation Function for Design Analysis ANOVA
# ==============================================================================

#'@param simData Simulated data using the simANCOVA() function
#'@param priorCoef Prior distribution on coefficients as specified in brms
#'@param priorIntercept Prior distribution on intercept as specified in brms
#'@param priorSigma Prior distribution on error variance as specified in brms
#'@importFrom brms brm

DA_ANOVA <- function(simData, priorCoef = "normal(0, 2)", priorIntercept = "normal(4, 1.5)", priorSigma = "student_t(3, 0, 0.5)"){
  
  # Define priors
  
  modelpriors <- c(set_prior(priorCoef, class = "b"),
                   set_prior(priorIntercept, class = "Intercept", lb=0),
                   set_prior(priorSigma, class = "sigma"))
  
  # Fit the first dataset
  res <- vector("list", length(simData))
  
  fitB <- brms::brm(DV ~ treatment, 
                    data = simData[[1]], 
                    family = "gaussian",
                    prior = modelpriors,
                    sample_prior = "yes")
  
  res[[1]] <- fitB
  
  # Fit all other datasets using the same pre-compiled Stan model
  
  for(i in 2:length(simData)){
    
    res[[i]] <- update(fitB, newdata = simData[[i]])
    
  }
  
  return(res)
  
}