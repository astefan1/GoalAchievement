# ==============================================================================
# Monte Carlo Simulation Function for Design Analysis ANCOVA
# ==============================================================================

#'@param simData Simulated data using the simANCOVA() function
#'@param priorCoef Prior distribution on coefficients as specified in brms
#'@param priorIntercept Prior distribution on intercept as specified in brms
#'@param priorSigma Prior distribution on error variance as specified in brms
#'@importFrom brms brm

DA_ANCOVA <- function(simData, priorCoef = "normal(0, 10)", priorIntercept = "normal(13.4, 15)", priorSigma = "student_t(3, 0, 7.5)"){
  
  # Define priors
  
  modelpriors <- c(set_prior(priorCoef, class = "b"),
                   set_prior(priorIntercept, class = "Intercept", lb=0),
                   set_prior(priorSigma, class = "sigma"))
  
  # Center pre-treatment scores
  for(i in 1:length(simData)){
    simData[[i]]$physAct_Pre_centered <- simData[[i]]$physAct_Pre-mean(simData[[i]]$physAct_Pre)
  }
  
  # Fit the first dataset
  res <- vector("list", length(simData))
  
  fitB <- brms::brm(physAct_Post ~ physAct_Pre_centered + treatment, 
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