# ==============================================================================
# GENERIC DATA SIMULATION FUNCTION FOR ANOVA
# ==============================================================================

#'@param iter Number of generated datasets
#'@param N Number of observations per dataset
#'@param meanControl Mean of the control group
#'@param sdWithinGroup Standard deviation within treatment groups
#'@param effSingle Effect size of single treatments
#'@param effCombi Effect combined treatment

simANOVA <- function(iter = 100, N = 250, meanControl = 13.4, sdWithinGroup = 7.2, effSingle = 0, effCombi = 0, retestReliability = 0.75){
  
  res <- vector("list", iter)
  
  # Dummy-coded treatment variables
  treatment <- rep(c(0:3), length.out=N*iter)
  control <- as.numeric(treatment==0)
  impInt <- as.numeric(treatment==1)
  mentCont <- as.numeric(treatment==2)
  combiTreat <- as.numeric(treatment==3)
  
  # Correlation structure
  DV <- rnorm(N*iter, mean=meanControl, sd = sdWithinGroup)
  
  # Adding treatment effects for post measurements
  DV <- DV + effSingle*impInt + effSingle*mentCont + effCombi*combiTreat
  
  treatment <- as.factor(treatment)
  levels(treatment) <- c("control", "impInt", "mentCont", "combiTreat")
  id <- 1:N*iter
  
  for(i in 1:iter){
    
    res[[i]] <- data.frame(id = id[(N*i-N+1):(N*i)],
                           DV = DV[(N*i-N+1):(N*i)],
                           treatment = treatment[(N*i-N+1):(N*i)])
    
  }
  
  return(res)
  
}
