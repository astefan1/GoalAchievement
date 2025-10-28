# ==============================================================================
# GENERIC DATA SIMULATION FUNCTION FOR ANCOVA (HYPOTHESIS 2)
# ==============================================================================

# This function lets you set the effectiveness of the three interventions
# freely (as difference from control group)

#'@param iter Number of generated datasets
#'@param N Number of observations per dataset
#'@param meanControl Post-treatment mean of the control group
#'@param sdWithinGroup Standard deviation within treatment groups
#'@param effSingle Effect size of single treatments
#'@param effCombi Effect combined treatment
#'@param retestReliability Correlation between measurements
#'@importFrom MASS mvrnorm

simANCOVA_H2 <- function(iter = 100, N = 250, meanControl = 2.4, sdWithinGroup = 1, effimpInt = 0, effmentCont = 0, effCombi = 0, retestReliability = 0.75){
  
  res <- vector("list", iter)
  
  # Dummy-coded treatment variables
  treatment <- rep(c(0:3), length.out=N*iter)
  control <- as.numeric(treatment==0)
  impInt <- as.numeric(treatment==1)
  mentCont <- as.numeric(treatment==2)
  combiTreat <- as.numeric(treatment==3)
  
  # Correlation structure
  physAct_PrePost <- MASS::mvrnorm(N*iter,
                                   mu = c(meanControl,meanControl), 
                                   Sigma=matrix(c(sdWithinGroup^2, 
                                                  sdWithinGroup^2*retestReliability,
                                                  sdWithinGroup^2*retestReliability, 
                                                  sdWithinGroup^2), 
                                                byrow=TRUE, 
                                                ncol=2))
  
  # Adding treatment effects for post measurements
  physAct_PrePost[, 2] <- physAct_PrePost[, 2] + effimpInt*impInt + effmentCont*mentCont + effCombi*combiTreat
  
  treatment <- as.factor(treatment)
  levels(treatment) <- c("control", "impInt", "mentCont", "combiTreat")
  id <- 1:N
  
  for(i in 1:iter){
    
    res[[i]] <- data.frame(id = id,
                           physAct_Post = physAct_PrePost[(N*i-N+1):(N*i), 2],
                           physAct_Pre = physAct_PrePost[(N*i-N+1):(N*i), 1],
                           treatment = treatment[(N*i-N+1):(N*i)])
    
  }
  
  return(res)
  
}
