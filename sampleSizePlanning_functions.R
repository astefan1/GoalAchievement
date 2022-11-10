# =================== FUNCTIONS FOR SAMPLE SIZE PLANNING =======================

#### Generic data simulation function #####

simANCOVA <- function(iter = 100, N = 250, meanControl = 13.4, sd_measurement = sqrt(20), sd_truescore = sqrt(32), effSingle = 0, effCombi = 0, betaPhysActPre = 1){
  
  res <- vector("list", iter)
  
  # Dummy-coded treatment variables
  treatment <- rep(c(0:3), length.out=N*iter)
  control <- as.numeric(treatment==0)
  impInt <- as.numeric(treatment==1)
  mentCont <- as.numeric(treatment==2)
  combiTreat <- as.numeric(treatment==3)
  
  # Physical Activity pre treatment (same for all groups, mean-centered)
  physAct_PreTrue <- rnorm(N*iter, mean=0, sd=sd_truescore)
  physAct_Pre <- physAct_PreTrue+rnorm(N*iter, mean=0, sd=sd_measurement)
  physAct_Pre <- physAct_Pre + meanControl
  
  # Physical Activity post treatment
  physAct_Post <- meanControl + betaPhysActPre*physAct_PreTrue + effSingle*impInt + effSingle*mentCont + effCombi*combiTreat + rnorm(N*iter, 0, sd_measurement)
  
  # Treatment as factor
  treatment <- as.factor(treatment)
  levels(treatment) <- c("control", "impInt", "mentCont", "combiTreat")
  
  # ID variable
  id <- rep(1:N, iter)
  
  # Assign to one output object
  for(i in 1:iter){
    
    res[[i]] <- data.frame(id = id[(N*i-N+1):(N*i)],
                         physAct_Post = physAct_Post[(N*i-N+1):(N*i)],
                         physAct_Pre = physAct_Pre[(N*i-N+1):(N*i)],
                         treatment = treatment[(N*i-N+1):(N*i)])
    
  }
  
  return(res)
  
}

#### Design Analysis Function ####

DA_ANCOVA <- function(simData){
  
  # Define priors
  
  modelpriors <- c(set_prior("normal(0, 15)", class = "b"),
                   set_prior("normal(13.4, 15)", class = "Intercept", lb=0),
                   set_prior("student_t(3, 0, 7.5)", class = "sigma"))
  
  # Center pre-treatment scores
  for(i in 1:length(simData)){
    simData[[i]]$physAct_Pre_centered <- simData[[i]]$physAct_Pre-mean(simData[[i]]$physAct_Pre)
  }
  
  # Fit the first dataset
  
  res <- vector("list", length(simData))
  
  fitB <- brm(physAct_Post ~ physAct_Pre_centered + treatment, 
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

#### Plotting functions ####

analyze_DA_postMeans <- function(DAresult){
  
  # extract posterior means
  N <- length(DAresult)
  postMeansSim <- matrix(NA, ncol=14, nrow=N)
  for(i in 1:N){
      postMeansSim[i,] <- colMeans(as_draws_df(DAresult[[i]]))
    }
  postMeansSim <- as.data.frame(postMeansSim)
  colnames(postMeansSim) <- colnames(as_draws_df(DAresult[[1]]))
  
  # plot
  layout(mat = matrix(c(1:12), nrow=4, ncol=3, byrow=TRUE))
  par(mar=c(0,4,0,2), cex.lab=2)
  boxplot(postMeansSim$b_Intercept, xaxt = "n",yaxt = "n", bty = "n", 
          col = "white", frame = FALSE, horizontal = TRUE)
  boxplot(postMeansSim$b_physAct_Pre_centered, xaxt = "n", yaxt = "n", 
          bty = "n", col = "white", frame = FALSE, horizontal = TRUE)
  boxplot(postMeansSim$b_treatmentimpInt, xaxt = "n", yaxt = "n", bty = "n", 
          col = "white", frame = FALSE, horizontal = TRUE)
  par(mar=c(5,4,0,2))
  hist(postMeansSim$b_Intercept, main="", xlab = "Intercept", ylab="")
  hist(postMeansSim$b_physAct_Pre_centered, main="", xlab = "beta1", ylab="")
  hist(postMeansSim$b_treatmentimpInt, main="", xlab = "beta2", ylab="")
  par(mar=c(0,4,0,2))
  boxplot(postMeansSim$b_treatmentmentCont, xaxt = "n", yaxt = "n", bty = "n", 
          col = "white", frame = FALSE, horizontal = TRUE)
  boxplot(postMeansSim$b_treatmentcombiTreat, xaxt = "n", yaxt = "n", bty = "n", 
          col = "white", frame = FALSE, horizontal = TRUE)
  boxplot(postMeansSim$sigma, xaxt = "n", yaxt = "n", bty = "n", 
          col = "white", frame = FALSE, horizontal = TRUE)
  par(mar=c(5,4,0,2))
  hist(postMeansSim$b_treatmentmentCont, main="", xlab = "beta3", ylab="")
  hist(postMeansSim$b_treatmentcombiTreat, main="", xlab = "beta4", ylab="")
  hist(postMeansSim$sigma, main="", xlab = "sigma", ylab="")

}

analyze_DA_Effects <- function(DAresult){
  
  N <- length(DAresult)
  
  # initialize result vectors
  BFimpInt <- rep(NA, N)
  BFmentCont <- rep(NA, N)
  BFcombi <- rep(NA, N)
  probimpInt <- rep(NA, N)
  probmentCont <- rep(NA, N)
  probcombi <- rep(NA, N)
  
  # compute BFs and posterior probabilities
  for(i in 1:N){
    BFimpInt[i] <- hypothesis(DAresult[[i]], "treatmentimpInt = 0")$hypothesis$Evid.Ratio
    BFmentCont[i] <- hypothesis(DAresult[[i]], "treatmentmentCont = 0")$hypothesis$Evid.Ratio
    BFcombi[i] <- hypothesis(DAresult[[i]], "treatmentcombiTreat = 0")$hypothesis$Evid.Ratio
    
    postSamples <- as_draws_df(DAresult[[i]])
    runs <- nrow(postSamples)
    probimpInt[i] <- sum(postSamples$b_treatmentimpInt > 0)/runs
    probmentCont[i] <- sum(postSamples$b_treatmentmentCont > 0)/runs
    probcombi[i] <- sum(postSamples$b_treatmentcombiTreat > 0)/runs
  }
  
  # BF distribution
  countsBFimpInt <- paste(paste(as.character(table(cut(BFimpInt, c(0, 1/10,10, Inf), labels=c("<1/10", "weak/moderate", ">10")))/length(BFimpInt)), collapse='              '), "\n <1/10      weak/moderate     >10 ")
  countsBFmentCont <- paste(paste(as.character(table(cut(BFmentCont, c(0, 1/10,10, Inf), labels=c("<1/10", "weak/moderate", ">10")))/length(BFmentCont)), collapse='              '), "\n <1/10      weak/moderate     >10 ")
  countsBFcombi <- paste(paste(as.character(table(cut(BFcombi, c(0, 1/10,10, Inf), labels=c("<1/10", "weak/moderate", ">10")))/length(BFcombi)), collapse='              '), "\n <1/10      weak/moderate     >10 ")
  probBFimpInt <- paste0("   p(BF01 > 1): ", round(sum(BFimpInt > 1)/N, digits=2), "\n", "   p(BF01 < 1): ", round(sum(BFimpInt < 1)/N, digits=2))
  probBFmentCont <- paste0("   p(BF01 > 1): ", round(sum(BFmentCont > 1)/N, digits=2), "\n", "   p(BF01 < 1): ", round(sum(BFmentCont < 1)/N, digits=2))
  probBFcombi <- paste0("   p(BF01 > 1): ", round(sum(BFcombi > 1)/N, digits=2), "\n", "   p(BF01 < 1): ", round(sum(BFcombi < 1)/N, digits=2))
  
  # plot
  par(mfrow=c(2,3), mar=c(5,4,4,2)+0.1)
  hist(log(BFimpInt), main="", xaxt="n", xlab = "BF01_beta2", ylab="")
  axis(1, at=log(c(1/1000, 1/100, 1/50, 1/30, 1/10, 1/3, 1, 3, 10, 30, 50, 100, 1000)),
       labels = c("1/1000", "1/100", "1/50", "1/30", "1/10", "1/3", "1", "3", "10", "30", "50", "100", "1000"))
  mtext(countsBFimpInt, side=3)
  mtext(probBFimpInt, side = 3, line = -3, adj = 0)
  hist(log(BFmentCont), main="", xaxt="n", xlab = "BF01_beta3", ylab="")
  axis(1, at=log(c(1/1000, 1/100, 1/50, 1/30, 1/10, 1/3, 1, 3, 10, 30, 50, 100, 1000)),
       labels = c("1/1000", "1/100", "1/50", "1/30", "1/10", "1/3", "1", "3", "10", "30", "50", "100", "1000"))
  mtext(countsBFmentCont, side=3)
  mtext(probBFmentCont, side = 3, line = -3, adj = 0)
  hist(log(BFcombi), main="", xaxt="n", xlab = "BF01_beta4", ylab="")
  axis(1, at=log(c(1/1000, 1/100, 1/50, 1/30, 1/10, 1/3, 1, 3, 10, 30, 50, 100, 1000)),
       labels = c("1/1000", "1/100", "1/50", "1/30", "1/10", "1/3", "1", "3", "10", "30", "50", "100", "1000"))
  mtext(countsBFcombi, side=3)
  mtext(probBFcombi, side = 3, line = -3, adj = 0)
  hist(probimpInt, main="", xlab = "p(beta2 > 0)", ylab="")
  hist(probmentCont, main="", xlab = "p(beta3 > 0)", ylab="")
  hist(probcombi, main="", xlab = "p(beta4 > 0)", ylab="")
}

analyze_DA_noDiffSingle <- function(DAresult){
  
  N <- length(DAresult)
  BF01 <- rep(NA, N)
  
  # compute BF
  for(i in 1:N){
    BF01[i] <- hypothesis(DAresult[[i]], "treatmentimpInt - treatmentmentCont = 0")$hypothesis$Evid.Ratio
  }
  countsBF01 <- paste(paste(as.character(table(cut(BF01, c(0, 1/10,10, Inf), labels=c("<1/10", "weak/moderate", ">10")))/length(BF01)), collapse='              '), "\n <1/10      weak/moderate     >10 ")
  probBF01 <- paste0("   p(BF01 > 1): ", round(sum(BF01 > 1)/N, digits=2), "\n", "   p(BF01 < 1): ", round(sum(BF01 < 1)/N, digits=2))
  
  # plot
  par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)
  hist(log(BF01), main="", xaxt="n", xlab = "BF01", ylab="")
  axis(1, at=log(c(1/1000, 1/100, 1/50, 1/30, 1/10, 1/3, 1, 3, 10, 30, 50, 100, 1000)),
       labels = c("1/1000", "1/100", "1/50", "1/30", "1/10", "1/3", "1", "3", "10", "30", "50", "100", "1000"))
  mtext(countsBF01, side=3)
  mtext(probBF01, side = 3, line = -3, adj = 0)
}

analyze_DA_combi <- function(DAresult){
  
  N <- length(DAresult)
  BFimpInt <- rep(NA, N)
  BFmentCont <- rep(NA, N)
  probimpInt <- rep(NA, N)
  probmentCont <- rep(NA, N)
  
  # compute BFs and posterior probabilities
  
  for(i in 1:N){
    
    postSamples <- as_draws_df(DAresult[[i]])
    runs <- nrow(postSamples)
    
    BFimpInt[i] <- hypothesis(DAresult[[i]], "treatmentcombiTreat - treatmentimpInt = 0")$hypothesis$Evid.Ratio
    BFmentCont[i] <- hypothesis(DAresult[[i]], "treatmentcombiTreat - treatmentmentCont = 0")$hypothesis$Evid.Ratio
    probimpInt[i] <- sum(postSamples$b_treatmentcombiTreat-postSamples$b_treatmentimpInt > 0)/runs
    probmentCont[i] <- sum(postSamples$b_treatmentcombiTreat-postSamples$b_treatmentmentCont > 0)/runs
  
  }
  
  # BF quantiles
  countsBFimpInt <- paste(paste(as.character(table(cut(BFimpInt, c(0, 1/10,10, Inf), labels=c("<1/10", "weak/moderate", ">10")))/length(BFimpInt)), collapse='              '), "\n <1/10      weak/moderate     >10 ")
  countsBFmentCont <- paste(paste(as.character(table(cut(BFmentCont, c(0, 1/10,10, Inf), labels=c("<1/10", "weak/moderate", ">10")))/length(BFmentCont)), collapse='              '), "\n <1/10      weak/moderate     >10 ")
  probBFimpInt <- paste0("   p(BF01 > 1): ", round(sum(BFimpInt > 1)/N, digits=2), "\n", "   p(BF01 < 1): ", round(sum(BFimpInt < 1)/N, digits=2))
  probBFmentCont <- paste0("   p(BF01 > 1): ", round(sum(BFmentCont > 1)/N, digits=2), "\n", "   p(BF01 < 1): ", round(sum(BFmentCont < 1)/N, digits=2))
  
  # plots
  par(mfrow=c(2,2), mar=c(5,4,4,2)+0.1)
  
  hist(log(BFimpInt), main="", xaxt="n", xlab = "BF01_beta2", ylab="")
  axis(1, at=log(c(1/1000, 1/100, 1/50, 1/30, 1/10, 1/3, 1, 3, 10, 30, 50, 100, 1000)),
       labels = c("1/1000", "1/100", "1/50", "1/30", "1/10", "1/3", "1", "3", "10", "30", "50", "100", "1000"))
  mtext(countsBFimpInt, side=3)
  mtext(probBFimpInt, side = 3, line = -3, adj = 0)
  hist(log(BFmentCont), main="", xaxt="n", xlab = "BF01_beta3", ylab="")
  axis(1, at=log(c(1/1000, 1/100, 1/50, 1/30, 1/10, 1/3, 1, 3, 10, 30, 50, 100, 1000)),
       labels = c("1/1000", "1/100", "1/50", "1/30", "1/10", "1/3", "1", "3", "10", "30", "50", "100", "1000"))
  mtext(countsBFmentCont, side=3)
  mtext(probBFmentCont, side = 3, line = -3, adj = 0)
  hist(probimpInt, main="", xlab = "p(beta4 > beta2)", ylab="")
  hist(probmentCont, main="", xlab = "p(beta4 > beta3)", ylab="")
  
}
