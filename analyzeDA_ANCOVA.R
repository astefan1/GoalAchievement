# ==============================================================================
# Functions to analyze the Monte Carlo draws of the design analysis
# ==============================================================================

#' Plot the means of the posterior distribution
#' @param DAresult Result object of the DA_ANCOVA() function
#' @param trueIntercept True value of the intercept parameter in the simulation
#' @param trueRR True retest reliability value in the simulation
#' @param trueSingleEffect True value of the effect of single treatments in the simulation
#' @param trueCombiEffect True value of the combined treatment effect in the simulation
#' @param xlimIntercept xlim values for the intercept figures
#' @param xlimBeta xlim values for the beta2, beta3, and beta4 figures
#' @param xlimBeta1 xlim value for the beta1 figures
#' @param xlimSigma xlim value for the sigma figures

analyze_DA_postMeans <- function(DAresult, trueIntercept=13.4, trueRR = 0.75, trueSingleEffect=0, trueCombiEffect=0, xlimIntercept=c(11,16), xlimBeta=c(-3,3), xlimBeta1=c(0.4,0.8), xlimSigma=c(5,7)){
  
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
  
  # Intercept Boxplot
  boxplot(postMeansSim$b_Intercept, xaxt = "n",yaxt = "n", bty = "n", 
          col = "white", frame = FALSE, horizontal = TRUE, ylim=xlimIntercept)
  abline(v=trueIntercept, lty = "dotted", lwd=2, col="blue")
  
  # Beta1 Boxplot
  boxplot(postMeansSim$b_physAct_Pre_centered, xaxt = "n", yaxt = "n", 
          bty = "n", col = "white", frame = FALSE, horizontal = TRUE, ylim=xlimBeta1)
  abline(v=trueRR, lty = "dotted", lwd=2, col="blue")
  
  # Beta2 Boxplot
  boxplot(postMeansSim$b_treatmentimpInt, xaxt = "n", yaxt = "n", bty = "n", 
          col = "white", frame = FALSE, horizontal = TRUE, ylim=xlimBeta)
  abline(v=trueSingleEffect, lty = "dotted", lwd=2, col="blue")
  
  par(mar=c(5,4,0,2))
  
  # Intercept Histogram
  hist(postMeansSim$b_Intercept, main="", xlab = "Intercept", ylab="", xlim=xlimIntercept)
  abline(v=trueIntercept, lty = "dotted", lwd=2, col="blue")
  
  # Beta1 Histogram
  hist(postMeansSim$b_physAct_Pre_centered, main="", xlab = "beta1", ylab="", xlim=xlimBeta1)
  abline(v=trueRR, lty = "dotted", lwd=2, col="blue")
  
  # Beta2 Histogram
  hist(postMeansSim$b_treatmentimpInt, main="", xlab = "beta2", ylab="", xlim=xlimBeta)
  abline(v=trueSingleEffect, lty = "dotted", lwd=2, col="blue")
  
  par(mar=c(0,4,0,2))
  
  # Beta3 Boxplot
  boxplot(postMeansSim$b_treatmentmentCont, xaxt = "n", yaxt = "n", bty = "n", 
          col = "white", frame = FALSE, horizontal = TRUE, ylim=xlimBeta)
  abline(v=trueSingleEffect, lty = "dotted", lwd=2, col="blue")
  
  # Beta4 Boxplot
  boxplot(postMeansSim$b_treatmentcombiTreat, xaxt = "n", yaxt = "n", bty = "n", 
          col = "white", frame = FALSE, horizontal = TRUE, ylim=xlimBeta)
  abline(v=trueCombiEffect, lty = "dotted", lwd=2, col="blue")
  
  # Sigma Boxplot
  boxplot(postMeansSim$sigma, xaxt = "n", yaxt = "n", bty = "n", 
          col = "white", frame = FALSE, horizontal = TRUE, ylim=xlimSigma)
  
  par(mar=c(5,4,0,2))
  
  # Beta3 Histogram
  hist(postMeansSim$b_treatmentmentCont, main="", xlab = "beta3", ylab="", xlim=xlimBeta)
  abline(v=trueSingleEffect, lty = "dotted", lwd=2, col="blue")
  
  # Beta4 Histogram
  hist(postMeansSim$b_treatmentcombiTreat, main="", xlab = "beta4", ylab="", xlim=xlimBeta)
  abline(v=trueCombiEffect, lty = "dotted", lwd=2, col="blue")
  
  # Sigma Histogram
  hist(postMeansSim$sigma, main="", xlab = "sigma", ylab="", xlim=xlimSigma)
  
}

#' Posterior probabilities and Bayes factors for all sub-hypotheses
#' @param DAresult Result object of the DA_ANCOVA() function

analyze_DA_Effects <- function(DAresult){
  
  N <- length(DAresult)
  
  # initialize result vectors
  H1a <- matrix(rep(NA, N*2), ncol=2)
  H1b <- matrix(rep(NA, N*2), ncol=2)
  H1c <- matrix(rep(NA, N*2), ncol=2)
  H1d <- matrix(rep(NA, N*2), ncol=2)
  H1e <- matrix(rep(NA, N*2), ncol=2)
  H1f <- matrix(rep(NA, N*2), ncol=2)
  
  # compute BFs
  for(i in 1:N){
    
    H1a[i,1] <- simplify2array(hypothesis(DAresult[[i]], "treatmentcombiTreat - treatmentmentCont = 0")$hypothesis$Evid.Ratio)
    H1b[i,1] <- simplify2array(hypothesis(DAresult[[i]], "treatmentcombiTreat - treatmentimpInt = 0")$hypothesis$Evid.Ratio)
    H1c[i,1] <- simplify2array(hypothesis(DAresult[[i]], "treatmentcombiTreat = 0")$hypothesis$Evid.Ratio)
    H1d[i,1] <- simplify2array(hypothesis(DAresult[[i]], "treatmentimpInt - treatmentmentCont = 0")$hypothesis$Evid.Ratio)
    H1e[i,1] <- simplify2array(hypothesis(DAresult[[i]], "treatmentmentCont = 0")$hypothesis$Evid.Ratio)
    H1f[i,1] <- simplify2array(hypothesis(DAresult[[i]], "treatmentimpInt = 0")$hypothesis$Evid.Ratio)
    
  }
  
  # compute Posterior Probabilities
  
  for(i in 1:N){
    
    H1a[i,2] <- sum(as_draws_df(DAresult[[i]])$b_treatmentcombiTreat-as_draws_df(DAresult[[i]])$b_treatmentmentCont > 0)/4000
    H1b[i,2] <- sum(as_draws_df(DAresult[[i]])$b_treatmentcombiTreat-as_draws_df(DAresult[[i]])$b_treatmentimpInt > 0)/4000
    H1c[i,2] <- sum(as_draws_df(DAresult[[i]])$b_treatmentcombiTreat > 0)/4000
    H1e[i,2] <- sum(as_draws_df(DAresult[[i]])$b_treatmentmentCont > 0)/4000
    H1f[i,2] <- sum(as_draws_df(DAresult[[i]])$b_treatmentimpInt > 0)/4000
    
  }
  
  return(list(H1a = H1a,
              H1b = H1b,
              H1c = H1c,
              H1d = H1d,
              H1e = H1e,
              H1f = H1f))
}

#' Plot BF Distribution
#' @param DAEffects Result object of analyze_DA_Effects()
#' @param hypothesis Which hypothesis is being tested? H1a-H1f

plotBFdist <- function(DAEffects, hypothesis="H1a"){
  
  df <- as.data.frame(DAEffects[[hypothesis]])
  colnames(df) <- c("BF", "postProb")
  BF <- 1/df$BF
  logBF <- log(1/df$BF)
  
  par(oma=c(6,0,0,0), mar=c(5,5,4,1))
  hist(logBF, breaks=c(min(logBF, na.rm=TRUE), 
                       log(c(1/100, 1/10, 1, 10, 100, 1000, 1e+6)), 
                       max(logBF, na.rm = TRUE)), 
       xlab = "Bayes Factor", ylab = "Density", 
       main=hypothesis, cex.axis=2, cex.lab = 2, cex.main=2, xaxt="n", 
       prob=TRUE, xlim=log(c(1/1000, 1e+6)))
  axis(1, at=log(c(1/100, 1/10, 0, 10, 100, 1000, 1e+6)), 
       labels = c("1/100", "1/10", "0", "10", "100", "1000", "1000000"), 
       cex.axis = 2)
  mtext(paste0("p(BF > 1) = ", 
               round(sum(logBF > 0, na.rm = TRUE)/sum(!is.na(logBF)), 
                     3)), 
        side = 1, line = 1, cex=2, outer = TRUE)
  mtext(paste0("p(BF > 6) = ", 
               round(sum(logBF > log(6), na.rm = TRUE)/sum(!is.na(logBF)), 
                     3), 
               ", p(BF > 10) = ", 
               round(sum(logBF > log(10), na.rm = TRUE)/sum(!is.na(logBF)), 
                     3)), 
        side = 1, line = 2.6, cex=2, outer = TRUE)
  mtext(paste0("p(BF < 1/6) = ", 
               round(sum(logBF < log(1/6), na.rm = TRUE)/sum(!is.na(logBF)), 
                     3), 
               ", p(BF < 1/10) = ", 
               round(sum(logBF < log(1/10), na.rm = TRUE)/sum(!is.na(logBF)), 
                     3)), 
        side = 1, line = 4.2, cex=2, outer = TRUE)
  abline(v=0, lwd=2, col="blue", lty="dashed")
  abline(v=log(10), lwd=2, col="blue", lty="dashed")
  abline(v=log(1/10), lwd=2, col="blue", lty="dashed")
  
}

#' Plot conditional posterior probability

plotPostProb <- function(DAEffects){
  
  par(mfrow=c(2,3))
  hist(DAEffects$H1a[DAEffects$H1a[,1]<1/6,2], ylab="Density", xlab = "Posterior Probability", main = "H1a", prob = TRUE, breaks = 30)
  hist(DAEffects$H1b[DAEffects$H1b[,1]<1/6,2], ylab="Density", xlab = "Posterior Probability", main = "H1b", prob = TRUE, breaks = 30)
  hist(DAEffects$H1c[DAEffects$H1c[,1]<1/6,2], ylab="Density", xlab = "Posterior Probability", main = "H1c", prob = TRUE, breaks = 30)
  hist(DAEffects$H1e[DAEffects$H1e[,1]<1/6,2], ylab="Density", xlab = "Posterior Probability", main = "H1e", prob = TRUE, breaks = 30)
  hist(DAEffects$H1f[DAEffects$H1f[,1]<1/6,2], ylab="Density", xlab = "Posterior Probability", main = "H1f", prob = TRUE, breaks = 30)
  
  
}

