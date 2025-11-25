# ==============================================================================
# Functions to analyze the Monte Carlo draws of the design analysis for H3 
# ==============================================================================

# #' Posterior probabilities and Bayes factors for all sub-hypotheses for 
#' one simulation condition (sample size, retest reliability, H0/H1)
#' @param DAresult Result object of the DA_ANCOVA() function

analyze_DA_Effects_H3 <- function(DAresult){
  
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
    
    H1b[i,2] <- sum(as_draws_df(DAresult[[i]])$b_treatmentcombiTreat-as_draws_df(DAresult[[i]])$b_treatmentimpInt > 0)/4000
    H1c[i,2] <- sum(as_draws_df(DAresult[[i]])$b_treatmentcombiTreat > 0)/4000
    H1d[i,2] <- sum(as_draws_df(DAresult[[i]])$b_treatmentimpInt-as_draws_df(DAresult[[i]])$b_treatmentmentCont < 0)/4000
    H1e[i,2] <- sum(as_draws_df(DAresult[[i]])$b_treatmentmentCont > 0)/4000

  }
  
  return(list(H1a = H1a,
              H1b = H1b,
              H1c = H1c,
              H1d = H1d,
              H1e = H1e,
              H1f = H1f))
}

#' Function to create a table with Bayes factors for each of the sub-hypotheses
#' @param DAresultH3 Result object of the analyze_DA_Effects_H2() function

BFtable_H3 <- function(DAresultH3){
  
  BFcat <- c("BF < 1/10", "1/10 < BF < 1/6", "1/6 < BF < 1/3", "1/3 < BF < 1", 
             "1 < BF < 3", "3 < BF < 6", "6 < BF < 10", "10 < BF")
  
  BFmatrix <- matrix(NA, nrow=0, ncol=8)
  for(i in 1:6){
    BFmatrix <- rbind(BFmatrix, 
                      table(cut(1/DAresultH3[[i]][,1],
                                c(-Inf, 0, 1/10, 1/6, 1/3, 1, 3, 6, 10, Inf)))[-1])
    BFmatrix[i,] <- BFmatrix[i,] / sum(BFmatrix[i,])*100
  }
  colnames(BFmatrix) <- BFcat
  rownames(BFmatrix) <- paste0("H3", letters[1:6])
  xtable(BFmatrix)
}

#' Function to create a table of posterior probabilities for each of the sub-
#' hypotheses conditional on initial BF > 6
#' @param DAresultH3 object of the analyze_DA_Effects_H2() function

postprobcat_H3 <- function(DAresultH3){
  
  postProbcat <- c(0, 0.5, 0.7, 0.9, 1.0)
  postProbMatrix <- matrix(NA, nrow=0, ncol=4)
  for(i in c(2:5)){
    postProbMatrix <- rbind(postProbMatrix,
                            table(cut(DAresultH3[[i]][DAresultH3[[i]][,1]<1/6,2], 
                                      postProbcat)))
  }
  colnames(postProbMatrix) <- c("p < 0.5", "0.5 < p < 0.7", "0.7 < p < 0.9", "0.9 < p < 1")
  rownames(postProbMatrix) <- paste0("H3", letters[c(2:5)])
  xtable(postProbMatrix)
}

#' Plot the means of the posterior distribution
#' @param DAresult Result object of the DA_ANCOVA() function
#' @param trueIntercept True value of the intercept parameter in the simulation
#' @param trueImpIntEffect True value of the effect of Implementation Intentions in the simulation
#' @param trueMentContEffect True value of the effect of Mental Contrasting in the simulation
#' @param trueCombiEffect True value of the combined treatment effect in the simulation
#' @param trueSigma True within-group standard deviation
#' @param xlimIntercept xlim values for the intercept figures
#' @param xlimBeta xlim values for the beta1, beta2, and beta3 figures
#' @param xlimSigma xlim value for the sigma figures

analyze_DA_postMeans <- function(DAresult, trueIntercept=4, trueImpIntEffect=0, trueMentContEffect=0, trueCombiEffect=0, trueSigma = 1.5, xlimIntercept=c(0,7), xlimBeta=c(-3,3), xlimSigma=c(0,10)){
  
  # extract posterior means
  N <- length(DAresult)
  postMeansSim <- matrix(NA, ncol=13, nrow=N)
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
  
  # Beta2 Boxplot
  boxplot(postMeansSim$b_treatmentimpInt, xaxt = "n", yaxt = "n", bty = "n", 
          col = "white", frame = FALSE, horizontal = TRUE, ylim=xlimBeta)
  abline(v=trueImpIntEffect, lty = "dotted", lwd=2, col="blue")
  
  # Beta3 Boxplot
  boxplot(postMeansSim$b_treatmentmentCont, xaxt = "n", yaxt = "n", bty = "n", 
          col = "white", frame = FALSE, horizontal = TRUE, ylim=xlimBeta)
  abline(v=trueMentContEffect, lty = "dotted", lwd=2, col="blue")
  
  par(mar=c(5,4,0,2))
  
  # Intercept Histogram
  hist(postMeansSim$b_Intercept, main="", xlab = "Intercept", ylab="", xlim=xlimIntercept)
  abline(v=trueIntercept, lty = "dotted", lwd=2, col="blue")
  
  # Beta2 Histogram
  hist(postMeansSim$b_treatmentimpInt, main="", xlab = "beta1", ylab="", xlim=xlimBeta)
  abline(v=trueImpIntEffect, lty = "dotted", lwd=2, col="blue")
  
  # Beta3 Histogram
  hist(postMeansSim$b_treatmentmentCont, main="", xlab = "beta2", ylab="", xlim=xlimBeta)
  abline(v=trueMentContEffect, lty = "dotted", lwd=2, col="blue")
  
  par(mar=c(0,4,0,2), cex.lab=2)
  # Beta4 Boxplot
  boxplot(postMeansSim$b_treatmentcombiTreat, xaxt = "n", yaxt = "n", bty = "n", 
          col = "white", frame = FALSE, horizontal = TRUE, ylim=xlimBeta)
  abline(v=trueCombiEffect, lty = "dotted", lwd=2, col="blue")
  
  # Sigma Boxplot
  boxplot(postMeansSim$sigma, xaxt = "n", yaxt = "n", bty = "n", 
          col = "white", frame = FALSE, horizontal = TRUE, ylim=xlimSigma)
  abline(v=trueSigma, lty = "dotted", lwd=2, col="blue")
  
  par(mar=c(5,4,0,2))
  
  plot.new()
  
  # Beta4 Histogram
  hist(postMeansSim$b_treatmentcombiTreat, main="", xlab = "beta3", ylab="", xlim=xlimBeta)
  abline(v=trueCombiEffect, lty = "dotted", lwd=2, col="blue")
  
  # Sigma Histogram
  hist(postMeansSim$sigma, main="", xlab = "sigma", ylab="", xlim=xlimSigma)
  abline(v=trueSigma, lty = "dotted", lwd=2, col="blue")
  
}
