# ==============================================================================
# Functions to analyze the Monte Carlo draws of the design analysis for H1
# ==============================================================================

#' Plot the means of the posterior distribution
#' @param DAresult Result object of the DA_ANCOVA() function
#' @param trueIntercept True value of the intercept parameter in the simulation
#' @param trueRR True retest reliability value in the simulation
#' @param trueImpIntEffect True value of the effect of Implementation Intentions in the simulation
#' @param trueMentContEffect True value of the effect of Mental Contrasting in the simulation
#' @param trueCombiEffect True value of the combined treatment effect in the simulation
#' @param xlimIntercept xlim values for the intercept figures
#' @param xlimBeta xlim values for the beta2, beta3, and beta4 figures
#' @param xlimBeta1 xlim value for the beta1 figures
#' @param xlimSigma xlim value for the sigma figures

analyze_DA_postMeans <- function(DAresult, trueIntercept=13.4, trueRR = 0.75, trueImpIntEffect=0, trueMentContEffect=0, trueCombiEffect=0, xlimIntercept=c(11,16), xlimBeta=c(-3,3), xlimBeta1=c(0.4,0.8), xlimSigma=c(5,7)){
  
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
  abline(v=trueImpIntEffect, lty = "dotted", lwd=2, col="blue")
  
  par(mar=c(5,4,0,2))
  
  # Intercept Histogram
  hist(postMeansSim$b_Intercept, main="", xlab = "Intercept", ylab="", xlim=xlimIntercept)
  abline(v=trueIntercept, lty = "dotted", lwd=2, col="blue")
  
  # Beta1 Histogram
  hist(postMeansSim$b_physAct_Pre_centered, main="", xlab = "beta1", ylab="", xlim=xlimBeta1)
  abline(v=trueRR, lty = "dotted", lwd=2, col="blue")
  
  # Beta2 Histogram
  hist(postMeansSim$b_treatmentimpInt, main="", xlab = "beta2", ylab="", xlim=xlimBeta)
  abline(v=trueImpIntEffect, lty = "dotted", lwd=2, col="blue")
  
  par(mar=c(0,4,0,2))
  
  # Beta3 Boxplot
  boxplot(postMeansSim$b_treatmentmentCont, xaxt = "n", yaxt = "n", bty = "n", 
          col = "white", frame = FALSE, horizontal = TRUE, ylim=xlimBeta)
  abline(v=trueMentContEffect, lty = "dotted", lwd=2, col="blue")
  
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
  abline(v=trueMentContEffect, lty = "dotted", lwd=2, col="blue")
  
  # Beta4 Histogram
  hist(postMeansSim$b_treatmentcombiTreat, main="", xlab = "beta4", ylab="", xlim=xlimBeta)
  abline(v=trueCombiEffect, lty = "dotted", lwd=2, col="blue")
  
  # Sigma Histogram
  hist(postMeansSim$sigma, main="", xlab = "sigma", ylab="", xlim=xlimSigma)
  
}

#' Posterior probabilities and Bayes factors for all sub-hypotheses for 
#' one simulation condition (sample size, retest reliability, H0/H1)
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

#' Summarize Bayes factors for all sample sizes of a certain condition (retest
#' reliability, H0/H1)
#' @param hypothesis "H0" or "H1"
#' @param rr 75 or 60 

DA_results <- function(hypothesis, rr){
  
  Ns <- seq(300, 600, by = 50)
  tests <- c("H1a", "H1b", "H1c", "H1d", "H1e", "H1f")
  res <- array(NA, dim = c(length(Ns), 9, 6))
  
  for(i in 1:length(Ns)){
    
    temp <- get(paste0("DA_", hypothesis, "_rr", rr, "_N", Ns[i]))
    tempEff <- analyze_DA_Effects(temp)
    
    for(j in 1:6){
      BF <- 1/tempEff[[tests[j]]][,1]
      res[i, , j] <- table(cut(BF, c(-Inf, 0, 1/10, 1/6, 1/3, 1, 3, 6, 10, Inf)))
    }
  }
  
  return(res)
}

#' Plot Bayes factor distributions across all sample sizes per sub-hypothesis
#' @param DA_result result object of the DA_results function
#' @param hyp Which sub-hypothesis (1 to 6 for H1a, H1b, ..., H1f)
#' @param main Plot title
#' @param truehyp Is H0 or H1 the data generating process?

plot_DAresults <- function(DA_result = res_H1_rr75, hyp = 1, main = "H1a", truehyp = "H1"){
  
  cols <- c(NA, NA, "NA", "pink", "yellow", "yellow", "green2", "green4")
  Ns <- seq(300, 600, by = 50)
  
  plot(Ns, rowSums(DA_result[, 2:5, hyp])/rowSums(DA_result[, 2:9, hyp]), ylim=c(0,1), 
       type="l", xlab = "Sample Size", ylab = "Cumulative Probability", main = main)
  polygon(c(Ns, rev(Ns)), 
          c(rowSums(DA_result[, 2:8, hyp])/rowSums(DA_result[, 2:9, hyp]), 
            rowSums(DA_result[, 2:9, hyp])/rowSums(DA_result[, 2:9, hyp])), 
          col="darkgreen")
  polygon(c(Ns, rev(Ns)), 
          c(DA_result[, 2, hyp]/rowSums(DA_result[, 2:9, hyp]), rep(0, 7)), 
          col="darkred")
  polygon(c(Ns, rev(Ns)), 
          c(rowSums(DA_result[, 2:3, hyp])/rowSums(DA_result[, 2:9, hyp]), 
            rev(DA_result[, 2, hyp]/rowSums(DA_result[, 2:9, hyp]))), 
          col="red")
  for(i in 4:8){
    polygon(c(Ns, rev(Ns)), 
            c(rowSums(DA_result[, 2:i, hyp])/rowSums(DA_result[, 2:9, hyp]), 
              rev(rowSums(DA_result[, 2:(i-1), hyp])/rowSums(DA_result[, 2:9, hyp]))), 
            col=cols[i])
  }
  abline(h=ifelse(truehyp=="H1", 0.05, 0.95), lwd=2, col="darkgrey", lty="dashed")
  points(Ns, rowSums(DA_result[, 2:5, hyp])/rowSums(DA_result[, 2:9, hyp]), 
         ylim=c(0,1), type="l", lwd=3)
  
}

