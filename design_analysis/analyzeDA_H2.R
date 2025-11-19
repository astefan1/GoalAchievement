# ==============================================================================
# Functions to analyze the Monte Carlo draws of the design analysis for H2
# ==============================================================================

# #' Posterior probabilities and Bayes factors for all sub-hypotheses for 
#' one simulation condition (sample size, retest reliability, H0/H1)
#' @param DAresult Result object of the DA_ANCOVA() function

analyze_DA_Effects_H2 <- function(DAresult){
  
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
    H1d[i,2] <- sum(as_draws_df(DAresult[[i]])$b_treatmentimpInt-as_draws_df(DAresult[[i]])$b_treatmentmentCont > 0)/4000
    H1f[i,2] <- sum(as_draws_df(DAresult[[i]])$b_treatmentimpInt > 0)/4000
    
  }
  
  return(list(H1a = H1a,
              H1b = H1b,
              H1c = H1c,
              H1d = H1d,
              H1e = H1e,
              H1f = H1f))
}

#' Function to create a table with Bayes factors for each of the sub-hypotheses
#' @param DAresultH2 Result object of the analyze_DA_Effects_H2() function

BFtable_H2 <- function(DAresultH2){
  
  BFcat <- c("BF < 1/10", "1/10 < BF < 1/6", "1/6 < BF < 1/3", "1/3 < BF < 1", 
             "1 < BF < 3", "3 < BF < 6", "6 < BF < 10", "10 < BF")
  
  BFmatrix <- matrix(NA, nrow=0, ncol=8)
  for(i in 1:6){
    BFmatrix <- rbind(BFmatrix, 
                                 table(cut(1/DAresultH2[[i]][,1],
                                           c(-Inf, 0, 1/10, 1/6, 1/3, 1, 3, 6, 10, Inf)))[-1])
    BFmatrix[i,] <- BFmatrix[i,] / sum(BFmatrix[i,])*100
  }
  colnames(BFmatrix) <- BFcat
  rownames(BFmatrix) <- paste0("H2", letters[1:6])
  xtable(BFmatrix)
}

#' Function to create a table of posterior probabilities for each of the sub-
#' hypotheses conditional on initial BF > 6
#' @param Result object of the analyze_DA_Effects_H2() function

postprobcat_H2 <- function(DAresultH2){
  
  postProbcat <- c(0, 0.5, 0.7, 0.9, 1.0)
  postProbMatrix <- matrix(NA, nrow=0, ncol=4)
  for(i in c(1,3,4,6)){
    postProbMatrix <- rbind(postProbMatrix,
                            table(cut(DAresultH2[[i]][DAresultH2[[i]][,1]<1/6,2], 
                                      postProbcat)))
  }
  colnames(postProbMatrix) <- c("p < 0.5", "0.5 < p < 0.7", "0.7 < p < 0.9", "0.9 < p < 1")
  rownames(postProbMatrix) <- paste0("H2", letters[c(1,3,4,6)])
  xtable(postProbMatrix)
}
