# ==============================================================================
# SUMMARY OF DESIGN ANALYSIS RESULTS ACROSS ALL CONDITIONS
# ==============================================================================

# Load all the analysis files (DA_H...) from generated_data (one by one to 
# prevent exceeding RAM)

# Load required functions and packages
source("analyzeDA_H1.R")
library(brms)
library(xtable)

################ Compute BF distributions for all conditions ###################

res_H1_rr75 <- DA_results(hypothesis = "H1", rr = 75)
save(res_H1_rr75, file = "./generated_data/res_H1_rr75.RData")
res_H1_rr60 <- DA_results(hypothesis = "H1", rr = 60)
save(res_H1_rr60, file = "./generated_data/res_H1_rr60.RData")
res_H0_rr75 <- DA_results(hypothesis = "H0", rr = 75)
save(res_H0_rr75, file = "./generated_data/res_H0_rr75.RData")
res_H0_rr60 <- DA_results(hypothesis = "H0", rr = 60)
save(res_H0_rr60, file = "./generated_data/res_H0_rr60.RData")

################ Plot preparation: BF distributions ############################

# the legend
plot.new()
legend("topleft", legend=rep(NA, 7), fill = c("darkred", "red", "pink", "yellow", 
                                              "green2", "green4", "darkgreen"),
       horiz=TRUE, bty = "n", cex=2, col="n")

# Bayes factor categories (relevant for all plots)
BFcat <- c("BF < 1/10", "1/10 < BF < 1/6", "1/6 < BF < 1/3", "1/3 < BF < 1", 
           "1 < BF < 3", "3 < BF < 6", "6 < BF < 10", "10 < BF")

####################### BF distribution plots ##################################

# H1, rr = .75

plot_DAresults(DA_result = res_H1_rr75, hyp = 1, main = "H1a") 
plot_DAresults(DA_result = res_H1_rr75, hyp = 2, main = "H1b")
plot_DAresults(DA_result = res_H1_rr75, hyp = 3, main = "H1c")
plot_DAresults(DA_result = res_H1_rr75, hyp = 4, main = "H1d", truehyp = "H0")
plot_DAresults(DA_result = res_H1_rr75, hyp = 5, main = "H1e")
plot_DAresults(DA_result = res_H1_rr75, hyp = 6, main = "H1f")

# H1, rr = .60

plot_DAresults(DA_result = res_H1_rr60, hyp = 1, main = "H1a") 
plot_DAresults(DA_result = res_H1_rr60, hyp = 2, main = "H1b")
plot_DAresults(DA_result = res_H1_rr60, hyp = 3, main = "H1c")
plot_DAresults(DA_result = res_H1_rr60, hyp = 4, main = "H1d", truehyp = "H0")
plot_DAresults(DA_result = res_H1_rr60, hyp = 5, main = "H1e")
plot_DAresults(DA_result = res_H1_rr60, hyp = 6, main = "H1f")

# H0, rr = .75

plot_DAresults(DA_result = res_H0_rr75, hyp = 1, main = "H1a", truehyp = "H0") 
plot_DAresults(DA_result = res_H0_rr75, hyp = 2, main = "H1b", truehyp = "H0")
plot_DAresults(DA_result = res_H0_rr75, hyp = 3, main = "H1c", truehyp = "H0")
plot_DAresults(DA_result = res_H0_rr75, hyp = 4, main = "H1d", truehyp = "H0")
plot_DAresults(DA_result = res_H0_rr75, hyp = 5, main = "H1e", truehyp = "H0")
plot_DAresults(DA_result = res_H0_rr75, hyp = 6, main = "H1f", truehyp = "H0")

# H0, rr = .60

plot_DAresults(DA_result = res_H0_rr60, hyp = 1, main = "H1a", truehyp = "H0") 
plot_DAresults(DA_result = res_H0_rr60, hyp = 2, main = "H1b", truehyp = "H0")
plot_DAresults(DA_result = res_H0_rr60, hyp = 3, main = "H1c", truehyp = "H0")
plot_DAresults(DA_result = res_H0_rr60, hyp = 4, main = "H1d", truehyp = "H0")
plot_DAresults(DA_result = res_H0_rr60, hyp = 5, main = "H1e", truehyp = "H0")
plot_DAresults(DA_result = res_H0_rr60, hyp = 6, main = "H1f", truehyp = "H0")

################## BF distribution tables for N=500 and N=600 ##################

# H1, rr = .75

BFtab_N500 <- round(t(res_H1_rr75[5, 2:9, ])/rowSums(t(res_H1_rr75[5, 2:9, ])), 3)*100
colnames(BFtab_N500) <- BFcat
rownames(BFtab_N500) <- paste0("H1", letters[1:6])
xtable(BFtab_N500)

BFtab_N600 <- round(t(res_H1_rr75[7, 2:9, ])/rowSums(t(res_H1_rr75[7, 2:9, ])), 3)*100
colnames(BFtab_N600) <- BFcat
rownames(BFtab_N600) <- paste0("H1", letters[1:6])
xtable(BFtab_N600)

# H1, rr = .60

BFtab_N500 <- round(t(res_H1_rr60[5, 2:9, ])/rowSums(t(res_H1_rr60[5, 2:9, ])), 3)*100
colnames(BFtab_N500) <- BFcat
rownames(BFtab_N500) <- paste0("H1", letters[1:6])
xtable(BFtab_N500)

BFtab_N600 <- round(t(res_H1_rr60[7, 2:9, ])/rowSums(t(res_H1_rr60[7, 2:9, ])), 3)*100
colnames(BFtab_N600) <- BFcat
rownames(BFtab_N600) <- paste0("H1", letters[1:6])
xtable(BFtab_N600)

# H0, rr = .75

BFtab_N500 <- round(t(res_H0_rr75[5, 2:9, ])/rowSums(t(res_H0_rr75[5, 2:9, ])), 3)*100
colnames(BFtab_N500) <- BFcat
rownames(BFtab_N500) <- paste0("H0", letters[1:6])
xtable(BFtab_N500)

BFtab_N600 <- round(t(res_H0_rr75[7, 2:9, ])/rowSums(t(res_H0_rr75[7, 2:9, ])), 3)*100
colnames(BFtab_N600) <- BFcat
rownames(BFtab_N600) <- paste0("H1", letters[1:6])
xtable(BFtab_N600)

# H0, rr = .60

BFtab_N500 <- round(t(res_H0_rr60[5, 2:9, ])/rowSums(t(res_H0_rr60[5, 2:9, ])), 3)*100
colnames(BFtab_N500) <- BFcat
rownames(BFtab_N500) <- paste0("H0", letters[1:6])
xtable(BFtab_N500)

BFtab_N600 <- round(t(res_H0_rr60[7, 2:9, ])/rowSums(t(res_H0_rr60[7, 2:9, ])), 3)*100
colnames(BFtab_N600) <- BFcat
rownames(BFtab_N600) <- paste0("H1", letters[1:6])
xtable(BFtab_N600)

########## Conditional Posterior Probabilities for N=500 and N=600 #############

postProbcat <- c(0, 0.5, 0.7, 0.9, 1.0)
cond <- c("H1, r=.75, N=500", "H1, r=.75, N=600", "H1, r=.60, N=500", 
          "H1, r=.60, N=600", "H0, r=.75, N=500", "H0, r=.75, N=600", 
          "H0, r=.60, N=500", "H0, r=.60, N=600")

postProb_H1_rr75_N500 <- analyze_DA_Effects(DA_H1_rr75_N500)
postProb_H1_rr75_N600 <- analyze_DA_Effects(DA_H1_rr75_N600)
postProb_H1_rr60_N500 <- analyze_DA_Effects(DA_H1_rr60_N500)
postProb_H1_rr60_N600 <- analyze_DA_Effects(DA_H1_rr60_N600)
postProb_H0_rr75_N500 <- analyze_DA_Effects(DA_H0_rr75_N500)
postProb_H0_rr75_N600 <- analyze_DA_Effects(DA_H0_rr75_N600)
postProb_H0_rr60_N500 <- analyze_DA_Effects(DA_H0_rr60_N500)
postProb_H0_rr60_N600 <- analyze_DA_Effects(DA_H0_rr60_N600)

condProbH1a <- rbind(
  table(cut(postProb_H1_rr75_N500$H1a[postProb_H1_rr75_N500$H1a[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H1_rr75_N600$H1a[postProb_H1_rr75_N600$H1a[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H1_rr60_N500$H1a[postProb_H1_rr60_N500$H1a[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H1_rr60_N600$H1a[postProb_H1_rr60_N600$H1a[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H0_rr75_N500$H1a[postProb_H0_rr75_N500$H1a[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H0_rr75_N600$H1a[postProb_H0_rr75_N600$H1a[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H0_rr60_N500$H1a[postProb_H0_rr60_N500$H1a[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H0_rr60_N600$H1a[postProb_H0_rr60_N600$H1a[,1]<1/6,2], postProbcat))
)
rownames(condProbH1a) <- cond

condProbH1b <- rbind(
  table(cut(postProb_H1_rr75_N500$H1b[postProb_H1_rr75_N500$H1b[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H1_rr75_N600$H1b[postProb_H1_rr75_N600$H1b[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H1_rr60_N500$H1b[postProb_H1_rr60_N500$H1b[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H1_rr60_N600$H1b[postProb_H1_rr60_N600$H1b[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H0_rr75_N500$H1b[postProb_H0_rr75_N500$H1b[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H0_rr75_N600$H1b[postProb_H0_rr75_N600$H1b[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H0_rr60_N500$H1b[postProb_H0_rr60_N500$H1b[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H0_rr60_N600$H1b[postProb_H0_rr60_N600$H1b[,1]<1/6,2], postProbcat))
)
rownames(condProbH1b) <- cond

condProbH1c <- rbind(
  table(cut(postProb_H1_rr75_N500$H1c[postProb_H1_rr75_N500$H1c[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H1_rr75_N600$H1c[postProb_H1_rr75_N600$H1c[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H1_rr60_N500$H1c[postProb_H1_rr60_N500$H1c[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H1_rr60_N600$H1c[postProb_H1_rr60_N600$H1c[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H0_rr75_N500$H1c[postProb_H0_rr75_N500$H1c[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H0_rr75_N600$H1c[postProb_H0_rr75_N600$H1c[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H0_rr60_N500$H1c[postProb_H0_rr60_N500$H1c[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H0_rr60_N600$H1c[postProb_H0_rr60_N600$H1c[,1]<1/6,2], postProbcat))
)
rownames(condProbH1c) <- cond

condProbH1e <- rbind(
  table(cut(postProb_H1_rr75_N500$H1e[postProb_H1_rr75_N500$H1e[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H1_rr75_N600$H1e[postProb_H1_rr75_N600$H1e[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H1_rr60_N500$H1e[postProb_H1_rr60_N500$H1e[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H1_rr60_N600$H1e[postProb_H1_rr60_N600$H1e[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H0_rr75_N500$H1e[postProb_H0_rr75_N500$H1e[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H0_rr75_N600$H1e[postProb_H0_rr75_N600$H1e[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H0_rr60_N500$H1e[postProb_H0_rr60_N500$H1e[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H0_rr60_N600$H1e[postProb_H0_rr60_N600$H1e[,1]<1/6,2], postProbcat))
)
rownames(condProbH1e) <- cond

condProbH1f <- rbind(
  table(cut(postProb_H1_rr75_N500$H1f[postProb_H1_rr75_N500$H1f[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H1_rr75_N600$H1f[postProb_H1_rr75_N600$H1f[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H1_rr60_N500$H1f[postProb_H1_rr60_N500$H1f[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H1_rr60_N600$H1f[postProb_H1_rr60_N600$H1f[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H0_rr75_N500$H1f[postProb_H0_rr75_N500$H1f[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H0_rr75_N600$H1f[postProb_H0_rr75_N600$H1f[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H0_rr60_N500$H1f[postProb_H0_rr60_N500$H1f[,1]<1/6,2], postProbcat)),
  table(cut(postProb_H0_rr60_N600$H1f[postProb_H0_rr60_N600$H1f[,1]<1/6,2], postProbcat))
)
rownames(condProbH1f) <- cond

xtable(condProbH1a)
xtable(condProbH1b)
xtable(condProbH1c)
xtable(condProbH1e)
xtable(condProbH1f)

####################### Parameter Recovery #####################################

par(oma=c(0,0,6,0))

analyze_DA_postMeans(DA_H1_rr75_N500, 
                     trueIntercept=13.4, 
                     trueRR = 0.75,
                     trueImpIntEffect=2.5,
                     trueMentContEffect=2.5,
                     trueCombiEffect=5, 
                     xlimIntercept=c(11,16), 
                     xlimBeta=c(0, 8), 
                     xlimBeta1=c(0.6,1), 
                     xlimSigma=c(3.5,6))
mtext("H1, r = .75, N = 500", side=3, outer=TRUE, cex = 2, line = 1.7)
analyze_DA_postMeans(DA_H1_rr75_N600, 
                     trueIntercept=13.4, 
                     trueRR = 0.75,
                     trueImpIntEffect=2.5,
                     trueMentContEffect=2.5,
                     trueCombiEffect=5, 
                     xlimIntercept=c(11,16), 
                     xlimBeta=c(0, 8), 
                     xlimBeta1=c(0.6,1), 
                     xlimSigma=c(3.5,6))
mtext("H1, r = .75, N = 600", side=3, outer=TRUE, cex = 2, line = 1.7)

analyze_DA_postMeans(DA_H1_rr60_N500, 
                     trueIntercept=13.4, 
                     trueRR = 0.6,
                     trueImpIntEffect=2.5,
                     trueMentContEffect=2.5,
                     trueCombiEffect=5, 
                     xlimIntercept=c(11,16), 
                     xlimBeta=c(0,8), 
                     xlimBeta1=c(0.4,0.8), 
                     xlimSigma=c(5,7))
mtext("H1, r = .60, N = 500", side=3, outer=TRUE, cex = 2, line = 1.7)
analyze_DA_postMeans(DA_H1_rr60_N600, 
                     trueIntercept=13.4, 
                     trueRR = 0.6,
                     trueImpIntEffect=2.5,
                     trueMentContEffect=2.5,
                     trueCombiEffect=5, 
                     xlimIntercept=c(11,16), 
                     xlimBeta=c(0,8), 
                     xlimBeta1=c(0.4,0.8), 
                     xlimSigma=c(5,7))
mtext("H1, r = .60, N = 600", side=3, outer=TRUE, cex = 2, line = 1.7)

analyze_DA_postMeans(DA_H0_rr75_N500, 
                     trueIntercept=13.4, 
                     trueRR = 0.75,
                     trueImpIntEffect=0,
                     trueMentContEffect=0,
                     trueCombiEffect=0, 
                     xlimIntercept=c(11,16), 
                     xlimBeta=c(-3,3), 
                     xlimBeta1=c(0.6,1), 
                     xlimSigma=c(3.5,6))
mtext("H0, r = .75, N = 500", side=3, outer=TRUE, cex = 2, line = 1.7)
analyze_DA_postMeans(DA_H0_rr75_N600, 
                     trueIntercept=13.4, 
                     trueRR = 0.75,
                     trueImpIntEffect=0,
                     trueMentContEffect=0,
                     trueCombiEffect=0, 
                     xlimIntercept=c(11,16), 
                     xlimBeta=c(-3,3), 
                     xlimBeta1=c(0.6,1), 
                     xlimSigma=c(3.5,6))
mtext("H0, r = .75, N = 600", side=3, outer=TRUE, cex = 2, line = 1.7)

analyze_DA_postMeans(DA_H0_rr60_N500, 
                     trueIntercept=13.4, 
                     trueRR = 0.6,
                     trueImpIntEffect=0,
                     trueMentContEffect=0,
                     trueCombiEffect=0, 
                     xlimIntercept=c(11,16), 
                     xlimBeta=c(-3,3), 
                     xlimBeta1=c(0.4,0.8), 
                     xlimSigma=c(5,7))
mtext("H0, r = .60, N = 500", side=3, outer=TRUE, cex = 2, line = 1.7)
analyze_DA_postMeans(DA_H0_rr60_N600, 
                     trueIntercept=13.4, 
                     trueRR = 0.6,
                     trueImpIntEffect=0,
                     trueMentContEffect=0, 
                     trueCombiEffect=0, 
                     xlimIntercept=c(11,16), 
                     xlimBeta=c(-3,3), 
                     xlimBeta1=c(0.4,0.8), 
                     xlimSigma=c(5,7))
mtext("H0, r = .60, N = 600", side=3, outer=TRUE, cex = 2, line = 1.7)

