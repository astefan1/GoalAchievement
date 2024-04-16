# ==============================================================================
# Simulation of synthetic dataset for preregistration
# ==============================================================================

# The following dataset is simulated to give an impression of what the final
# dataset will look like after data collection and exclusion of participants
# who did not meet the inclusion criteria. Unlike the data simulation for the 
# design analyses that give primacy to realistic effect sizes, this simulation
# focuses on ecological validity, i.e., restricting the simulated values to 
# values that occur on Likert scales, and on running the proposed analyses
# on these data.

set.seed(1234)
preregDat <- data.frame(matrix(NA, nrow=660, ncol=0))

# Physical Activity at Baseline: Godin Score
preregDat$baseline_physAct <- sample(c(0, 5, 9, 14, 15, 18, 19, 20, 23), replace = TRUE, size = 660)

# Experienced Automaticity at Baseline
preregDat$baseline_automaticity1 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_automaticity2 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_automaticity3 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_automaticity4 <- sample(1:7, replace = TRUE, size = 660)

# Self-Determination at Baseline
preregDat$baseline_selfdet_amot1 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_selfdet_amot2 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_selfdet_amot3 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_selfdet_extReg1 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_selfdet_extReg2 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_selfdet_extReg3 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_selfdet_intro1 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_selfdet_intro2 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_selfdet_intro3 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_selfdet_idReg1 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_selfdet_idReg2 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_selfdet_idReg3 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_selfdet_intReg1 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_selfdet_intReg2 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_selfdet_intReg3 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_selfdet_intMot1 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_selfdet_intMot2 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_selfdet_intMot3 <- sample(1:7, replace = TRUE, size = 660)

# Sociocognitive Variables at Baseline
preregDat$baseline_sociocog_att1 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_sociocog_att2 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_sociocog_att3 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_sociocog_att4 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_sociocog_att5 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_sociocog_subjnorm1 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_sociocog_behControl1 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_sociocog_behControl2 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_sociocog_behControl3 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_sociocog_intStrength1 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_sociocog_intStrength2 <- sample(1:7, replace = TRUE, size = 660)
preregDat$baseline_sociocog_intStrength3 <- sample(1:7, replace = TRUE, size = 660)

# Treatment Group
preregDat$treatment <- rep(c("control", "impInt", "mentCont", "combiTreat"), length.out=660)

# Goal Commitment Post Intervention
preregDat$post_commitment1 <- sample(1:7, replace = TRUE, size = 660)
preregDat$post_commitment2 <- sample(1:7, replace = TRUE, size = 660)
preregDat$post_commitment3 <- sample(1:7, replace = TRUE, size = 660)
preregDat$post_affcommitment1 <- sample(1:7, replace = TRUE, size = 660)
preregDat$post_affcommitment2 <- sample(1:7, replace = TRUE, size = 660)
preregDat$post_affcommitment3 <- sample(1:7, replace = TRUE, size = 660)

# Sociodemographic Information 
preregDat$gender <- sample(c("male", "female"), size = 660, replace = TRUE)
preregDat$age <- round(rnorm(660, mean=33, sd=5))
preregDat$age[preregDat$age<18] <- 18

# Physical Activity at Follow-Up
preregDat$follow_moderateActivity <- sample(c(0:7), size = 660, replace = TRUE)
preregDat$follow_strenuousActivity <- sample(c(0:7), size = 660, replace = TRUE)
preregDat$follow_physAct <- 5*preregDat$follow_moderateActivity+9*preregDat$follow_strenuousActivity 

# Experienced Automaticity at Follow-Up
preregDat$follow_automaticity1 <- sample(1:7, replace = TRUE, size = 660)
preregDat$follow_automaticity2 <- sample(1:7, replace = TRUE, size = 660)
preregDat$follow_automaticity3 <- sample(1:7, replace = TRUE, size = 660)
preregDat$follow_automaticity4 <- sample(1:7, replace = TRUE, size = 660)

save(preregDat, file="./generated_data/preregDat.RData")
