# ==============================================================================
# DATA PREPROCESSING & PARTICIPANT DEMOGRAPHICS
# ==============================================================================

###########################  Load packages #####################################

library(readxl)

#############################  Load data #######################################

GAData <- read_excel("../Dataset MCIIAP.xlsx")

nrow(GAData) # number of recruited participants in phase 1

############## Compute study completion statistics #############################

# Create a variable that records whether participants did not participate in phase 2
varNamesFollow <- colnames(GAData)[grepl("follow", colnames(GAData))]
GAData$NoDataInPhase2 <- apply(GAData[, varNamesFollow], 1, function(x) all(is.na(x)))

sum(GAData$NoDataInPhase2) # 47 participants did not complete phase 2
nrow(GAData)-sum(GAData$NoDataInPhase2) # 613 participants had at least partial data throughout the study

# Check if there are any other missing values (FALSE = no)
any(is.na(GAData[!GAData$NoDataInPhase2, !grepl("Timing", colnames(GAData))]))

####################### Double-check scale scores ##############################

# Scale scores were initially manually computed by the first author and
# appended to the dataset. Here, we verify these calculations and replace the
# manually computed variables where necessary

# Godin Scores (check ok)
any(GAData$baseline_physAct != 5*GAData$baseline_physActModerate+9*GAData$baseline_physActStrenuous)
any(GAData$follow_physAct != 5*GAData$follow_physActModerate+9*GAData$follow_physActStrenuous, na.rm = TRUE)

# Automaticity (check ok)
any(GAData$baseline_automaticity != rowMeans(GAData[, c("baseline_automaticity1", "baseline_automaticity2", "baseline_automaticity3", "baseline_automaticity4")]))
any(GAData$follow_automaticity != rowMeans(GAData[, c("follow_automaticity1", "follow_automaticity2", "follow_automaticity3", "follow_automaticity4")]), na.rm=TRUE)

# Self-determination: Amotivation (variable replaced due to minor rounding errors)
GAData$baseline_selfdet_amot <- rowMeans(GAData[, c("baseline_selfdet_amot1", "baseline_selfdet_amot2", "baseline_selfdet_amot3")])

# Self-determination: External Regulation (variable replaced due to minor rounding errors)
GAData$baseline_selfdet_extReg <- rowMeans(GAData[, c("baseline_selfdet_extReg1", "baseline_selfdet_extReg2", "baseline_selfdet_extReg3")])

# Self-determination: Introjection (variable replaced due to minor rounding errors)
GAData$baseline_selfdet_intro <- rowMeans(GAData[, c("baseline_selfdet_intro1", "baseline_selfdet_intro2", "baseline_selfdet_intro3")])

# Self-determination: Integrated regulation (variable replaced due to minor rounding errors)
GAData$baseline_selfdet_intReg <- rowMeans(GAData[, c("baseline_selfdet_intReg1", "baseline_selfdet_intReg2", "baseline_selfdet_intReg3")])

# Self-determination: Identified regulation (variable replaced due to minor rounding errors)
GAData$baseline_selfdet_idReg != rowMeans(GAData[, c("baseline_selfdet_idReg1", "baseline_selfdet_idReg2", "baseline_selfdet_idReg3")])

# Socio-cognitive attitudes (check ok)
GAData$baseline_sociocog_att4ReverseCoded_recoded <- 8-GAData$baseline_sociocog_att4ReverseCoded
GAData$baseline_sociocog_att5ReverseCoded_recoded <- 8-GAData$baseline_sociocog_att5ReverseCoded
any(GAData$baseline_sociocog_att != rowMeans(GAData[, c("baseline_sociocog_att1", "baseline_sociocog_att2", "baseline_sociocog_att3", "baseline_sociocog_att4ReverseCoded_recoded", "baseline_sociocog_att5ReverseCoded_recoded")]))

# Socio-cognitive subjective norms (check ok)
any(GAData$baseline_sociocog_subjnorm != GAData$baseline_sociocog_subjnorm1)

# Socio-cognitive behavioral control (variable replaced due to minor rounding errors)
GAData$baseline_sociocog_behControl2ReverseCoded_recoded <- 8-GAData$baseline_sociocog_behControl2ReverseCoded
GAData$baseline_sociocog_behControl <- rowMeans(GAData[, c("baseline_sociocog_behControl1", "baseline_sociocog_behControl2ReverseCoded_recoded", "baseline_sociocog_behControl3")])

# Goal commitment direct (variable replaced due to minor rounding errors)
GAData$baseline_commitDirect <- rowMeans(GAData[, c("baseline_commitDirect1", "baseline_commitDirect2", "baseline_commitDirect3")])
GAData$post_commitDirect <- rowMeans(GAData[, c("post_commitDirect1", "post_commitDirect2", "post_commitDirect3")])

# Goal commitment indirect 
any(GAData$baseline_commitIndirect != rowMeans(GAData[, c("baseline_commitIndirect1", "baseline_commitIndirect2", "baseline_commitIndirect3", "baseline_commitIndirect4")]))
any(GAData$post_commitIndirect != rowMeans(GAData[, c("post_commitIndirect1", "post_commitIndirect2", "post_commitIndirect3", "post_commitIndirect4")]))

write.csv(GAData, file = "GAData_preprocessed.csv")

######################## Demographics ##########################################

# Age
mean(GAData$Age)
sd(GAData$Age)

mean(GAData$Age[GAData$NoDataInPhase2])
sd(GAData$Age[GAData$NoDataInPhase2])

# Gender
table(GAData$Gender[GAData$NoDataInPhase2])
table(GAData$Gender)

# SocioProfessionalCat

table(GAData$SocioProfessionalCat[GAData$NoDataInPhase2])
sum((GAData$SocioProfessionalCat[!GAData$NoDataInPhase2] != "Student") &(GAData$SocioProfessionalCat[!GAData$NoDataInPhase2] != "Unemployed"))

table(GAData$SocioProfessionalCat)
660-sum((GAData$SocioProfessionalCat == "Students")) - sum((GAData$SocioProfessionalCat == "Unemployed")) - sum((GAData$SocioProfessionalCat == "Other"))

