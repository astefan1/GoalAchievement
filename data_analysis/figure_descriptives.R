# ==============================================================================
# Descriptive plots
# ==============================================================================

rm(list = ls())
library(ggplot2)

############################# Load data (ITT) ##################################

# Full dataset
GAData <- read.csv("GAData_preprocessed.csv")

# Exclude participants with high GLTEQ scores
GAData_ExclGLTEQ <- GAData[GAData$follow_physAct < 190, ]

# Exclude participants with > 40 hours per week
hoursPerWeek <- rowSums(GAData[, c("follow_physActStrenuous", "follow_physActModerate", "follow_physActMild")])/2
GAData_ExclHours <- GAData[hoursPerWeek <= 40, ]

############################# Compute statistics ###############################

# Extract physical activity means
means_fulldata <- c(tapply(GAData$baseline_physAct, GAData$treatment, mean),
                    tapply(GAData$follow_physAct, GAData$treatment, mean, na.rm=TRUE))
means_ExclGLTEQ <- c(tapply(GAData_ExclGLTEQ$baseline_physAct, GAData_ExclGLTEQ$treatment, mean),
                     tapply(GAData_ExclGLTEQ$follow_physAct, GAData_ExclGLTEQ$treatment, mean, na.rm=TRUE))
means_ExclHours <- c(tapply(GAData_ExclHours$baseline_physAct, GAData_ExclHours$treatment, mean),
                     tapply(GAData_ExclHours$follow_physAct, GAData_ExclHours$treatment, mean, na.rm=TRUE))

# Merge
meansdat_PA <- data.frame(groupmeans = c(means_fulldata, means_ExclGLTEQ, means_ExclHours),
                          dataset = factor(rep(c("fulldata", "ExclGLTEQ", "ExclHours"), each = 8), levels = c("fulldata", "ExclGLTEQ", "ExclHours"), labels = c("Full Data", "GLTEQ < 190", "< 40h")),
                          condition = factor(rep(c("combiTreat", "control", "impInt", "mentCont"), 6), levels = c("control", "impInt", "mentCont", "combiTreat"), labels = c("Control", "Imp. Intentions", "Mental Contrasting", "Combined")),
                          time = factor(rep(rep(c("baseline", "follow-up"), each = 4), 3)))

# Extract automaticity means
meansdat_auto <- data.frame(groupmeans = c(tapply(GAData$baseline_automaticity, GAData$treatment, mean),
                                           tapply(GAData$follow_automaticity, GAData$treatment, mean, na.rm=TRUE)),
                            condition = factor(rep(c("combiTreat", "control", "impInt", "mentCont"), 2), levels = c("control", "impInt", "mentCont", "combiTreat"), labels = c("Control", "Imp. Intentions", "Mental Contrasting", "Combined")),
                            time = factor(rep(c("baseline", "follow-up"), each = 4)))

# Extract direct commitment means
meansdat_DComm <- data.frame(groupmeans = c(tapply(GAData$baseline_commitDirect, GAData$treatment, mean),
                                            tapply(GAData$post_commitDirect, GAData$treatment, mean, na.rm=TRUE)),
                             condition = factor(rep(c("combiTreat", "control", "impInt", "mentCont"), 2), levels = c("control", "impInt", "mentCont", "combiTreat"), labels = c("Control", "Imp. Intentions", "Mental Contrasting", "Combined")),
                             time = factor(rep(c("baseline", "post-test"), each = 4)))

# Extract indirect commitment means
meansdat_IComm <- data.frame(groupmeans = c(tapply(GAData$baseline_commitIndirect, GAData$treatment, mean),
                                            tapply(GAData$post_commitIndirect, GAData$treatment, mean, na.rm=TRUE)),
                             condition = factor(rep(c("combiTreat", "control", "impInt", "mentCont"), 2), levels = c("control", "impInt", "mentCont", "combiTreat"), labels = c("Control", "Imp. Intentions", "Mental Contrasting", "Combined")),
                             time = factor(rep(c("baseline", "post-test"), each = 4)))

################################# Plotting #####################################

# ggplot(meansdat_PA, aes(x = time, y = groupmeans, color = condition, shape = dataset)) +
#   geom_point(size = 5, alpha = 0.7) +
#   scale_color_manual(values =c("#000000", "#009292", "#B66DFF", "#DB6D00")) +
#   theme_minimal() +
#   geom_line(data = meansdat_PA[1:8,], aes(x = time, y = groupmeans, colour = condition, group = condition), alpha = 0.2) +
#   geom_line(data = meansdat_PA[9:16,], aes(x = time, y = groupmeans, colour = condition, group = condition), alpha = 0.2) +
#   geom_line(data = meansdat_PA[17:24,], aes(x = time, y = groupmeans, colour = condition, group = condition), alpha = 0.2) +
#   labs(title = "Physical Activity (GLTEQ Score)", x = "", y = "", color = "Condition", shape = "") +
#   theme(axis.text = element_text(size = 14),
#         plot.title = element_text(size = 14)) +
#   ylim(0, 30)

p1 <- ggplot(meansdat_PA[1:8, ], aes(x = time, y = groupmeans, color = condition)) +
  geom_point(size = 5, alpha = 0.7) +
  scale_color_manual(values =c("#000000", "#009292", "#B66DFF", "#DB6D00")) +
  theme_minimal() +
  geom_line(aes(x = time, y = groupmeans, colour = condition, group = condition), alpha = 0.2) +
  labs(title = "Physical Activity (GLTEQ Score)\n", x = "", y = "", color = "Condition", shape = "") +
  theme(axis.text = element_text(size = 20),
        plot.title = element_text(size = 20),
        legend.position = "bottom") +
  ylim(0, 30)

p2 <- ggplot(meansdat_PA[9:16, ], aes(x = time, y = groupmeans, color = condition)) +
  geom_point(size = 5, alpha = 0.7) +
  scale_color_manual(values =c("#000000", "#009292", "#B66DFF", "#DB6D00")) +
  theme_minimal() +
  geom_line(aes(x = time, y = groupmeans, colour = condition, group = condition), alpha = 0.2) +
  labs(title = "Physical Activity (GLTEQ Score) \n after outlier exclusions (GLTEQ > 190)", x = "", y = "", color = "Condition", shape = "") +
  theme(axis.text = element_text(size = 20),
        plot.title = element_text(size = 20),
        legend.position = "bottom") +
  ylim(0, 30)

p3 <- ggplot(meansdat_PA[17:24, ], aes(x = time, y = groupmeans, color = condition)) +
  geom_point(size = 5, alpha = 0.7) +
  scale_color_manual(values =c("#000000", "#009292", "#B66DFF", "#DB6D00")) +
  theme_minimal() +
  geom_line(aes(x = time, y = groupmeans, colour = condition, group = condition), alpha = 0.2) +
  labs(title = "Physical Activity (GLTEQ Score) \n after outlier exclusions (> 40 hours)", x = "", y = "", color = "Condition", shape = "") +
  theme(axis.text = element_text(size = 20),
        plot.title = element_text(size = 20),
        legend.position = "bottom") +
  ylim(0, 30)

p4 <- ggplot(meansdat_auto, aes(x = time, y = groupmeans, color = condition)) +
  geom_point(size = 5, alpha = 0.7) +
  geom_line(aes(x = time, y = groupmeans, colour = condition, group = condition), alpha = 0.2) +
  scale_color_manual(values =c("#000000", "#009292", "#B66DFF", "#DB6D00")) +
  theme_minimal() +
  labs(title = "Automaticity", x = "", y = "", color = "Condition", shape = "") +
  theme(axis.text = element_text(size = 20),
        plot.title = element_text(size = 20),
        legend.position = "bottom") +
  ylim(1, 4)

p5 <- ggplot(meansdat_DComm, aes(x = time, y = groupmeans, color = condition)) +
  geom_point(size = 5, alpha = 0.7) +
  geom_line(aes(x = time, y = groupmeans, colour = condition, group = condition), alpha = 0.2) +
  scale_color_manual(values =c("#000000", "#009292", "#B66DFF", "#DB6D00")) +
  theme_minimal() +
  labs(title = "Direct Commitment", x = "", y = "", color = "Condition", shape = "") +
  theme(axis.text = element_text(size = 20),
        plot.title = element_text(size = 20),
        legend.position = "bottom") +
  ylim(4,6)

p6 <- ggplot(meansdat_IComm, aes(x = time, y = groupmeans, color = condition)) +
  geom_point(size = 5, alpha = 0.7) +
  geom_line(aes(x = time, y = groupmeans, colour = condition, group = condition), alpha = 0.2) +
  scale_color_manual(values =c("#000000", "#009292", "#B66DFF", "#DB6D00")) +
  theme_minimal() +
  labs(title = "Indirect Commitment", x = "", y = "", color = "Condition", shape = "") +
  theme(axis.text = element_text(size = 20),
        plot.title = element_text(size = 20),
        legend.position = "bottom") +
  ylim(4, 6)

ggsave("./figures/figure_descriptives_1.pdf", device = "pdf", plot = p1, width = 6, height = 4.5, units = "in")
ggsave("./figures/figure_descriptives_2.pdf", device = "pdf", plot = p2, width = 6, height = 4.5, units = "in")
ggsave("./figures/figure_descriptives_3.pdf", device = "pdf", plot = p3, width = 6, height = 4.5, units = "in")
ggsave("./figures/figure_descriptives_4.pdf", device = "pdf", plot = p4, width = 6, height = 4.5, units = "in")
ggsave("./figures/figure_descriptives_5.pdf", device = "pdf", plot = p5, width = 6, height = 4.5, units = "in")
ggsave("./figures/figure_descriptives_6.pdf", device = "pdf", plot = p6, width = 6, height = 4.5, units = "in")
