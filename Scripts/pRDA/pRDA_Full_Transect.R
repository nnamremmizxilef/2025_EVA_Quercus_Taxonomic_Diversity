# PARTIAL REDUNDANCY ANALYSIS (pRDA) FULL TRANSECT

# Uncorrelated variables: eas + hoc + vec + tpi + twi + mpi + ddg + pti + prd + tph



# BASICS

# install packages if necessary
#install.packages("vegan")
#install.packages("readxl")
#install.packages("ggplot2")

# load packages
library(vegan)
library(readxl)
library(ggplot2)

sessionInfo()



# ORGANIZE DATA

# load raw data file
raw_data_full <- read_xlsx("Data/2025_EVA_data.xlsx", sheet = "Data_analyses_full")

# subset data file into (uncorrelated) response and explanatory variables
rda_data_explain_full <- raw_data_full[, c("lon", "hoc", "vec", "tpi", "twi", "mpi", "ddg", "pti", "prd", "tph")]
rda_data_response_full <- raw_data_full[, c("Q_robur", "Q_petraea", "Q_pubescens")]

# center and standardize explanatory variables
rda_data_explain_full <- scale(rda_data_explain_full, center = TRUE, scale = TRUE)

# transform to data frame
rda_data_response_full <- as.data.frame(rda_data_response_full)
rda_data_explain_full <- as.data.frame(rda_data_explain_full)



# RUN MODELS

# pRDA analysis

# run rda function
prda_analysis_full <- rda(rda_data_response_full ~ hoc + vec + tpi + twi + mpi + ddg + pti + prd + tph + Condition (lon), data = rda_data_explain_full, na.action = na.omit)
summary(prda_analysis_full)


# follow up

# check vif (should be below 5)
vif.cca(prda_analysis_full)
# all variables have vif < 5

# check explanatory power
RsquareAdj(prda_analysis_full)$adj.r.squared

# check significance with ANOVA
anova(prda_analysis_full, perm.max = 1000)                # check if the model is significant
anova(prda_analysis_full, by = "axis", perm.max = 1000)   # check if axis 1 is significant
# model and RDA1 highly significant



# RESULTS SUMMARY AND VISUALIZATION

# define species names (w/o robur!)
spec_labels <- c(as.vector(""),as.vector("Q. petraea"), as.vector("Q. pubescens"))

# plot and save RDA
pdf(file = "Results/pRDA/pRDA_Full_Transect.pdf", width = 8, height = 5)
ordiplot(prda_analysis_full, type = "none", xlab = "RDA 1 (12.97%)", ylab = "RDA 2 (0.11%)", cex.lab = 1, xlim = c(-1,1), ylim = c(-0.5 ,0.2), main = bquote('1) Full sampling transect; adj.'~ R^2 ~ '= 0.08'))
text(prda_analysis_full, dis = "sp", col = "blue", cex = 1.2, labels = spec_labels)
text(prda_analysis_full, dis = "bp", col = "red", cex = 1.2)
dev.off()
