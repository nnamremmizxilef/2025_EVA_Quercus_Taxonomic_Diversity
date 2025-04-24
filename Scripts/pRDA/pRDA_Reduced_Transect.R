# PARTIAL REDUNDANCY ANALYSIS (pRDA) REDUCED TRANSECT

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
raw_data_reduced <- read_xlsx("Data/2025_EVA_data.xlsx", sheet = "Data_analyses_reduced")

# subset data file into (uncorrelated) response and explanatory variables
rda_data_explain_reduced <- raw_data_reduced[, c("lon", "hoc", "vec", "tpi", "twi", "mpi", "ddg", "pti", "prd", "tph")]
rda_data_response_reduced <- raw_data_reduced[, c("Q_robur", "Q_petraea", "Q_pubescens")]

# center and standardize explanatory variables
rda_data_explain_reduced <- scale(rda_data_explain_reduced, center = TRUE, scale = TRUE)

# transform to data frame
rda_data_response_reduced <- as.data.frame(rda_data_response_reduced)
rda_data_explain_reduced <- as.data.frame(rda_data_explain_reduced)



# RUN MODELS


# pRDA analysis

# run rda function
prda_analysis_reduced <- rda(rda_data_response_reduced ~ hoc + vec + tpi + twi + mpi + ddg + pti + prd + tph + Condition (lon), data = rda_data_explain_reduced, na.action = na.exclude)
summary(prda_analysis_reduced)


# follow up

# check vif (should be below 5)
vif.cca(prda_analysis_reduced)
# all variables have vif < 5

# check explanatory power
RsquareAdj(prda_analysis_reduced)$adj.r.squared

# check significance with ANOVA
anova(prda_analysis_reduced, perm.max = 1000)                # check if the model is significant
anova(prda_analysis_reduced, by = "axis", perm.max = 1000)   # check if axis 1 is significant
# model and RDA1 highly significant



# RESULTS SUMMARY AND VISUALIZATION

# define species names (w/o robur!)
spec_labels <- c(as.vector(""),as.vector("Q. petraea"), as.vector("Q. pubescens"))

# plot and save RSA
pdf(file = "Results/pRDA/pRDA_reduced_transect.pdf", width = 8, height = 5)
ordiplot(prda_analysis_reduced, type = "none", xlab = "RDA 1 (17.21%)", ylab = "RDA 2 (0.03%)", cex.lab = 1, xlim = c(-1, 1), ylim = c(-0.5 ,0.5), scaling = 0, main = bquote('2) Reduced sampling transect; adj.'~ R^2 ~ '= 0.13'))
text(prda_analysis_reduced, dis = "sp", col = "blue", cex = 1.2, labels = spec_labels)
text(prda_analysis_reduced, dis = "bp", col = "red", cex = 1.2)
dev.off()
