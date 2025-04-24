##### STEPWISE VARIANCE INFLATION FACTOR EXCLUSION ##### 



# BASICS

# install packages if necessary
#install.packages("performance")
#install.packages("readxl")

# load packages
library(performance)
library(readxl)

sessionInfo()



# ORGANIZE DATA

# load raw data file
raw_data_full <- read_xlsx("Data/2025_EVA_data.xlsx", sheet = "Data_analyses_full")

# exclude highly correlating facotrs (selected based on Pearson correlation)
vif_data_uncorr <- subset(raw_data_full, select = -c(lat, asp, tri, lid))



# CALCULATE VARIANCE INFLATION FACTOR (VIF)

# calculate vif for Q. pubescens results (round 1)

# create linear model for vif calculation (test environmental factors against genotypes)
model_pubescens <- lm(Q_pubescens ~ lon + slp + hoc + vec + tpi + twi + mpi + ddg + pti + prd + tph, data = vif_data_uncorr)

# calculate vif and upper 95% CI
(vif_results_pubescens <- check_collinearity(model_pubescens, ci = 0.95))

# remove variables with vif > 5 (slp)
vif_data_uncorr_2 <- subset(raw_data_full, select = -c(lat, asp, tri, lid, slp))


# calculate vif for pubescens results (round 2)

# create linear model for vif calculation (test environmental factors against genotypes)
model_pubescens <- lm(Q_pubescens ~ lon + hoc + vec + tpi + twi + mpi + ddg + pti + prd + tph, data = vif_data_uncorr_2)

# calculate vif and upper 95% CI
(vif_results_pubescens <- check_collinearity(model_pubescens, ci = 0.95))

# no more variables to exclude
# remaining uncorrelated variables: lon + hoc + vec + tpi + twi + mpi + ddg + pti + prd + tph
