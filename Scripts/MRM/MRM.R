# MULTIPLE REGRESSION ON DISTANCE MATRICES



# BASICS

# install packages if necessary
#install.packages("tidyverse")
#install.packages("graph4lg")
#install.packages("ggpubr")
#install.packages("vegan")
#install.packages("scales")
#install.packages("readxl")
#install.packages("phytools")
#install.packages("ecodist")

# load packages
library(tidyverse)
library(graph4lg)
library(ggpubr)
library(vegan)
library(scales)
library(readxl)
library(phytools)
library(ecodist)

sessionInfo()



# ORGANIZE DATA

# load raw data file
raw_data_full <- read_xlsx("Data/2025_EVA_data.xlsx", sheet = "Data_analyses_full")

# subset and scale environmental data
data_env <- raw_data_full[,c("prd", "tph")]
data_prd <- as.vector(data_env$prd)
data_tph <- as.vector(data_env$tph)

# subset taxonomic data
data_taxo <- raw_data_full$Q_pubescens

# subset x and y coordintes
data_xy <- as.data.frame(raw_data_full[, c("ID", "lon", "lat")])



# RUN MODELS

# define row & column names
row_column_names <- as.vector(raw_data_full$ID)

# calculate geographic distance matrix
geo_dist <- mat_geo_dist(data_xy, ID = "ID", x = "lon", y = "lat")
geo_dist_dist <- scale(geo_dist)
geo_dist_dist <- as.dist(geo_dist_dist)

# calculate tph distance matrix and distance object
tph_matrix <- as.matrix(dist(data_tph, method = "euclidean", upper = FALSE, diag = FALSE))
tph_matrix <- scale(tph_matrix)
rownames(tph_matrix) <- row_column_names
colnames(tph_matrix) <- row_column_names
tph_dist <- as.dist(tph_matrix)

# calculate prd distance matrix and distance object
prd_matrix <- as.matrix(dist(data_prd, method = "euclidean", upper = FALSE, diag = FALSE))
prd_matrix <- scale(prd_matrix)
rownames(prd_matrix) <- row_column_names
colnames(prd_matrix) <- row_column_names
prd_dist <- as.dist(prd_matrix)

# calculate taxonomic distance matrix and distance object
taxo_matrix <- as.matrix(dist(data_taxo, method = "euclidean", upper = FALSE, diag = FALSE))
rownames(taxo_matrix) <- row_column_names
colnames(taxo_matrix) <- row_column_names
taxo_dist <- as.dist(taxo_matrix)



# RUN MODELS

# calculate and save coefficients of the mrm for geographic distances and environmental factors
mrm_tph_prd <- MRM(taxo_dist ~ geo_dist_dist + prd_dist + tph_dist, nperm = 999, method = "logistic")
mrm_tph_prd$coef
write.table(mrm_tph_prd$coef, file="Results/MRM/MRM_Coefficients.txt")
