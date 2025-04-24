##### PAIRWISE PEARSON CORRELATION #####



# BASICS

# install packages if necessary
#install.packages("corrplot")
#install.packages("readxl")


# load packages
library(corrplot)
library(readxl)

sessionInfo()



# ORGANIZE DATA

# load raw data file
raw_data_full <- read_xlsx("Data/2025_EVA_data.xlsx", sheet = "Data_analyses_full")

# subset data for pearson correlation
pearson_data <- raw_data_full[, c("lon", "lat", "alt", "slp", "asp", "tri", "hoc", "vec", "tpi", "twi", "mpi", "ddg", "pti", "prd", "lid", "tph", "sph")]



# CHECK PEARSON CORRELATION

# create correlation matrix (w/o soil type -> categorical)
correlation_matrix <- cor(na.omit(pearson_data))

# visualize correlation matrix with numbers and save to results
pdf(file = "Results/Multicollinearity/Pairwise_Pearson.pdf", width = 10, height = 10)
corrplot(correlation_matrix, method = "number", tl.col = "black", tl.srt = 45, number.cex = 1, type = "upper")
dev.off()

# exclude one of each factor-pair with R^2 > 0,75
# chosen based on biological relevance: lat, asp, tri, lid
