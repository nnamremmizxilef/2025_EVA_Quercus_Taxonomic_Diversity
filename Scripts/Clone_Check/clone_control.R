# CONTROL FOR CLONES/FULL SIBLINGS



# BASICS

# install packages if necessary
#install.packages("poppr")
#install.packages("adegenet")
#install.packages("ape")
#install.packages("dartR")

# load packages
library(poppr)
library(adegenet)
library(ape)
library(dartR)

sessionInfo()



# ORGANIZE DATA

# load structure file as genind object
oakid_genind <- read.structure("Data/STRUCTURE_Input/test_individuals.stru", onerowperind = TRUE, n.loc = 50, n.ind = 380, col.lab = 0, col.pop = 2, col.others = 1, row.marknames = 0, quiet = TRUE)

# convert to genlight, and then to snpclone object
oakid_genlight <- gi2gl(oakid_genind, parallel = FALSE, verbose = NULL)
oakid_snpclone <- as.snpclone(oakid_genlight)

# assign individual names to snpclone object
ind_names <- oakid_genlight@other
oakid_snpclone@ind.names <- ind_names$X

# check for multilocus genotypes (naive definition!)
mll(oakid_snpclone) <- "original"
oakid_snpclone

# copy table and assign tree IDs
mlg.table(oakid_snpclone)
# THIS IS ONLY FOR THE NAIVE ALGORITHM



# RUN MODELS

# look at histogramm for different filtering methods
clones_filtered <- filter_stats(oakid_snpclone, distance = bitwise.dist, plot = TRUE)

# get filtering thresholds
print(farthest_thresh <- cutoff_predictor(clones_filtered$farthest$THRESHOLDS))
print(average_thresh  <- cutoff_predictor(clones_filtered$average$THRESHOLDS))
print(nearest_thresh  <- cutoff_predictor(clones_filtered$nearest$THRESHOLDS))

# filter dataset (farthest neighbor)
mlg.filter(oakid_snpclone, distance = bitwise.dist, algorithm = "f") <- farthest_thresh
oakid_snpclone

# create output table with multilocus genotype and individual ID
mlg.table(oakid_snpclone)
mll(oakid_snpclone) <- "contracted"
mlg <- as.vector(mll(oakid_snpclone))
id <- as.vector(oakid_snpclone@ind.names)
mlg_results <- data.frame(id, mlg)

# save results
write.table(mlg_results, file="Results/Clone_Check/Individual_Mlgs.txt", row.names = FALSE, col.names = TRUE)
