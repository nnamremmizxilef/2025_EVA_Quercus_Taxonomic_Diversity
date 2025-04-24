# SUPPORT VECTOR MACHINE



# BASICS

# install packages if necessary
#install.packages("poppr")
#install.packages("e1071")
#install.packages("adegenet")

# load packages
library(poppr)
library(e1071)
library(adegenet)

sessionInfo()



# ORGANIZE DATA

# load training data set
genind_train <- read.structure("Data/STRUCTURE_Input/training_individuals.stru", n.ind = 194, n.loc = 50, 
                               onerowperind = T, 
                               col.lab = 1, col.pop = 2, col.others = 0,
                               row.marknames = 0, NA.char = "-9", pop = NULL, sep = NULL,
                               ask = TRUE, quiet = FALSE)

genind_train <- missingno(genind_train, type = "zero") # mean impute missing data
pop <- as.data.frame(genind_train$pop, quote=F); colnames(pop) <- "pop"
train <- cbind(pop,genind_train$tab)


# load test data (nucleotides should be coded like this: A=1, C=2, T=3, G=4)
genind_test <- read.structure("Data/STRUCTURE_Input/test_individuals.stru", n.ind = 380,  n.loc = 50, onerowperind = T,
                              col.lab = 1, col.pop = 2, col.others = 0,
                              row.marknames = 0, NA.char = "-9", pop = NULL, sep = NULL,
                              ask = TRUE, quiet = FALSE)

genind_test <- missingno(genind_test, type = "mean") # mean impute missing data
test <- genind_test$tab



# RUN MODELS

# SVM assignment
o <- matrix(0, ncol = 0, nrow = nrow(test)) 
final1 <- data.frame(o)
final2 <- data.frame(o)
final3 <- data.frame(o)

for (w in 1:10){
  
  svm_model <- svm(train$pop ~ ., data=train[,colnames(train)%in%colnames(test)], type="C-classification", probability=T, kernel="linear", epsilon=1, tolerance=0.01, cost=0.1)
  pred <- predict(svm_model,test, type="C-classification", probability=T)
  dat <- attr(pred, "probabilities")
  
  final1 <- cbind(final1,dat[,1])
  final2 <- cbind(final2,dat[,2])
  final3 <- cbind(final3,dat[,3])
}

# get assignment probabilities
Q_robur <- rowSums(final1)/10
Q_petraea <- rowSums(final2)/10
Q_pubescens <- rowSums(final3)/10
finalprobs <- cbind(Q_robur,Q_petraea,Q_pubescens)


# RESULTS SUMMARY AND VISUALIZATION

# plot and save SVM assignment probabilities
pdf("Results/SVM/SVM_Assignment.pdf", width = 20, height = 8)
barplot(t(finalprobs[,1:3]), col=c("120","blue", "1"))
dev.off()

# save SVM assignment probabilities as tables
write.table(finalprobs, file="Results/SVM/SVM_Assignment_Probabilities.txt", quote=F)
