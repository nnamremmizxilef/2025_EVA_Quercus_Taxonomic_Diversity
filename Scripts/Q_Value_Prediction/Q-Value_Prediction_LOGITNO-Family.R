# GAMLSS Q-VALUE PREDICTION

# sph and prd as explainatory variables
# Q-values Q. pubescens as response variable



# BASICS

# install packages if necessary
#install.packages("readxl")
#install.packages("gamlss")
#install.packages("lattice")

# load packages
library(readxl)
library(gamlss)
library(lattice)

sessionInfo()



# ORGANIZE DATA

# load raw data file
raw_data_full <- read_xlsx("Data/2025_EVA_data.xlsx", sheet = "Data_analyses_full")

# subset data needed for gamlss analysis
gam_input <- raw_data_full[, c("ID", "Q_pubescens", "prd", "tph")]



# RUN MODELS

# fit additive loess fits (with default span of 0.5)

# degree 1
r11 <- gamlss(Q_pubescens ~ lo(~ prd, df = 3) + lo(~ tph, df = 3), data=gam_input, family=LOGITNO)

# degree 2
r12 <- gamlss(Q_pubescens ~ lo(~ prd, df = 3) + lo(~ tph, df = 3), data=gam_input, family=LOGITNO)

# select the best model based on AIC (penalty K = 2,5)
AIC(r11,r12, k = 2.5)
# both models equally good

# test if span can improve model
r112 <- gamlss(Q_pubescens ~ lo(~ prd, span = 0.2, df = 3) + lo(~ tph, span = 0.2, df = 3), data=gam_input, family=LOGITNO)
r114 <- gamlss(Q_pubescens ~ lo(~ prd, span = 0.4, df = 3) + lo(~ tph, span = 0.4, df = 3), data=gam_input, family=LOGITNO)
r116 <- gamlss(Q_pubescens ~ lo(~ prd, span = 0.6, df = 3) + lo(~ tph, span = 0.6, df = 3), data=gam_input, family=LOGITNO)
r118 <- gamlss(Q_pubescens ~ lo(~ prd, span = 0.8, df = 3) + lo(~ tph, span = 0.8, df = 3), data=gam_input, family=LOGITNO)

# select the best model based on AIC (penalty K = 2,5)
AIC(r11, r112, r114, r116, r118, k = 2.5)
# all models equally good



# RESULTS SUMMARY AND VISUALIZATION

# create data frame for predictions and predict Q-values
new_q <- data.frame(expand.grid(prd = seq(10, 140, 5), tph = seq(3.4, 7.6, 0.1)))
new_q$pred <- predict(r11, newdata = new_q, type = "response")
prdn <- seq(10, 140, 5)
tphn <- seq(3.4, 7.6, 0.1)
op <- par(mfrow = c(1,1))

# contour plot
pdf("Results/Q_Value_Prediction/Contour_Plot.pdf")
contour(prdn, tphn, matrix(new_q$pred, nrow = length(prdn)), nlevels = 30,
xlab = "Potential rooting depth (cm)", ylab = "Topsoil pH")
dev.off()

# wireframe plot
pdf("Results/Q_Value_Prediction/Wireframe_Plot.pdf", width = 10, height = 8)
wireframe(pred ~ prd*tph, new_q, drape = TRUE, aspect = c(87/100, 0.8),
          par.settings = list(axis.line = list(col = 'transparent')),
          scales = list(arrows = FALSE, col = "black"), zlim = c(0,1),
          cex.title = 1,
          colorkey = list(space="bottom",height=0.8,title = "Predicted Quercus pubescens Q-values"),
          border = FALSE,
          xlab = "                           Potential rooting depth (cm)",
          ylab = "Topsoil pH", zlab = " Predicted\n Q-values")
dev.off()

