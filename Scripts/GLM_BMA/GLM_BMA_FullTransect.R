##### GENERALIZED LINEAR MODELS / BAYESIAN MODEL AVERAGING (FULL TRANSECT) #####

# Uncorrelated variables: eas + hoc + vec + tpi + twi + mpi + ddg + pti + prd + tph



# BASICS

# install packages if necessary
#install.packages("readxl")
#install.packages("tidyverse")
#install.packages("jtools")
#install.packages("performance")
#install.packages("gamlss")
#install.packages("ggstance")
#install.packages("BMS")
#install.packages("ggthemes")
#install.packages("viridis")

# load packages
library(readxl)
library(tidyverse)
library(jtools)
library(performance)
library(gamlss)
library(ggstance)
library(BMS)
library(ggthemes)
library(viridis)

sessionInfo()



# ORGANIZE DATA

# load raw data file
raw_data_full <- read_xlsx("Data/2025_EVA_data.xlsx", sheet = "Data_analyses_full")

# subset dataset for GLM analysis
GLM_full_transect <- subset(raw_data_full, select = -c(Q_robur, Q_petraea))

# scale dataset (only use uncorrelated explainatory variables)
GLM_full_explain <- scale(GLM_full_transect[,c("lon", "hoc", "vec", "tpi", "twi", "mpi", "ddg", "pti", "prd", "tph")])
GLM_full_response <- GLM_full_transect[,"Q_pubescens"]
GLM_full_data_final <- as.data.frame(na.omit(cbind(GLM_full_response, GLM_full_explain)))



# RUN MODELS


# BMA (Bayesian Model Averaging): Q-values ~ Environmental Factors

# define model
model_full <- bms(GLM_full_data_final, burn=1e6, iter=3e6, g="UIP", mprior="uniform", nmodel=1000, mcmc="bd") 

# get coefficient summary
coef(model_full, std.coefs= T, order.by.pip = T, include.constant = T)

# visualize and save results (pdf can be opened after closing R)
pdf("Results/GLM_BMA/BMA_Results_Full_Transect.pdf", width = 10, height = 6)
image(model_full[1:100],order=T, col=c("#440145FF","yellow"), main = "Full sampling transect (Q-values ~ E-values)")
dev.off



# Generalized Linear Model (GLM)

# create linear model for check of collinearity
GLM_full_lm_vif <- lm(Q_pubescens ~ lon + vec + hoc + ddg + tpi + mpi + twi + pti + prd + tph, data = GLM_full_data_final)

# create gamlss object of linear model and check statistics
gam_full <- gamlss(Q_pubescens ~ lon + vec + hoc + ddg + tpi + mpi + twi + pti + prd + tph, data = GLM_full_data_final, family = LOGITNO)
step_test_full <- stepGAIC(gam_full,direction="both", trace=T, k=2)
summary_step_test_full <- summary(step_test_full)
Rsq(step_test_full)
check_collinearity(GLM_full_lm_vif, ci = 0.95)
coefficients(step_test_full)



# RESULTS SUMMARY AND VISUALIZATION

# Summarize Data (not necessary here, results file is given in folder)

# write dataframe (not necessary here, file is given folder)
# GLM_results_full <- data.frame(estimate=rep(NA,nrow(summary(step_test_full))-1))
# GLM_results_full$estimate <-  step_test_full$mu.coefficients
# GLM_results_full$term <- names(step_test_full$mu.coefficients)
# GLM_results_full$error <- summary(step_test_full)[-nrow(summary(step_test_full)),2]
# GLM_results_full$conf.low <- confint(step_test_full)[-nrow(confint(step_test_full)),1]
# GLM_results_full$conf.high <- confint(step_test_full)[-nrow(confint(step_test_full)),2]
# GLM_results_full$model <- "Q~E"
# GLM_results_full <- GLM_results_full[-1,]
# GLM_results_full$term <- as.factor(GLM_results_full$term)
# GLM_results_full$model <- as.factor(GLM_results_full$model)
# names(GLM_results_full) <- c("estimate", "term", "error", "conf.low", "conf.high", "Model")
# write.csv(GLM_results_full,"Results/GLM_BMA/GLM_results_full.txt", row.names=F)
# manually add p-value levels to a colum called "p.value" (< 0.5 = ".", < 0.05 = "*", < 0.005 = "**", < 0.0005 = "***") from summary_step_test and delete insignificant factors; save file as "GLM_BMA_Results_FullTransect_final.txt"
# manually add PIP values from the BMA for significant variables

# load results data file
plot_full_input <- read.csv("Results/GLM_BMA/GLM_BMA_Results_FullTransect_final.txt", header = TRUE)
plot_full_input <- plot_full_input[order(plot_full_input$estimate),]
plot_full_input$term <- factor(plot_full_input$term, levels = plot_full_input$term[order(abs(plot_full_input$estimate))])

# visualize data
plot_full <- ggplot(data = plot_full_input, aes(y = term, x = estimate, xmin = conf.low,
              xmax = conf.high, colour = Sign)) + scale_colour_manual(values = setNames(plot_full_input$Color_Code, plot_full_input$Sign))
plot_full <- plot_full + ggstance::geom_pointrangeh(aes(y = term, x = estimate, xmin = conf.low,
              xmax = conf.high, size = PIP, colour = Sign)) + scale_radius(range = c(0.4,1.4), breaks = c(0.25,0.5,0.75,1), limits = c(0,1)) 
plot_full <- plot_full + theme_minimal() + xlim(-2,2) +
              theme(axis.title.y = element_blank(),
              legend.key.size = unit(2, "line"),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 14),
              axis.text = element_text(size = 14),
              axis.title.x = element_text(size = 16),
              panel.grid.major.x = element_line(linetype = "solid", linewidth = 0.5),
              panel.grid.minor.x = element_line(linetype = "solid", linewidth = 1),
              panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
              axis.line = element_line(linetype = "solid", linewidth = 1)) +
              xlab("Estimate") + theme(legend.position = "right") + 
              geom_text(label = plot_full_input$p.value, nudge_y = 0.1, size = 9) +
              geom_vline(xintercept = 0, linetype = 2, linewidth = 1, color = "black") +
              labs(size = "Posterior \ninclusion \nprobability", color = "Sign") +
              labs(title = "Full sampling transect", subtitle = bquote(R^2 ~ '= 0.38')) +
              theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size = 14))
plot_full

# save plot
pdf("Results/GLM_BMA/GLM_BMA_Plot_Full_Transect.pdf", width = 10, height = 6)
plot_full
dev.off()
