#TMLE code on binarized fluid variables

library(data.table)
library(haldensify)
library(tmle3shift)
library(data.table)
library(dplyr)
library(sl3) 
library(tmle3)
library(tmle3shift)
library(ggplot2)
library(SuperLearner)
library(tmle)
library(glmnet)
library(earth)
library(nnet)
library(ranger)
library(xgboost)
library(mgcv)  



## binarized fluid exposure variable, hotdeck imputation, all pts, all times

setwd("/wynton/home/pirracchio/vadims")


# new code
tmleupmc1 <- dplyr::filter(fread("UPMC_hd_fluidbins_targets.csv"), interval == 7)
tmleupmc2 <- tmleupmc1[1:10000,]

SL.library <- c("SL.glm", "SL.gam", "SL.glmnet", "SL.earth", "SL.nnet", "SL.ranger", "SL.xgboost")


fluid_vars <- c("fluid_categ0-5", "fluid_categ5-10", "fluid_categ10-15", "fluid_categ15-20", "fluid_categ20-25", 
                "fluid_categ25-30", "fluid_categ30-35", "fluid_categ35-40", "fluid_categ40-45", "fluid_categ45-50", 
                "fluid_categ50-55", "fluid_categ55-60", "fluid_categ60-65", "fluid_categ65-70", "fluid_categ70-75", 
                "fluid_categ75-80", "fluid_categ80-85", "fluid_categ85-90", "fluid_categ90-95", "fluid_categ95-100", 
                "fluid_categ100+")

covars <- c("age", "gender", "race", "weight", "hospital", "admit_year", 
            "sirs_total", "alb", "alt", "ast", "base_excess", "elix",       
            "bicarb", "bili", "bun", "cl", "creat", "dbp", "fio2", 
            "gcs", "gluc", "hr", "hgb", "inr", "lactate", "map", 
            "mechvent", "paco2", "pao2", "pf_ratio", "ph", "plt", "k", 
            "rr", "shock_index", "na", "o2_sat","sbp", "temp", "adjbw",             
            "wbc", "urine", "urine_sum", "norepi_equiv", "max_prev_norepi_equiv", 
            "icu", "egfr", "pulsepress", "surg", "ibw", "hosp_id", "height", "id", 
            "empi_id", "icu_ever_inwindow", "dtt0_hours", "interval")


#Q
sl_outcome <- SuperLearner(Y = tmleupmc2$dead, 
                           X = tmleupmc2[, covars], 
                           SL.library = SL.library, method = "method.NNLS", family = binomial())


sl_treatment <- SuperLearner(Y = tmleupmc2[[fluid_var]], 
                             X = tmleupmc2[, covars], 
                             SL.library = SL.library, method = "method.NNLS", family = binomial())



# for (fluid_var in fluid_vars) {
#   sl_outcome <- SuperLearner(Y = tmleupmc2$dead, X = tmleupmc2[, c(fluid_var, covars)], 
#                              SL.library = SL.library, method = "method.NNLS", family = binomial())
#   sl_treatment <- SuperLearner(Y = tmleupmc2[[fluid_var]], X = tmleupmc2[, covariates], 
#                                SL.library = SL.library, method = "method.NNLS", family = binomial())
#   tmle_fit <- tmle(
#     Y = tmleupmc2$dead, 
#     A = tmleupmc2[[fluid_var]], 
#     W = tmleupmc2[, covars],
#     Q.SL.library = list(Q = sl_outcome),
#     g.SL.library = list(g0 = sl_treatment, g1 = sl_treatment),
#     family = "binomial"
#   )}

# new code
output <- list()
results <- data.frame()
for (fluid_var in fluid_vars) {
  cat("fitting ", fluid_var, " effect estimate\n")
  tmle_fit <- tmle(
    Y = data_short$dead, 
    A = data_short[[fluid_var]], 
    W = data_short[, covars],
    Q.SL.library = SL.library,
    g.SL.library = SL.library,
    family = "binomial"
  )
  output[[fluid_var]] <- tmle_fit
  cat("finished fitting ", fluid_var, " effect estimate\n")
}


for (fluid_var in fluid_vars) {
  summary(tmle_fit)
  
  #estimate and CI
  results <- rbind(results, 
                   data.frame(fluid_range = fluid_var, 
                              estimate = summary(tmle_fit)$psi, 
                              lower = summary(tmle_fit)$ci.lower, 
                              upper = summary(tmle_fit)$ci.upper))
}

saveRDS(list("results" = results, "outputs" = outputs), 
        file = "insert/filepath/here.RDS")

#results$estimate <- as.numeric(results$estimate)
#results$lower <- as.numeric(results$lower)
#results$upper <- as.numeric(results$upper)


#plot
#ggplot(results, aes(x = fluid_range, y = estimate)) +
#  geom_point() +
#  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
#  theme_minimal() +
#  xlab("Fluid Volume") +
#  ylab("Estimated Causal Effect") +
#  ggtitle("Causal Effect of Fluid on Mortality") +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))


