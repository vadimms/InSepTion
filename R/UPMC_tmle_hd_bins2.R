#TMLE code on binarized fluid variables -- take 2

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
#library(nnet)
library(ranger)
library(xgboost)
library(mgcv)  



## binarized fluid exposure variable, hotdeck imputation, all pts

setwd("/wynton/home/pirracchio/vadims")


tmleupmc1 <- dplyr::filter(fread("UPMC_hd_fluidbins_targets.csv"), interval == 7)
tmleupmc2 <- tmleupmc1[1:1000,]

SL.library <- c("SL.glm", "SL.gam", "SL.glmnet", "SL.earth", "SL.ranger", "SL.xgboost")

#"SL.nnet"

fluid_vars <- c("fluid_categ0-5", "fluid_categ5-10", "fluid_categ10-15", "fluid_categ15-20", "fluid_categ20-25", 
                "fluid_categ25-30", "fluid_categ30-35", "fluid_categ35-40", "fluid_categ40-45", "fluid_categ45-50", 
                "fluid_categ50-55", "fluid_categ55-60", "fluid_categ60-65", "fluid_categ65-70", "fluid_categ70-75", 
                "fluid_categ75-80", "fluid_categ80-85", "fluid_categ85-90", "fluid_categ90-95", "fluid_categ95-100", 
                "fluid_categ100+")

W <- c("age", "gender",  "weight",
       "sirs_total", "alb", "alt", "ast", "base_excess", "elix",       
       "bicarb", "bili", "bun", "cl", "creat", "dbp", "fio2", 
       "gcs", "gluc", "hr", "hgb", "inr", "lactate", "map", 
       "paco2", "pao2", "pf_ratio", "ph", "plt", "k", 
       "rr", "shock_index", "na", "o2_sat","sbp", "temp", "adjbw",             
       "wbc", "urine", "urine_sum", "norepi_equiv", "max_prev_norepi_equiv", 
       "egfr", "pulsepress", "ibw",  "height", "surg", "icu_ever_inwindow", "icu",  
       "admit_year", "mechvent", "race", "dtt0_hours")


#remove "empi_id" to avoid overfitting
#"hosp_id", "id",


#1-hot categorical variables 

covars <- model.matrix(~ . - 1, data = tmleupmc2[, .SD, .SDcols = W])


#Q
sl_outcome <- SuperLearner(Y = tmleupmc2$dead, 
                           X = tmleupmc2[, .SD, .SDcols = covars], 
                           SL.library = SL.library, method = "method.NNLS", family = binomial())

sl_treatment <- list()

for (i in fluid_vars) {
  cat("fitting ", i, " sl_treatment\n")
  sl_treatment[[i]] <- SuperLearner(Y = tmleupmc2[, get(i)],
                                    X = tmleupmc2[, .SD, .SDcols = covars], 
                                    SL.library = SL.library, method = "method.NNLS", family = binomial())
}


output <- list()
results <- data.frame()

for (i in fluid_vars) {
  cat("fitting ", i, " effect estimate\n")
  tmle_fit <- tmle(
    Y = tmleupmc2$dead, 
    A = tmleupmc2[, get(i)], 
    W = tmleupmc2[,  .SD, .SDcols = covars],
    Q.SL.library = SL.library,
    g.SL.library = SL.library,
    family = "binomial"
  )
  output[[i]] <- tmle_fit
  cat("finished fitting ", i, " effect estimate\n")
}


for (i in fluid_vars) {
  summary(output[[i]])
  
  #estimate and CI
  results <- rbind(results, 
                   data.frame(fluid_range = colnames(fluid_vars[,i]), 
                              estimate = summary(output[[i]])$psi, 
                              lower = summary(output[[i]])$ci.lower, 
                              upper = summary(output[[i]])$ci.upper))
}




saveRDS(list("results" = results, "output" = output), 
        file = "/wynton/home/pirracchio/vadims/TMLEbins2_hd.RDS")



# 
# for (i in fluid_vars) {
#   out_summary <- summary(output[[i]])
#   
#   if (!is.null(out_summary$psi) && !is.null(out_summary$ci.lower) && !is.null(out_summary$ci.upper)) {
#     results <- rbind(results, 
#                      data.frame(fluid_range = i, 
#                                 estimate = out_summary$psi, 
#                                 lower = out_summary$ci.lower, 
#                                 upper = out_summary$ci.upper))
#   } else {
#     cat("Missing components for fluid_range:", i, "\n")
#   }
# }
# 
# 
# saveRDS(list("results" = results, "output" = output, "output_summary" = out_summary), 
#         file = "/wynton/home/pirracchio/vadims/TMLEbins1_hd.RDS")
# 
# 
