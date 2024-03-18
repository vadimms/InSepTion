#LTMLE code on binarized fluid variable

library(SuperLearner)
library(ltmle)
library(sl3) 
library(tmle3)
library(tmle3shift)

library(tidyr)
library(ggplot2)
library(data.table)
library(dplyr)
library(tmle)
library(glmnet)
library(earth)
#library(nnet)
library(ranger)
library(xgboost)
library(mgcv)  


setwd("/wynton/home/pirracchio/vadims")




tmleupmc1

tmleupmc1 <- fread("UPMC_hd_fluidbins_targets.csv")


####  adding censor nodes

full_intervals <- tibble(interval = 6:18)


full_data <- tmleupmc1 |>
  distinct(hosp_id) |> 
  ungroup() |>
  cross_join(tibble(interval = 6:18)) 

full_tmleupmc1 <- full_data |>
  left_join(tmleupmc1, by = c("hosp_id", "interval"))

full_tmleupmc2 <- full_tmleupmc1 |>
  mutate(censor = if_else(is.na(age), 1, 0))

full_tmleupmc2 <- full_tmleupmc2 |>
  arrange(hosp_id, interval)

# table(full_tmleupmc2$interval, full_tmleupmc2$censor)


#may figure out left-censoring later
full_tmleupmc3 <- full_tmleupmc2 |>
  filter(interval>6)



# baseline_covars <-  full_tmleupmc3[, .(interval, hosp_id, age, gender, weight, elix, adjbw, ibw, 
#                                        height, surg, admit_year, race, dtt0_hours, empi_id)]
# 
# 
# baseline_covars <- baseline_covars |>
#   filter(interval==7) |>
#   arrange(hosp_id)

###############
# 
# all_vars <- c("censor", "age", "gender",  "weight",
#               "sirs_total", "alb", "alt", "ast", "base_excess", "elix",       
#               "bicarb", "bili", "bun", "cl", "creat", "dbp", "fio2", 
#               "gcs", "gluc", "hr", "hgb", "inr", "lactate", "map", 
#               "paco2", "pao2", "pf_ratio", "ph", "plt", "k", 
#               "rr", "shock_index", "na", "o2_sat","sbp", "temp", "adjbw",             
#               "wbc", "urine", "urine_sum", "norepi_equiv", "max_prev_norepi_equiv", 
#               "egfr", "pulsepress", "ibw",  "height", "surg", "icu_ever_inwindow",   
#               "admit_year", "mechvent", "race", "dtt0_hours", "empi_id", "id")
###############


#clarify "icu" variable


########## If going for wide data

# wide_tmleupmc <- dcast(full_tmleupmc3, hosp_id ~ interval,
#                        value.var = c("censor", "sirs_total", "alb", "alt", "ast", "base_excess",        
#                                      "bicarb", "bili", "bun", "cl", "creat", "dbp", "fio2", 
#                                      "gcs", "gluc", "hr", "hgb", "inr", "lactate", "map", 
#                                      "paco2", "pao2", "pf_ratio", "ph", "plt", "k", 
#                                      "rr", "shock_index", "na", "o2_sat","sbp", "temp",             
#                                      "wbc", "urine", "urine_sum", "norepi_equiv", "max_prev_norepi_equiv", 
#                                      "egfr", "pulsepress", "icu_ever_inwindow", "mechvent")
#                          )
# 
# 
# wide_tmleupmc1 <- merge(baseline_covars, wide_tmleupmc, all=TRUE)
# 


#If going to do in wide, use wide_tmleupmc1
tmleupmc2 <- full_tmleupmc3[1:1000,]


SL.library <- c("SL.glm", "SL.gam", "SL.glmnet", "SL.earth", "SL.ranger", "SL.xgboost")
#"SL.nnet"


results <- list()

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

covars <- model.matrix(~ . - 1, data = tmleupmc2[, .SD, .SDcols = W])


for (i in fluid_vars) {
  ltmle_fit <- ltmle(data = tmleupmc2, 
                     Y = "dead", 
                     A = i, 
                     W = covars, 
                     id = "hosp_id", 
                     timevar = "interval",
                     SL.library = SL.library,
                     Q.SL.library = SL.library,
                     g.SL.library = SL.library,
                     family = "binomial")
  
  summary_fit <- summary(ltmle_fit)
  estimate <- summary_fit$psi
  ci_lower <- summary_fit$ci.lower
  ci_upper <- summary_fit$ci.upper
  
  
  results[[fluid_var]] <- data.frame(fluid_var, estimate, ci_lower, ci_upper)
}

ltmle_results <- do.call(rbind, results)




ggplot(ltmle_results, aes(x = i, y = estimate, ymin = ci_lower, ymax = ci_upper)) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  theme_minimal() +
  xlab("Fluid Range") +
  ylab("Estimated Causal Effect") +
  ggtitle("Estimated Causal Effects of Binarized Fluid Variables on Outcome")


saveRDS(ltmle_results, 
        file = "/wynton/home/pirracchio/vadims/LTMLE_bins_hd.RDS")