library(tmle)
library(tidyverse)
library(lubridate)
library(slider) 
library(SuperLearner)
library(mgcv)
library(tidyr)
library(ggplot2)

library(haldensify)
library(tmle3shift)
library(data.table)
library(dplyr)
library(sl3) 
library(tmle3)
library(randomForest)
library(xgboost)
library(ranger)
library(speedglm)
library(Rsolnp)

install.packages("Rsolnp")

#remove time >3
tmleupmc <- hdupmc |>  filter(time < 4)

#remove _imp logical columns
logical_columns <- sapply(tmleupmc, is.logical)

tmleupmc2 <- tmleupmc[, !logical_columns]

#remove extra columns
str(tmleupmc2)

tmleupmc = subset(tmleupmc2, select = -c(sofa_total, action, 
                                         enc_start_dt, enc_end_dt, dt_t0_first, death_date, icu_start_dt, icu_end_dt))

#clarify outcome variable
tmleupmc <- tmleupmc |>
  group_by(hosp_id) |>
  mutate(fluid_12h_cckgibw = max(fluid_sum_cckgibw)) 

summary(tmleupmc$fluid_12h_cckgibw)

sum(is.na(tmleupmc$fluid_12h_cckgibw))

#dataset
summary(tmleupmc$time)
summary(tmleupmc$dtt0_hours)
summary(tmleupmc$dead) #9.1%  == 28471 / (28471 + 284654)
summary(tmleupmc$dead_90) #23.2%
summary(tmleupmc$max_prev_norepi_equiv)

summary(tmleupmc$fluid_12h_cckgibw)


tmleupmc |> 
  filter(time == 0) |> 
  summarize(across(c(sbp, map, max_prev_norepi_equiv), mean))

########sl3
#sl_bayesglm <- Lrnr_pkg$new("arm", "bayesglm")
#sl_rf <- Lrnr_rf$new()

sl_glm <- Lrnr_glm$new()
sl_rf <- Lrnr_ranger$new()  
sl_xgboost <- Lrnr_xgboost$new()
sl_gam <- Lrnr_gam$new()
sl_hal <- Lrnr_hal9001$new(max_degree = 3, n_folds = 3)

sl_list <- list(sl_glm, sl_rf, sl_xgboost, sl_gam, sl_hal)

stacked_sl3 <- Lrnr_sl$new(learners = sl_list)


#model
#excluded: "dtt0_hours","sofa_total", "fluid", "fluid_sum",


Y <- "dead"
A <- "fluid_12h_cckgibw"
W <- c("age", "gender", "race", "weight", "hospital", "admit_year", "elix",
       "sirs_total", "alb", "alt", "ast", "base_excess",        
       "bicarb", "bili", "bun", "cl", "creat", "dbp", "fio2", 
       "gcs", "gluc", "hr", "hgb", "inr", "lactate", "map", 
       "mechvent", "paco2", "pao2", "pf_ratio", "ph", "plt", "k", 
       "rr", "shock_index", "na", "o2_sat","sbp", "temp",                 
       "wbc", "urine", "urine_sum", "norepi_equiv", "max_prev_norepi_equiv", 
       "icu", "egfr", "pulsepress")

sl3task <- make_sl3_Task(data = tmleupmc, covariates = W, outcome = Y)

print(typeof(Y))
print(typeof(A))
print(typeof(W))


#tmle3
tmle_result <- tmle3(
  task = sl3task,
  learner_Y = stacked_sl3,
  learner_A = stacked_sl3)
  #intervention = 30
  #var_estimate_method = "bootstrap",


tmle_result <- tmle3(
  data = tmleupmc,                    
  Y = "dead",
  A = "fluid_12h_cckgibw",
  W = c("age", "gender", "race", "weight", "hospital", "admit_year", "elix",
        "sirs_total", "alb", "alt", "ast", "base_excess",        
        "bicarb", "bili", "bun", "cl", "creat", "dbp", "fio2", 
        "gcs", "gluc", "hr", "hgb", "inr", "lactate", "map", 
        "mechvent", "paco2", "pao2", "pf_ratio", "ph", "plt", "k", 
        "rr", "shock_index", "na", "o2_sat","sbp", "temp",                 
        "wbc", "urine", "urine_sum", "norepi_equiv", "max_prev_norepi_equiv", "icu", "egfr", "pulsepress"), 
  learner_Y = stacked_sl3,       
  learner_A = stacked_sl3)

hdupmc <- fread("C:/Users/vadim/OneDrive/Desktop/UPMC/UPMC hot deck complete.csv")

summary(hdupmc$fluid_12h_cckgibw)

as.numeric(hdupmc$fluid_12h_cckgibw)
hist(hdupmc$fluid_12h_cckgibw)

summary(hdupmc$fluid_sum_cckgibw)
hist(hdupmc$fluid_sum_cckgibw, breaks = 10)

?hist

hdupmc$fluid_12h_decile <- (hdupmc$fluid_12h_cckgibw / 10)

prop.table(table(hdupmc$fluid_12h_decile, hdupmc$dead))

# ATE
ate <- tmle_result$estimate

##################################


# learners used for conditional mean of the outcome
mean_lrnr <- Lrnr_mean$new()
fglm_lrnr <- Lrnr_glm_fast$new()
rf_lrnr <- Lrnr_ranger$new()
hal_lrnr <- Lrnr_hal9001$new(max_degree = 3, n_folds = 3)

# SL for the outcome regression
sl_reg_lrnr <- Lrnr_sl$new(
  learners = list(mean_lrnr, fglm_lrnr, rf_lrnr, hal_lrnr),
  metalearner = Lrnr_nnls$new()
)

# learners used for conditional densities for (g_n)
haldensify_lrnr <- Lrnr_haldensify$new(
  n_bins = c(5, 10, 20),
  lambda_seq = exp(seq(-1, -10, length = 200))
)
# semiparametric density estimator with homoscedastic errors (HOSE)
hose_hal_lrnr <- make_learner(Lrnr_density_semiparametric,
                              mean_learner = hal_lrnr
)
# semiparametric density estimator with heteroscedastic errors (HESE)
hese_rf_glm_lrnr <- make_learner(Lrnr_density_semiparametric,
                                 mean_learner = rf_lrnr,
                                 var_learner = fglm_lrnr
)

# SL for the conditional treatment density
sl_dens_lrnr <- Lrnr_sl$new(
  learners = list(hose_hal_lrnr, hese_rf_glm_lrnr),
  metalearner = Lrnr_solnp_density$new()
)

learner_list <- list(Y = sl_reg_lrnr, A = sl_dens_lrnr)

# initialize a tmle specification
tmle_spec <- tmle_shift(
  shift_val = 5,
  shift_fxn = shift_additive,
  shift_fxn_inv = shift_additive_inv
)


node_list <- list(
  W = c("age", "gender", "race", "weight", "hospital", "admit_year", "elix",
        "sirs_total", "alb", "alt", "ast", "base_excess",        
        "bicarb", "bili", "bun", "cl", "creat", "dbp", "fio2", 
        "gcs", "gluc", "hr", "hgb", "inr", "lactate", "map", 
        "mechvent", "paco2", "pao2", "pf_ratio", "ph", "plt", "k", 
        "rr", "shock_index", "na", "o2_sat","sbp", "temp",                 
        "wbc", "urine", "urine_sum", "norepi_equiv", "max_prev_norepi_equiv", "icu", "egfr", "pulsepress"),
  A = "fluid_12h_cckgibw",  
  Y = "dead"  )

 
tmle_fit <- tmle3(tmle_spec, tmleupmc, node_list, learner_list)

