library(data.table)
library(haldensify)
library(sl3)
library(tmle3)
library(tmle3shift)


# learners used for conditional mean of the outcome
mean_lrnr <- Lrnr_mean$new()
fglm_lrnr <- Lrnr_glm_fast$new()
rf_lrnr <- Lrnr_ranger$new()
hal_lrnr <- Lrnr_hal9001$new(max_degree = 3, n_folds = 3)

# SL for the outcome regression
sl_reg_lrnr <- Lrnr_sl$new(
  learners = list(mean_lrnr, fglm_lrnr, rf_lrnr),
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


# tmle ----------------------------------------------------------------------------------------

# initialize a tmle specification
node_list <- list(
  W = c("age", "gender", "race", "weight", "hospital", "admit_year", "elix",
         "sirs_total", "alb", "alt", "ast", "base_excess",        
         "bicarb", "bili", "bun", "cl", "creat", "dbp", "fio2", 
         "gcs", "gluc", "hr", "hgb", "inr", "lactate", "map", 
         "mechvent", "paco2", "pao2", "pf_ratio", "ph", "plt", "k", 
         "rr", "shock_index", "na", "o2_sat","sbp", "temp", "adjbw",             
         "wbc", "urine", "urine_sum", "norepi_equiv", "max_prev_norepi_equiv", 
         "icu", "egfr", "pulsepress", "surg", "ibw", "hosp_id", "height", "id", 
         "empi_id", "icu_ever_inwindow", "dtt0_hours", "interval"),
  A = "fluid_12h_cckgibw",
  Y = "dead"
)

tmle_spec <- tmle_shift(
  shift_val = 1,
  shift_fxn = shift_additive,
  shift_fxn_inv = shift_additive_inv
)
start_time <- proc.time()
tmle_fit <- tmle3(tmle_spec, upmc, node_list, learner_list)
fit_time <- proc.time() - start_time

# # initialize a tmle specification for the variable importance parameter
# # what's the grid of shifts we wish to consider?
# delta_grid <- seq(-10, 10, 1)
# 
# # initialize a tmle specification
# tmle_spec <- tmle_vimshift_delta(
#   shift_grid = delta_grid,
#   max_shifted_ratio = 2
# )
# tmle_fit <- tmle3(tmle_spec, upmc, node_list, learner_list)


# parallelization ---------------------------------------------------------

# let's load the future package and set n-1 cores for parallel processing
library(future)
ncores <- min(16, max(1, availableCores()-1))
plan(multisession, workers = ncores)

set.seed(0)
start_time <- proc.time()
tmle_parallel <- tmle3(tmle_spec, upmc, node_list, learner_list)
fit_time_parallel <- proc.time() - start_time

fit_time/fit_time_parallel


# tmle hal ----------------------------------------------------------------


