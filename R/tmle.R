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
  W = c("age"),
  A = "fluid_12h_cckgibw",
  Y = "dead"
)

tmle_spec <- tmle_shift(
  shift_val = 1,
  shift_fxn = shift_additive,
  shift_fxn_inv = shift_additive_inv
)
tmle_fit <- tmle3(tmle_spec, upmc, node_list, learner_list)


# initialize a tmle specification for the variable importance parameter
# what's the grid of shifts we wish to consider?
delta_grid <- seq(-10, 10, 1)

# initialize a tmle specification
tmle_spec <- tmle_vimshift_delta(
  shift_grid = delta_grid,
  max_shifted_ratio = 2
)
tmle_fit <- tmle3(tmle_spec, upmc, node_list, learner_list)
