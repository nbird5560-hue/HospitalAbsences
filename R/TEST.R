#library(mgcv)
#library(dplyr)
#library(future.apply)
#df. <- df %>% mutate(across(where(is.character), ~as.factor(.x)),
#                     month_num = as.numeric(month))
#system.time(
#initial_mod <- bam(hours_lost ~ s(month_num, bs="cc", k=5) + factor(year) +
#                     offset(log(hours_available)),
#                   data = df., family = tw(), discrete = TRUE)
#)

#system.time(
#rho_est <- acf(residuals(initial_mod), plot=FALSE)$acf[2]
#)
#system.time(
#fit_bam <- bam(
#  hours_lost ~
#    s(month_num, bs="cc", k=6) +           # Seasonality
#    factor(year) +                     # Annual shift
#    s(month_year, bs="re") +            # Random effect for timestamp shocks
#    benchmark_group + nhse_region_name +          # Your X variables
#    offset(log(hours_available)),      # The rate normalizer
#  data = df.,
#  family = tw(),                       # Tweedie (handles rare 0s + skewed counts)
#  rho = rho_est,
#  AR.start = start_series,
#  discrete = TRUE                      # Optimized for 64k rows
#)
#)

##### Bagging
#fit_bagged_member <- function(seed, data, rho_val) {
#  set.seed(seed)

# Subsample 70% of the data
#  sub_df <- data %>% sample_frac(0.7)

# Fit model (dropping 'discrete = TRUE' to avoid the offset bug)
# We use 'use.chol = TRUE' for a different speed optimization
#  tryCatch({
#    mod <- bam(
#      hours_lost ~ s(month, bs="cc", k=6) + fYear +
#        s(MonthYear, bs="re") + covariate1 + covariate2 +
#        offset(log_h_avail),
#      data = sub_df,
#      family = tw(),
#      rho = rho_val,
#      use.chol = TRUE
#    )
#    return(mod)
#  }, error = function(e) return(NULL))
#}

# --- 3. Run in Parallel ---
#plan(multisession) # Use all available cores
#num_iterations <- 20 # Start small, increase to 50+ for production

#bagged_models <- future_lapply(1:num_iterations, function(i) {
#  fit_bagged_member(i, df_clean, rho_est)
#}, future.seed = TRUE)

#bagged_models <- Filter(Negate(is.null), bagged_models)


######## Prediction

#predict_bagged <- function(models, newdata) {
# newdata must have 'log_h_avail'

# Get predictions from every model in the bag
# We exclude the random effect 's(MonthYear)' for predicting future/new months
#  preds_matrix <- sapply(models, function(m) {
#    predict(m, newdata = newdata, type = "response", exclude = "s(MonthYear)")
#  })

  # Average across the rows (the models)
#  final_predictions <- rowMeans(preds_matrix)
#  return(final_predictions)
#}

# Example usage:
# new_data_preds <- predict_bagged(bagged_models, my_test_df)
