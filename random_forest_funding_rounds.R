library(tidymodels)
library(tidyverse)
library(ranger)

load("dealroom_data_final.RData")

dealroom_data_final <- dealroom_data_final %>%
  select(-dealroom_signal_._founding_team_score,
         -dealroom_signal_._growth_rate,
         -dealroom_signal_._timing,
         -revenue_model_saas,
         -last_funding_year,
         -first_funding_year,
         -has_investor,
         -closing_year)

str(dealroom_data_final)
#calculate IQR and define bounds for outliers
#this takes away everything above 7 rounds
iqr_value <- IQR(dealroom_data_final$total_rounds_number)
q1 <- quantile(dealroom_data_final$total_rounds_number, 0.25)
q3 <- quantile(dealroom_data_final$total_rounds_number, 0.75)
lower_bound <- q1 - 1.5 * iqr_value
upper_bound <- q3 + 1.5 * iqr_value
#filter out outlier rows based on IQR method
dealroom_data_final <- dealroom_data_final %>%
  filter(total_rounds_number >= lower_bound & total_rounds_number <= upper_bound)

#calculate IQR and define bounds for outliers
#this takes away everything above 7 rounds
iqr_value <- IQR(dealroom_data_final$avg_valuation, na.rm = TRUE)
q1 <- quantile(dealroom_data_final$avg_valuation, 0.25, na.rm = TRUE)
q3 <- quantile(dealroom_data_final$avg_valuation, 0.75, na.rm = TRUE)
lower_bound <- q1 - 1.5 * iqr_value
upper_bound <- q3 + 1.5 * iqr_value
#filter out outlier rows based on IQR method
dealroom_data_final <- dealroom_data_final %>%
  filter((avg_valuation >= lower_bound & avg_valuation <= upper_bound)  | is.na(avg_valuation))

#calculate IQR and define bounds for outliers
#this takes away everything above 7 rounds
iqr_value <- IQR(dealroom_data_final$avg_years_education, na.rm = TRUE)
q1 <- quantile(dealroom_data_final$avg_years_education, 0.25, na.rm = TRUE)
q3 <- quantile(dealroom_data_final$avg_years_education, 0.75, na.rm = TRUE)
lower_bound <- q1 - 1.5 * iqr_value
upper_bound <- q3 + 1.5 * iqr_value
#filter out outlier rows based on IQR method
dealroom_data_final <- dealroom_data_final %>%
  filter((avg_years_education >= lower_bound & avg_years_education <= upper_bound) | is.na(avg_years_education))

#calculate IQR and define bounds for outliers
#this takes away everything above 7 rounds
iqr_value <- IQR(dealroom_data_final$avg_years_experience, na.rm = TRUE)
q1 <- quantile(dealroom_data_final$avg_years_experience, 0.25, na.rm = TRUE)
q3 <- quantile(dealroom_data_final$avg_years_experience, 0.75, na.rm = TRUE)
lower_bound <- q1 - 1.5 * iqr_value
upper_bound <- q3 + 1.5 * iqr_value
#filter out outlier rows based on IQR method
dealroom_data_final <- dealroom_data_final %>%
  filter((avg_years_experience >= lower_bound & avg_years_experience <= upper_bound) | is.na(avg_years_experience))

str(dealroom_data_final)

set.seed(912340)
# data_split <- initial_split(dealroom_data_final, prop = 0.8)
# train_data <- training(data_split)
# test_data <- testing(data_split)

# train_data <- dealroom_data_final %>%
#   filter(as.numeric(as.character(launch_year)) %in% 2014:2019)
# 
# test_data <- dealroom_data_final %>%
#   filter(as.numeric(as.character(launch_year)) %in% 2020:2024)

# train_data <- dealroom_data_final %>%
#   filter(as.numeric(as.character(launch_year)) %in% c(2014:2018, 2021))
# 
# test_data <- dealroom_data_final %>%
#   filter(as.numeric(as.character(launch_year)) %in% c(2019, 2020, 2022, 2023, 2024))

train_data <- dealroom_data_final %>%
  filter(as.numeric(as.character(launch_year)) <= 2021)

test_data <- dealroom_data_final %>%
  filter(as.numeric(as.character(launch_year)) %in% 2022:2024)

is_binary_factor <- function(x) {
  is.factor(x) && length(levels(x)) == 2
}

binary_vars <- sapply(train_data, is_binary_factor)
binary_vars_names <- names(dealroom_data_final)[binary_vars]
binary_vars_names <- names(dealroom_data_final)[sapply(dealroom_data_final, function(x) is.factor(x) && length(levels(x)) == 2)]

binary_levels <- sapply(dealroom_data_final[binary_vars_names], levels)

#correctly adjust factor levels
dealroom_data_final <- dealroom_data_final %>%
  mutate(across(all_of(binary_vars_names), function(x) {
    if (all(c("0", "1") %in% levels(x))) {
      fct_relevel(x, "0", "1")
    } else {
      x
    }
  }))
dealroom_data_final <- dealroom_data_final %>%
  mutate(across(all_of(binary_vars_names), ~ as.integer(as.factor(.x)) - 1))

# rf_recipe <- recipe(total_rounds_number ~ ., data = train_data) %>%
#   step_rm(name, hq_region, hq_country, hq_city, last_funding_month, first_funding_month, valuation_month) %>%
#   # step_unknown(all_nominal(), -all_of(binary_vars_names)) %>%
#   step_impute_median(all_numeric(), -all_outcomes()) %>%
#   # step_novel(all_nominal(), -all_of(binary_vars_names)) %>%
#   step_impute_mode(all_nominal(), -all_of(binary_vars_names)) %>%
#   step_dummy(all_nominal(), one_hot = TRUE) %>%
#   step_zv(all_predictors()) %>%
#   step_normalize(all_numeric(), -all_outcomes())
# Create a new recipe
rf_recipe <- recipe(total_rounds_number ~ ., data = train_data) %>%
  #remove irrelevant columns
  step_rm(name, hq_region, hq_country, hq_city, last_funding_month, first_funding_month, valuation_month, valuation_year) %>%
  #handle missing values - impute median for numeric columns and mode for categorical columns
  step_impute_median(all_numeric(), -all_outcomes()) %>%
  step_impute_mode(all_nominal(), -all_outcomes()) %>%
  #dummy for categorical variables
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  #remove zero variance predictors
  step_zv(all_predictors()) %>%
  #normalize numeric predictors
  step_normalize(all_numeric(), -all_outcomes())

#Random Forest model specification
rf_model <- rand_forest(
  mtry = 23,
  trees = 1000,
  mode = "regression"
) %>%
  set_engine("ranger")

#create workflow
rf_workflow <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_model)

#set up cross-validation
set.seed(82001)
# cv_folds <- vfold_cv(train_data, v = 5, strata = total_rounds_number)
cv_folds <- vfold_cv(train_data, v = 5)
#define metrics to evaluate the model
metrics <- metric_set(rmse, rsq)

# #tune the model using a grid search or random search
# grid <- grid_latin_hypercube(
#   mtry(range = c(1, 25)), #1-20
#   size = 15 #20
# )

#perform the tuning
tune_results <- tune_grid(
  rf_workflow,
  resamples = cv_folds,
  grid = grid,
  metrics = metrics
)

#select the best model based on RMSE
best_params <- select_best(tune_results, "rmse")
#finalize the workflow with the best parameters
final_rf_workflow <- finalize_workflow(rf_workflow, best_params)

#fit the final model to the full training dataset
final_rf_fit <- fit(final_rf_workflow, data = train_data)

#evaluate the model on the test set
rf_predictions <- predict(final_rf_fit, test_data)

rf_predictions <- rf_predictions %>%
  rename(.pred = starts_with(".pred"))

rf_results <- bind_cols(test_data, rf_predictions) %>%
  metrics(truth = total_rounds_number, estimate = .pred)

print(rf_results)
