library(tidymodels)
library(tidyverse)
library(xgboost)
library(recipes)

load("dealroom_data_final.RData")

dealroom_data_final <- dealroom_data_final %>%
  select(-dealroom_signal_._founding_team_score,
         -dealroom_signal_._growth_rate,
         -dealroom_signal_._timing,
         -revenue_model_saas,
         -last_funding_year,
         -first_funding_year,
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

# #calculate IQR and define bounds for outliers
# #this takes away everything above 7 rounds
# iqr_value <- IQR(dealroom_data_final$number_of_patents)
# q1 <- quantile(dealroom_data_final$number_of_patents, 0.25)
# q3 <- quantile(dealroom_data_final$number_of_patents, 0.75)
# lower_bound <- q1 - 1.5 * iqr_value
# upper_bound <- q3 + 1.5 * iqr_value
# #filter out outlier rows based on IQR method
# dealroom_data_final <- dealroom_data_final %>%
#   filter(number_of_patents >= lower_bound & number_of_patents <= upper_bound)


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

#function to check if a variable is a factor with exactly two levels
is_binary_factor <- function(x) {
  is.factor(x) && length(levels(x)) == 2
}

#apply function to each column of the dataset
binary_vars <- sapply(train_data, is_binary_factor)
binary_vars_names <- names(dealroom_data_final)[binary_vars]
#ensure binary_vars_names is correctly defined with all binary factor columns
binary_vars_names <- names(dealroom_data_final)[sapply(dealroom_data_final, function(x) is.factor(x) && length(levels(x)) == 2)]
# dealroom_data_final <- dealroom_data_final %>%
#   mutate(across(all_of(binary_vars_names), ~fct_relevel(.x, "0", "1")))
# #convert binary factor variables to numeric
# dealroom_data_final <- dealroom_data_final %>%
#   mutate(across(all_of(binary_vars_names), ~ as.integer(as.factor(.x)) - 1))


#list the levels of each factor considering as binary
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

recipe <- recipe(total_rounds_number ~ ., data = train_data) %>%
  step_rm(name, hq_region, hq_country, hq_city, last_funding_month, first_funding_month, valuation_year, valuation_month) %>%
  step_unknown(all_nominal(), -all_of(binary_vars_names)) %>%
  step_impute_median(all_numeric(), -all_outcomes()) %>%
  step_novel(all_nominal(), -all_of(binary_vars_names)) %>%
  step_impute_mode(all_nominal(), -all_of(binary_vars_names)) %>%
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric(), -all_outcomes())

#define model specification with potential tuning parameters
boost_model <- boost_tree(
  mode = "regression",
  trees = tune(),
  tree_depth = tune(),
  min_n = tune(),
  learn_rate = tune(),
  loss_reduction = tune()
) %>%
  set_engine("xgboost")

# boost_model <- boost_tree(
#   mode = "regression",
#   trees = 445,
#   tree_depth = 6,
#   min_n = 3,
#   learn_rate = 0.02621532,
#   loss_reduction = 1
# ) %>%
#   set_engine("xgboost")

workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(boost_model)

metrics <- metric_set(rmse, rsq)
set.seed(82001)
cv_folds <- vfold_cv(train_data, v = 10)

library(dials)  #for parameter objects

# #define parameter ranges using dials
# trees <- trees(range = c(500, 1000))
# tree_depth <- tree_depth(range = c(6, 8))
# min_n <- min_n(range = c(10, 20))
# learn_rate <- learn_rate(range = c(0.01, 0.05))
# loss_reduction <- loss_reduction(range = c(0.5, 1.0))

#set up tuning grid
tune_grid <- grid_latin_hypercube(
  trees(),
  tree_depth(),
  min_n(),
  learn_rate(),
  loss_reduction(),
  size = 20
)
#run the tuning process
tune_results <- tune_grid(
  workflow,
  resamples = cv_folds,
  grid = tune_grid,
  metrics = metrics
)
#extract the best parameters based on RMSE
best_params <- select_best(tune_results, "rmse")

#finalize the workflow with the best parameters
final_model <- finalize_workflow(workflow, best_params)
final_fit <- fit(final_model, data = train_data)

#predict on the test set
predictions <- predict(final_fit, new_data = test_data)
results <- bind_cols(test_data, predictions) %>%
  select(total_rounds_number, .pred)

#calculate evaluation metrics on the test set
eval_metrics <- metrics(results, truth = total_rounds_number, estimate = .pred)
print(eval_metrics)
#feature importance analysis
fitted_model <- extract_fit_parsnip(final_fit)

#predict and evaluate on the test set
predictions <- predict(final_fit, new_data = test_data)
importance_matrix <- xgb.importance(model = fitted_model$fit)
library(vip)
importance_matrix <- vip(fitted_model)
print(importance_matrix)