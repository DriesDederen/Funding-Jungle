library(tidyverse)
library(xgboost)
library(tidymodels)
library(recipes)

load("dealroom_data_final.RData")
str(dealroom_data_final)

dealroom_data_final <- dealroom_data_final %>%
  mutate(
    last_funding_year = as.numeric(as.character(last_funding_year)),
    target_invest = as.numeric(last_funding_year >= 2022)
  ) %>%
  select(
    -dealroom_signal_._founding_team_score,
    -dealroom_signal_._growth_rate,
    -dealroom_signal_._timing,
    -revenue_model_saas,
    -last_funding_year,
    -first_funding_year,
    -closing_year
  )

str(dealroom_data_final$target_invest)

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

set.seed(912340)
dealroom_data_final <- dealroom_data_final %>%
  filter(as.numeric(as.character(launch_year)) < 2022)

data_split <- initial_split(dealroom_data_final, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

str(dealroom_data_final)
str(train_data)
str(test_data)
#### binary factor handle
is_binary_factor <- function(x) {
  is.factor(x) && length(levels(x)) == 2
}

binary_vars <- sapply(train_data, is_binary_factor)
binary_vars_names <- names(dealroom_data_final)[binary_vars]
binary_vars_names <- names(dealroom_data_final)[sapply(dealroom_data_final, function(x) is.factor(x) && length(levels(x)) == 2)]

binary_levels <- sapply(dealroom_data_final[binary_vars_names], levels)

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

print(binary_levels)
print(binary_vars_names)

dealroom_data_final <- mutate(dealroom_data_final, target_invest = factor(target_invest, levels = c("0", "1")))
train_data <- mutate(train_data, target_invest = factor(target_invest, levels = c("0", "1")))
test_data <- mutate(test_data, target_invest = factor(target_invest, levels = c("0", "1")))

str(dealroom_data_final)
str(train_data)
str(test_data)

print(dim(train_data))
print(dim(test_data))
print(length(train_data$target_invest))

recipe <- recipe(target_invest ~ ., data = train_data) %>%
  step_rm(name, hq_region, hq_country, hq_city, last_funding_month, first_funding_month, valuation_month, valuation_year) %>%
  step_unknown(all_nominal(), -all_of(c(binary_vars_names, "target_invest"))) %>%
  step_impute_median(all_numeric(), -all_outcomes()) %>%
  step_novel(all_nominal(), -all_of(c(binary_vars_names, "target_invest"))) %>%
  step_impute_mode(all_nominal(), -all_of(c(binary_vars_names, "target_invest"))) %>%
  step_dummy(all_nominal(), one_hot = TRUE, -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric(), -all_outcomes())

prep_recipe <- prep(recipe)

#check the final structure of the training data after recipe preparation
baked_data <- bake(prep_recipe, new_data = NULL)
print(dim(baked_data))
print(colnames(baked_data))

if ("target_invest" %in% colnames(baked_data)) {
  print(table(baked_data$target_invest))
} else {
  print("target_invest is missing from the processed data.")
}

#model specification with potential tuning parameters
boost_model <- boost_tree(
  mode = "classification",
  trees = tune(),  # Enable tuning of the number of trees
  tree_depth = tune(),  # Enable tuning of tree depth
  min_n = tune(),  # Enable tuning of minimum n
  learn_rate = tune(),  # Enable tuning of learning rate
  loss_reduction = tune()  # Enable tuning of loss reduction
) %>%
  set_engine("xgboost")

# #model specification for a boosting tree model
# boost_model <- boost_tree(
#   mode = "classification",
#   trees = 1144,
#   tree_depth = 4,
#   min_n = 3,
#   learn_rate = 0.03576789,
#   loss_reduction = 5.776196e-09
# ) %>%
#   set_engine("xgboost")

workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(boost_model)

metrics <- metric_set(roc_auc, accuracy, sensitivity, precision)
set.seed(82001)
cv_folds <- vfold_cv(train_data, v = 10, strata = target_invest)

# library(dials)
# trees <- trees(range = c(500, 1000))
# tree_depth <- tree_depth(range = c(6, 8))
# min_n <- min_n(range = c(10, 20))
# learn_rate <- learn_rate(range = c(0.01, 0.05))
# loss_reduction <- loss_reduction(range = c(0.5, 1.0))

#stup tuning grid
tune_grid <- grid_latin_hypercube(
  trees(),
  tree_depth(),
  min_n(),
  learn_rate(),
  loss_reduction(),
  size = 20
)

tune_results <- tune_grid(
  workflow,
  resamples = cv_folds,
  grid = tune_grid,
  metrics = metrics
)

#select best model based on ROC AUC
best_model <- select_best(tune_results, "roc_auc")

#workflow update with the best model
final_workflow <- finalize_workflow(workflow, best_model)

#fit final model with training data
final_fit <- fit(final_workflow, data = train_data)

#predict on the test data
predictions <- predict(final_fit, new_data = test_data, type = "prob")
print(head(predictions))

#predict on test set
predictions <- predict(final_fit, new_data = test_data)
print(head(predictions))

#create df with true values and predictions
evaluation_df <- data.frame(True_Value = test_data$target_invest, Predictions = predictions$.pred_class)
print(evaluation_df)


#True Positives, True Negatives, False Positives, False Negatives
TP <- sum(evaluation_df$True_Value == 1 & evaluation_df$Predictions == 1)
TN <- sum(evaluation_df$True_Value == 0 & evaluation_df$Predictions == 0)
FP <- sum(evaluation_df$True_Value == 0 & evaluation_df$Predictions == 1)
FN <- sum(evaluation_df$True_Value == 1 & evaluation_df$Predictions == 0)

#sensitivity (True Positive Rate)
sensitivity <- TP / (TP + FN)
#specificity (True Negative Rate)
specificity <- TN / (TN + FP)
#accuracy
accuracy <- (TP + TN) / (TP + TN + FP + FN)
#precision
precision <- TP / (TP + FP)

cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")

confusion_matrix <- matrix(c(TP, FP, FN, TN), nrow = 2, byrow = TRUE,
                           dimnames = list('Predicted' = c('Positive', 'Negative'),
                                           'Actual' = c('Positive', 'Negative')))
cat("Confusion Matrix:\n")
print(confusion_matrix)

#extract fitted model from final fit
fitted_model <- extract_fit_engine(final_fit)

importance_matrix <- xgb.importance(model = fitted_model)
print(importance_matrix)