#1. Split
set.seed(123)
data_split <- initial_split(predata, prop = 0.80, strata = GAD_7_Severity)
train_data <- training(data_split)
#test_data <- testing(data_split)

# 2. Recipe
mhealth_recipe <- recipe(GAD_7_Severity ~ Age + Gender + User_Archetype + 
                           Late_Night_Usage + Daily_Screen_Time_Hours + 
                           Sleep_Duration_Hours, data = train_data) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_zv(all_predictors())

# 3. Model & Workflow
multinom_spec <- multinom_reg() %>% set_engine("nnet") %>% set_mode("classification")
mhealth_wflow <- workflow() %>% add_recipe(mhealth_recipe) %>% add_model(multinom_spec)

# 4. CV & Evaluation
set.seed(123)
folds <- vfold_cv(train_data, v = 5, strata = GAD_7_Severity)
cv_results <- fit_resamples(mhealth_wflow, resamples = folds,
                            metrics = metric_set(accuracy, roc_auc, sens, yardstick::spec))

collect_metrics(cv_results) %>% kable()