#-------------------------------decision tree model-----------------------------
#| cache: TRUE
set.seed(1212) 
crash_split <- initial_split(crash, strata = injuries) 
crash_train <- training(crash_split) 
crash_test <- testing(crash_split)  

set.seed(123) 
crash_folds <- vfold_cv(crash_train, strata = injuries) 
crash_folds
#-------------------------------------------------------------------------------
#| cache: TRUE

names(crash)

crash_rec <- recipe(injuries ~ ., data = crash_train) %>%
  step_downsample(injuries)  

bag_spec <- bag_tree(min_n = 10) %>%   
  set_engine("rpart", times = 25) %>%   
  set_mode("classification")  
crash_wf <- workflow() %>%   
  add_recipe(crash_rec) %>%   
  add_model(bag_spec)  

crash_wf
#-------------------------------------------------------------------------------
#| cache: TRUE
doParallel::registerDoParallel() 
crash_res <- fit_resamples(crash_wf,   
                           crash_folds,   
                           control = control_resamples(save_pred = TRUE))
#-------------------------------------------------------------------------------
#| cache: TRUE
collect_metrics(crash_res)
#-------------------------------------------------------------------------------
#| cache: TRUE
crash_fit <- last_fit(crash_wf, crash_split) 
collect_metrics(crash_fit)
#------------------------------importance plot----------------------------------
#| cache: TRUE
#| label: fig-VarImp
#| fig-cap: "The importance of predictor variables to describe the severity of accidents"
crash_imp <- crash_fit$.workflow[[1]] %>%   
  pull_workflow_fit()  
crash_imp$fit$imp %>%   
  slice_max(value, n = 10) %>%   
  ggplot(aes(value, fct_reorder(term, value))) +   
  geom_col(alpha = 0.8, fill = "midnightblue") +   
  labs(x = "Variable importance score", y = NULL)
#------------------------------------ROC curve----------------------------------
#| cache: TRUE
#| label: fig-ROC
#| fig-cap: "ROC curve"
collect_predictions(crash_fit) %>%   
  roc_curve(injuries, .pred_injuries) %>%   
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +   
  geom_line(size = 1.5, color = "midnightblue") +   
  geom_abline(lty = 2, alpha = 0.5,     color = "gray50",     size = 1.2) +
  coord_equal()