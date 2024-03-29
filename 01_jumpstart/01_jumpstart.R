# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: ADVANCED TIME SERIES FORECASTING FOR BUSINESS
# MODULE: TIME SERIES JUMPSTART 

# GOAL: Forecast Daily Email Users - Next 8-WEEKS

# OBJECTIVES ----
# - Dive into a time-series analysis project
# - Experience Frameworks: modeltime
# - Experience 2 Algorithms:
#   1. Prophet
#   2. LM w/ Engineered Features

# LIBRARIES ----
library(tidymodels)
library(modeltime)
library(tidyverse)
library(timetk)
library(lubridate)



# DATA -----

mailchimp_usrs_tbl <- readRDS("00_data/mailchimp_users.rds")
mailchimp_usrs_tbl




# 1.0 EDA & DATA PREP ----
# * DAILY SUBSCRIBERS INCREASES

mailchimp_usrs_tbl %>% glimpse()

# * Count of Optins by day ----
optins_day_tbl <-
  mailchimp_usrs_tbl %>%
  summarise_by_time(.date_var = optin_time,
                    .by = "day",
                    optins = n())
optins_day_tbl


# * Summary Diagnostics ----
optins_day_tbl %>% tk_summary_diagnostics(.date_var = optin_time)  

# * Pad by TIme series ----

optins_day_prepared_tbl <- optins_day_tbl %>%
  pad_by_time(.date_var = optin_time,
              .by = "day",
              .pad_value = 0)

# * Visualisation ----
optins_day_prepared_tbl %>%
  plot_time_series(optin_time, optins)



# 2.0 EVALUATION PERIOD ----

# * FIltering ----

evaluation_tbl <- optins_day_prepared_tbl %>%
  filter_by_time(.date_var = optin_time,
                 .start_date = "2018-11-20",
                 .end_date = "end")

evaluation_tbl %>%
  plot_time_series(optin_time, optins)

# * train-test splits ----

splits <- evaluation_tbl %>%
  time_series_split(date_var = optin_time,
                    assess = "8 week",
                    cumulative = TRUE)
 splits
 
 splits %>%
   tk_time_series_cv_plan() %>% 
   plot_time_series_cv_plan(optin_time, optins)
 


# 3.0 PROPHET FORECASTING ----

# * Prophet model using Modeltime/Parsnip ----
 
 model_prophet_fit <-prophet_reg() %>%
   set_engine("prophet") %>%
   fit(optins ~ optin_time, data = training(splits))
 
 model_prophet_fit

 
 # * Modeltime process ----
 
 model_tbl <-  modeltime_table(model_prophet_fit)

 # * Callibaration ----
 
 calibration_tbl <- model_tbl %>%
   modeltime_calibrate(new_data = testing(splits))
 
 # * Visualize Forecast
 calibration_tbl %>%
   modeltime_forecast(actual_data = evaluation_tbl) %>%
   plot_modeltime_forecast()
 
 # * Get accuracy metrics ----
 
 calibration_tbl %>% 
   modeltime_accuracy()
 
 
 
# 4.0 FORECASTING WITH FEATURE ENGINEERING ----

# * Identify possible features ----
 
 evaluation_tbl %>%
   plot_seasonal_diagnostics(optin_time, log(optins))
 
 # * Receipe Spec ----
 
 recipe_spec <- recipe(optins ~ . , data = training(splits)) %>%
   
   #TS Signature
   step_timeseries_signature(optin_time) %>%
   
   step_rm(
     ends_with(".iso"),
     ends_with(".xts"),
     contains("hour"),
     contains("minute"),
     contains("second"),
     contains("am.pm")
   ) %>%
   
   step_normalize(ends_with("index.num"), ends_with("_year")) %>%
   
   step_dummy(all_nominal())
 
 recipe_spec %>%
   prep() %>%
   juice() %>%
   glimpse()
 
 # * Machine learning spec ---
 
 model_spec <- linear_reg() %>%
   set_engine("lm")
 
 
 workflow_fit_lm <- workflow() %>%
   add_model(model_spec) %>%
   add_recipe(recipe_spec) %>%
   fit(training(splits))
 
 workflow_fit_lm
 
# * Modeltime Process ----
 calibration_tbl <-
   modeltime_table(model_prophet_fit, workflow_fit_lm) %>%
   modeltime_calibrate(testing(splits))
 
 calibration_tbl %>%
   modeltime_accuracy()
 
 calibration_tbl %>%
   modeltime_forecast(new_data = testing(splits),
                      actual_data = evaluation_tbl) %>%
   plot_modeltime_forecast()
 
 # 5.0 SUMMARY & NEXT STEPS ----

# * What you've learned ----
# - You've been exposed to:
#   - Tidymodels / Modeltime Framework
# - You've seen 2 modeling approaches:
#   - Prophet - Univariate, Automatic
#   - Linear Regression Model - Many recipe steps
# - You've experienced Feature Engineering
#   - Visualizations: ACF, Seasonality
#   - Feature Engineering from Date Variables
#
# * Where you are going! ----
# - You still need to learn:
#   - New algorithms
#   - Machine Learning - How to tune parameters
#   - Feature Engineering Strategies
#   - Ensembling - Competition winning strategy
#   - and a lot more!

