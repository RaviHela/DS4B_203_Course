# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: ADVANCED TIME SERIES FORECASTING FOR BUSINESS
# MODULE: VISUALIZATION TOOLS ----

# OBJECTIVES ----
# Introduction to:
# - Time Plot
# - Autocorrelation
# - Seasonality 
# - Anomalies
# - Seasonal Decomposition (STL)
# - Time Series Regression (TSLM)

# LIBRARIES ----

library(tidyverse)
library(timetk)
library(lubridate)

# DATA ----

google_analytics_tbl <- read_rds("00_data/google_analytics_summary_hourly.rds")
google_analytics_tbl 

mailchimp_users_tbl <- read_rds("00_data/mailchimp_users.rds")
mailchimp_users_tbl

# DATA PREPARATION ---- 

google_analytics_long_hour_tbl <- google_analytics_tbl %>%
  mutate(date = ymd_h(dateHour)) %>%
  select(-dateHour) %>%
  pivot_longer(cols = pageViews:sessions)

google_analytics_long_hour_tbl 

subscribers_day_tbl <- mailchimp_users_tbl %>%
  summarise_by_time(.date_var = optin_time,
                    .by = "day",
                    optins = n()) %>%
  pad_by_time(.by = "day", .pad_value = 0)

subscribers_day_tbl

# 1.0 TIME SERIES PLOT ----
# - Primary Visualization Tool
# - Spot issues and understand one or more time series

?plot_time_series



# * Basics ----
subscribers_day_tbl %>%
  plot_time_series(.date_var = optin_time, .value = optins)



# * Facets/Groups ----
google_analytics_long_hour_tbl %>%
  #group_by(name) %>%
  plot_time_series(
    .date_var = date,
    .value = value,
    .color_var = name,
    .facet_vars = name
  )

# * Mutations/Transformations ----

subscribers_day_tbl %>%
  plot_time_series(.date_var = optin_time,
                   .value = log(optins + 1))

google_analytics_long_hour_tbl %>%
  #group_by(name) %>%
  plot_time_series(
    .date_var = date,
    .value = log(value + 1),
    .color_var = name,
    .facet_vars = name
  )

# * Smoother Adjustment

subscribers_day_tbl %>%
  plot_time_series(optin_time, log(optins + 1), .smooth = FALSE)

subscribers_day_tbl %>%
  plot_time_series(optin_time, log(optins + 1), 
                   # .smooth_span = 0.75, 
                   .smooth_period = "50 days",
                   .smooth_degree = 2,
                   .smooth_message = TRUE)

google_analytics_long_hour_tbl %>%
  group_by(name) %>%
  plot_time_series(date, log(value + 1), .smooth_period = "7 days")

# * Static ggplot ----

subscribers_day_tbl %>%
  plot_time_series(optin_time, optins, .plotly_slider = TRUE)

subscribers_day_tbl %>%
  plot_time_series(optin_time, optins, .interactive = FALSE)



# 2.0 ACF Diagnostics ----
# - Detecting Lagged Features
?plot_acf_diagnostics

# * ACF / PACF -----
# - Date Features & Fourier Series 

subscribers_day_tbl %>%
  plot_acf_diagnostics(optin_time, log(optins + 1), .lags = "1 year")

# * CCF ----
# - Lagged External Regressors

google_analytics_day_tbl <- google_analytics_long_hour_tbl %>%
  pivot_wider(names_from = name, values_from = value) %>%
  summarise_by_time(date, .by = "day", across(pageViews:sessions, .fns = sum))


subscribers_ga_day_tbl <- subscribers_day_tbl %>%
  left_join(google_analytics_day_tbl,
            by = c("optin_time" = "date"))
  
subscribers_ga_day_tbl %>%
  drop_na() %>%
  plot_acf_diagnostics(optin_time, optins, 
                       .ccf_vars = pageViews:sessions,
                       .show_ccf_vars_only = TRUE,
                       .facet_ncol = 3)
  





# 3.0 SEASONALITY ----
# - Detecting Time-Based Features

?plot_seasonal_diagnostics

google_analytics_long_hour_tbl %>%
  group_by(name) %>%
  plot_seasonal_diagnostics(.date_var = date,
                            .value = log(value + 1),
                            .feature_set = "hour",
                            .geom = "violin")


  



# 4.0 ANOMALIES ----
# - Detecting Events & Possible Data Issues

?plot_anomaly_diagnostics

subscribers_day_tbl %>%
  plot_anomaly_diagnostics(.date_var = optin_time,
                           .value = optins,
                           .alpha = .01, 
                           .max_anomalies = .01)
  

subscribers_day_tbl %>%
  tk_anomaly_diagnostics(.date_var = optin_time,
                           .value = optins,
                           .alpha = .01, 
                           .max_anomalies = .01)

#  Group Anomalies

google_analytics_long_hour_tbl %>%
  group_by(name) %>%
  plot_anomaly_diagnostics(date, value)




# 5.0 SEASONAL DECOMPOSITION ----
# - Detecting Trend and Seasonal Cycles

?plot_stl_diagnostics

subscribers_day_tbl %>%
  plot_stl_diagnostics(optin_time, log(optins + 1), 
                       .frequency = "1 month", 
                       .trend = "1 year")

#Grouped

google_analytics_long_hour_tbl %>%
  group_by(name) %>%
  plot_stl_diagnostics(date, log(value + 1))
  



# 6.0 TIME SERIES REGRESSION PLOT ----
# - Finding features

?plot_time_series_regression


subscribers_day_tbl %>%
  plot_time_series_regression(
    .date_var = optin_time,
    log(optins + 1) ~ as.numeric(optin_time) +
      wday(optin_time, label = TRUE) +
    month(optin_time, label = TRUE),
    .show_summary = TRUE
  )

#Group

google_analytics_long_hour_tbl %>%
  #group_by(name) %>%
  filter(name == "pageViews") %>%
  plot_time_series_regression(
    .date_var = date,
    log(value + 1) ~ as.numeric(date) +
      as.factor(hour(date)) +
      wday(date, label = TRUE) +
      month(date, label = TRUE),
    .show_summary = TRUE
  )
  
  



