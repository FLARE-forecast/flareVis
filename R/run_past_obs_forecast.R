library(tidyverse)
source("R/run_past_obs_forecast.R")


forecast_date <- '2022-04-04 00:00:00'
use_s3 <- FALSE


## READ IN DATA
scores_s3 <- arrow::s3_bucket('scores/ler_ms3/parquet',
                              endpoint_override = 's3.flare-forecast.org',
                              anonymous = TRUE)

scores <- arrow::open_dataset(scores_s3) |>
  filter(reference_datetime >= '2022-03-01 00:00:00',
         reference_datetime <= '2022-04-30 00:00:00',
         site_id == 'fcre',
         variable == 'temperature',
         depth %in% c(1,7)) |>
  collect()

past_obs <- scores |>
  filter(reference_datetime <= forecast_date) |>
  filter(reference_datetime == as.Date(datetime)) |>
  drop_na(observation) |>
  distinct(reference_datetime, observation, model_id, .keep_all=TRUE) |>
  select(observation, depth, reference_datetime, datetime, model_id)


future_predictions <- scores |>
  filter(reference_datetime == forecast_date,
         datetime >= (as.Date(forecast_date))) |>
  select(-c(observation,reference_datetime))

full_data <- full_join(future_predictions,past_obs, by = c('depth', 'datetime', 'model_id'))

full_data$reference_datetime <- as.character(full_data$reference_datetime)
full_data$datetime <- as.character(full_data$datetime)

full_data$datetime <- ifelse(is.na(full_data$datetime),full_data$reference_datetime,full_data$datetime)
full_data$datetime <- as.Date(full_data$datetime)


## CALL FUNCTION AND DISPLAY PLOT
forecast_fig <- past_obs_forecast(full_data, tzone = "America/New_York",
                                  variable_name = "temperature", depth = "depth",
                                  ylim = NULL , group_id = 'model_id')

forecast_fig

