library(tidyverse)
library(arrow)
library(plotly)

source('~/flareVis/R/')

noaa_s3 <- arrow::s3_bucket("drivers/noaa/gefs-v12-reprocess/stage2/parquet/0", endpoint_override = "s3.flare-forecast.org", anonymous = TRUE)

noaa_data <- arrow::open_dataset(noaa_s3) |>
  dplyr::filter(site_id == "sunp",
                reference_datetime == lubridate::as_datetime('2023-05-01 00:00:00'),
                horizon < 408, # just want first 16 days of horizon
                variable == 'air_temperature') |>
  select(datetime,parameter, prediction) |>
  collect()

noaa_data_noon <- noaa_data |>
  mutate(time = format(as.POSIXct(datetime), format = '%H:%M')) |>
  filter(time == '12:00') |>
  mutate(datetime = format(as.POSIXct(datetime), format = '%Y-%m-%d')) |>
  rename(param_noaa = parameter, air_temp = prediction) |>
  select(param_noaa, datetime, air_temp)
noaa_data_noon$datetime <- as.Date(noaa_data_noon$datetime)

sunp_forecasts <- arrow::s3_bucket('forecasts/parquet', endpoint_override = 's3.flare-forecast.org', anonymous = TRUE) |>
  arrow::open_dataset() |>
  filter(site_id == 'sunp',
         variable == 'temperature',
         reference_datetime == '2023-05-01 00:00:00',
         depth == 0) |>
  collect()

sunp_forecasts$horizon <-  as.numeric(strftime(sunp_forecasts$datetime, format = "%j")) - as.numeric(strftime(sunp_forecasts$reference_datetime, format = "%j")) + 1

sunp_forecasts <- sunp_forecasts |>
  filter(datetime > '2023-04-30') |>
  arrange(datetime,parameter)

## make vector for matching NOAA forecasts
param_seq <- seq(1:31)
param_day_vec <- rep(param_seq,9)[1:256] ## create vector of ensemble matches for one day (256 ensemble members / day) -- rep 9 times so we can select first 256 values
param_noaa <- rep(param_day_vec, 17) ## create vector for all days

sunp_noaa_forecasts <- cbind(sunp_forecasts,param_noaa)

sunp_noaa_forecasts <- right_join(sunp_noaa_forecasts, noaa_data_noon, by = c('datetime', 'param_noaa'))

#convert to Fahrenheit
sunp_noaa_forecasts$air_temp_f <- (sunp_noaa_forecasts$air_temp - 273.15) * 9/5 + 32
sunp_noaa_forecasts$water_temp_f <- (sunp_noaa_forecasts$prediction * (9/5)) + 32


# summarize and filter data for a specific date
sunp_data_now <- sunp_noaa_forecasts |>
  group_by(datetime) |>
  mutate(median_value = median((air_temp_f + water_temp_f), na.rm = TRUE),
         q025_value = quantile((air_temp_f + water_temp_f),probs=c(0.025))[[1]],
         q975_value = quantile((air_temp_f + water_temp_f),probs=c(0.975))[[1]]) |>
  ungroup() |>
  distinct(datetime, .keep_all = TRUE) |>
  filter(horizon == 0)

sunp_data_future <- sunp_noaa_forecasts |>
  group_by(datetime) |>
  mutate(median_value = median((air_temp_f + water_temp_f), na.rm = TRUE),
         q025_value = quantile((air_temp_f + water_temp_f),probs=c(0.025))[[1]],
         q975_value = quantile((air_temp_f + water_temp_f),probs=c(0.975))[[1]]) |>
  ungroup() |>
  distinct(datetime, .keep_all = TRUE) |>
  filter(horizon == 14)


fig1 <- temperature_gauge_func(sunp_data_now$median_value, sunp_data_now$q025_value, sunp_data_now$q975_value)
fig2 <- temperature_gauge_func(sunp_data_future$median_value, sunp_data_future$q025_value, sunp_data_future$q975_value)

