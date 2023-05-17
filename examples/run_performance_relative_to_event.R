# Title: run_performance_relative_to_event
# Author: Mary Lofton
# Date: 17MAY23

# install packages
library(lubridate)
library(tidyverse)
library(patchwork)
library(arrow)

# source plot functions
source('~/RProjects/flareVis/R/performance_relative_to_event.R')

# pull and filter data
forecast_dates <- c("2022-09-05 00:00:00", "2022-09-12 00:00:00", "2022-09-19 00:00:00", "2022-09-26 00:00:00", "2022-10-03 00:00:00", "2022-10-10 00:00:00", "2022-10-17 00:00:00", "2022-10-24 00:00:00", "2022-10-31 00:00:00")
s3_ler <- arrow::s3_bucket(bucket = "scores/ler_ms3/parquet",
                           endpoint_override =  "s3.flare-forecast.org",
                           anonymous = TRUE)
scores_parquets <- arrow::open_dataset(s3_ler) |>
  filter(model_id %in% c('Simstrat', 'GOTM', 'GLM'),
         reference_datetime %in% forecast_dates,
         horizon < 15,
         horizon > 0,
         variable == 'temperature',
         datetime == as.Date("2022-10-19"),
         depth == 1) |>
  collect()

# plot code
p <- performance_relative_to_event(data = scores_parquets,
                                   ylims = NULL,
                                   variable_name = "temperature",
                                   max_horizon_past = -max(scores_parquets$horizon),
                                   score = "crps",
                                   group_id = "model_id",
                                   focal_date = "2022-10-19")
p
