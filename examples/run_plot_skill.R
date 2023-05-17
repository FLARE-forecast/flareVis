library(tidyverse)
source('R/plot_skill.R')
forecast_dates <- c("2022-09-05 00:00:00", "2022-09-12 00:00:00", "2022-09-19 00:00:00",
                    "2022-09-26 00:00:00", "2022-10-03 00:00:00", "2022-10-10 00:00:00",
                    "2022-10-17 00:00:00", "2022-10-24 00:00:00", "2022-10-31 00:00:00")

models <- c('Simstrat', 'GOTM', 'GLM')

# collect the data from s3 bucket
scores_s3 <- arrow::s3_bucket('scores/ler_ms3/parquet',
                              endpoint_override = 's3.flare-forecast.org',
                              anonymous = TRUE)


score_data <- arrow::open_dataset(scores_s3) |>
  dplyr::filter(model_id %in% c('Simstrat', 'GOTM', 'GLM'),
         reference_datetime %in% forecast_dates,
         horizon < 15,
         horizon > 0,
         variable == 'temperature') |>
  collect()

# what data to plot
agg_scores <- score_data |>
  group_by(model_id, horizon) |>
  summarise(crps = mean(crps, na.rm = T))


# plot data
plot_skill(data = agg_scores,
           score = 'crps',
           variable_name = 'temperature',
           group_id = 'model_id')



# using a user generated score
agg_scores <- score_data |>
  mutate(bias = mean - observation) |>
  group_by(model_id, horizon) |>
  summarise(bias = mean(bias, na.rm = T))

# plot data
plot_skill(data = agg_scores,
           score = 'bias',
           variable_name = 'temperature',
           group_id = 'model_id')
