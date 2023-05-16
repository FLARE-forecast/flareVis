forecast_s3 <- arrow::s3_bucket('forecasts/parquet', endpoint_override = 's3.flare-forecast.org', anonymous = TRUE)

sunp_data <- arrow::open_dataset(forecast_s3) |>
  dplyr::filter(site_id == 'sunp',
                reference_datetime ==  '2023-05-01 00:00:00',
                variable == 'temperature',
                depth == 1.0) |>
  collect()

forecast_data <- sunp_data |>
  mutate(prediction_f = (prediction * (9/5)) + 32)

flare_forcasts_fig <- flare_forecasts(forecast_data = sunp_data, variable_name = 'temperature')
