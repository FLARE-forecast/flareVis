noaa_s3 <- arrow::s3_bucket("drivers/noaa/gefs-v12-reprocess/stage2/parquet/0", endpoint_override = "s3.flare-forecast.org", anonymous = TRUE)

sunp_noaa_data <- arrow::open_dataset(noaa_s3) |>
  dplyr::filter(site_id == 'sunp',
                reference_datetime == lubridate::as_datetime('2021-09-02 00:00:00'),
                variable == variable_name) |>
  collect()


sunp_noaa_noon <- sunp_noaa_data |>
  mutate(time = format(as.POSIXct(datetime), format = '%H:%M')) |>
  filter(time == '12:00') |>
  mutate(datetime = format(as.POSIXct(datetime), format = '%Y-%m-%d')) |>
  mutate(prediction = (prediction - 273.15) * 9/5 + 32)

sunp_noaa_noon$datetime <- as.Date(noaa_noon$datetime)


sunp_noaa_fig <- noaa_forecasts(noaa_noon_df = sunp_noaa_noon, variable_name = 'temperature')
