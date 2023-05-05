
forecast_s3 <- arrow::s3_bucket('forecasts/parquet', endpoint_override = 's3.flare-forecast.org', anonymous = TRUE)

forecast_data <- arrow::open_dataset(forecast_s3) |>
  dplyr::filter(site_id == 'sunp',
                #reference_datetime > lubridate::as_datetime('2022-10-01 00:00:00'),
                reference_datetime == '2023-05-01 00:00:00',
                variable == 'temperature',
                depth == 1.0)
noaa_forecasts <- function(forecast_bucket, site, variable_name, forecast_start_date, plot_depth){

  forecast_data <- arrow::open_dataset(forecast_bucket) |>
    dplyr::filter(site_id == site,
                  #reference_datetime > lubridate::as_datetime('2022-10-01 00:00:00'),
                  reference_datetime == lubridate::as_datetime(forecast_start_date),
                  variable == variable_name,
                  depth == plot_depth) |>
    collect()


  if (variable_name == 'temperature'){
    # noaa_noon <- noaa_data |>
    #   mutate(time = format(as.POSIXct(datetime), format = '%H:%M')) |>
    #   filter(time == '12:00') |>
    #   mutate(datetime = format(as.POSIXct(datetime), format = '%Y-%m-%d')) |>
    #   mutate(prediction = (prediction - 273.15) * 9/5 + 32)
    #
    # noaa_noon$datetime <- as.Date(noaa_noon$datetime)

    p <- ggplot(forecast_data, aes(x=datetime,y=prediction, group = parameter)) +
      geom_line() +
      ylab('Temperature (deg F)') +
      ggtitle('FLARE Forecast Members')

  } else {
    noaa_noon <- forecast_data |>
      mutate(time = format(as.POSIXct(datetime), format = '%H:%M')) |>
      filter(time == '12:00') |>
      mutate(datetime = format(as.POSIXct(datetime), format = '%Y-%m-%d'))

    noaa_noon$datetime <- as.Date(noaa_noon$datetime)

    p <- ggplot(forecast_data, aes(x=datetime,y=prediction, group = parameter)) +
      geom_line()
  }

  p

  invisible(noaa_data)
}

sunp_forecast <- noaa_forecasts(forecast_bucket = forecast_s3, site = 'sunp', variable_name = 'temperature',forecast_start_date = '2023-05-01 00:00:00', depth = 1.0)

