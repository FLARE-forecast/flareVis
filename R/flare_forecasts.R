
forecast_s3 <- arrow::s3_bucket('forecasts/parquet', endpoint_override = 's3.flare-forecast.org', anonymous = TRUE)


flare_forecasts <- function(forecast_bucket, site, variable_name, forecast_start_date, plot_depth){

  forecast_data <- arrow::open_dataset(forecast_bucket) |>
    dplyr::filter(site_id == site,
                  #reference_datetime > lubridate::as_datetime('2022-10-01 00:00:00'),
                  reference_datetime ==  forecast_start_date,
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
      xlab('Date')
      ggtitle('FLARE Forecast Members') +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))

  } else {

    p <- ggplot(forecast_data, aes(x=datetime,y=prediction, group = parameter)) +
      geom_line()
  }

  return(p)

  #invisible(forecast_data)
}

flare_forecasts(forecast_bucket = forecast_s3, site = 'sunp', variable_name = 'temperature', forecast_start_date = '2023-05-01 00:00:00', plot_depth = 1.0)

