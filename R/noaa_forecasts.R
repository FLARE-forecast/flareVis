#' @param forecast_start_date forecast reference_datetime value (yyy-mm-dd HH:MM:SS)

noaa_s3 <- arrow::s3_bucket("drivers/noaa/gefs-v12-reprocess/stage2/parquet/0", endpoint_override = "s3.flare-forecast.org", anonymous = TRUE)
#
# noaa_data <- arrow::open_dataset(noaa_s3) |>
#   dplyr::filter(site_id == "sunp",
#                 #reference_datetime > lubridate::as_datetime('2022-10-01 00:00:00'),
#                 reference_datetime == lubridate::as_datetime('2023-04-01 00:00:00'),
#                 horizon < 408, # just want first 16 days of horizon
#                 variable == 'air_temperature') %>%
#   filter(variable == 'air_temperature') |>
#   collect()


noaa_forecasts <- function(noaa_bucket, site, variable_name, forecast_start_date){

  noaa_data <- arrow::open_dataset(noaa_bucket) |>
    dplyr::filter(site_id == site,
                  #reference_datetime > lubridate::as_datetime('2022-10-01 00:00:00'),
                  reference_datetime == lubridate::as_datetime(forecast_start_date),
                  variable == variable_name) |>
                  collect()


  if (variable_name == 'air_temperature'){
    noaa_noon <- noaa_data |>
      mutate(time = format(as.POSIXct(datetime), format = '%H:%M')) |>
      filter(time == '12:00') |>
      mutate(datetime = format(as.POSIXct(datetime), format = '%Y-%m-%d')) |>
      mutate(prediction = (prediction - 273.15) * 9/5 + 32)

    noaa_noon$datetime <- as.Date(noaa_noon$datetime)

    p <- ggplot(noaa_noon, aes(x=datetime,y=prediction, group = parameter)) +
      geom_line() +
      ylab('Temperature (deg F)') +
      xlab('Date')
      ggtitle('NOAA Ensemble Forecasts') +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))

  } else {
    noaa_noon <- noaa_data |>
      mutate(time = format(as.POSIXct(datetime), format = '%H:%M')) |>
      filter(time == '12:00') |>
      mutate(datetime = format(as.POSIXct(datetime), format = '%Y-%m-%d'))

    noaa_noon$datetime <- as.Date(noaa_noon$datetime)

    p <- ggplot(noaa_noon, aes(x=datetime,y=prediction, group = parameter)) +
      geom_line()
  }

  return(p)

  #invisible(noaa_data)
}

sunp_noaa <- noaa_forecasts(noaa_bucket = noaa_s3, site = 'sunp', variable_name = 'air_temperature', forecast_start_date = '2023-05-01 00:00:00')

