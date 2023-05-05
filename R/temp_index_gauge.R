#' @param met_data dataframe for the noaa (or other) met data
#' @param flare_data dataframe for the flare forecast
#' @param forecast_date referece_datetime value to use for forecast data
#' @param plot_depth depth for the data used in the plot
#' @param site site to be used for the plot
#'
temp_index_gauge <- function(met_data, flare_data, forecast_date, plot_depth, site){

  noaa_data <- met_data |>
    dplyr::filter(site_id == site,
                  reference_datetime == lubridate::as_datetime(forecast_date),
                  horizon < 408, # just want first 16 days of horizon
                  variable == 'air_temperature')

  noaa_data_noon <- noaa_data |>
    mutate(time = format(as.POSIXct(datetime), format = '%H:%M')) |>
    filter(time == '12:00') |>
    mutate(datetime = format(as.POSIXct(datetime), format = '%Y-%m-%d')) |>
    rename(param_noaa = parameter, air_temp = prediction) |>
    select(param_noaa, datetime, air_temp)
  noaa_data_noon$datetime <- as.Date(noaa_data_noon$datetime)


  sunp_forecasts <- flare_data |>
    filter(site_id == site,
           variable == 'temperature',
           #reference_datetime <= '2023-04-01',
           reference_datetime == forecast_date,
           depth == plot_depth)


  sunp_forecasts$horizon <-  as.numeric(strftime(sunp_forecasts$datetime, format = "%j")) - as.numeric(strftime(sunp_forecasts$reference_datetime, format = "%j")) + 1

  sunp_forecasts <- sunp_forecasts |>
    filter(datetime > as.Date(forecast_date)-1) |>
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

  combined_temp <- sunp_noaa_forecasts$air_temp_f[1] + sunp_noaa_forecasts$water_temp_f[1]

  fig <- plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = combined_temp,
    title = list(text = "Swimming Suitability Index (surface)"),
    #title = list(text = "Percent Oxygen Saturation (33ft)"),
    type = "indicator",
    mode = "number+gauge",
    gauge = list(
      axis =list(range = list(50, 180), ticktext = list('Cold','Moderate','Mild','Warm'), tickvals = list(50,120,160,200), tickmode = 'array', tickfont = list(size = 15)),
      bar = list(
        # color = 'white',
        # line = list(width = 1),
        thickness = 0
      ),
      steps = list(
        list(range = c(0,120), color = "darkblue", name = 'TEST_NAME'),
        list(range = c(120,160), color = "#ffffb2", name = 'D'),
        list(range = c(160, 180), color = "#e09999", name = 'C')),
      #list(range = c(75, 100), color = '#7fbf7f', name = 'B')),#,
      #list(range = c(97.5,100), color = "#008000", name = 'A')),
      threshold = list(
        line = list(color = "red", width = 4),
        thickness = 0.75,
        value = combined_temp)))

  return(fig)

  }
