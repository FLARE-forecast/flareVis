library(tidyverse)
library(arrow)
library(plotly)

#ref_dates <- paste(as.character(seq(as.Date('2022-01-01'), as.Date('2023-01-01'), 'day')),'00:00:00')


s3 <- arrow::s3_bucket("drivers/noaa/gefs-v12-reprocess/stage2/parquet/0", endpoint_override = "s3.flare-forecast.org", anonymous = TRUE)

noaa_data <- arrow::open_dataset(s3) |>
  dplyr::filter(site_id == "sunp",
                #reference_datetime > lubridate::as_datetime('2022-10-01 00:00:00'),
                reference_datetime == lubridate::as_datetime('2023-04-01 00:00:00'),
                horizon < 408, # just want first 16 days of horizon
                variable == 'air_temperature') %>%
  filter(variable == 'air_temperature') |>
  collect()
#FLAREr:::unset_arrow_vars(vars)

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
         #reference_datetime <= '2023-04-01',
         reference_datetime == '2023-04-01 00:00:00',
         depth == 0) |>
  collect()

sunp_forecasts$horizon <-  as.numeric(strftime(sunp_forecasts$datetime, format = "%j")) - as.numeric(strftime(sunp_forecasts$reference_datetime, format = "%j")) + 1

sunp_forecasts <- sunp_forecasts |>
  filter(datetime > '2023-03-31') |>
  arrange(datetime,parameter)

## make vector for matching NOAA forecasts
param_seq <- seq(1:31)
param_day_vec <- rep(param_seq,9)[1:256] ## create vector of ensemble matches for one day (256 ensemble members / day)
param_noaa <- rep(param_day_vec, 17) ## create vector for all days

sunp_forecasts <- cbind(sunp_forecasts,param_noaa)

sunp_forecasts <- right_join(sunp_forecasts, noaa_data_noon, by = c('datetime', 'param_noaa'))

#convert to Fahrenheit
sunp_forecasts$air_temp <- (sunp_forecasts$air_temp - 273.15) * 9/5 + 32


## Make plots (today and future)



temperature_gauge_func <- function(current_value){
  fig <- plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = current_value,
    #title = list(text = "Percent Oxygen Saturation (5ft)"),
    title = list(text = "Percent Oxygen Saturation (33ft)"),
    type = "indicator",
    mode = "number+gauge",
    gauge = list(
      axis =list(range = list(50, 180)),
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
        value = current_value)))

  return(fig)
}

temperature_gauge_func(100)

# add_trace
#
# oxygen_gauge_func(current_value = oxy_value_10m$DOSat)
#
# oxygen_gauge_func <- function(current_value){
#   fig <- plot_ly(
#     domain = list(x = c(0, 1), y = c(0, 1)),
#     value = current_value,
#     #title = list(text = "Percent Oxygen Saturation (5ft)"),
#     title = list(text = "Percent Oxygen Saturation (33ft)"),
#     type = "indicator",
#     mode = "number+gauge",
#     gauge = list(
#       axis =list(range = list(0, 100)),
#       bar = list(
#         # color = 'white',
#         # line = list(width = 1),
#         thickness = 0
#       ),
#       steps = list(
#         list(range = c(0,25), color = "#b20000", name = 'TEST_NAME'),
#         list(range = c(25,50), color = "#e09999", name = 'D'),
#         list(range = c(50, 75), color = "#ffffb2", name = 'C'),
#         list(range = c(75, 100), color = '#7fbf7f', name = 'B')),#,
#       #list(range = c(97.5,100), color = "#008000", name = 'A')),
#       threshold = list(
#         line = list(color = "red", width = 4),
#         thickness = 10,
#         value = current_value)))

  # fig <- add_trace(
  #   fig,
  #   labels = c("Bad", "Okay", "Good", "Great"),
  #   rotation = 90,
  #   textinfo = "label",
  #   textposition = "inside",
  #   hoverinfo = "none",
  #   showlegend= FALSE
  # )

  return(fig)
}

oxygen_gauge_func(current_value = 90)

oxygen_gauge_func(current_value = oxy_value_10m$DOSat)

# sunp_scores <- arrow::s3_bucket('scores/parquet/', endpoint_override = 's3.flare-forecast.org', anonymous = TRUE) %>%
#   arrow::open_dataset() %>%
#   #filter(reference_datetime %in% ref_dates) %>%
#   #reference_datetime < '2023-01-01 00:00:00',
#   filter(site_id == 'sunp',
#          model_id == 'test_runS3', #%>%
#          variable == 'temperature') %>%
#   collect()
