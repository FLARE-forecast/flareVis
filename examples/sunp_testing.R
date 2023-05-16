library(tidyverse)
library(arrow)
library(plotly)

#ref_dates <- paste(as.character(seq(as.Date('2022-01-01'), as.Date('2023-01-01'), 'day')),'00:00:00')


noaa_s3 <- arrow::s3_bucket("drivers/noaa/gefs-v12-reprocess/stage2/parquet/0", endpoint_override = "s3.flare-forecast.org", anonymous = TRUE)

noaa_data <- arrow::open_dataset(noaa_s3) |>
  dplyr::filter(site_id == "sunp",
                #reference_datetime > lubridate::as_datetime('2022-10-01 00:00:00'),
                reference_datetime == lubridate::as_datetime('2023-05-01 00:00:00'),
                horizon < 408, # just want first 16 days of horizon
                variable == 'air_temperature') |>
  select(datetime,parameter, prediction) |>
  #mutate(time = format(as.POSIXct(datetime), format = '%H:%M')) |>
  #filter(time == '12:00') |>
  #mutate(datetime = format(as.POSIXct(datetime), format = '%Y-%m-%d')) |>
  #rename(param_noaa = parameter, air_temp = prediction) |>
  #select(param_noaa, datetime, air_temp) |>
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



## Make plots (today and future)

temperature_gauge_func <- function(median, q025, q975){
  fig <- plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    #width =500,
    #height = 250,
    value = median,
    #title = list(text = "Swimming Suitability Index (surface)"),
    #title = list(text = "Percent Oxygen Saturation (33ft)"),
    type = "indicator",
    mode = "number+gauge",
    gauge = list(
      axis =list(range = list(50, 180), ticktext = list('Cold (< 120F)','Moderate (120-140F)','Mild (> 140F)','Warm'),
                 tickvals = list(50,120,160,200), tickmode = 'array', tickfont = list(size = 18), ticklen = 8, tickwidth = 3, ticks = 'outside',
                 tickcolor = 'black'),
      bar = list(
        # color = 'white',
        # line = list(width = 1),
        thickness = 0
      ),
      steps = list(
        list(range = c(50,120), color = "darkblue", name = 'TEST_NAME'),
        list(range = c(120,160), color = "#ffffb2", name = 'D'),
        list(range = c(160, 180), color = "#e09999", name = 'C'),#),
        list(range = c(q025, q975), color = 'gray', name = 'B')),#,
      #list(range = c(97.5,100), color = "#008000", name = 'A')),
      threshold = list(
        line = list(color = "brown", width = 4),
        thickness = 0.75,
        value = median)))

  return(fig)
}

fig1 <- temperature_gauge_func(sunp_data_now$median_value, sunp_data_now$q025_value, sunp_data_now$q975_value)

fig2 <- temperature_gauge_func(sunp_data_future$median_value, sunp_data_future$q025_value, sunp_data_future$q975_value)

#temperature_gauge_func(sunp_noaa_forecasts$air_temp_f[1] + sunp_noaa_forecasts$water_temp_f[1])

# temperature_gauge_func()
#
# # add_trace
# #
# # oxygen_gauge_func(current_value = oxy_value_10m$DOSat)
# #
# # oxygen_gauge_func <- function(current_value){
# #   fig <- plot_ly(
# #     domain = list(x = c(0, 1), y = c(0, 1)),
# #     value = current_value,
# #     #title = list(text = "Percent Oxygen Saturation (5ft)"),
# #     title = list(text = "Percent Oxygen Saturation (33ft)"),
# #     type = "indicator",
# #     mode = "number+gauge",
# #     gauge = list(
# #       axis =list(range = list(0, 100)),
# #       bar = list(
# #         # color = 'white',
# #         # line = list(width = 1),
# #         thickness = 0
# #       ),
# #       steps = list(
# #         list(range = c(0,25), color = "#b20000", name = 'TEST_NAME'),
# #         list(range = c(25,50), color = "#e09999", name = 'D'),
# #         list(range = c(50, 75), color = "#ffffb2", name = 'C'),
# #         list(range = c(75, 100), color = '#7fbf7f', name = 'B')),#,
# #       #list(range = c(97.5,100), color = "#008000", name = 'A')),
# #       threshold = list(
# #         line = list(color = "red", width = 4),
# #         thickness = 10,
# #         value = current_value)))
#
#   # fig <- add_trace(
#   #   fig,
#   #   labels = c("Bad", "Okay", "Good", "Great"),
#   #   rotation = 90,
#   #   textinfo = "label",
#   #   textposition = "inside",
#   #   hoverinfo = "none",
#   #   showlegend= FALSE
#   # )
#
#   return(fig)
# }

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
