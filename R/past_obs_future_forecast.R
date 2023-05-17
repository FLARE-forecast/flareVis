#' Anomaly plot for temperature
#' @param data data frame to be plotted
#' @param tzone time zone for datetime in plot
#' @param variable_name variable that is being forecasted
#' @param group_id groupings to visualize in plot (i.e., model_id)
#'
#' @return
#' @export
#'

#note - still working on being able to specify differennt depths, groups, and horizons
past_obs_future_forecast <-
  function(data, depths = 0.5, tzone = "America/New_York", ylim = NULL ,site_name = "", variable_name= NA, group_id = NA) {

    if (variable_name == 'temperature'){

      time_plot <- ggplot(data = full_data, aes(datetime, observation, color=depth)) +
        geom_point() +
        geom_line() +
        geom_line(aes(datetime,median)) +
        geom_line(aes(datetime,quantile97.5)) +
        geom_line(aes(datetime,quantile02.5)) +
        geom_vline(xintercept = as.Date(forecast_date), linetype = 'dashed') +
        ylab('Temperature (degC)') +
        xlab('Date') +
        annotate("text", x=as.Date(forecast_date) -10, y=27, label= "Past") +
        annotate("text", x=as.Date(forecast_date) +7, y=27, label= "Future")


      print(time_plot)

    }
    }

#set date for forecast
forecast_date <- '2023-04-01 00:00:00'

## READ IN DATA
scores_s3 <- arrow::s3_bucket('scores/parquet', endpoint_override = 's3.flare-forecast.org', anonymous = TRUE)

scores <- arrow::s3_bucket('scores/parquet/', endpoint_override = 's3.flare-forecast.org') %>%
     arrow::open_dataset() %>%
     filter(reference_datetime >= '2023-03-01 00:00:00',
            reference_datetime <= '2023-04-30 00:00:00',
            site_id == 'fcre',
            variable == 'temperature',
            depth == 1) %>%
     collect()

past_obs <- scores %>%
  filter(datetime < forecast_date) %>%
  drop_na(observation) %>%
  distinct(reference_datetime, observation, .keep_all=TRUE) %>%
  select(observation, depth, datetime)


future_predictions <- scores %>%
  filter(reference_datetime == forecast_date,
         datetime != (as.Date(forecast_date) -1)) %>%
  select(-observation)

full_data <- full_join(future_predictions,past_obs, by = c('depth', 'datetime'))

full_data$reference_datetime <- as.character(full_data$reference_datetime)
full_data$datetime <- as.character(full_data$datetime)

full_data$datetime <- ifelse(is.na(full_data$datetime),full_data$reference_datetime,full_data$datetime)
full_data$datetime <- as.Date(full_data$datetime)


## CALL FUNCTION AND DISPLAY PLOT
forecast_fig <- past_obs_future_forecast(scores, tzone = NULL, ylim= NULL, variable_name = "temperature",group_id = model_id)

forecast_fig
