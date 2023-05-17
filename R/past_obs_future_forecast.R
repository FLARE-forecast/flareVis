#' Anomaly plot for temperature
#' @param data data frame to be plotted
#' @param tzone time zone for datetime in plot
#' @param variable_name variable that is being forecasted
#' @param group_id groupings to visualize in plot (i.e., model_id)
#'
#' @return
#' @export
#'

past_obs_future_forecast <-
  function(data, tzone = "America/New_York", depth = NULL,
           ylim = NULL , site_name = "", variable_name = NA, group_id = NA) {

    if (variable_name == 'temperature'){

      time_plot <- ggplot(data = full_data, aes(datetime, observation, color=as.factor(depth))) +
        geom_point() +
        geom_line() +
        facet_wrap(~as.factor(get(group_id))) +
        geom_line(aes(datetime,median)) +
        geom_line(aes(datetime,quantile97.5)) +
        geom_line(aes(datetime,quantile02.5)) +
        geom_vline(xintercept = as.Date(forecast_date), linetype = 'dashed') +
        ylab(expression("Temperature ("*degree*C*")")) +
        xlab('Date') +  labs(color='Depth (m)') +
        annotate("text", x=as.Date(forecast_date) -7, y=27, label= "Past") +
        annotate("text", x=as.Date(forecast_date) +7, y=27, label= "Future")


      print(time_plot)
    }
  }



#set date for forecast
forecast_date <- '2022-04-04 00:00:00'
use_s3 <- FALSE

## READ IN DATA
  scores_s3 <- arrow::s3_bucket('scores/ler_ms3/parquet',
                                             endpoint_override = 's3.flare-forecast.org',
                                             anonymous = TRUE)

  scores <- arrow::open_dataset(scores_s3) |>
    filter(reference_datetime >= '2022-03-01 00:00:00',
           reference_datetime <= '2022-04-30 00:00:00',
           site_id == 'fcre',
           variable == 'temperature',
           depth %in% c(1,7)) |>
    collect()

past_obs <- scores |>
  filter(reference_datetime <= forecast_date) |>
  filter(reference_datetime == as.Date(datetime)) |>
  drop_na(observation) |>
  distinct(reference_datetime, observation, model_id, .keep_all=TRUE) |>
  select(observation, depth, reference_datetime, datetime, model_id)


future_predictions <- scores |>
  filter(reference_datetime == forecast_date,
         datetime >= (as.Date(forecast_date))) |>
  select(-c(observation,reference_datetime))

full_data <- full_join(future_predictions,past_obs, by = c('depth', 'datetime', 'model_id'))

full_data$reference_datetime <- as.character(full_data$reference_datetime)
full_data$datetime <- as.character(full_data$datetime)

full_data$datetime <- ifelse(is.na(full_data$datetime),full_data$reference_datetime,full_data$datetime)
full_data$datetime <- as.Date(full_data$datetime)


## CALL FUNCTION AND DISPLAY PLOT
forecast_fig <- past_obs_future_forecast(full_data, tzone = "America/New_York",
                                         variable_name = "temperature", depth = "depth",
                                         ylim = NULL , group_id = 'model_id')

forecast_fig

