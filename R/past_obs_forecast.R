#' Anomaly plot for temperature
#' @param data data frame to be plotted
#' @param tzone time zone for datetime in plot
#' @param site_id site you want to visualize in plot
#' @param variable_name variable that is being forecasted
#' @param group_id groupings to visualize in plot (i.e., model_id)
#'
#' @return
#' @export
#'

past_obs_forecast <-
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

