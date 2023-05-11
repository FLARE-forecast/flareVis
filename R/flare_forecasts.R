#' @param forecast_data dataframe containing forecast data
#' @param variable_name variable that is being forecasted

flare_forecasts <- function(forecast_data, variable_name){

  if (variable_name == 'temperature'){

    p <- ggplot(forecast_data, aes(x=datetime,y=prediction_f, group = parameter)) +
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

}


#
# ## make plot with Whitney's data -- includes DA
#
# old_sunp_data <- read_csv('/home/addelany/Documents/whitney_code/sunp-2021-09-02-all_UC.csv.gz')
#
# sunp_data_subset <- old_sunp_data |>
#   dplyr::filter(variable == 'temperature',
#               depth == 1.0) |>
#   mutate(prediction_f = (prediction * (9/5)) + 32)
#
# sunp_plot <- ggplot(sunp_data_subset, aes(x=datetime,y=prediction_f, group = parameter)) +
#   geom_line() +
#   ylab('Temperature (deg F)') +
#   xlab('Date') +
#   ggtitle('FLARE Forecast Members') +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(axis.text = element_text(size = 16)) +
#   theme(axis.title = element_text(size = 20))
#
# sunp_plot
