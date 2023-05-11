#' Anomaly plot for temperature
#' @param data data frame to be plotted
#' @param depths vector of depths to be facets
#' @param tzone time zone for datetime in plot
#' @param ylims vector of c(lower,upper) bounds of temperature on plot
#' @param variable_name variable that is being forecasted
#'
#' @return
#' @export
#'

flare_forecasts <- function(data, depths = 0.5, tzone = "America/New_York", ylims = c(-4,4), variable_name= NA){

  if (variable_name == 'temperature'){

    p <- ggplot(data, aes(x=datetime,y=prediction_f, group = parameter)) +
      geom_line() +
      ylab('Temperature (deg F)') +
      xlab('Date')
      ggtitle('FLARE Forecast Members') +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))

  } else {

    p <- ggplot(data, aes(x=datetime,y=prediction, group = parameter)) +
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
