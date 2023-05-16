#' NOAA member forecasts plot
#' @param data data frame to be plotted
#' @param depths vector of depths to be facets
#' @param tzone time zone for datetime in plot
#' @param ylims vector of c(lower,upper) bounds of temperature on plot
#' @param variable_name variable that is being forecasted
#'
#' @return
#' @export
#'

noaa_forecasts <- function(data, depths = 0.5, tzone = "America/New_York", ylims = c(-4,4), variable_name= NA){

  if (variable_name == 'air_temperature'){

    p <- ggplot(data, aes(x=datetime,y=prediction, group = parameter)) +
      geom_line() +
      ylab('Temperature (deg F)') +
      xlab('Date') +
      ggtitle('NOAA Ensemble Forecasts') +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text = element_text(size = 16)) +
      theme(axis.title = element_text(size = 20))

  } else {

    p <- ggplot(data, aes(x=datetime,y=prediction, group = parameter)) +
      geom_line()
  }

  return(p)

  #invisible(noaa_data)
}
