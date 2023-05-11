#' @param noaa_noon_df dataframe that contains noaa forecasts with a single prediction per day
#' @param variable_name variabe that is being forecasted

noaa_forecasts <- function(noaa_noon_df, variable_name){

  if (variable_name == 'air_temperature'){

    p <- ggplot(noaa_noon_df, aes(x=datetime,y=prediction, group = parameter)) +
      geom_line() +
      ylab('Temperature (deg F)') +
      xlab('Date') +
      ggtitle('NOAA Ensemble Forecasts') +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text = element_text(size = 16)) +
      theme(axis.title = element_text(size = 20))

  } else {

    p <- ggplot(noaa_noon_df, aes(x=datetime,y=prediction, group = parameter)) +
      geom_line()
  }

  return(p)

  #invisible(noaa_data)
}
