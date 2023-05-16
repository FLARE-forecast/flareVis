#' Anomaly plot for temperature
#' @param data data frame to be plotted
#' @param depths vector of depths to be facets
#' @param tzone time zone for datetime in plot
#' @param ylims vector of c(lower,upper) bounds of temperature on plot
#'
#' @return
#' @export
#'

temp_anomaly <- function(data, depths = 0.5, tzone = "America/New_York", ylims = c(-4,4)){

temp_anomaly_plot <- ggplot(data=data, aes(x=DateTime,y=difference)) +
  geom_line() +
  #geom_line(color = 'darkred') +
  #geom_line(data = temp_anomaly_lower, aes(DateTime,difference), color = 'darkblue') +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  xlab('Day') +
  ylab('Water Temperature Anomaly (deg F)') +
  ylim(ylims) +
  ggtitle('Water Temperature Anomaly') +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=as.POSIXct('2020-07-07'), y=0.3, label= "above normal", size = 4) +
  annotate("text", x=as.POSIXct('2020-07-07'), y=-0.3, label= "below normal", size = 4)

return(temp_anomaly_plot)
}

