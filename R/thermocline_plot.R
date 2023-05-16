#' Thermocline plot
#' @param td_observed observational data to be plotted
#' @param td_forecast forecast data to be plotted
#' @param depths vector of depths to be facets
#' @param tzone time zone for datetime in plot
#' @param ylims vector of c(lower,upper) bounds of temperature on plot
#' @param variable_name variable that is being forecasted
#'
#' @return
#' @export
#'

thermocline_plot <- function(td_observed, td_forecast, depths = 0.5, tzone = "America/New_York", ylims = c(-4,4)){

p <- ggplot(td_forecast, aes(DateTime,td_calc)) +
  geom_line() +
  geom_point(data=td_observed, aes(DateTime,td_calc)) +
  #ylim(c(0,100)) +
  #scale_y_reverse(limits = c(min(depth_calc),max(depth_calc))) +
  scale_y_reverse( lim=c(100,0)) +
  geom_vline(xintercept =  as.POSIXct('2020-06-10'), linetype = 'dashed') +
  ylab('Lake Depth (ft)') +
  xlab('Date') +
  ggtitle('What Depth Is The Cold Layer?') +
  annotate("text", x=as.POSIXct('2020-06-05'), y=15, label= "Past", size = 8) +
  annotate("text", x=as.POSIXct('2020-06-16'), y=15, label= "Future", size = 8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12))

return(p)

}



