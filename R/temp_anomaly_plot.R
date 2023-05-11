#' @param anomaly_df dataframe containing anomaly information

temp_anomaly <- function(amonoly_df){

temp_anomaly_plot <- ggplot(data=temp_anomaly_upper, aes(x=DateTime,y=difference)) +
  #geom_line() +
  geom_line(color = 'darkred') +
  geom_line(data = temp_anomaly_lower, aes(DateTime,difference), color = 'darkblue') +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  xlab('Day') +
  ylab('Water Temperature Anomaly (deg F)') +
  ggtitle('Water Temperature Anomaly') +
  theme(plot.title = element_text(hjust = 0.5))

return(temp_anomaly_plot)
}
