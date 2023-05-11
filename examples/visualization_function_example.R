library(tidyverse)
library(arrow)

example_plot <- function(data, depths, tzone, ylims){

  p <- ggplot(data, aes(x=datetime,y=prediction, group = parameter)) +
    geom_line() +
    ylab('Temperature (deg C)') +
    xlab('Date') +
  ggtitle('FLARE Forecast Members') +
    ylim(ylims) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

  return(p)
}

## READ IN DATA
forecast_s3 <- arrow::s3_bucket('forecasts/parquet', endpoint_override = 's3.flare-forecast.org', anonymous = TRUE)

sunp_data <- arrow::open_dataset(forecast_s3) |>
  dplyr::filter(site_id == 'sunp',
                reference_datetime ==  '2023-05-01 00:00:00',
                variable == 'temperature',
                depth == 1.0) |>
  collect()


## CALL FUNCTION AND DISPLAY PLOT
forecast_fig <- example_plot(sunp_data, depths = NULL, tzone = NULL, ylims = c(5,25))

forecast_fig
