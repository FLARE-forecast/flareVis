# Title: performance_relative_to_event
# Author: Mary Lofton
# Date: 17MAY23

#Function to render plot of forecast performance relative to an event date
#'@param data data frame with (at a minimum) the following columns:
#'horizon: the forecast horizon
#'depth: depth of the forecast in meters
#'a column whose name is the forecast scoring metric you wish to plot (e.g., "crps")
#'optionally, a column of group ids (e.g., model or team ids, different depths you want to plot, etc.)
#'@param ylims optionally, y axis limits for plotting, provided as c(min, max)
#'@param variable_name character string of target variable, eg., "temperature"
#'@param max_horizon_past maximum days prior to event that you would like to plot scores for as a *negative* number (e.g., -35)
#'@param score character string of the column name containing the score you would like to plot (e.g., "crps")
#'@param group_id character string of the column name containing the variable you would like to group by in plot (e.g., "model_id" or "depth")
#'@param focal_date character string of event date (e.g., date of turnover)

performance_relative_to_event <- function(data,
                                          ylims,
                                          variable_name,
                                          max_horizon_past,
                                          score,
                                          group_id,
                                          focal_date){

  plot_data <- data %>%
    mutate(horizon_past = -horizon) %>%
    filter(horizon_past >= max_horizon_past) %>%
    rename(score = any_of(score),
           group_id = any_of(group_id))

  if(score == "crps"){
    ylab1 = "CRPS "
    if(variable_name == "temperature"){
      ylab = paste0(ylab1,"(°C)")
    }
  } else if(score == "logs"){
    ylab = "Ignorance score"
  }

  p <- ggplot(data = plot_data, aes(x = horizon_past, y = score, color = group_id)) +
    geom_line() +
    geom_point() +
    xlim(max_horizon_past,0) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(x = "Days prior to event date", y = ylab, title = paste0("Forecasts for ",focal_date)) +
    scale_y_continuous(limits = ylims)

  return(p)

}
