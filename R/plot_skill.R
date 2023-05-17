#' Anomaly plot for temperature
#' @param data data frame to be plotted
#' @param ylims vector of c(lower,upper) bounds
#' @param variable_name variable that is being forecasted
#' @param score what score to plot. Can take FLARE scores (crps, logs) or user generated score
#' @param group_id what do want to compare? this will be plotted as colour aesthetic
#' @return
#' @export
#'


plot_skill <- function(data,
                       ylim = NULL,
                       variable_name = NA,
                       score = 'crps',
                       group_id = 'model_id') {

  if(score == "crps"){
    ylab1 = "CRPS "
    if(variable_name == "temperature"){
      ylab = paste0(ylab1,"(°C)")
    }
  } else if(score == "logs"){
    ylab = "Ignorance score"
  } else {
    ylab = score
  }

  p <- ggplot(data, ggplot2::aes(x = horizon, y = get(score), colour = as.factor(get(group_id)))) +
    geom_line() +
    geom_point() +
    scale_y_continuous(limits = ylim) +
    scale_colour_discrete(name = group_id) +
    labs(y = ylab)

  return(p)
}




