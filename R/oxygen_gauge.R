#' @param oxy_data dataframe containing oxygen saturation data. Should include DateTime, Depth, and DOSat
#' @param plot_date date for the plot
#' @param plot_depth depth for the plot

oxygen_gauge <- function(median_oxy, q025_oxy, q975_oxy){

fig <- plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = median_oxy,
    #title = list(text = "Percent Oxygen Saturation"),
    type = "indicator",
    mode = "number+gauge",
    gauge = list(
      axis =list(range = list(0, 100), tickmode = 'array', tickfont = list(size = 18), ticklen = 5, tickwidth = 2, ticks = 'outside',
                 tickcolor = 'black'),
      bar = list(
        # color = 'white',
        # line = list(width = 1),
        thickness = 0
      ),
      steps = list(
        list(range = c(0,25), color = "#b20000", name = 'TEST_NAME'),
        list(range = c(25,50), color = "#e09999", name = 'D'),
        list(range = c(50, 75), color = "#ffffb2", name = 'C'),
        list(range = c(75, 100), color = '#7fbf7f', name = 'B'),
        list(range = c(q025_oxy, q975_oxy), color = 'grey', name = 'B')),#,
      #list(range = c(97.5,100), color = "#008000", name = 'A')),
      threshold = list(
        line = list(color = "red", width = 4),
        thickness = 0.75,
        value = median_oxy)))

return(fig)
}
