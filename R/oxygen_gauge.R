#' @param oxy_data dataframe containing oxygen saturation data. Should include DateTime, Depth, and DOSat
#' @param plot_date date for the plot
#' @param plot_depth depth for the plot

oxygen_gauge <- function(oxy_data,plot_date, plot_depth){

oxy_values <- oxy_data %>%
  filter(DateTime == plot_date,
         Depth == plot_depth) %>%
  select(DOSat)


fig <- plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = oxy_values$DOSat,
    #title = list(text = "Percent Oxygen Saturation (5ft)"),
    title = list(text = "Percent Oxygen Saturation (33ft)"),
    type = "indicator",
    mode = "number+gauge",
    gauge = list(
      axis =list(range = list(0, 100)),
      bar = list(
        # color = 'white',
        # line = list(width = 1),
        thickness = 0
      ),
      steps = list(
        list(range = c(0,25), color = "#b20000", name = 'TEST_NAME'),
        list(range = c(25,50), color = "#e09999", name = 'D'),
        list(range = c(50, 75), color = "#ffffb2", name = 'C'),
        list(range = c(75, 100), color = '#7fbf7f', name = 'B')),#,
      #list(range = c(97.5,100), color = "#008000", name = 'A')),
      threshold = list(
        line = list(color = "red", width = 4),
        thickness = 0.75,
        value = current_value)))

fig

invisible(oxy_values)
}
