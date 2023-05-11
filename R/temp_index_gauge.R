#' swimming suitability dial plot
#' @param median median oxygen value
#' @param q025 2.5% confidence value
#' @param q975 97.5% confidence value
#' @return
#' @export
#'

temperature_index_gauge <- function(median, q025, q975){
  fig <- plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = median,
    type = "indicator",
    mode = "number+gauge",
    gauge = list(
      axis =list(range = list(50, 180), ticktext = list('Cold (< 120F)','Moderate (120-140F)','Mild (> 140F)','Warm'),
                 tickvals = list(50,120,160,200), tickmode = 'array', tickfont = list(size = 18), ticklen = 8, tickwidth = 3, ticks = 'outside',
                 tickcolor = 'black'),
      bar = list(
        # color = 'white',
        # line = list(width = 1),
        thickness = 0
      ),
      steps = list(
        list(range = c(50,120), color = "darkblue", name = 'TEST_NAME'),
        list(range = c(120,160), color = "#ffffb2", name = 'D'),
        list(range = c(160, 180), color = "#e09999", name = 'C'),#),
        list(range = c(q025, q975), color = 'gray', name = 'B')),#,
      #list(range = c(97.5,100), color = "#008000", name = 'A')),
      threshold = list(
        line = list(color = "brown", width = 4),
        thickness = 0.75,
        value = median)))

  return(fig)
}


