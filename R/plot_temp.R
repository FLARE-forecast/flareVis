#' Faceted plot of water temperature
#'
#' @param data data frame to be plotted
#' @param depths vector of depths to be facets
#' @param tzone time zone for datetime in plot
#' @param ylims vector of c(lower,upper) bounds of temperature on plot
#'
#' @return
#' @export
#'
#' @examples
plot_temp <- function(data, depths = 0.5, tzone = "America/New_York", ylims = c(-5,35)) {

  # Generate labels for plots
  my_breaks <- lubridate::with_tz(seq(min(data$datetime), max(data$datetime), by = "1 day"), tzone)
  my_label <- seq(lubridate::as_datetime(data$reference_datetime)[1], max(data$datetime), by = "5 days")
  my_labels <- as.character(my_breaks)
  my_labels[which(!(my_breaks %in% my_label))] <- " "
  y_label <- expression(paste('Water temperature (',degree,'C)', sep = ""))

  # Generate the pot
  data |>
    # Filter the data and get in the right format
    dplyr::filter(depth %in% depths) |>
    dplyr::mutate(datetime = lubridate::with_tz(lubridate::as_datetime(datetime), tzone),
                  reference_datetime = lubridate::with_tz(lubridate::as_datetime(reference_datetime), tzone),
                  depth = paste0("Depth: ", depth)) |>
    dplyr::filter(datetime >= reference_datetime) |>

    ggplot2::ggplot(aes(x = datetime)) +
    ggplot2::geom_ribbon(aes(ymin = quantile10, ymax = quantile90), fill = "lightblue", color = "lightblue") +
    ggplot2::geom_line(aes(y = mean)) +
    ggplot2::scale_x_continuous(breaks = my_breaks, labels = my_labels) +
    ggplot2::facet_wrap(~depth) +
    ggplot2::labs(y = y_label) +
    ggplot2::ylim(ylims) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=0.2)) +
    ggplot2::theme(text = ggplot2::element_text(size = 20))
}

