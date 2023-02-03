#' Faceted plot of water temperature
#'
#' @param score_df data frame to be plotted
#' @param depths vector of depths to be facets
#' @param tzone time zone for datetime in plot
#' @param ylims lower,upper bounds of temperature on plot
#'
#' @return
#' @export
#'
#' @examples
plot_temp <- function(score_df, depths = 0.5, tzone = "America/New_York", ylims = c(-5,35)) {

  # Generate labels for plots
  my_breaks <- lubridate::with_tz(seq(min(score_df$datetime), max(score_df$datetime), by = "1 day"), tzone)
  my_label <- seq(lubridate::as_datetime(score_df$reference_datetime)[1], max(score_df$datetime), by = "5 days")
  my_labels <- as.character(my_breaks)
  my_labels[which(!(my_breaks %in% my_label))] <- " "
  y_label <- expression(paste('Water temperature (',degree,'C)', sep = ""))

  # Generate the pot
  score_df |>
    # Filter the score_df and get in the right format
    dplyr::filter(depth %in% depths) |>
    dplyr::mutate(datetime = lubridate::with_tz(lubridate::as_datetime(datetime), tzone),
                  reference_datetime = lubridate::with_tz(lubridate::as_datetime(reference_datetime), tzone),
                  depth = paste0("Depth: ", depth)) |>
    dplyr::filter(datetime >= reference_datetime) |>

    ggplot(aes(x = datetime)) +
    geom_ribbon(aes(ymin = quantile10, ymax = quantile90), fill = "lightblue", color = "lightblue") +
    geom_line(aes(y = mean)) +
    scale_x_continuous(breaks = my_breaks, labels = my_labels) +
    facet_wrap(~depth) +
    labs(y = y_label) +
    ylim(ylims) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.2)) +
    theme(text = element_text(size = 20))
}

