#' Temperature plot with all depths on a single panel
#'
#' @param data data frame of scores
#' @param depths vector of depths to be facets
#' @param tzone time zone for datetime in plot
#' @param ylims vector of c(lower,upper) bounds of temperature on plot
#' @param site_name name of site
#'
#' @return
#' @export
#'
plot_temp_single_panel <- function(data, depths = 0.5, tzone = "America/New_York", ylims = c(-5,35), site_name = "", obs_hist){

  # Fix dates and rename columns to match plotting code
  curr_tibble <- data |>
    dplyr::filter(depth %in% depths) |>
    dplyr::mutate(datetime = lubridate::with_tz(lubridate::as_datetime(datetime), tzone),
                  reference_datetime = lubridate::with_tz(lubridate::as_datetime(reference_datetime), tzone)) |>#,
    dplyr::filter(datetime >= reference_datetime) |>
    rename(date = datetime, forecast_mean = mean, forecast_sd = sd, forecast_upper_90 = quantile90, forecast_lower_90 = quantile10,
           observed = observation, forecast_start_day = reference_datetime)

  ggplot2::ggplot(curr_tibble, ggplot2::aes(x = as.Date(date))) +
    ggplot2::ylim(ylims) +
    ggplot2::xlim(c(as.Date(min((curr_tibble$date)) - lubridate::days(5)), (as.Date(max(curr_tibble$date)) + lubridate::days(5)))) +
    ggplot2::geom_line(ggplot2::aes(y = forecast_mean, color = as.factor(depth)), size = 0.5)+
    ggplot2::geom_ribbon(ggplot2::aes(ymin = forecast_lower_90, ymax = forecast_upper_90,
                                      fill = as.factor(depth)),
                         alpha = 0.2) +
    ggplot2::geom_point(data = obs_hist, ggplot2::aes(x=as.Date(reference_datetime),y = observation, color = as.factor(depth)), size = 2) +
    #ggplot2::geom_point(ggplot2::aes(y = observed, color = as.factor(depth)), size = 2) +
    ggplot2::geom_vline(aes(xintercept = as.Date(forecast_start_day),
                            linetype = "solid"),
                        alpha = 1) +
    #alpha = forecast_start_day_alpha) +
    #ggplot2::annotate(x = as.Date(curr_tibble$forecast_start_day - 2*24*60*60), y = max(curr_tibble$forecast_upper_90), label = 'Past', geom = 'text') +
    #ggplot2::annotate(x = as.Date(curr_tibble$forecast_start_day + 3*24*60*60), y = max(curr_tibble$forecast_upper_90), label = 'Future', geom = 'text') +
    ggplot2::annotate(x = as.Date(curr_tibble$forecast_start_day - 24*60*60), y = max(curr_tibble$forecast_upper_90), label = 'Past', geom = 'text') +
    ggplot2::annotate(x = as.Date(curr_tibble$forecast_start_day + 24*60*60), y = max(curr_tibble$forecast_upper_90), label = 'Future', geom = 'text') +
    ggplot2::theme_light() +
    ggplot2::scale_fill_manual(name = "Depth (m)",
                               values = c("#D55E00", '#009E73', '#0072B2'),
                               labels = as.character(depths)) +
    #labels = c('0.1', '5.0', '10.0')) +
    ggplot2::scale_color_manual(name = "Depth (m)",
                                values = c("#D55E00", '#009E73', '#0072B2'),
                                labels = as.character(depths)) +
    #labels = c('0.1', '5.0', '10.0')) +
    # ggplot2::scale_x_date(date_breaks = '4 days',
    #                       date_labels = '%b %d\n%a',
    #                       limits = c(as.Date(min(curr_tibble$date) - 10), as.Date(max(curr_tibble$date)))) +
    #limits = c(as.Date(min(obs_hist$date)), as.Date(max(curr_tibble$date)))) +
    #limits = c(as.Date(config$run_config$start_datetime) - 1, as.Date(config$run_config$forecast_start_datetime) + num_days_plot)) +
    ggplot2::scale_linetype_manual(name = "",
                                   values = c('solid'),
                                   labels = c('Forecast Date')) +
    ggplot2::scale_y_continuous(name = 'Temperature (°C)',
                                limits = ylims,
                                sec.axis = sec_axis(trans = (~.*(9/5) + 32), name = 'Temperature (°F)')) +
    ggplot2::labs(x = "Date",
                  y = "Temperature (°C)", #state_names[i],
                  fill = 'Depth (m)',
                  color = 'Depth',
                  title = paste0(site_name," water temperature forecast, ", lubridate::date(curr_tibble$forecast_start_day)),
                  caption = 'Points represent sensor observations of water temperature. Lines represents the mean prediction from the forecast ensembles, or the most likely outcome.\n The shaded areas represent the 90% confidence interval of the forecast, or the possible range of outcomes based on the forecast.') +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
                   plot.title = element_text(size = 16))
}
