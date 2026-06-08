#' Plot measles and rubella cases over time
#'
#' @param measles_data a dataframe of measles and rubella cases using load_data()
#' @param start start year to plot cases
#' @param end end year to plot cases
#'
#' @returns ggplot plot of cases between given years
#'
#' @importFrom tibble tibble
#' @importFrom dplyr filter group_by summarize
#' @importFrom ggplot2 ggplot geom_ribbon geom_line scale_color_manual scale_x_continuous scale_y_continuous geom_text geom_point theme_bw labs theme aes element_blank
#' @export
#'
#' @examples
#' plot_cases(load_data(), 2012, 2024)
plot_cases <- function(measles_data, start = 2012, end = 2024) {

  new_start <- start
  new_end <- end

  if (!is.numeric(start)) {
    stop("Please enter a numeric value for the beginning year.")
  } else if (!is.numeric(end)) {
    stop("Please enter a numeric value for the end year.")
  } else if ((start != floor(start)) | (end != floor(end))) {
    stop("Please enter an integer between 2012 and 2025.")
  } else if (start < 2012 | start > 2025) {
    stop("Please enter a year between 2012 and 2024.")
  } else if (end < 2012 | end > 2025) {
    stop("Please enter a year between 2012 and 2024.")
  } else if (start > end) {
    warning("Years not in order. Switching start and end year.")

    new_start <- end
    new_end <- start

  } else if(end - start == 0) {
    stop("Please select an end year that is different from the start year.")
  }

  measles_data |>
    filter(year >= new_start,
           year <= new_end) |>
    group_by(year) |>
    summarize(measles = sum(measles_total, na.rm = TRUE),
              rubella = sum(rubella_total, na.rm = TRUE)) |>
    ggplot() +
    geom_ribbon(aes(x = year, ymin = measles, ymax = rubella),
                position = "identity",
                inherit.aes = FALSE,
                fill = "#F8F5F0") +
    geom_line(aes(x = year,
                  y = measles,
                  color = "Measles"),
              linewidth = 1) +
    geom_line(aes(x = year,
                  y = rubella,
                  color = "Rubella"),
              linewidth = 1) +
    geom_point(aes(x = year,
                   y = measles,
                   color = "Measles")) +
    geom_point(aes(x = year,
                   y = rubella,
                   color = "Rubella")) +
    scale_color_manual(name = "Disease",
                       values = c("Measles" = "#325D88", "Rubella" = "#3E3F3A")) +
    scale_x_continuous(breaks = unique(measles_data$year),
                       expand = c(0.01,0.01)) +
    scale_y_continuous(breaks = seq(0, 600000, by = 100000),
                       labels = scales::label_comma()) +
    theme_bw() +
    labs(x = "Year",
         y = "Number of Cases",
         title = "Measles and Rubella Cases Over Time") +
    theme(panel.grid.minor = element_blank())

}
