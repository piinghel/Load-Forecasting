#' Summarized and visualized data using the interactive library plotly
#'
#' @param df: tsibble dataframe
#' @param freq: frequency of the data (default 'daily')
#' @param ylab: name of the y lalel axis for the figure (default Value)
#' @param vars: variables to select
#' @return a tsibble dataframe


# make a plot
compare_fifthFluv <- function(df, freq = 'daily', ylab = "Value") {
  # summarize
  df <- tibbletime::as_tbl_time(df, index = date) %>%
    mutate(difference = fluvius - fifthplay) %>%
    select(c("date", "fluvius", "fifthplay", "difference")) %>%
    collapse_by(freq) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      fluvius = sum(fluvius),
      fifthplay = sum(fifthplay),
      difference = sum(difference)
    )
  
  # plot object
  df_long <- melt(df, id = "date")
  p <-
    ggplot(data = df_long, aes(x = date, y = value, colour = variable)) +
    geom_line() + ylab(ylab)
  
  list(dataframe = df, figure = p)
  
}
