#' Detect anomalies in time series data
#'
#' This function detects anomalies in time series data by looking for values that are
#' outside of the expected range, or that are repeated values. The function can also
#' detect spikes in the data.
#'
#' @param df A data frame containing the time series water quality or quantity data.
#' @param overwrite A vector of quality codes that CAN be overwritten by the function.
#' @param sensorMin The minimum value as reportable from the sensor for the given data.
#' @param sensorMax The maximum value as reportable from the sensor for the given data.
#' @param window The number of hours the average data logging interval is used to calculate the rolling window for flatline detection. For example if the average read time is hourly, flatln = 3 means consecutive values across a 3 hour interval will flag.
#' @param prec The precision of the sensor OR how much variation is acceptable to determine a repeating value
#' @return A data frame with the original data and a new column called Quality that indicates the type of anomaly, if any.
#'
#' @importFrom stats sd
#' @importFrom stats median
#' @examples
#'
#'df <- waterQUAC::TSS_data
#'
#'manual_codes = c(1:4000)
#'sensorMin = 0
#'sensorMax = 650
#'
#'tst <- ts_anom(df = df,
#'               overwrite = manual_codes,
#'               sensorMin = 0,
#'               sensorMax = 650)
#'tst |>
#'  plotly::plot_ly() |>
#'  plotly::add_markers(
#'    x =  ~ ts,
#'    y =  ~ Value,
#'    type = "scatter",
#'    color = ~ Quality
#'  )
#'
#' @export

ts_anom <- function(df, overwrite, sensorMin, sensorMax, window = 10, prec = 0.0001) {

  # Define the pattern to match variations of "quality"
  pattern <- "(?i)quality"

  # Check if any column name matches the pattern
  if (!any(grepl(pattern, colnames(df)))) {
    df$quality <- NA
  } else {
    q_name <- colnames(df)[grep(pattern, colnames(df))]
  }

  # Find the column name that is of class "posixct"
  posixct_column <- names(df)[sapply(df, function(x) any(class(x) == "POSIXct"))]

  sp <- tibble::tibble(ts = df[[posixct_column]])
  #Flatline detection

  # Calculate the time differences between consecutive timestamps
  time_diff <- diff(sp[["ts"]])
  # Calculate the average data logging interval per day and reduce it to match the defined window. interval = points per window
  interval <- round(((1440 / as.numeric(mean(time_diff), units = "mins")) / 24) * window)

  sp$centerSD <- zoo::rollapply(df[,2], width = interval, FUN = sd, fill = TRUE, align = 'center', na.rm = TRUE)   # a rolling window of Standard Deviation in parameter values - CENTERED -- rep_width determines the window width for all of these options
  sp$leftSD <-   zoo::rollapply(df[,2], width = interval, FUN = sd, fill = TRUE, align = 'left', na.rm = TRUE)     # a rolling window of Standard Deviation in parameter values - LEFT -- rep_width determines the window width for all of these options
  sp$rightSD <-  zoo::rollapply(df[,2], width = interval, FUN = sd, fill = TRUE, align = 'right', na.rm = TRUE)    # a rolling window of Standard Deviation in parameter values - RIGHT -- rep_width determines the window width for all of these options

  #Spike detection
  sp$median <- zoo::rollapply(suppressWarnings(log(df[,2])), width = interval, FUN = median,  partial = TRUE, na.rm = TRUE, align = 'center')   # rolling median of the log(value) for given width - med_width - centered
  sp$sd <-     zoo::rollapply(sp$median, width = interval*10, FUN = sd, na.rm=TRUE, partial = TRUE, align = 'center')            # rolling standard deviation of the median log(value) as calculated above for a larger window - centered


  # Use `{{ }}` for tidy evaluation of q_name inside mutate
  df <- df %>%
    dplyr::mutate(
      {{ q_name }} := dplyr::case_when(
        .data[[q_name]] > 0 & !(q_name %in% overwrite) ~ as.character(.data[[q_name]]),
        df[[2]] < 0 ~ 'impossible',
        df[[2]] < sensorMin ~ 'belowLimits',
        df[[2]] > sensorMax ~ 'aboveLimits',
        sp$centerSD < prec ~ 'repeatingValue',
        sp$leftSD < prec ~ 'repeatingValue',
        sp$rightSD < prec ~ 'repeatingValue',
        suppressWarnings(log(df[[2]])) > (3 * sp$sd + sp$median) ~ 'spikeUp',
        suppressWarnings(log(df[[2]])) < -(3 * sp$sd) + sp$median ~ 'spikeDown',
        TRUE ~ 'OK'
      )
    )

  df <- df %>%
    dplyr::mutate(
      {{ q_name }} := factor(
        .data[[q_name]],
        levels = c("OK", "impossible", "belowLimits",
                   "aboveLimits", "repeatingValue",
                   "spikeUp", "spikeDown")
      )
    )

  return(df)
}

