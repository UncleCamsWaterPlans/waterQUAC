#' Detect anomalies in time series data
#'
#' This function detects anomalies in time series data by looking for values that are
#' outside of the expected range, or that are repeated values. The function can also
#' detect spikes in the data.
#'
#' @param df A data frame containing the time series water quality or quantity data.
#' @param OVERWRITE A vector of quality codes that can be overwritten by the function.
#' @param sensorMin The minimum value as reportable from the sensor for the given data.
#' @param sensorMax The maximum value as reportable from the sensor for the given data.
#' @param flatln The number of times the average data logging interval is used to calculate the rolling window for flatline detection. For example if the average read time is hourly, flatln = 3 means consecutive values across a 3 hour interval will flag.
#' @param med_width The width of the rolling window used to calculate the median of the log(value).
#' @param sd_width The width of the rolling window used to calculate the standard deviation of the median log(value).
#' @return A data frame with the original data and a new column called Quality that indicates the type of anomaly, if any.
#'
#' @examples
#' df <- ts.anom(df, OVERWRITE = c('0', '1'), sensorMin = -10, sensorMax = 10, flatln = 3, med_width = 36, sd_width = 100)
#'
#' @seealso freqy

ts.anom <- function(df, OVERWRITE, sensorMin, sensorMax, flatln = 3, med_width = 36, sd_width = 100) {
  
  #check for a quality column if not add one
  if (!"Quality" %in% names(df)) {
    df$Quality <- NA
  }
  
  sp <- tibble::tibble(ts = df$ts)
  #Flatline detection 
  
  # Calculate the time differences between consecutive timestamps
  time_diff <- diff(df[["ts"]])
  # Calculate the average data logging interval
  average_interval <- as.numeric(mean(time_diff)) * (flatln)
  
  sp$centerSD <- zoo::rollapply(df[,2], width = average_interval, FUN = sd, fill = TRUE, align = 'center')   # a rolling window of Standard Deviation in parameter values - CENTERED -- rep_width determines the window width for all of these options
  sp$leftSD <-   zoo::rollapply(df[,2], width = average_interval, FUN = sd, fill = TRUE, align = 'left')     # a rolling window of Standard Deviation in parameter values - LEFT -- rep_width determines the window width for all of these options
  sp$rightSD <-  zoo::rollapply(df[,2], width = average_interval, FUN = sd, fill = TRUE, align = 'right')    # a rolling window of Standard Deviation in parameter values - RIGHT -- rep_width determines the window width for all of these options
  
  #Spike detection
  sp$median <- zoo::rollapply(log(df[,2]), width = med_width, FUN = median,  partial = TRUE, na.rm = TRUE, align = 'center')   # rolling median of the log(value) for given width - med_width - centered
  sp$sd <-     zoo::rollapply(sp$median, width = sd_width, FUN = sd, na.rm=TRUE, partial = TRUE, align = 'center')            # rolling standard deviation of the median log(value) as calculated above for a larger window - centered
  
  
  df <- df |>
    dplyr::mutate(`Quality` = dplyr::case_when(
      `Quality` > 0 & !(`Quality` %in% OVERWRITE) ~ as.character(`Quality`), # if a quality code exists and it is not listed as an OVERWRITEABLE code, retain Quality code
      df[,2] < 0 ~ 'impossible',                                         # bad - impossible value
      df[,2] < sensorMin ~ 'belowLimits',
      df[,2] > sensorMax ~ 'aboveLimits',                                     # bad - exceed sensor limits
      sp$centerSD == 0 ~ 'repeatingValue',                              # bad - repeating values
      sp$leftSD == 0 ~ 'repeatingValue',                                # bad - repeating values
      sp$rightSD == 0 ~ 'repeatingValue',                               # bad - repeating values
      log(df[,2]) > (3* sp$sd + sp$median) ~ 'spikeUp',   # uncertain - possible spike
      log(df[,2]) < -(3* sp$sd) + sp$median ~ 'spikeDown',  # uncertain - possible spike
      TRUE ~ 'OK' ))                                              #Q - Good - Auto QC
  df$Quality <- as.factor(df$Quality)
  
  return(df)
}

freqy <- function(df, ts_column) {
  # Convert 'ts_column' to POSIXct type if it's not already in that format
  df[[ts_column]] <- as.POSIXct(df[[ts_column]])
  
  # Calculate the time differences between consecutive timestamps
  time_diff <- diff(df[[ts_column]])
  
  # Calculate the average data logging interval
  average_interval <- mean(time_diff)
  
  # Return the average data logging interval
  return(average_interval)
}

as.numeric(freqy(df , "ts"))

APIKEY <- Sys.getenv("EIO_KEY")
lev <-WQI::loggerRef$`OPUSResults - OPUS1016 TSSeq`[WQI::loggerRef$GSnum == "1160116"]
tm <- format(Sys.time() - 1500 * 86400, "%Y-%m-%dT%H:%M:%SZ")

df <- WQI::EIO_Hist(APIKEY, lev, tm)


OVERWRITE <-
  c(155,
    101,
    105,
    2200,
    1020,
    2000,
    2010:2015,
    2210:2215,
    3061:3068,
    3211:3214) #overwritable QC codes, all else are retained



tst <- ts.anom(df, 1:10000, 0, 650)

#tst1 <- ts.anom(filter(tst, Quality == "OK"), OVERWRITE, 0, 30)



fig <- tst %>% 
  plotly::plot_ly() %>% 
  plotly::add_markers(x=~ts, y=~Value, type = "scatter", color = ~Quality) 

fig


install.packages("DescTools")
df1 <- df
df1$outlier <- DescTools::LOF(df1$Value, 100)

ggplotly(
ggplot(df1, aes(x = ts, y = Value, color = outlier)) +
  geom_point() +
  scale_color_gradient(low = "green", high = "red") +
  xlab("Timestamp") +
  ylab("Value") +
  ggtitle("Plot of Value with Outlier Score") +
  theme_minimal()
)
