#' @title A function for the quality coding of real time water quality data
#'
#' This function checks the quality of water data.
#'
#' @param df A data frame containing water LEVEL data.
#' @param OVERWRITE A vector of quality codes that can be overwritten.
#' @param WLmax Maximum reportable water level as per the sensor specifications.
#'
#' @return A data frame with quality codes.
#'
#' @examples
#' df <- WaterCheck(df = RTlevel, OVERWRITE = OVERWRITE, WLmax = 30)
#'
#' @export
#'

OVERWRITE <-c(155,101,105,2200,1020,2000, 2010:2015, 2210:2215, 3061:3068, 3211:3214) #overwritable QC codes, all else are retained
RTlevel <- WQI::EIO_Hist(APIKEY = keyring::key_get("EIO_API"),
                         param = WQI::loggerRef$`Main - Level`[WQI::loggerRef$GSnum == "1080025"],
                         START = "2021-01-01T00:00:00Z"
                         )
freq <- nrow(RTlevel) / as.numeric(RTlevel[nrow(RTlevel), 1] - RTlevel[1,1])
time_diffs <- diff(RTlevel$ts)

# Calculate the average time difference
avg_time_diff <- as.numeric(mean(time_diffs))

WaterCheck <- function(df, OVERWRITE, WLmax) {
  med <- zoo::rollapply(log(df$Value), width = 30, FUN = median,  partial = TRUE, na.rm = TRUE, align = 'center')
  sd <- zoo::rollapply(med, width = 100, FUN = sd, na.rm= TRUE, partial = TRUE, align = 'center')

  df <- df %>%
    dplyr::mutate (`Quality` = dplyr::case_when(
      # `Quality` > 0 & !(`Quality` %in% OVERWRITE) ~ as.character(`Quality`), # if a quality code exists and it is not listed as an OVERWRITEABLE code, retain Quality code
      `Value` < 0 ~ '3201',                                         # bad - impossible value
      `Value` > WLmax ~ '3201',                                     # bad - exceed sensor limits
      zoo::rollapply(Value, width = 10, FUN = sd, fill = TRUE, align = 'center') == 0 ~ '3214',                              # bad - repeating values
      zoo::rollapply(Value, width = 10, FUN = sd, fill = TRUE, align = 'left') == 0 ~ '3214',                                # bad - repeating values
      zoo::rollapply(Value, width = 10, FUN = sd, fill = TRUE, align = 'right') == 0 ~ '3214',                               # bad - repeating values
      log(`Value`) > (3* sd + med) ~ '2211',   # uncertain - possible spike
      log(`Value`) < -(3* sd) + med ~ '2212',  # uncertain - possible spike
      TRUE ~ '1220' ))                                              #Q - Good - Auto QC

  return(df)
}

df <- WaterCheck(df = RTlevel, OVERWRITE = OVERWRITE, WLmax = 30)
df$Quality <- as.factor(df$Quality)

plot_ly(df, x = ~ts, y = ~Value, color = ~Quality) %>%
  add_markers() %>%
  layout(
    title = "Water Level QC",
    xaxis = list(
      title = "",
      tickangle = 60,
      tickfont = list(size = 14)
    ),
    yaxis = list(
      title = "Water level (m)",
      titlefont = list(size = 16),
      tickfont = list(size = 15)
    ),
    legend = list(
      x = 0.9,
      y = 0.7,
      bgcolor = "white",
      bordercolor = "black",
      borderwidth = 1,
      font = list(size = 10),
      titlefont = list(face = "bold")
    )
  )


OPUSCheck <- function(df, OVERWRITE, probeType, pathLength, InstallType) {

  #generate a 'limits' file per probe and pathlength to list the upper and lower thresholds for the OPUS and NICO probes
  Limits <- WQI::PathFinder(ProbeType = probeType,
                            PathLength = pathLength)

  df <- df %>%
    mutate ('TSSeq_Qual' = case_when(
      `TSSeq_Qual` > 0 & !(`TSSeq_Qual` %in% OVERWRITE)  ~ as.character(`TSSeq_Qual`), # if a quality code exists and it is not listed as an OVERWRITEABLE code, retain Quality code
      `SQI` < 0.8 ~ '3061',                                   # WQ - Bad - Auto (Poor spectral quality)
      `abs360` >= 0.8 ~ '3062',                               # WQ - Bad - Auto (Above probe limits)
      `TSS_centerSD` == 0 ~ '3068',                           # bad - repeating values
      `TSS_leftSD` == 0 ~ '3068',                             # bad - repeating values
      `TSS_rightSD` == 0 ~ '3068',                            # bad - repeating values
      log(`TSSeq`) > (4* `TSS_sd` + `TSS_median`) ~ '2030',   # uncertain - possible spike
      log(`TSSeq`) < -(4* `TSS_sd`) + `TSS_median` ~ '2030',  # uncertain - possible spike
      `TSSeq`> Limits$TSSUpperLim ~ '2011',                   # WQ - Uncertain - Auto (Above Parameter Limits)
      `TSSeq`< Limits$TSSLowerLim ~ '2012',                   # WQ - Uncertain - Auto (Below Parameter Limits)
      TRUE ~ '1020' ),                                        # WQ - Good - Auto QC
      'N-NO3_Qual' = case_when(
        `N-NO3_Qual` > 0 & !(`N-NO3_Qual`%in% OVERWRITE) ~ as.character(`N-NO3_Qual`), # if a quality code exists and it is not listed as an OVERWRITEABLE code, retain Quality code
        `SQI` < 0.8 ~ '3061',                                     # WQ - Bad - Auto (Poor spectral quality)
        `abs360` >= 0.8 ~ '3062',                                 # WQ - Bad - Auto (Above probe limits)
        `abs210` > 3 ~ '3062',                                    # WQ - Bad - Auto (Above probe limits)
        InstallType == "ExSitu" & `abs210`<= 0.15 ~ '2035',       # WQ - Uncertain - Possible Dry Read
        `NO3_centerSD` == 0 ~ '3068',                             # bad - repeating values
        `NO3_leftSD` == 0 ~ '3068',                               # bad - repeating values
        `NO3_rightSD` == 0 ~ '3068',                              # bad - repeating values
        log(`N-NO3`) > (4* `NO3_sd` + `NO3_median`) ~ '2030',     # uncertain - possible spike
        log(`N-NO3`) < -(4* `NO3_sd`) + `NO3_median` ~ '2030',    # uncertain - possible spike
        `N-NO3`> Limits$NO3UpperLim ~ '2011',                     # WQ - Uncertain - Auto (Above Parameter Limits)
        `N-NO3`< Limits$NO3LowerLim ~ '2012',                     # WQ - Uncertain - Auto (Below Parameter Limits)
        TRUE ~ '1020' ))                                          #WQ - Good - Auto QC

  return(df)
}


NICOCheck <- function(df, OVERWRITE, probeType, pathLength) {

  #generate a 'limits' file per probe and pathlength to list the upper and lower thresholds for the OPUS and NICO probes
  Limits <- WQI::PathFinder(ProbeType = probeType,
                            PathLength = pathLength)

  df <- df %>%
    mutate ('N-NO3_Qual' = case_when(
      `N-NO3_Qual` > 0  & !(`N-NO3_Qual`%in% OVERWRITE) ~ as.character(`N-NO3_Qual`), # if a quality code exists and it is not listed as an OVERWRITEABLE code, retain Quality code
      `SQI` < 0.5 ~ '3061',                                   # WQ - Bad - Auto (Poor spectral quality)
      `SQI` > 0.5 & `SQI` < 0.8 ~ '2061',                     # WQ - Uncertain - Caution spectral quality
      `refD` < 13000 ~ '3020',                                # WQ - Bad - Probe failure - Lamp is burnt out
      `refB` < `refA` ~ '2051',                               # Uncertain - organics interference
      `refA` < 150 ~ '2014',                                  # Below limits of the probe (non-descript...sorry)
      `refB` < 150 ~ '2014',                                  # Below limits of the probe
      `refC` < 150 ~ '2014',                                  # Below limits of the probe
      `NO3_centerSD` == 0 ~ '3068',                           # bad - repeating values
      `NO3_leftSD` == 0 ~ '3068',                             # bad - repeating values
      `NO3_rightSD` == 0 ~ '3068',                            # bad - repeating values
      log(`N-NO3`) > (4* `NO3_sd` + `NO3_median`) ~ '2030',   # uncertain - possible spike
      log(`N-NO3`) < -(4* `NO3_sd`) + `NO3_median` ~ '2030',  # uncertain - possible spike
      `N-NO3`> Limits$NO3UpperLim ~ '2011',                   #WQ - Uncertain - Auto (Above Parameter Limits)
      `N-NO3`< Limits$NO3LowerLim ~ '3064',                   #WQ - Bad - Auto (Below Parameter Limits)
      TRUE ~ '1020' ))                                        #WQ - Good - Auto QC
  return(df)
}


SondeCheck <- function(df, OVERWRITE) {

  df <- df %>%
    mutate ('cond_qual' = case_when(
      `cond_qual` > 0 & !(`cond_qual` %in% OVERWRITE)  ~ as.character(`cond_qual`),  # if a quality code exists and it is not listed as an OVERWRITEABLE code, retain Quality code
      `cond_value` > 100000 ~ '2011',                                # WQ - Bad - Auto (Impossible values)
      `cond_value` <= 0 ~ '3069',                                    # WQ - Bad - Auto (Impossible values)
      `cond_centerSD` == 0 ~ '3068',                                 # bad - repeating values
      `cond_leftSD` == 0 ~ '3068',                                   # bad - repeating values
      `cond_rightSD` == 0 ~ '3068',                                  # bad - repeating values
      log(`cond_value`) > (3* `cond_sd` + `cond_median`) ~ '2030',   # uncertain - possible spike
      log(`cond_value`) < -(3* `cond_sd`) + `cond_median` ~ '2030',  # uncertain - possible spike
      TRUE ~ '1020' ),                                               # WQ - Good - Auto QC
      'turb_qual' = case_when(
        `turb_qual` > 0 & !(`turb_qual` %in% OVERWRITE)  ~ as.character(`turb_qual`),
        `turb_value` > 4000 ~ '2011',                                    # WQ - Bad - Auto (Impossible values)
        `turb_value` <= 0 ~ '3069',                                      # WQ - Bad - Auto (Impossible values)
        `turb_centerSD` == 0 ~ '3068',                                   # bad - repeating values
        `turb_leftSD` == 0 ~ '3068',                                     # bad - repeating values
        `turb_rightSD` == 0 ~ '3068',                                    # bad - repeating values
        log(`turb_value`) > (3* `turb_sd` + `turb_median`) ~ '2030',     # uncertain - possible spike
        log(`turb_value`) < -(3* `turb_sd`) + `turb_median` ~ '2030',    # uncertain - possible spike
        TRUE ~ '1020' ),                                                 # WQ - Good - Auto QC
      'temp_qual' = case_when(
        `temp_qual` > 0 & !(`temp_qual` %in% OVERWRITE)  ~ as.character(`temp_qual`),
        `temp_value` > 50 ~ '2011',         # WQ - Bad - Auto (Impossible values)
        `temp_value` <= 0 ~ '3069',             # WQ - Bad - Auto (Impossible values)
        `temp_centerSD` == 0 ~ '3068',
        `temp_leftSD` == 0 ~ '3068',
        `temp_rightSD` == 0 ~ '3068',
        log(`temp_value`) > (3* `temp_sd` + `temp_median`) ~ '2030',
        log(`temp_value`) < -(3* `temp_sd`) + `temp_median` ~ '2030',
        TRUE ~ '1020' ))             #WQ - Good - Auto QC

  return(df)
}


ts.anom <- function(df, value, prefix, rep_width = 10, med_width, sd_width) {

  sp <- tibble(ts = df$ts)
  #Flatline detection cond
  sp$centerSD <- zoo::rollapply(value, width = rep_width, FUN = sd, fill = TRUE, align = 'center')   # a rolling window of Standard Deviation in parameter values - CENTERED -- rep_width determines the window width for all of these options
  sp$leftSD <-   zoo::rollapply(value, width = rep_width, FUN = sd, fill = TRUE, align = 'left')     # a rolling window of Standard Deviation in parameter values - LEFT -- rep_width determines the window width for all of these options
  sp$rightSD <-  zoo::rollapply(value, width = rep_width, FUN = sd, fill = TRUE, align = 'right')    # a rolling window of Standard Deviation in parameter values - RIGHT -- rep_width determines the window width for all of these options

  #Spike detection
  sp$median <- zoo::rollapply(log(value), width = med_width, FUN = median,  partial = TRUE, na.rm = TRUE, align = 'center')   # rolling median of the log(value) for given width - med_width - centered
  sp$sd <-     zoo::rollapply(sp$median, width = sd_width, FUN = sd, na.rm=TRUE, partial = TRUE, align = 'center')            # rolling standard deviation of the median log(value) as calculated above for a larger window - centered

  # add prefix to newly generated columns
  colnames(sp) <- paste0(prefix,"_", colnames(sp))

  # add new columns to original dataframe and return
  df <- df %>%
    full_join(sp, by = c("ts" = paste0(prefix, "_ts")))

  return(df)
}
