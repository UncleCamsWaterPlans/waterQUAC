library(tidyverse)
library(plotly)
library(readr)

#example Total Suspended Solids dataframe
df <- waterQUAC::TSS_data

drl_water_level <- read_csv("drl_wl_2022.csv")
drl_water_level$ts <- as.POSIXct(drl_water_level$ts, format = "%d/%m/%Y %H:%M", tz = "Australia/Brisbane")


#overwritable QC codes, all else are retained. In this case, all codes will be overwritten
manual_codes = c(1:4000)
#upper and lower limits for the sensor uses (Trios Opus)


tst <- ts_anom(df = df,
               overwrite = manual_codes,
               window = 12,
               sensorMin = 0,
               sensorMax = 650,
               diag = TRUE)


plotly::plot_ly(tst) |>
  plotly::add_markers(
    x =  ~ts,
    y =  ~Value,
    type = "scatter",
    color = ~Quality,
    #name = "Sensor values"
  ) |>
  plotly::add_lines(
    x = ~ts,
    y = ~median,
    line = list(color = 'black', dash = "dot"),
    name = "Rolling Median"
  ) |>
  plotly::add_lines(
    x = ~ts,
    y = ~median + 4 * sd,
    line = list(color = 'red', dash = "dash"),
    name = "+4 SD"
  ) |>
  plotly::add_lines(
    x = ~ts,
    y = ~median - 4 * sd,
    line = list(color = 'red', dash = "dash"),
    name = "-4 SD"
  )



result <- detect_sensor_drift(tst, value_col = "Value", overwrite = c('OK', 'spike', 'impossible'))
head(result)



plotly::plot_ly(result) |>
  plotly::add_markers(
    x =  ~ts,
    y =  ~Value,
    type = "scatter",
    color = ~Quality
  )
