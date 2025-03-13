# waterQUAC
An R library for quality control and anomaly detection in water quality datasets.
## Overview
waterQUAC has been developed as a way of sharing water quality anomaly detection functions and data extraction resources. This package provides tools to streamline quality control processes for researchers and water quality professionals.
## Features
### Anomaly Detection
The package offers robust functionality for:
- Sensor limits validation
- Spike detection
- Flat line (recurring value) detection

### Data Extraction
waterQUAC includes data-extraction functions for the following resources:
- [Water Monitoring Information Portal](https://water-monitoring.information.qld.gov.au/)
- [Long Paddock - SILO](https://www.longpaddock.qld.gov.au/)
- [OpenWeatherMap](https://openweathermap.org/api)
- [Eagle.IO](https://www.bentley.com/software/eagle-io/)
  
## Installation
The library can be installed directly from GitHub:
```r
remotes::install_github("https://github.com/UncleCamsWaterPlans/waterQUAC")
```

## Usage
### Anomaly Detection
```r
library(waterQUAC)
library(plotly)

#example Total Suspended Solids dataframe
df <- waterQUAC::TSS_data

#overwritable QC codes, all else are retained. In this case, all codes will be overwritten
manual_codes = c(1:4000)
#upper and lower limits for the sensor uses (Trios Opus)
sensorMin = 0
sensorMax = 650

tst <- ts_anom(df = df,
              overwrite = manual_codes,
              sensorMin = 0,
              sensorMax = 650)
tst |>
 plotly::plot_ly() |>
 plotly::add_markers(
   x =  ~ ts,
   y =  ~ Value,
   type = "scatter",
   color = ~ Quality
 )
```
### Data Extraction
```r
library(waterQUAC)
library(plotly)


# import discharge data from WMIP (Herbert River at Ingham - 1160001F)
discharge <- waterQUAC::wmip_hist("116001F", 
                                "discharge",
                                "AT",
                                "20220701",
                                "20230630")

# extract gridded weather obs data from that locaiton (SILO)
rain <- waterQUAC::silo_grid(lat = "-18.62831", 
                                    long = "146.16486",
                                    start = "20220701",
                                    finish = "20230630",
                                    username = "example@email.com.au")

#plot daily rainfall against stream discharge

discharge %>%
  plot_ly() %>%
  add_trace(
    x =  ~ time,
    y =  ~ value,
    mode = "lines",
    name = "Stream Discharge (m^3/s)",
    type = "scatter",
    fill = "tozeroy",
    line = list(
      color = "tozeroy",
      width = 2.5,
      dash = 'solid'
    ),
    connectgaps = TRUE
  )  %>%
  add_trace(
    x = ~ rain$Date,
    y = ~ rain$Rain,
    type = 'bar',
    yaxis = "y3",
    name = "Daily Rainfall",
    marker = list(
      color = "darkblue",
      opacity = 0.3,
      size = 10
    )
  ) %>%
  
  layout(
    legend = list(orientation = 'h'),
    xaxis = list(title = FALSE, 
                 showgrid = FALSE,
                 domain=c(0,0.85)),
    yaxis = list(
      title = list(text = "<b>Stream Level</b> (m)", font = list(size = 15)),
      showgrid = FALSE,
      side = "right"
    ),
    yaxis3 = list(
      tickfont = list(color = "darkblue"),
      showgrid = FALSE,
      overlaying = "y",
      side = "right",
      anchor = "free",
      position = 0.92,
      autorange = "reversed",
      title = list(text = "<b>Rainfall</b> (mm)", font = list(size = 15))
    )
  ) %>%
  #Add modebar buttons
  config(
    modeBarButtonsToAdd = list(
      'drawline',
      'drawopenpath',
      'drawclosedpath',
      'drawcircle',
      'drawrect',
      'eraseshape'
    )
  )
```
## Citation
If you use waterQUAC in your research, please cite accordingly (see About > Cite this repository). 
