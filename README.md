# waterQUAC
An R library for quality control and anomaly detection in water quality datasets.
## Overview
waterQUAC has been developed as a way of sharing water quality anomaly detection functions and data extraction resources. This package provides tools to streamline quality control processes for researchers and water quality professionals.
## Features:
### Detecting anomalies in time series water quality or quantity data:
The waterQUAC R library was developed to provide water quality practitioners with tools for automated data validation and anomaly detection. The package includes the `TSanom()` function which identifies anomalies in time series environmental sensor data by applying a combination of rule-based checks, designed to detect:
- **Physical bounds violations**: Detects values falling outside manufacturer-specified sensor limits.
- **Impossible values**: Identifies values that are physically implausible, such as negative values from sensors reporting non-negative measurements.
- **Flatlining**: Identifies repeated or constant values using rolling standard deviation over a defined time window. This is often associated with errors in digital sensors whereby an unsuccessful measurement results in reporting the last value in the memory.
- **Spikes**: Detects sudden deviations from the rolling median centred on a predefined time window. The rolling median is then compared to a threshold calculated by the rolling standard deviation of the same window. This technique is similar to Bollinger bands, a common metric in financial trading, substituting the mean with the median to reduce sensitivity to outliers and better represent the expected baseline signal.

This function modifies the quality code for each observation with each point giving a first pass to the incoming dataset allowing end users and applications to omit obviously suspect data. Data that does not get flagged in any of the defined rules gets a default of `[OK]`. User-assigned quality codes can be preserved or selectively overwritten, ensuring that manual or expert-reviewed flags persist.  

These methods provide a first-pass filter for raw telemetry data, enabling automated QA/QC at scale. These functions have been integrated into Microsoft-Azure based engineering pipelines which has been scaled to batch process over 100 sensors across Queensland at hourly intervals. These sensors monitor six key water quality parameters: Water Level, Conductivity, Turbidity, Temperature, Nitrate (as N), and Total Suspended Solids. The pipeline processes data hourly, aligned with telemetry intervals, and outputs validated datasets to internal systems and external stakeholders.


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
