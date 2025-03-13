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

## Citation
If you use waterQUAC in your research, please cite accordingly. 
