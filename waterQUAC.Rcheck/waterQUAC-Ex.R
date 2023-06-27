pkgname <- "waterQUAC"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "waterQUAC-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('waterQUAC')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("EIO_Hist")
### * EIO_Hist

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: EIO_Hist
### Title: Extract historic data from WQI's eagle.IO instance
### Aliases: EIO_Hist

### ** Examples

#td <- 86400
#START <- format(Sys.time() -30*td, "%Y-%m-%dT%H:%M:%SZ") # note this will be in UTC
#content <- EIO_Hist(APIKEY = "XYZ", param = "5903e538bd10c2fa0ce50648", START = 1)
#library(tidyverse)
#reportableParamRef <- WQI::reportableParamRef
#param <- reportableParamRef |>
#  filter(GSnum == '1160122')
#param <- param$`N-NO3`




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("EIO_Hist", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("EIO_Node")
### * EIO_Node

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: EIO_Node
### Title: Extract node data from WQI's eagle.IO instance
### Aliases: EIO_Node

### ** Examples

#content <- EIO_Node(APIKEY = "XYZ", param = "59cca1064f2ee90c99b94b2e")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("EIO_Node", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("LP_nearSite")
### * LP_nearSite

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: LP_nearSite
### Title: Retrieve nearby-site information from LongPaddock
### Aliases: LP_nearSite

### ** Examples

df <- LP_nearSite(station = "32002")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("LP_nearSite", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("LongPaddock_grid")
### * LongPaddock_grid

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: LongPaddock_grid
### Title: Retrieve gridded data from LongPaddock
### Aliases: LongPaddock_grid

### ** Examples

lat = "-18.50"
long = "145.98"

df <- LongPaddock_grid(lat = "-18.50",
                      long = "145.98",
                     start = format(Sys.time() - (700*86400), "%Y%m%d"),
                     format = "alldata",
                     username = "cameron.roberts@des.qld.gov.au")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("LongPaddock_grid", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("LongPaddock_ppd")
### * LongPaddock_ppd

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: LongPaddock_ppd
### Title: Retrieve patched point data from LongPaddock
### Aliases: LongPaddock_ppd

### ** Examples

df <- LongPaddock_ppd(station = "32043",
                     start = format(Sys.time() - (365*86400), "%Y%m%d"),
                     format = "standard",
                     username = "email@test.com")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("LongPaddock_ppd", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("NICO_Hist")
### * NICO_Hist

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: NICO_Hist
### Title: Extract historic NICO data from WQI's eagle.IO instance
### Aliases: NICO_Hist

### ** Examples

loggerRef <- WQI::loggerRef
site <- "1350053"
dex <- loggerRef |>
  dplyr::filter(GSnum == site)

NNO3 <- dex$`OPUSResults - OPUS1000 N NO3`
refA <- dex$`Nitrate - NICO RefA`
refB <- dex$`Nitrate - NICO RefB`
refC <- dex$`Nitrate - NICO RefC`
refD <- dex$`Nitrate - NICO RefD`
SQI <- dex$`OPUSResults - OPUS1060 SQI`




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("NICO_Hist", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("OPUS_Hist")
### * OPUS_Hist

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: OPUS_Hist
### Title: Extract historic OPUS data from WQI's eagle.IO instance
### Aliases: OPUS_Hist

### ** Examples

loggerRef <- WQI::loggerRef
site <- "1111019"
dex <- loggerRef |>
  dplyr::filter(GSnum == site)

NNO3 <- dex$`OPUSResults - OPUS1000 N NO3`
TSSeq <- dex$`OPUSResults - OPUS1016 TSSeq`
abs210 <- dex$`OPUSResults - OPUS1036 Abs210`
abs254 <- dex$`OPUSResults - OPUS1042 Abs254`
abs360 <- dex$`OPUSResults - OPUS1034 Abs360`
SQI <- dex$`OPUSResults - OPUS1060 SQI`




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("OPUS_Hist", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("OpenWeather")
### * OpenWeather

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: OpenWeather
### Title: Access to the OpenWeather API data (Free Tier)
### Aliases: OpenWeather

### ** Examples

#dat <- OpenWeather(City = "Ayr", time = (as.integer(Sys.time())), APIKEY = "XYZ")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("OpenWeather", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PathFinder")
### * PathFinder

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PathFinder
### Title: Decision tree for Trios OPUS and NICO Path-length limits for
###   N-NO3 and TSSeq
### Aliases: PathFinder

### ** Examples

#Limits <- PathFinder(ProbeType = "OPUS", PathLength = 5)
#Limits$NO3LowerLim




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PathFinder", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("WMIP_Extract")
### * WMIP_Extract

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: WMIP_Extract
### Title: Data extraction from the Water Monitoring Information portal
### Aliases: WMIP_Extract

### ** Examples

#df_test <- WMIP_Extract("110001D", "20220801", "temperature")





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("WMIP_Extract", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plt_rain")
### * plt_rain

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plt_rain
### Title: Plot rainfall data
### Aliases: plt_rain

### ** Examples

df <- LP_nearSite(station = "32002")

fig <- plt_rain(df)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plt_rain", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ts.anom")
### * ts.anom

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ts.anom
### Title: Detect anomalies in time series data
### Aliases: ts.anom

### ** Examples


#OVERWRITE <-
#  c(155,
#    101,
#    105,
#    2200,
#    1020,
#    2000,
#    2010:2015,
#    2210:2215,
#    3061:3068,
#    3211:3214) #overwritable QC codes, all else are retained
#
#
#
#tst <- ts.anom(df, OVERWRITE, 0, 650)
#
#tst %>%
#  plotly::plot_ly() %>%
#  plotly::add_markers(x=~ts, y=~Value, type = "scatter", color = ~Quality)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ts.anom", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
