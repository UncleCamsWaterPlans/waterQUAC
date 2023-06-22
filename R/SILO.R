library(httr)
library(jsonlite)
library(tidyverse)


LongPaddock_ppd <- function(station, start, finish = format(Sys.time() + 86400, "%Y%m%d"), format = "standard", username) {


  URLData <- paste("https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?station=",station,"&start=",start,"&finish=",finish,"&format=",format,"&comment=no&username=",username,"",sep = "")

  #API call GET
  APIData <- httr::GET(URLData)

  notes <- read_delim(rawToChar(APIData$content), delim = "\"", col_names = FALSE)
  ind <- grep("Patched Point data for station",notes)
  loc <- notes[,ind]

  # Extract station, latitude, and longitude
  station <- str_match(loc$X55, "station:(.*?)Lat:")[, 2]
  latitude <- str_match(loc$X55, "Lat:(.*?)Long:")[, 2]
  longitude <- str_match(loc$X55, "Long:(.*)")[, 2]

  # Remove leading/trailing whitespace
  station <- trimws(station)
  latitude <- trimws(latitude)
  longitude <- trimws(longitude)

  ind2 <- grep("Elevation", notes)
  el <- notes[,ind2]
  elevation <- str_extract(el, "\\d+")

  Data=readr::read_table(rawToChar(APIData$content), comment = "\"")[-1,]
  Data <- Data[, -which(names(Data) == "Date2")]
  Data$Date <- as.POSIXct(Data$Date, format = "%Y%m%d")
  Data <- Data %>%
    dplyr::mutate_if(is.character, as.numeric) %>%
    add_column(station = station,
               lat = as.numeric(latitude),
               long = as.numeric(longitude),
               elevation = as.numeric(elevation))


  return(Data)
}

#examples
df <- LongPaddock_ppd(station = "32043",
                      start = format(Sys.time() - (365*86400), "%Y%m%d"),
                      format = "standard",
                      username = "cameron.roberts@des.qld.gov.au")





LongPaddock_grid <- function(lat, long, start, finish = format(Sys.time() + 86400, "%Y%m%d"), format = "standard", username) {


  URLData <- paste("https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php?lat=",lat,"&lon=",long,"&start=",start,"&finish=",finish,"&format=",format,"&comment=no&username=",username,"&password=apirequest",sep = "")

  #API call GET
  APIData <- httr::GET(URLData)

  notes <- read_delim(rawToChar(APIData$content), delim = "\"", col_names = FALSE)

  ind2 <- grep("Elevation", notes)
  el <- notes[,ind2]
  elevation <- str_extract(el, "\\d+")

  Data=readr::read_table(rawToChar(APIData$content), comment = "\"")[-1,]
  Data <- Data[, -which(names(Data) == "Date2")]
  Data$Date <- as.POSIXct(Data$Date, format = "%Y%m%d")
  Data <- Data %>%
    dplyr::mutate_if(is.character, as.numeric) %>%
    dplyr::add_column(lat = as.numeric(lat),
               long = as.numeric(long),
               elevation = as.numeric(elevation))


  return(Data)
}
lat = "-18.50"
long = "145.98"


#examples
df <- LongPaddock_grid(lat = "-18.50",
                       long = "145.98",
                      start = format(Sys.time() - (700*86400), "%Y%m%d"),
                      format = "alldata",
                      username = "cameron.roberts@des.qld.gov.au")


LP_nearSite <- function(station, radius = "50") {

  URLData <- paste("https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?format=near&station=",station,"&radius=",radius, sep = "")

  #API call GET
  APIData <- httr::GET(URLData)
  Data <- read_delim(rawToChar(APIData$content), delim = "|", col_names = TRUE)

  return(Data)
}



#examples
df <- LP_nearSite(station = "32002")


plt_rain <- function(df) {
  fig <- df %>%
    plotly::plot_ly() %>%
    plotly::add_trace(
      x = ~ Date,
      y = ~ Rain,
      type = 'bar',
      name = "Daily Rainfall",
      marker = list(
        color = "darkblue",
        opacity = 0.8
      )
    ) %>%
    plotly::layout(yaxis = list(
      showgrid = TRUE,
      tickfont = list(color = "darkblue"),
      showgrid = FALSE,
      side = "left",
      anchor = "free",
      title = list(text = "<b>Rainfall</b> (mm)", font = list(size = 15))
    ),
    xaxis = list(showgrid = TRUE))

  fig

}


#examples
plt_rain(df)




fig <- df %>%
  plotly::plot_ly() %>%
  plotly::add_trace(
    x = ~ Date,
    y = ~ Rain,
    type = 'bar',
    name = "Daily Rainfall",
    marker = list(
      color = "darkblue",
      opacity = 0.8
    )
  ) %>%
  plotly::layout(yaxis = list(
    showgrid = TRUE,
    tickfont = list(color = "darkblue"),
    showgrid = FALSE,
    side = "left",
    anchor = "free",
    title = list(text = "<b>Rainfall</b> (mm)", font = list(size = 15))
  ),
  xaxis = list(showgrid = TRUE))

fig
