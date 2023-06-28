#' Retrieve patched point data from LongPaddock
#'
#' This function retrieves patched point data from LongPaddock. The function takes
#' the station ID, start date, finish date, format, and username as input. The
#' function returns a tibble with the patched point data.
#'
#' @param station The station ID.
#' @param start The start date in the format "YYYYMMDD".
#' @param finish The finish date in the format "YYYYMMDD".
#' @param format The format of the data. The default is "standard".
#' @param username The username for LongPaddock.
#' @return A tibble with the patched point data.
#'
#' @examples
#' df <- LongPaddock_ppd(station = "32043",
#'                      start = format(Sys.time() - (365*86400), "%Y%m%d"),
#'                      format = "standard",
#'                      username = "example@email.com.au")
#'
#' @seealso https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php
#' @export
#'
LongPaddock_ppd <- function(station, start, finish = format(Sys.time() + 86400, "%Y%m%d"), format = "standard", username) {


  URLData <- paste("https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?station=",station,"&start=",start,"&finish=",finish,"&format=",format,"&comment=no&username=",username,"",sep = "")

  #API call GET
  APIData <- httr::GET(URLData)

  notes <- readr::read_delim(rawToChar(APIData$content), delim = "\"", col_names = FALSE)
  ind <- grep("Patched Point data for station",notes)
  loc <- notes[,ind]

  # Extract station, latitude, and longitude
  station <- stringr::str_match(loc$X55, "station:(.*?)Lat:")[, 2]
  latitude <- stringr::str_match(loc$X55, "Lat:(.*?)Long:")[, 2]
  longitude <- stringr::str_match(loc$X55, "Long:(.*)")[, 2]

  # Remove leading/trailing whitespace
  station <- trimws(station)
  latitude <- trimws(latitude)
  longitude <- trimws(longitude)

  ind2 <- grep("Elevation", notes)
  el <- notes[,ind2]
  elevation <- stringr::str_extract(el, "\\d+")

  Data=readr::read_table(rawToChar(APIData$content), comment = "\"")[-1,]
  Data <- Data[, -which(names(Data) == "Date2")]
  Data$Date <- as.POSIXct(Data$Date, format = "%Y%m%d")
  Data <- Data |>
    dplyr::mutate_if(is.character, as.numeric) |>
    tibble::add_column(station = station,
               lat = as.numeric(latitude),
               long = as.numeric(longitude),
               elevation = as.numeric(elevation))


  return(Data)
}


#' Retrieve gridded data from LongPaddock
#'
#' This function retrieves gridded data from LongPaddock. The function takes
#' the latitude, longitude, start date, finish date, format, and username as input. The
#' function returns a tibble with the gridded data.
#'
#' @param lat The latitude.
#' @param long The longitude.
#' @param start The start date in the format "YYYYMMDD".
#' @param finish The finish date in the format "YYYYMMDD".
#' @param format The format of the data. The default is "standard".
#' @param username The username for LongPaddock.
#' @return A tibble with the gridded data.
#'
#' @examples
#' df <- LongPaddock_grid(lat = "-18.50",
#'                       long = "145.98",
#'                      start = format(Sys.time() - (700*86400), "%Y%m%d"),
#'                      format = "alldata",
#'                      username = "example@email.com.au")
#'
#' @seealso https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php
#'
#' @export

LongPaddock_grid <- function(lat, long, start, finish = format(Sys.time() + 86400, "%Y%m%d"), format = "standard", username) {


  URLData <- paste("https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php?lat=",lat,"&lon=",long,"&start=",start,"&finish=",finish,"&format=",format,"&comment=no&username=",username,"&password=apirequest",sep = "")

  #API call GET
  APIData <- httr::GET(URLData)

  notes <- readr::read_delim(rawToChar(APIData$content), delim = "\"", col_names = FALSE)

  ind2 <- grep("Elevation", notes)
  el <- notes[,ind2]
  elevation <- stringr::str_extract(el, "\\d+")

  Data=readr::read_table(rawToChar(APIData$content), comment = "\"")[-1,]
  Data <- Data[, -which(names(Data) == "Date2")]
  Data$Date <- as.POSIXct(Data$Date, format = "%Y%m%d")
  Data <- Data |>
    dplyr::mutate_if(is.character, as.numeric) |>
    tibble::add_column(lat = as.numeric(lat),
               long = as.numeric(long),
               elevation = as.numeric(elevation))


  return(Data)
}




#' Retrieve nearby-site information from LongPaddock
#'
#' This function retrieves nearby-site information from LongPaddock. The function takes
#' the station ID and radius as input. The
#' function returns a tibble with the near-site data.
#'
#' @param station The station ID.
#' @param radius The radius in kilometers.
#' @return A tibble with the near-site data.
#'
#' @examples
#' df <- LP_nearSite(station = "32002")
#'
#' @seealso https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?format=near
#'
#' @export




LP_nearSite <- function(station, radius = "50") {

  URLData <- paste("https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?format=near&station=",station,"&radius=",radius, sep = "")

  #API call GET
  APIData <- httr::GET(URLData)
  Data <- readr::read_delim(rawToChar(APIData$content), delim = "|", col_names = TRUE)

  return(Data)
}




#' Plot rainfall data
#'
#' This function plots rainfall data using Plotly. The function takes
#' a dataframe with rainfall data as input. The
#' function returns a Plotly figure.
#'
#' @param df A dataframe with rainfall data.
#' @return A Plotly figure.
#'
#' @examples
#' df <- LP_nearSite(station = "32002")
#'
#' fig <- plt_rain(df)
#'
#' @seealso https://plotly.com/r/
#'
#' @export


plt_rain <- function(df) {
  fig <- df |>
    plotly::plot_ly() |>
    plotly::add_trace(
      x = ~ Date,
      y = ~ Rain,
      type = 'bar',
      name = "Daily Rainfall",
      marker = list(
        color = "darkblue",
        opacity = 0.8
      )
    ) |>
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
