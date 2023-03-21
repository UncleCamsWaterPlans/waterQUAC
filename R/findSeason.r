#'@title Find Season (Wet/Dry)
#'
#'@descriptionFunction A simple function which takes a vector of sampling dates and matching vector of first flush dates and determines sampling dates season. If inside the wet season window (first flush start date plus 182 days) it is \"Wet\" season otherwise it is \"Dry\" season
#'				
#'@param FF_Start_Dates a vector of first flush start dates. Must be in YYYY-MM-DD format.
#'
#'@return  A vector of matching length with values for season, either \"Dry\" or \"Wet\"."  
findSeason <- function(FF_Start_Dates, Sampling_Dates) 
{
    End_Dates <- (as.POSIXct(FF_Start_Dates, format = "%Y-%m-%d") + 
        182 * 86400)
    Season <- character(length(FF_Start_Dates))
    for (i in 1:length(FF_Start_Dates)) {
        if (FF_Start_Dates[i] <= Sampling_Dates[i] & Sampling_Dates[i] < 
            End_Dates[i]) 
            Season[i] <- "Wet"
        else Season[i] <- "Dry"
    }
    return(Season)
}