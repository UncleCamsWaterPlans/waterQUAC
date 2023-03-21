#'@title Find first flush end dates
#'
#'
#'
#'
#'@description A very simple function which	add	s 18	2 da	ys to a first flush start date to find the end date of	the	wet	sea	son window.
#'
#'				
#'@param FF_Start_Dates a vector of first	flu	sh s	tart	dates. Must be in YYYY-MM-DD format.
#'			
#'
#'@return A matching vector of first flush	end	dates.
find_ff_end <- function(FF_Start_Dates) 
{
    end <- (as.POSIXct(FF_Start_Dates, format = "%Y-%m-%d") + 
        182 * 86400)
    return(end)
}