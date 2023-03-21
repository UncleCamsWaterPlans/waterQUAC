#'@title Find sanpling year for given vector or object
#'
#'
#'
#'
#'@descriptionFunction for replacing a date vector or object,in yyyy-mm-dd format, with the corresponding WQI sampling year. e.g. 2017-08-01 becomes 2017-2018
#'
#'				
#'@param dates The Date Vector or Object you want to convert to sampling year Vector or Object
#'			
#'
#'@return vector/object containing Sampling Year data for each date.
findSamplingYr <- function(dates) 
{
    mnth <- lubridate::month(dates)
    yr <- lubridate::year(dates)
    sampleYr <- character(length(dates))
    for (i in 1:length(dates)) {
        if (mnth[i] >= 7) 
            sampleYr[i] <- paste(yr[i], "-", yr[i] + 1, sep = "")
        else if (mnth[i] < 7) 
            sampleYr[i] <- paste(yr[i] - 1, "-", yr[i], sep = "")
        i + 1
    }
    as.factor(sampleYr)
}