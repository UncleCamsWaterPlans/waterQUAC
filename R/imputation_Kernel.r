#'@title Non-parametric distribution
#'
#'@description Function To be used in conjunction with list_kernel_imputations function below Works out how many days are missing (of the 182 risk window) for the input Input will be Daily Average RA PAF for a site and year Sample the density curve according to the number of missing data points To sample from a kernel density Work out the standard deviation from the smooth kernel density curve of the input data Sample the number of data points to become the 'means' of their own gaussian distributions Randomly sample a data point from each of these inidividual gaussian distributions   
#'
#'@param impute_variable dataframe of Daily Average RA PRM for a site and year 
#'
#'@return csv file of daily average PRM values saved in output_path location
#'
imputation_Kernel <- function(impute_variable) 
{
    n = 182 - length(impute_variable)
    bw = stats::density(impute_variable)$bw
    means = sample(impute_variable, n, replace = TRUE)
    density_samples = abs(stats::rnorm(n, mean = means, sd = bw))
    density_samples
}