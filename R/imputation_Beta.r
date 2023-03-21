#'@title Parametric distribution
#'
#'@description Function To be used in conjunction with list_beta_imputations function below Works out how many days are missing (of the 182 risk window) for the input Input will be Daily Average ms-PAF for a site and year Use maximum likelihood to estimate beta shape parameters from the input data Sample from the fitted beta distribution according to the number of missing days
#'
#'
#'@param impute_variable dataframe of Daily Average RA PRM for a site and year 
#'
#'@return csv file of daily average PRM values saved in output_path location
imputation_Beta <- function(impute_variable) 
{
    n = 182 - length(impute_variable)
    impute_variable = impute_variable[which(!is.na(impute_variable))]
    Beta_parameters = MASS::fitdistr(impute_variable/100, distr = "beta")
    shape1_beta = Beta_parameters$estimate[1]
    shape2_beta = Beta_parameters$estimate[2]
    beta_samples = stats::rbeta(n = n, shape1 = shape1_beta, 
        shape2 = shape2_beta)
    beta_samples * 100
}
