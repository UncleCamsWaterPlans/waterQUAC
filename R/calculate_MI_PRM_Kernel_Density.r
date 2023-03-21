#'@title Calculate Wet Season PRM using MI Ke	rnal Density
#'
#'@description This function runs both imputation Kernal and Beta	functions over a given dataframe producing a Wet	Season PRM value	for	each	site and sampling year combination
#'                                          )				
#'@param Daily_Avg_PRM_data dataframe of Daily	PRM values which	are	used	in Wet Season PRM calculation
#'@param PRM_group column name which holds daily PRM values
#'@param Save_csv_files this determines whether a dataframe is	returned	or csvfiles
#'@param file_name name and location for where	the treated csv	file	will	be saved
#'
#'@return csv file of Wet Season PRM values	saved in file_name location
calculate_MI_PRM_Kernel_Density <- function(Daily_Avg_PRM_data, PRM_group = "Total PRM", Save_csv_files = FALSE, 
    file_name = NULL) 
{
    Daily_Avg_PRM_data <- Daily_Avg_PRM_data %>% dplyr::rename(Daily.Ave.PRM = PRM_group)
    list_observed_data <- split(Daily_Avg_PRM_data$Daily.Ave.PRM, 
        f = list(Daily_Avg_PRM_data$`Site Name`, Daily_Avg_PRM_data$Station, 
            Daily_Avg_PRM_data$`Sampling Year`), drop = TRUE)
    list_kernel_imputations <- rep(list(vector("list", 1000)), 
        length(list_observed_data))
    names(list_kernel_imputations) <- names(list_observed_data)
    for (i in 1:length(list_kernel_imputations)) {
        for (j in 1:length(list_kernel_imputations[[i]])) {
            list_kernel_imputations[[i]][[j]] = imputation_Kernel(list_observed_data[[i]])
            list_kernel_imputations
        }
    }
    list_kernel_imputations_observed <- rep(list(vector("list", 
        length(list_kernel_imputations[[1]]))), length(list_observed_data))
    names(list_kernel_imputations_observed) <- names(list_observed_data)
    for (i in 1:length(list_kernel_imputations_observed)) {
        for (j in 1:length(list_kernel_imputations_observed[[i]])) {
            list_kernel_imputations_observed[[i]][[j]] = c(list_observed_data[[i]], 
                list_kernel_imputations[[i]][[j]])
            list_kernel_imputations_observed
        }
    }
    list_kernel_MI_means <- rep(list(vector("list", length(list_kernel_imputations[[1]]))), 
        length(list_observed_data))
    names(list_kernel_MI_means) <- names(list_observed_data)
    for (i in 1:length(list_kernel_MI_means)) {
        for (j in 1:length(list_kernel_MI_means[[i]])) {
            list_kernel_MI_means[[i]][[j]] = mean(list_kernel_imputations_observed[[i]][[j]])
            list_kernel_MI_means
        }
    }
    estimate_ci_imputed_data <- function(means.1000) {
        MI_Ave_PRM <- mean(means.1000)
        n <- length(means.1000)
        conf.level <- 0.95
        z <- qt((1 + conf.level)/2, df = n - 1)
        se <- sd(unlist(means.1000))/sqrt(n)
        CI <- z * se
        MI_CI_lower <- MI_Ave_PRM - CI
        MI_CI_upper <- MI_Ave_PRM + CI
        data.frame(MI_Ave_PRM)
    }
    list_kernel_MI_CIs <- vector("list", length(list_observed_data))
    names(list_kernel_MI_CIs) <- names(list_observed_data)
    for (i in 1:length(list_kernel_MI_CIs)) {
        list_kernel_MI_CIs[[i]] = estimate_ci_imputed_data(unlist(list_kernel_MI_means[[i]]))
        list_kernel_MI_CIs
    }
    MI_estimates <- do.call("rbind", list_kernel_MI_CIs, 1)
    MI_estimates$`Site Name` <- unlist(lapply(strsplit(rownames(MI_estimates), 
        "\\."), `[[`, 1))
    MI_estimates$Station <- unlist(lapply(strsplit(rownames(MI_estimates), 
        "\\."), `[[`, 2))
    MI_estimates$`Sampling Year` <- unlist(lapply(strsplit(rownames(MI_estimates), 
        "\\."), `[[`, 3))
    rownames(MI_estimates) <- NULL
    MI_estimates <- MI_estimates[, c("Site Name", "Station", 
        "Sampling Year", "MI_Ave_PRM")]
    MI_estimates
    if (Save_csv_files == FALSE) {
        return(MI_estimates)
    }
    else {
        utils::write.csv(MI_estimates, file_name, row.names = FALSE)
    }
}