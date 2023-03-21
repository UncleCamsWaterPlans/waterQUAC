#'@title Function for replacing LOR values with relative LOR multiplier values
#'
#'@description Pass function a dataset and a character vector with the names of the pesticide columns for which the LOR's need to be treated Note these names are case sensitive and sensitive to spelling variations. The names in the list, the dataset columns and the relative_LOR_multipliers table HAVE TO MATCH EXACTLY Convert LOR's according to following rules: Find the first detect All LOR's before the first detect are converted to a very small number (1E-11) Detects are left as is All LOR's after the first detect are multiplied by a relative guideline value factor which is matched by pesticide name and taken from the relative_LOR column in the relative_LOR_multipliers table Note: This function is very specific to the listed pesticides and naming convensions. This should be generalised to be more widely applicable when more time is available.
#'
#'@param Raw_data This is the dataset that contains LOR values that you wish to treat.
#'@param names_pesticide_columns This is a vector containing all pesticide names that are used to calculate PRM (currently 22). The order must match the relative LOR multiplier table.
#'@param relative_LOR_mulitipliers This is a dataframe that shows relative LOR multipliers for their respective pesticide.
#'
#'@return the entered dataset with treated LOR values as a dataframe.
#'
treat_LORs_all_data <- function(Raw_data, names_pesticide_columns, relative_LOR_mulitipliers) 
{
    list_site_year_dataframes <- split(Raw_data, f = list(Raw_data$`Site Name`, 
        Raw_data$`Sampling Year`), drop = TRUE)
    updated_LOR_list <- list_site_year_dataframes
    for (k in 1:length(list_site_year_dataframes)) {
        updated_LOR_list[[k]] <- treat_LORs(list_site_year_dataframes[[k]], 
            names_pesticide_columns, relative_LOR_mulitipliers)
        updated_LOR_list
    }
    Treated_LORs_Baseline <- do.call(rbind, updated_LOR_list)
    Treated_LORs_Baseline <- Treated_LORs_Baseline %>% dplyr::select("Station", 
        "Site Name", "Sampling Year", "Date", "Ametryn", "Atrazine", 
        "Chlorpyrifos", "Diuron", "Fipronil", "Fluroxypyr", "Haloxyfop", 
        "Hexazinone", "Imazapic", "Imidacloprid", "Isoxaflutole", 
        "MCPA", "Metolachlor", "Metribuzin", "Metsulfuron", "Pendimethalin", 
        "Prometryn", "Simazine", "Tebuthiuron", "Terbuthylazine", 
        "Triclopyr", "'2,4-D'")
    return(Treated_LORs_Baseline)
}