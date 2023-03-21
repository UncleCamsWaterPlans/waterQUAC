#'@title Function for replacing LOR values with relative LOR multiplier values
#'
#'@description Pass function a dataset and a character vector with the names of the pesticide columns for which the LOR's need to be treated Note these names are case sensitive and sensitive to spelling variations. The names in the list, the dataset columns and the relative_LOR_multipliers table HAVE TO MATCH EXACTLY Convert LOR's according to following rules: Find the first detect All LOR's before the first detect are converted to a very small number (1E-11) Detects are left as is All LOR's after the first detect are multiplied by a relative guideline value factor which is matched by pesticide name and taken from the relative_LOR column in the relative_LOR_multipliers table Note: This function is very specific to the listed pesticides and naming convensions. This should be generalised to be more widely applicable when more time is available.
#'
#'@param dataset This is the dataset that contains LOR values that you wish to treat. 
#'
#'@return the entered dataset with treated LOR values.
#'
treat_LORs <- function(dataset, names_pesticide_columns, relative_LOR_mulitipliers) 
{
    for (i in 1:length(names_pesticide_columns)) {
        first_detect <- suppressWarnings(min(which(grepl("<", 
            dataset[, names_pesticide_columns[i]]) == FALSE)))
        detects <- which(grepl("<", dataset[, names_pesticide_columns[i]]) == 
            FALSE)
        for (j in 1:length(dataset[, names_pesticide_columns[i]])) {
            dataset[j, names_pesticide_columns[i]] <- ifelse(j < 
                first_detect, "0.00000000001", ifelse(j %in% 
                detects, dataset[j, names_pesticide_columns[i]], 
                as.numeric(gsub(pattern = "<", replacement = "", 
                  dataset[j, names_pesticide_columns[i]])) * 
                  relative_LOR_mulitipliers$relative_LOR[which(relative_LOR_mulitipliers$analyte == 
                    names_pesticide_columns[i])]))
            dataset
        }
    }
    dataset
}