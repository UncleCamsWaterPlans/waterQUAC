#'@title calculate Daily Average PRM Categories
#'
#'@description This function takes a LOR treated data frame of	concentration values for the 22 PRM pesticides and produces daily average	values for every site and day.
#'
#'@param LOR_Treated_data dataframe of LOR treated pesticide	concentration values		
#'@param Save_csv_files this determines whether a dataframe	is returned	or csv files
#'@param output_filepath this sets the file path for all csv	files to be	saved into
#'		
#'@return list of 2 dataframes, daily average PRM values	and all chemicals table		
#'
#'
#'@export
calculate_Daily_Average_RA_PRM_Categories <- function(LOR_Treated_data, Save_csv_files = FALSE, output_filepath = NULL) 
{
    `Site Name` <- LOR_Treated_data[, "Site Name"]
    Station <- LOR_Treated_data[, "Station"]
    `Sampling Year` <- LOR_Treated_data[, "Sampling Year"]
    Date <- LOR_Treated_data[, "Date"]
    Isoxaflutole_parameters <- data.frame(Pesticide.type = "Herbicide", 
        Distribution.type = "Inverse Weibull", log.alpha = -0.385904591, 
        alpha = 0.679835385, log.beta = -1.242018461, beta = 0.288800696)
    Haloxyfop_parameters <- data.frame(Pesticide.type = "Herbicide", 
        Distribution.type = "Log-logistic", log.alpha = 9.736670973, 
        alpha = 16927.09672, log.beta = 0.313737397, beta = 1.368530309)
    Chlorpyrifos_parameters <- data.frame(Pesticide.type = "Insecticide", 
        Distribution.type = "Burr Type III", log.b = 1.307471952, 
        b = 3.696816161, log.c = -1.002926184, c = 0.366804532, 
        log.k = 0.3404141, k = 1.40552947)
    Imazapic_parameters <- data.frame(Pesticide.type = "Herbicide", 
        Distribution.type = "Log-logistic", log.alpha = 3.115540332, 
        alpha = 22.54560918, log.beta = -0.287699081, beta = 0.749987243)
    Metsulfuron_parameters <- data.frame(Pesticide.type = "Herbicide", 
        Distribution.type = "Inverse Weibull", log.alpha = -1.350296558, 
        alpha = 0.259163392, log.beta = -0.82416039, beta = 0.438603093)
    Fipronil_parameters <- data.frame(Pesticide.type = "Insecticide", 
        Distribution.type = "Burr Type III", log.b = -5.108354396, 
        b = 0.006046024, log.c = -0.614167078, c = 0.541091394, 
        log.k = 1.6728064, k = 5.3270967)
    Pendimethalin_parameters <- data.frame(Pesticide.type = "Herbicide", 
        Distribution.type = "Burr Type III", log.b = 1.181856154, 
        b = 3.260420431, log.c = -0.334798726, c = 0.71548208, 
        log.k = 0.4320021, k = 1.54033831)
    Chlorothalonil_parameters <- data.frame(Pesticide.type = "Fungicide", 
        Note = "Not included at this stage")
    Imidacloprid_parameters <- data.frame(Pesticide.type = "Insecticide", 
        Distribution.type = "Inverse Weibull", log.alpha = -0.689527, 
        alpha = 0.501813371, log.beta = -0.173493, beta = 0.840723036)
    Ametryn_parameters <- data.frame(Pesticide.type = "Herbicide", 
        Distribution.type = "Burr Type III", log.b = 1.70321271129072, 
        b = 5.491562, log.c = 0.051791759, c = 1.053156, log.k = 0.027215171, 
        k = 1.027589)
    Atrazine_parameters <- data.frame(pesticide.type = "Herbicide", 
        Distribution.type = "Burr Type III", log.b = 2.435718313, 
        b = 11.42402, log.c = -0.380527521, c = 0.6835008, log.k = 0.556916028, 
        k = 1.745282)
    Bromacil <- data.frame(Pesticide.type = "Herbicide", Distribution.type = "Log-Logistic", 
        log.alpha = 2.318335968, alpha = 10.15876, log.beta = -0.117259051, 
        beta = 0.8893548)
    Prometryn_parameters <- data.frame(Pesticide.type = "Herbicide", 
        Distribution.type = "Burr Type III", log.b = 1.945054953, 
        b = 6.994016, log.c = 0.004043219, c = 1.004051, log.k = 0.046399659, 
        k = 1.047493)
    Tebuthiuron_parameters <- data.frame(Pesticide.type = "Herbicide", 
        Distribution.type = "Log-logistic", log.alpha = 3.973218745, 
        alpha = 53.15535, log.beta = 0.642836234, beta = 1.901867)
    Terbuthylazine_parameters <- data.frame(Pesticide.type = "Herbicide", 
        Distribution.type = "Burr Type III", log.b = 1.579371269, 
        b = 4.851904, log.c = -0.082197727, c = 0.9210898, log.k = 0.732467232, 
        k = 2.080207)
    Fluometuron_parameters <- data.frame(Pesticide.type = "Herbicide", 
        Distribution.type = "Log-Logistic", log.alpha = 4.945979068, 
        alpha = 140.6084, log.beta = 0.749108486, beta = 2.115114)
    Simazine_parameters <- data.frame(Pesticide.type = "Herbicide", 
        Distribution.type = "Burr Type III", log.b = 4.660057347, 
        b = 105.6421, log.c = 0.716940996, c = 2.048158, log.k = 0.186694505, 
        k = 1.205259)
    Diuron_parameters <- data.frame(Pesticide.type = "Herbicide", 
        Distribution.type = "Burr Type III", log.b = -3.093033062, 
        b = 0.04536415, log.c = -0.682096599, c = 0.5055559, 
        log.k = 2.079757129, k = 8.002525)
    Terbutryn_parameters <- data.frame(Pesticide.type = "Herbicide", 
        Distribution.type = "Burr Type III", log.b = 0.97132771, 
        b = 2.641449, log.c = -0.601273034, c = 0.5481134, log.k = 0.782565245, 
        k = 2.187075)
    Hexazinone_parameters <- data.frame(Pesticide.type = "Herbicide", 
        Distribution.type = "Inverse Weibull", local.log.alpha = 0.258963617, 
        local.alpha = 1.295586667, local.log.beta = -1.76241848, 
        local.beta = 0.17162928)
    Metribuzin_parameters <- data.frame(Pesticide.type = "Herbicide", 
        Distribution.type = "Inverse Weibull", local.log.alpha = 0.5369, 
        local.alpha = 1.710695478, local.log.beta = -1.58798, 
        local.beta = 0.20433796)
    `2,4-D_parameters` <- data.frame(Pesticide.type = "Herbicide", 
        Distribution.type = "Inverse Weibull", log.alpha = -0.658974563, 
        alpha = 0.5173816, log.beta = -4.936336367, beta = 0.007180858)
    MCPA_parameters <- data.frame(Pesticide.type = "Herbicide", 
        Distribution.type = "Burr Type III", log.b = 10.24155642, 
        b = 28044.74, log.c = 0.219595444, c = 1.245573, log.k = -1.409133471, 
        k = 0.2443549)
    Fluroxypyr_parameters <- data.frame(Pesticide.type = "Herbicide", 
        Distribution.type = "Log-logistic", log.alpha = 7.187286375, 
        alpha = 1322.51, log.beta = 0.628023319, beta = 1.873903)
    Triclopyr_parameters <- data.frame(Pesticide.type = "Herbicide", 
        Distribution.type = "Burr Type III", log.b = 6.962690537, 
        b = 1056.472, log.c = 1.700066731, c = 5.474313, log.k = -2.312677626, 
        k = 0.09899582)
    Metolachlor_parameters <- data.frame(Pesticide.type = "Herbicide", 
        Distribution.type = "Burr Type III", log.b = 6.386127229, 
        b = 593.5534261, log.c = -0.288986309, c = 0.74902246, 
        log.k = -0.6023196, k = 0.54754006)
    BurrIIIFunction <- function(analyte_column, parameter_b, 
        parameter_c, parameter_k) {
        analyte_column_numeric <- as.numeric(analyte_column)
        PRM <- 1/(1 + (parameter_b/analyte_column_numeric)^parameter_c)^parameter_k * 
            100
        PRM
    }
    LoglogisticFunction <- function(analyte_column, parameter_alpha, 
        parameter_beta) {
        analyte_column_numeric <- as.numeric(analyte_column)
        PRM <- 1/(1 + (analyte_column_numeric/parameter_alpha)^(-1 * 
            parameter_beta)) * 100
        PRM
    }
    InverseWeibullFunction <- function(analyte_column, parameter_local.alpha, 
        parameter_local.beta) {
        analyte_column_numeric <- as.numeric(analyte_column)
        PRM <- exp(-1/(analyte_column_numeric * parameter_local.beta)^parameter_local.alpha) * 
            100
        PRM
    }
    Ametryn_PRM <- BurrIIIFunction(analyte_column = LOR_Treated_data[, 
        "Ametryn"], parameter_b = Ametryn_parameters$b, parameter_c = Ametryn_parameters$c, 
        parameter_k = Ametryn_parameters$k)
    PRM_propn_Ametryn <- Ametryn_PRM/100
    Atrazine_PRM <- BurrIIIFunction(analyte_column = LOR_Treated_data[, 
        "Atrazine"], parameter_b = Atrazine_parameters$b, parameter_c = Atrazine_parameters$c, 
        parameter_k = Atrazine_parameters$k)
    PRM_propn_Atrazine <- Atrazine_PRM/100
    Prometryn_PRM <- BurrIIIFunction(analyte_column = LOR_Treated_data[, 
        "Prometryn"], parameter_b = Prometryn_parameters$b, parameter_c = Prometryn_parameters$c, 
        parameter_k = Prometryn_parameters$k)
    PRM_propn_Prometryn <- Prometryn_PRM/100
    Tebuthiuron_PRM <- LoglogisticFunction(LOR_Treated_data[, 
        "Tebuthiuron"], parameter_alpha = Tebuthiuron_parameters$alpha, 
        parameter_beta = Tebuthiuron_parameters$beta)
    PRM_propn_Tebuthiuron <- Tebuthiuron_PRM/100
    Terbuthylazine_PRM <- BurrIIIFunction(analyte_column = LOR_Treated_data[, 
        "Terbuthylazine"], parameter_b = Terbuthylazine_parameters$b, 
        parameter_c = Terbuthylazine_parameters$c, parameter_k = Terbuthylazine_parameters$k)
    PRM_propn_Terbuthylazine <- Terbuthylazine_PRM/100
    Simazine_PRM <- BurrIIIFunction(analyte_column = LOR_Treated_data[, 
        "Simazine"], parameter_b = Simazine_parameters$b, parameter_c = Simazine_parameters$c, 
        parameter_k = Simazine_parameters$k)
    PRM_propn_Simazine <- Simazine_PRM/100
    Diuron_PRM <- BurrIIIFunction(analyte_column = LOR_Treated_data[, 
        "Diuron"], parameter_b = Diuron_parameters$b, parameter_c = Diuron_parameters$c, 
        parameter_k = Diuron_parameters$k)
    PRM_propn_Diuron <- Diuron_PRM/100
    Hexazinone_PRM <- InverseWeibullFunction(analyte_column = LOR_Treated_data[, 
        "Hexazinone"], parameter_local.alpha = Hexazinone_parameters$local.alpha, 
        parameter_local.beta = Hexazinone_parameters$local.beta)
    PRM_proptn_Hexazinone <- Hexazinone_PRM/100
    Metribuzin_PRM <- InverseWeibullFunction(analyte_column = LOR_Treated_data[, 
        "Metribuzin"], parameter_local.alpha = Metribuzin_parameters$local.alpha, 
        parameter_local.beta = Metribuzin_parameters$local.beta)
    PRM_proptn_Metribuzin <- Metribuzin_PRM/100
    RA_Table_PRM_PSIIs <- data.frame(1 - PRM_propn_Ametryn, 1 - 
        PRM_propn_Atrazine, 1 - PRM_propn_Prometryn, 1 - PRM_propn_Tebuthiuron, 
        1 - PRM_propn_Terbuthylazine, 1 - PRM_propn_Simazine, 
        1 - PRM_propn_Diuron, 1 - PRM_proptn_Hexazinone, 1 - 
            PRM_proptn_Metribuzin)
    PRM_All_PSIIs <- (1 - (apply(RA_Table_PRM_PSIIs, 1, FUN = prod, 
        na.rm = TRUE))) * 100
    PRM_All_PSIIs_withdate <- data.frame(Station, `Site Name`, 
        `Sampling Year`, Date, PRM_All_PSIIs)
    PRM_All_PSIIs_proptn <- PRM_All_PSIIs/100
    Isoxaflutole_PRM <- InverseWeibullFunction(analyte_column = LOR_Treated_data[, 
        "Isoxaflutole"], parameter_local.alpha = Isoxaflutole_parameters$alpha, 
        parameter_local.beta = Isoxaflutole_parameters$beta)
    PRM_propn_Isoxaflutole <- Isoxaflutole_PRM/100
    Haloxyfop_PRM <- LoglogisticFunction(analyte_column = LOR_Treated_data[, 
        "Haloxyfop"], parameter_alpha = Haloxyfop_parameters$alpha, 
        parameter_beta = Haloxyfop_parameters$beta)
    PRM_propn_Haloxyfop <- Haloxyfop_PRM/100
    Imazapic_PRM <- LoglogisticFunction(analyte_column = LOR_Treated_data[, 
        "Imazapic"], parameter_alpha = Imazapic_parameters$alpha, 
        parameter_beta = Imazapic_parameters$beta)
    PRM_propn_Imazapic <- Imazapic_PRM/100
    Metsulfuron_PRM <- InverseWeibullFunction(analyte_column = LOR_Treated_data[, 
        "Metsulfuron"], parameter_local.alpha = Metsulfuron_parameters$alpha, 
        parameter_local.beta = Metsulfuron_parameters$beta)
    PRM_propn_Metsulfuron <- Metsulfuron_PRM/100
    Pendimethalin_PRM <- BurrIIIFunction(analyte_column = LOR_Treated_data[, 
        "Pendimethalin"], parameter_b = Pendimethalin_parameters$b, 
        parameter_c = Pendimethalin_parameters$c, parameter_k = Pendimethalin_parameters$k)
    PRM_propn_Pendimethalin <- Pendimethalin_PRM/100
    `2,4-D_PRM` <- InverseWeibullFunction(analyte_column = LOR_Treated_data[, 
        "'2,4-D'"], parameter_local.alpha = `2,4-D_parameters`$alpha, 
        parameter_local.beta = `2,4-D_parameters`$beta)
    `PRM_propn_2,4-D` <- `2,4-D_PRM`/100
    MCPA_PRM <- BurrIIIFunction(analyte_column = LOR_Treated_data[, 
        "MCPA"], parameter_b = MCPA_parameters$b, parameter_c = MCPA_parameters$c, 
        parameter_k = MCPA_parameters$k)
    PRM_propn_MCPA <- MCPA_PRM/100
    Fluroxypyr_PRM <- LoglogisticFunction(analyte_column = LOR_Treated_data[, 
        "Fluroxypyr"], parameter_alpha = Fluroxypyr_parameters$alpha, 
        parameter_beta = Fluroxypyr_parameters$beta)
    PRM_propn_Fluroxypyr <- Fluroxypyr_PRM/100
    Triclopyr_PRM <- BurrIIIFunction(analyte_column = LOR_Treated_data[, 
        "Triclopyr"], parameter_b = Triclopyr_parameters$b, parameter_c = Triclopyr_parameters$c, 
        parameter_k = Triclopyr_parameters$k)
    PRM_propn_Triclopyr <- Triclopyr_PRM/100
    Metolachlor_PRM <- BurrIIIFunction(analyte_column = LOR_Treated_data[, 
        "Metolachlor"], parameter_b = Metolachlor_parameters$b, 
        parameter_c = Metolachlor_parameters$c, parameter_k = Metolachlor_parameters$k)
    PRM_propn_Metolachlor <- Metolachlor_PRM/100
    RA_Table_PRM_Other_Herbicides <- data.frame(1 - PRM_propn_Isoxaflutole, 
        1 - PRM_propn_Haloxyfop, 1 - PRM_propn_Imazapic, 1 - 
            PRM_propn_Metsulfuron, 1 - PRM_propn_Pendimethalin, 
        1 - `PRM_propn_2,4-D`, 1 - PRM_propn_MCPA, 1 - PRM_propn_Fluroxypyr, 
        1 - PRM_propn_Triclopyr, 1 - PRM_propn_Metolachlor)
    PRM_Other_Herbicides <- (1 - (apply(RA_Table_PRM_Other_Herbicides, 
        1, FUN = prod, na.rm = TRUE))) * 100
    PRM_Other_Herbicides_withdate <- data.frame(Station, `Site Name`, 
        `Sampling Year`, Date, PRM_Other_Herbicides)
    PRM_Other_Herbicides_proptn <- PRM_Other_Herbicides/100
    Chlorpyrifos_PRM <- BurrIIIFunction(analyte_column = LOR_Treated_data[, 
        "Chlorpyrifos"], parameter_b = Chlorpyrifos_parameters$b, 
        parameter_c = Chlorpyrifos_parameters$c, parameter_k = Chlorpyrifos_parameters$k)
    PRM_propn_Chlorpyrifos <- Chlorpyrifos_PRM/100
    Fipronil_PRM <- BurrIIIFunction(analyte_column = LOR_Treated_data[, 
        "Fipronil"], parameter_b = Fipronil_parameters$b, parameter_c = Fipronil_parameters$c, 
        parameter_k = Fipronil_parameters$k)
    PRM_propn_Fipronil <- Fipronil_PRM/100
    Imidacloprid_PRM <- InverseWeibullFunction(analyte_column = LOR_Treated_data[, 
        "Imidacloprid"], parameter_local.alpha = Imidacloprid_parameters$alpha, 
        parameter_local.beta = Imidacloprid_parameters$beta)
    PRM_propn_Imidacloprid <- Imidacloprid_PRM/100
    RA_Table_PRM_Insecticides <- data.frame(1 - PRM_propn_Chlorpyrifos, 
        1 - PRM_propn_Fipronil, 1 - PRM_propn_Imidacloprid)
    PRM_Insecticides <- (1 - (apply(RA_Table_PRM_Insecticides, 
        1, FUN = prod, na.rm = TRUE))) * 100
    PRM_Insecticides_withdate <- data.frame(Station, `Site Name`, 
        `Sampling Year`, Date, PRM_Insecticides)
    PRM_Insecticides_proptn <- PRM_Insecticides/100
    RA_Table_Total <- cbind(RA_Table_PRM_PSIIs, RA_Table_PRM_Other_Herbicides, 
        RA_Table_PRM_Insecticides)
    PRM_Total <- (1 - (apply(RA_Table_Total, 1, FUN = prod, na.rm = TRUE))) * 
        100
    PRM_Total_withdate <- data.frame(Station, `Site Name`, `Sampling Year`, 
        Date, PRM_Total)
    PRM_Total_proptn <- PRM_Total/100
    Daily_Average_Function <- function(PRM_Values) {
        Daily_Ave <- aggregate(PRM_Values, by = list(Station, 
            `Site Name`, `Sampling Year`, Date), FUN = mean)
        names(Daily_Ave) <- c("Station", "Site Name", "Sampling Year", 
            "Date", "Daily.Ave.PRM")
        Daily_Ave
    }
    Daily_Average_RA_PRM_PSIIs <- Daily_Average_Function(PRM_All_PSIIs)
    Daily_Average_RA_PRM_PSIIs$Daily.Ave.PRM <- ifelse(Daily_Average_RA_PRM_PSIIs$Daily.Ave.PRM == 
        0, 1e-11, Daily_Average_RA_PRM_PSIIs$Daily.Ave.PRM)
    Daily_Average_RA_PRM_Other_Herbicides <- Daily_Average_Function(PRM_Other_Herbicides)
    Daily_Average_RA_PRM_Other_Herbicides$Daily.Ave.PRM <- ifelse(Daily_Average_RA_PRM_Other_Herbicides$Daily.Ave.PRM == 
        0, 1e-11, Daily_Average_RA_PRM_Other_Herbicides$Daily.Ave.PRM)
    Daily_Average_RA_PRM_Insecticides <- Daily_Average_Function(PRM_Insecticides)
    Daily_Average_RA_PRM_Insecticides$Daily.Ave.PRM <- ifelse(Daily_Average_RA_PRM_Insecticides$Daily.Ave.PRM == 
        0, 1e-11, Daily_Average_RA_PRM_Insecticides$Daily.Ave.PRM)
    Daily_Average_RA_PRM_Total <- Daily_Average_Function(PRM_Total)
    Daily_Average_RA_PRM_Total$Daily.Ave.PRM <- ifelse(Daily_Average_RA_PRM_Total$Daily.Ave.PRM == 
        0, 1e-11, Daily_Average_RA_PRM_Total$Daily.Ave.PRM)
    Daily_PRM_Chemicals <- data.frame(Station, `Site Name`, `Sampling Year`, 
        Date, Ametryn_PRM, Atrazine_PRM, Diuron_PRM, Hexazinone_PRM, 
        Metribuzin_PRM, Prometryn_PRM, Simazine_PRM, Tebuthiuron_PRM, 
        Terbuthylazine_PRM, Isoxaflutole_PRM, Haloxyfop_PRM, 
        Imazapic_PRM, Metsulfuron_PRM, Pendimethalin_PRM, `2,4-D_PRM`, 
        MCPA_PRM, Triclopyr_PRM, Fluroxypyr_PRM, Metolachlor_PRM, 
        Chlorpyrifos_PRM, Fipronil_PRM, Imidacloprid_PRM)
    if (Save_csv_files == FALSE) {
        Daily_Average_RA_PRM_Total <- Daily_Average_RA_PRM_Total %>% 
            dplyr::rename(`Total PRM` = "Daily.Ave.PRM")
        Daily_Average_RA_PRM_PSIIs <- Daily_Average_RA_PRM_PSIIs %>% 
            dplyr::rename(`PSII PRM` = "Daily.Ave.PRM")
        Daily_Average_RA_PRM_Other_Herbicides <- Daily_Average_RA_PRM_Other_Herbicides %>% 
            dplyr::rename(`Other Herb PRM` = "Daily.Ave.PRM")
        Daily_Average_RA_PRM_Insecticides <- Daily_Average_RA_PRM_Insecticides %>% 
            dplyr::rename(`Insecticide PRM` = "Daily.Ave.PRM")
        Daily_Average_combined <- Daily_Average_RA_PRM_Total %>% 
            dplyr::full_join(Daily_Average_RA_PRM_PSIIs)
        Daily_Average_combined <- Daily_Average_combined %>% 
            dplyr::full_join(Daily_Average_RA_PRM_Other_Herbicides)
        Daily_Average_combined <- Daily_Average_combined %>% 
            dplyr::full_join(Daily_Average_RA_PRM_Insecticides)
        Daily_PRM_All_Chems_List <- list(Daily_Average_combined, 
            Daily_PRM_Chemicals)
        return(Daily_PRM_All_Chems_List)
    }
    else {
        utils::write.csv(Daily_PRM_Chemicals, paste(output_path, 
            paste("PRM_All_Chemicals", "PRM22", Sys.getenv("USERNAME"), 
                as.character(Sys.Date()), sep = "_"), ".csv", 
            sep = ""), row.names = FALSE)
        utils::write.csv(Daily_Average_RA_PRM_PSIIs, paste(output_path, 
            paste("PRM_All_PSIIs_Daily_Average", "PRM22", Sys.getenv("USERNAME"), 
                as.character(Sys.Date()), sep = "_"), ".csv", 
            sep = ""), row.names = FALSE)
        utils::write.csv(Daily_Average_RA_PRM_Other_Herbicides, 
            paste(output_path, paste("PRM_Other_Herbicides_Daily_Average", 
                "PRM22", Sys.getenv("USERNAME"), as.character(Sys.Date()), 
                sep = "_"), ".csv", sep = ""), row.names = FALSE)
        utils::write.csv(Daily_Average_RA_PRM_Insecticides, paste(output_path, 
            paste("PRM_Insecticides_Daily_Average", "PRM22", 
                Sys.getenv("USERNAME"), as.character(Sys.Date()), 
                sep = "_"), ".csv", sep = ""), row.names = FALSE)
        utils::write.csv(Daily_Average_RA_PRM_Total, paste(output_path, 
            paste("PRM_Total_Daily_Average", "PRM22", Sys.getenv("USERNAME"), 
                as.character(Sys.Date()), sep = "_"), ".csv", 
            sep = ""), row.names = FALSE)
    }
}