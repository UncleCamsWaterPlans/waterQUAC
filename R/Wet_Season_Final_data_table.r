#'@title Format Wet Season PRM data for visualisation
#'
#'@description This function combines Wet Season MI tables for the different pesticide groups and calculates proportional values, adjusted proportional values and PRM Protected and Affected. This data is formatted for use in WQI plotting package \"WaterboyProductions\". This data is used in communicating Pesticide Risk in waterways.
#'
#'
#'@param MI_estimates_KD All PRM Wet Season MI data table
#'@param All_Chemicals_data data frame of all chemical data output from the Daily average calculations
#'@param STATION whether station number column is present 
#'@param Save_csv_files this determines whether a dataframe is returned or csv files
#'@param file_name name and location for where the treated csv file will be saved
#'
#'@return csv file of Wet Season PRM values saved in file_name location.
#'
Wet_Season_Final_data_table <- function(MI_estimates_KD, All_Chemicals_data, STATION = FALSE, 
    Save_csv_files = FALSE, file_name = NULL) 
{
    Wet_Season_data <- MI_estimates_KD
    All_Chemicals_data[All_Chemicals_data == 0] <- 1e-11
    Summary <- All_Chemicals_data %>% dplyr::group_by(`Site Name`, 
        `Sampling Year`) %>% dplyr::summarise_if(is.numeric, 
        mean, na.rm = TRUE)
    Summary[is.na(Summary)] <- 0
    if (STATION == FALSE) {
        Summary$row_sum = rowSums(Summary[, c(4:25)])
    }
    else {
        Summary$row_sum = rowSums(Summary[, c(4:25)])
    }
    names(Summary) <- gsub("\\_PRM", "", names(Summary))
    Summary$Prop_Ametryn <- Summary$Ametryn/Summary$row_sum
    Summary$Prop_Atrazine <- Summary$Atrazine/Summary$row_sum
    Summary$Prop_Diuron <- Summary$Diuron/Summary$row_sum
    Summary$Prop_Hexazinone <- Summary$Hexazinone/Summary$row_sum
    Summary$Prop_Metribuzin <- Summary$Metribuzin/Summary$row_sum
    Summary$Prop_Prometryn <- Summary$Prometryn/Summary$row_sum
    Summary$Prop_Simazine <- Summary$Simazine/Summary$row_sum
    Summary$Prop_Tebuthiuron <- Summary$Tebuthiuron/Summary$row_sum
    Summary$Prop_Terbuthylazine <- Summary$Terbuthylazine/Summary$row_sum
    Summary$Prop_Isoxaflutole <- Summary$Isoxaflutole/Summary$row_sum
    Summary$Prop_Haloxyfop <- Summary$Haloxyfop/Summary$row_sum
    Summary$Prop_Imazapic <- Summary$Imazapic/Summary$row_sum
    Summary$Prop_Metsulfuron <- Summary$Metsulfuron/Summary$row_sum
    Summary$Prop_Pendimethalin <- Summary$Pendimethalin/Summary$row_sum
    Summary$Prop_X2.4.D <- Summary$X2.4.D/Summary$row_sum
    Summary$Prop_MCPA <- Summary$MCPA/Summary$row_sum
    Summary$Prop_Triclopyr <- Summary$Triclopyr/Summary$row_sum
    Summary$Prop_Fluroxypyr <- Summary$Fluroxypyr/Summary$row_sum
    Summary$Prop_Metolachlor <- Summary$Metolachlor/Summary$row_sum
    Summary$Prop_Chlorpyrifos <- Summary$Chlorpyrifos/Summary$row_sum
    Summary$Prop_Fipronil <- Summary$Fipronil/Summary$row_sum
    Summary$Prop_Imidacloprid <- Summary$Imidacloprid/Summary$row_sum
    Summary <- Summary %>% dplyr::rename(`'2,4-D'` = "X2.4.D", 
        `Prop_2,4-D` = "Prop_X2.4.D")
    Summary$Site_and_Year = paste(Summary$`Site Name`, Summary$`Sampling Year`)
    Wet_Season_data$group_sum = rowSums(Wet_Season_data[, c(4:6)])
    Wet_Season_data$Prop_Insecticides <- Wet_Season_data$`Insecticide PRM`/Wet_Season_data$group_sum
    Wet_Season_data$Prop_OH <- Wet_Season_data$`Other Herb PRM`/Wet_Season_data$group_sum
    Wet_Season_data$Prop_PSIIs <- Wet_Season_data$`PSII PRM`/Wet_Season_data$group_sum
    Wet_Season_data$TPRMround <- format(round(Wet_Season_data$`Total PRM`, 
        1), nsmall = 1)
    Wet_Season_data$TPRMround <- as.numeric(Wet_Season_data$TPRMround)
    Wet_Season_data$PercProt_round <- 100 - Wet_Season_data$TPRMround
    levels <- c(-Inf, 1, 5, 10, 20, Inf)
    labels <- c("Very Low", "Low", "Moderate", "High", "Very High")
    Wet_Season_data <- Wet_Season_data %>% dplyr::mutate(Risk = cut(TPRMround, 
        levels, labels = labels))
    Wet_Season_data$Site_and_Year = paste(Wet_Season_data$`Site Name`, 
        Wet_Season_data$`Sampling Year`)
    FinalDf <- merge(Summary, Wet_Season_data, by = c("Site_and_Year", 
        "Site Name", "Sampling Year"))
    FinalDf <- FinalDf[-1]
    FinalDf <- FinalDf %>% dplyr::select("Site Name", "Sampling Year", 
        "PercProt_round", "Risk", "Ametryn", "Atrazine", "Diuron", 
        "Hexazinone", "Metribuzin", "Prometryn", "Simazine", 
        "Tebuthiuron", "Terbuthylazine", "Isoxaflutole", "Haloxyfop", 
        "Imazapic", "Metsulfuron", "Pendimethalin", "'2,4-D'", 
        "MCPA", "Triclopyr", "Fluroxypyr", "Metolachlor", "Chlorpyrifos", 
        "Fipronil", "Imidacloprid", "row_sum", "Prop_Ametryn", 
        "Prop_Atrazine", "Prop_Diuron", "Prop_Hexazinone", "Prop_Metribuzin", 
        "Prop_Prometryn", "Prop_Simazine", "Prop_Tebuthiuron", 
        "Prop_Terbuthylazine", "Prop_Isoxaflutole", "Prop_Haloxyfop", 
        "Prop_Imazapic", "Prop_Metsulfuron", "Prop_Pendimethalin", 
        "Prop_2,4-D", "Prop_MCPA", "Prop_Triclopyr", "Prop_Fluroxypyr", 
        "Prop_Metolachlor", "Prop_Chlorpyrifos", "Prop_Fipronil", 
        "Prop_Imidacloprid", "Insecticide PRM", "Other Herb PRM", 
        "PSII PRM", "Total PRM", "group_sum", "Prop_Insecticides", 
        "Prop_OH", "Prop_PSIIs", "TPRMround")
    FinalDf$AdjProp_Ametryn <- FinalDf$Prop_Ametryn * FinalDf$`Total PRM`
    FinalDf$AdjProp_Atrazine <- FinalDf$Prop_Atrazine * FinalDf$`Total PRM`
    FinalDf$AdjProp_Diuron <- FinalDf$Prop_Diuron * FinalDf$`Total PRM`
    FinalDf$AdjProp_Hexazinone <- FinalDf$Prop_Hexazinone * FinalDf$`Total PRM`
    FinalDf$AdjProp_Metribuzin <- FinalDf$Prop_Metribuzin * FinalDf$`Total PRM`
    FinalDf$AdjProp_Prometryn <- FinalDf$Prop_Prometryn * FinalDf$`Total PRM`
    FinalDf$AdjProp_Simazine <- FinalDf$Prop_Simazine * FinalDf$`Total PRM`
    FinalDf$AdjProp_Tebuthiuron <- FinalDf$Prop_Tebuthiuron * 
        FinalDf$`Total PRM`
    FinalDf$AdjProp_Terbuthylazine <- FinalDf$Prop_Terbuthylazine * 
        FinalDf$`Total PRM`
    FinalDf$AdjProp_Isoxaflutole <- FinalDf$Prop_Isoxaflutole * 
        FinalDf$`Total PRM`
    FinalDf$AdjProp_Haloxyfop <- FinalDf$Prop_Haloxyfop * FinalDf$`Total PRM`
    FinalDf$AdjProp_Imazapic <- FinalDf$Prop_Imazapic * FinalDf$`Total PRM`
    FinalDf$AdjProp_Metsulfuron <- FinalDf$Prop_Metsulfuron * 
        FinalDf$`Total PRM`
    FinalDf$AdjProp_Pendimethalin <- FinalDf$Prop_Pendimethalin * 
        FinalDf$`Total PRM`
    FinalDf$AdjProp_2.4.D <- FinalDf$"Prop_2,4-D" * FinalDf$`Total PRM`
    FinalDf$AdjProp_MCPA <- FinalDf$Prop_MCPA * FinalDf$`Total PRM`
    FinalDf$AdjProp_Triclopyr <- FinalDf$Prop_Triclopyr * FinalDf$`Total PRM`
    FinalDf$AdjProp_Fluroxypyr <- FinalDf$Prop_Fluroxypyr * FinalDf$`Total PRM`
    FinalDf$AdjProp_Metolachlor <- FinalDf$Prop_Metolachlor * 
        FinalDf$`Total PRM`
    FinalDf$AdjProp_Chlorpyrifos <- FinalDf$Prop_Chlorpyrifos * 
        FinalDf$`Total PRM`
    FinalDf$AdjProp_Fipronil <- FinalDf$Prop_Fipronil * FinalDf$`Total PRM`
    FinalDf$AdjProp_Imidacloprid <- FinalDf$Prop_Imidacloprid * 
        FinalDf$`Total PRM`
    if (Save_csv_files == FALSE) {
        return(FinalDf)
    }
    else {
        utils::write.csv(FinalDf, file_name, row.names = FALSE)
    }
}