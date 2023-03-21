#'@title 	Tidy column names and types for use in other functions contained in this package
#'							
#'@description Function for correctly formatting column names and types for	following LOR and PRM treatments						
#'	     						
#'@param	Raw_data Data frame containing LORs you want treated					
#'@param	 STATION This is the vector containing guaging station numbers					
#'@param	SITENAME This is the vector containing site names					
#'@param	 SAMPNUM This is the vector for WQI sample number					
#'@param	SAMPTYPE this is the vector for sample type e.g. blank, dupe					
#'@param	DATE.TIME.SAMPLED this the vector of date times of sampling					
#'@param	    DATE vector of sample dates					
#'@param	    TIME vector of sample times					
#'@param	 SRCSAMP sample source vector					
#'@param	COLLSAMP sample collection authority vector					
#'@param	Conductivity sampled conductivity vector					
#'@param	Turbidity sample turbidity vector					
#'@param	      pH sample pH vector					
#'@param	     TSS sample TSS vector					
#'@param	Chlorpyrifos This is the vector (column) for Chlorpyrifos					
#'@param	Fipronil This is the vector (column) for Fipronil					
#'@param	Imidacloprid This is the vector (column) for Imidacloprid					
#'@param	Haloxyfop This is the vector (column) for Haloxyfop					
#'@param	Imazapic This is the vector (column) for Imazapic					
#'@param	Metsulfuron This is the vector (column) for Metsulfuron					
#'@param	Pendimethalin This is the vector (column) for Pendimethalin					
#'@param	Metolachlor This is the vector (column) for Metolachlor					
#'@param	  Two4_D This is the vector (column) for 2-4,D					
#'@param	    MCPA This is the vector (column) for MCPA					
#'@param	Fluroxypyr This is the vector (column) for Fluroxypyr					
#'@param	Triclopyr This is the vector (column) for Triclopyr					
#'@param	Isoxaflutole This is the vector (column) for Isoxaflutole					
#'@param	 Ametryn This is the vector (column) for Ametryn					
#'@param	Atrazine This is the vector (column) for Atrazine					
#'@param	Prometryn This is the vector (column) for Prometryn					
#'@param	Terbuthylazine This is the vector (column) for Terbuthylazine					
#'@param	Tebuthiuron This is the vector (column) for Tebuthiuron					
#'@param	Simazine This is the vector (column) for Simazine					
#'@param	  Diuron This is the vector (column) for Diuron					
#'@param	Hexazinone this is the vector (column) for Hexazinone					
#'@param	Metribuzin This is the vector (column) for Metribuzin					
#'							
#'@return a formatted data frame that should work with other functions in	this package.						
#'							
Tidy_Raw_Data <- function(Raw_data, STATION = "SITE.NO", SITENAME = "SITE.NAME", 
    SAMPNUM = "WQ.ANALYSIS.NO", SAMPTYPE = "SAMPLE.TYPE", DATE.TIME.SAMPLED = "DATE.TIME.SAMPLED", 
    DATE = "DATE.SAMPLED", TIME = "TIME.SAMPLED", SRCSAMP = "SAMPLE.SOURCE", 
    COLLSAMP = "COLLECT.AUTHORITY", Conductivity = "X3.Conductivity...25.C..µS.cm.", 
    Turbidity = "X5.Turbidity..NTU.", pH = "X7.pH...25.C..pH.units.", 
    TSS = "X9.Total.suspended.solids..mg.L.", Chlorpyrifos = "X107.Chlorpyrifos..µg.L.", 
    Fipronil = "X112.Fipronil..µg.L.", Imidacloprid = "X48.Imidacloprid..µg.L.", 
    Haloxyfop = "X84.Haloxyfop..acid...µg.L.", Imazapic = "X58.Imazapic..µg.L.", 
    Metsulfuron = "X74.Metsulfuron.methyl..µg.L.", Pendimethalin = "X138.Pendimethalin..µg.L.", 
    Metolachlor = "X83.Metolachlor..µg.L.", Two4_D = "X105.2.4.D..µg.L.", 
    MCPA = "X77.MCPA..µg.L.", Fluroxypyr = "X80.Fluroxypyr..µg.L.", 
    Triclopyr = "X79.Triclopyr..µg.L.", Isoxaflutole = "X117.Isoxaflutole.metab...DKN...µg.L.", 
    Ametryn = "X63.Ametryn..µg.L.", Atrazine = "X66.Atrazine..µg.L.", 
    Prometryn = "X67.Prometryn..µg.L.", Terbuthylazine = "X68.Terbuthylazine..µg.L.", 
    Tebuthiuron = "X64.Tebuthiuron..µg.L.", Simazine = "X65.Simazine..µg.L.", 
    Diuron = "X73.Diuron..µg.L.", Hexazinone = "X85.Hexazinone..µg.L.", 
    Metribuzin = "X88.Metribuzin..µg.L.") 
{
    Raw_data[Raw_data == "DA" | Raw_data == "CO" | Raw_data == 
        "LS" | Raw_data == "IS" | Raw_data == "NA" | Raw_data == 
        "NQ" | Raw_data == "NR" | Raw_data == "OK" | Raw_data == 
        "UR"] <- ""
    Raw_data[Raw_data == "TR" | Raw_data == "ND"] <- "<0.05"
    Raw_data[is.na(Raw_data)] <- ""
    Raw_data[!apply(Raw_data == "", 1, all), ]
    Raw_data <- Raw_data %>% dplyr::rename(Station = STATION, 
        `Site Name` = SITENAME, SAMPNUM = SAMPNUM, SAMPTYPE = SAMPTYPE, 
        DATE.TIME.SAMPLED = DATE.TIME.SAMPLED, Date = DATE, Time = TIME, 
        SRCSAMP = SRCSAMP, COLLSAMP = COLLSAMP, Conductivity = Conductivity, 
        Turbidity = Turbidity, pH = pH, TSS = TSS, Chlorpyrifos = Chlorpyrifos, 
        Fipronil = Fipronil, Imidacloprid = Imidacloprid, Haloxyfop = Haloxyfop, 
        Imazapic = Imazapic, Metsulfuron = Metsulfuron, Pendimethalin = Pendimethalin, 
        Metolachlor = Metolachlor, `'2,4-D'` = Two4_D, MCPA = MCPA, 
        Fluroxypyr = Fluroxypyr, Triclopyr = Triclopyr, Isoxaflutole = Isoxaflutole, 
        Ametryn = Ametryn, Atrazine = Atrazine, Prometryn = Prometryn, 
        Terbuthylazine = Terbuthylazine, Tebuthiuron = Tebuthiuron, 
        Simazine = Simazine, Diuron = Diuron, Hexazinone = Hexazinone, 
        Metribuzin = Metribuzin)
    Raw_data$DateTime <- as.POSIXct(Raw_data$DATE.TIME.SAMPLED, 
        format = "%d/%m/%Y %H:%M")
    Raw_data$Date <- as.Date(Raw_data$Date, format = "%d/%m/%Y")
    Raw_data$`Sampling Year` <- findSamplingYr(Raw_data$Date)
    Raw_data <- Raw_Data <- dplyr::select(Raw_data, Station, 
        `Site Name`, `Sampling Year`, Date, "Ametryn", "Atrazine", 
        "Chlorpyrifos", "Diuron", "Fipronil", "Fluroxypyr", "Haloxyfop", 
        "Hexazinone", "Imazapic", "Imidacloprid", "Isoxaflutole", 
        "MCPA", "Metolachlor", "Metribuzin", "Metsulfuron", "Pendimethalin", 
        "Prometryn", "Simazine", "Tebuthiuron", "Terbuthylazine", 
        "Triclopyr", "'2,4-D'")
    return(Raw_data)
}