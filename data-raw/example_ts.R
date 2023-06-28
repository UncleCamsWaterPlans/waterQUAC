
APIKEY <- Sys.getenv("EIO_KEY")
lev <- WQI::loggerRef$`OPUSResults - OPUS1016 TSSeq`[WQI::loggerRef$GSnum == "1160116"]
tm <- format(Sys.time() - 1500 * 86400, "%Y-%m-%dT%H:%M:%SZ")

TSS_data <- WQI::EIO_Hist(APIKEY, lev, tm)

usethis::use_data(TSS_data)
