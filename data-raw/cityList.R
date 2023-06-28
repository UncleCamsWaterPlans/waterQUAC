#Extracting the city list file provided through the website:
R.utils::gunzip("data-raw/citylistjson.gz")
cityList <- jsonlite::fromJSON(txt = "data-raw/citylistjson.tmp", flatten=TRUE)
usethis::use_data(cityList, overwrite = TRUE)
