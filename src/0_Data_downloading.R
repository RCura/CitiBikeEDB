baseURL <- "https://s3.amazonaws.com/tripdata/"
yearToDownload <- 2016
monthsToDownload <- 1:12
yearMonth <- paste0(yearToDownload, ifelse(monthsToDownload < 10, yes = paste0(0,monthsToDownload), no = monthsToDownload))
fileNames <- paste0(yearMonth,"-citibike-tripdata.zip")

saveTripData <- function(fileName, baseURL){
  fileURL <- paste0(baseURL, fileName)
  outputName <- paste0("data/", fileName)
  download.file(url = fileURL, destfile = outputName)
  unzip(zipfile = outputName, exdir = "data/")
  file.remove(outputName)
}

mapply(FUN = saveTripData, fileName = fileNames, baseURL = baseURL)
