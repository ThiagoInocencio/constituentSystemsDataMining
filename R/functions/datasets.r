# function to load datasets
loadDataSet <- function(dataset) {
  selectedDataSet <- switch(dataset,
         A={here("R/datasets", "data_set_base_A.csv")},
         B={here("R/datasets", "data_set_base_B.csv")},
         C={here("R/datasets", "data_set_base_C.csv")}
  )

  dataset <- read.csv(selectedDataSet, header = TRUE, stringsAsFactors = FALSE)

  dataset
}

loadDataSetProcessed <- function(executionStatus, base) {

  folderName <- here("R/datasets", paste(executionStatus$SystemDefinition$SystemName, executionStatus$BaseSelected,sep="/"))

  cat(folderName)

  if (file.exists(folderName)) {

    dataSetTransformed <- read.csv(paste(folderName, "/data.csv", sep=""), header = TRUE, stringsAsFactors = FALSE, sep=";", fileEncoding="UTF-8-BOM")


  } else {
    dataSetTransformed <- processDataBase(definitions$systemDefinition, base)
    createPath(folderName)
    write_excel_csv2(dataSetTransformed, paste(folderName, "data.csv", sep="/"));
  }

  dataSetTransformed

}
