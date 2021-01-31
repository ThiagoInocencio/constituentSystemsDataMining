# Script: runManySystem.R
# Autor: Thiago Inocêncio
# Last Update: 31/10/2020

# ----------------------------------------------------------------------------
# |
# | Creating a dataframe to store the execution status of methodology
# |
# ----------------------------------------------------------------------------
executionStatus <- data.frame(
  IdExecution = "",
  DataTransformationRunTime = "0",
  ExecutionTecniqueRunTime = "0",
  totalRunTime = "0",
  BaseSelected = "",
  Tecnique = "",
  params = "",
  NameOfIdealSystem = "",
  amountOfCandidatesRecomendation = "",
  AmountOfCoveredFeatures = "",
  PercentOfCoveredFeatures = 0,
  AmountOfNotCoveredFeatures = "",
  PercentOfNotCoveredFeatures = 0,
  AmountOfNeededSystems = "",
  AmountOfRecomendedSystems = "",
  AmountOfFeatures = "",
  AllFeaturesWereCovered = "",
  SystemDefinition = "",
  FeaturesCovered = "",
  FeaturesNotCovered = ""
)

# Setting the path
#install.packages("here")
library(here)

# loading packages
source(here("R/packages", "main.R"))
#installPackages()
loadPackages()

# loading useful functions
source(here("R/functions", "datasets.r"))
source(here("R/functions", "transformation.r"))
source(here("R/functions", "utils.r"))

# loadin the algorithms functions
source(here("R/algorithms", "ranking.r"))
source(here("R/algorithms", "k-means.r"))
source(here("R/algorithms", "hclust.r"))
source(here("R/algorithms", "dbscan.r"))

# loading the execution configurations
definitions <- fromJSON(here("R/systems", "ideal_systems_definition.json"))


for(i in 1:nrow(definitions)) {

  definition <- definitions[i,]

  executionStatus$Tecnique <- definition$params$tecnique$name
  executionStatus$SystemDefinition <- definition$systemDefinition
  executionStatus$NameOfIdealSystem <- definition$systemDefinition$SystemName
  executionStatus$params <- definition$params
  executionStatus$BaseSelected <- definition$params$base
  executionStatus$amountOfCandidatesRecomendation <- definition$params$numberOfCandidates
  executionStatus$params$base <- NULL
  executionStatus$params$numberOfCandidates <- NULL
  executionStatus$params$tecnique$name <- NULL

  # loading the database
  base <- loadDataSet(executionStatus$BaseSelected)

  # processing dataset with one-hot-enconding
  start_time_data <- Sys.time()
  dataSetTransformed <- loadDataSetProcessed(executionStatus, base)
  dataSetTransformed$LastUpdate <- NULL
  Sys.sleep(1)
  end_time_data <- Sys.time()
  runtime <- difftime(end_time_data,start_time_data,units = "secs")
  executionStatus$DataTransformationRunTime <- as.double(runtime)

  # execution of methodology algorithm
  start_time <- Sys.time()
  executionStatus <- switch(
    executionStatus$Tecnique,
    ranking={executeRanking(dataSetTransformed, base, definition, executionStatus)},
    kmeans={executeKMeans(dataSetTransformed, base, definition, executionStatus)},
    hclust={executeHClust(dataSetTransformed, base, definition, executionStatus)},
    dbscan={executeDBScan(dataSetTransformed, base, definition, executionStatus)}
  )
  end_time <- Sys.time()
  runtime <- difftime(end_time,start_time,units = "secs")
  executionStatus$ExecutionTecniqueRunTime <- as.double(runtime)

  executionStatus$totalRunTime <- executionStatus$ExecutionTecniqueRunTime + executionStatus$DataTransformationRunTime

  write_excel_csv2(executionStatus, here("R/results", executionStatus$Tecnique, executionStatus$IdExecution, "execution_status.csv"));

  saveNewExecution(executionStatus)

}
