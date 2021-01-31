executeDBScan <- function(dataSetTransformed, base, definitions, executionStatus) {

  # loading packages
  loadRankingPackages()
  library(dbscan)

  # loading functions
  source(here("R/functions", "coverage.r"))
  source(here("R/functions", "utils.r"))
  source(here("R/functions", "chart.r"))

  # generating a random folder name
  folderName <- randomName()
  folderName <- paste("D", executionStatus$BaseSelected, folderName, sep="_")
  createPath(here("R/results", executionStatus$Tecnique, folderName))

  executionStatus$IdExecution <- folderName

  # CONSTANTS
  EPS <- executionStatus$params$tecnique$eps
  MIN_PTS <- executionStatus$params$tecnique$MinPts
  numberOfAttributes <- length(as.array(names(dataSetTransformed)))

  # execução do dbscan nos dados
  dbs <- dbscan(dataSetTransformed[,2:numberOfAttributes], eps = EPS, MinPts = MIN_PTS)

  # cluster of ideal system
  get_cluster <- head(dbs$cluster,1)

  # recuperando os sistemas no mesmo cluster
  systems_candidates <- dataSetTransformed[dbs$cluster==get_cluster,]

  executionStatus$AmountOfRecomendedSystems <- nrow(systems_candidates)

  if(nrow(systems_candidates) < executionStatus$amountOfCandidatesRecomendation)
    closest_systems_candidates <- systems_candidates[0:nrow(systems_candidates),]
  else
    closest_systems_candidates <- systems_candidates[0:executionStatus$amountOfCandidatesRecomendation,]

  executionStatus <- calculateCoverageStatus(closest_systems_candidates, executionStatus, folderName)

  executionStatus
}
