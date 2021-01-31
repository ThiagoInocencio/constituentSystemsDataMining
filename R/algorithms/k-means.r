executeKMeans <- function (dataSetTransformed, base, definitions, executionStatus) {

  # loading packages
  loadRankingPackages()

  # loading functions
  source(here("R/functions", "coverage.r"))
  source(here("R/functions", "utils.r"))
  source(here("R/functions", "chart.r"))

  # generating a random folder name
  folderName <- randomName()
  folderName <- paste("K", executionStatus$BaseSelected, folderName, sep="_")
  createPath(here("R/results", executionStatus$Tecnique, folderName))

  executionStatus$IdExecution <- folderName


  # CONSTANTS
  K <- executionStatus$params$tecnique$k
  numberOfAttributes <- length(as.array(names(dataSetTransformed)))

  # execução do k-means
  test <- kmeans(dataSetTransformed[,2:numberOfAttributes], centers = K)

  # cluster of ideal system
  get_cluster <- head(test$cluster,1)

  # recuperando os sistemas no mesmo cluster
  systems_candidates <- dataSetTransformed[test$cluster==get_cluster,]


  #mostrando os sistemas candidatos
  #View(systems_candidates)

  #plotcluster(systems_candidates[,2:numberOfAttributes], test$cluster)

  #clusplot(systems_candidates[,2:numberOfAttributes], test$cluster, color=TRUE, shade=TRUE,  labels=4, lines=0)

  executionStatus$AmountOfRecomendedSystems <- nrow(systems_candidates)

  if(nrow(systems_candidates) < executionStatus$amountOfCandidatesRecomendation)
  closest_systems_candidates <- systems_candidates[0:nrow(systems_candidates),]
  else
    closest_systems_candidates <- systems_candidates[0:executionStatus$amountOfCandidatesRecomendation,]

  # coverage status
  executionStatus <- calculateCoverageStatus(closest_systems_candidates, executionStatus, folderName)

  # generate HeatMap
  generateHeatMap(closest_systems_candidates, executionStatus, folderName)

  executionStatus
}
