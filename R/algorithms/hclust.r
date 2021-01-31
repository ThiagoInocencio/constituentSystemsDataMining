executeHClust <- function(dataSetTransformed, base, definitions, executionStatus) {

  # loading packages
  loadRankingPackages()

  # loading functions
  source(here("R/functions", "coverage.r"))
  source(here("R/functions", "utils.r"))
  source(here("R/functions", "chart.r"))

  # generating a random folder name
  folderName <- randomName()
  folderName <- paste("H", executionStatus$BaseSelected, folderName, sep="_")
  createPath(here("R/results", executionStatus$Tecnique, folderName))

  executionStatus$IdExecution <- folderName

  # CONSTANTS
  K <- executionStatus$params$tecnique$k
  numberOfAttributes <- length(as.array(names(dataSetTransformed)))

  ms <- dist(dataSetTransformed[,2:numberOfAttributes], method = "euclidean")
  agrupamento <- hclust(ms, method = "ward.D")

  grupos <- cutree(agrupamento, K)

  get_cluster <- head(grupos,1)
  # = 24

  # recuperando os sistemas no mesmo cluster
  systems_candidates <- dataSetTransformed[grupos==get_cluster,]

  pdf(here("R/results", executionStatus$Tecnique, folderName, "hclust.pdf"), width = 250, height = 100)
  plot(agrupamento, main = "Agrupamento Hierárquico Aglomerativo", xlab = "Sistemas", ylab = "Distância")

  rect.hclust(agrupamento, k=K, border="red")

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
