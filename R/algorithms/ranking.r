executeRanking <- function(dataSetTransformed, base, definitions, executionStatus) {

   # loading packages
   loadRankingPackages()

   # loading functions
   source(here("R/functions", "coverage.r"))
   source(here("R/functions", "utils.r"))
   source(here("R/functions", "chart.r"))

   # generating a random folder name
   folderName <- randomName()
   folderName <- paste("R", executionStatus$BaseSelected, folderName, sep="_")
   createPath(here("R/results", executionStatus$Tecnique, folderName))

   executionStatus$IdExecution <- folderName

   dist <- dist(dataSetTransformed, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)

   dist_from_constituent <- SliceExtract_dist(dist, 1)

   closest_systems_candidates <- order(dist_from_constituent)

   closest_systems_candidates <- dataSetTransformed[closest_systems_candidates[0:executionStatus$amountOfCandidatesRecomendation],]

   executionStatus$AmountOfRecomendedSystems <- nrow(closest_systems_candidates)

   # coverage status
   executionStatus <- calculateCoverageStatus(closest_systems_candidates, executionStatus, folderName)

   executionStatus
}



