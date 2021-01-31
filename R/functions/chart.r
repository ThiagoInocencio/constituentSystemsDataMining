generateHeatMap <- function(closest_systems_candidates, executionStatus, folderName) {


  nSystems <- executionStatus$AmountOfRecomendedSystems

  if(as.integer(executionStatus$amountOfCandidatesRecomendation) < as.integer(executionStatus$AmountOfRecomendedSystems))
    nSystems <- executionStatus$amountOfCandidatesRecomendation


  # heatmaps
  closest_systems_candidates$coveragePercent <- NULL
  closest_systems_candidates$coverage <- NULL

  closest_systems_candidates[closest_systems_candidates == 2] <- 1

  # closest systems
  closest <- closest_systems_candidates[0:nSystems,-1] %>%
    rownames_to_column() %>%
    gather(colname, value, -rowname)


  closest$rowname <- factor(closest$rowname, levels = unique(closest$rowname), ordered=FALSE)


  my_plot_2 <- ggplot(closest, aes(x = rowname, y = colname, fill = value)) +
    geom_tile(aes(fill = value), colour = "red", size = 3) +
    scale_fill_gradient(low="white", high="black") +
    labs(x = "Sistemas", y = "Caracteristicas") +
    theme(axis.text.x=element_text(size=60, angle = 90, hjust = 1),
          axis.text.y=element_text(size=60),
          axis.title=element_text(size=80,face="bold"))

  ggsave(here("R/results", executionStatus$Tecnique, paste(folderName, "/heat_map.png", sep="")),
         my_plot_2, width = 80, height = 40, limitsize = FALSE)

}

generateCoverageMap <- function(coveredSystems, executionStatus, folderName, system_candidates) {

  nSystems <- executionStatus$AmountOfRecomendedSystems

  if(as.integer(executionStatus$amountOfCandidatesRecomendation) < as.integer(executionStatus$AmountOfRecomendedSystems))
    nSystems <- executionStatus$amountOfCandidatesRecomendation

  system_candidates <- tibble::rownames_to_column(system_candidates, "id")
  coveredSystems$SystemName <- paste("(", system_candidates$id, ") ", coveredSystems$SystemName, sep="")


  #ploting line graph of covered systems
  ggplot(data=coveredSystems, aes(x=NumberOfSystems, y=FeaturesCovered, label=SystemName))+
    geom_line()+
    geom_point() +
    geom_point(data=subset(coveredSystems, NewFeature > 0), aes(x=NumberOfSystems, y=FeaturesCovered), colour="red", size = 4)+
    scale_y_continuous(breaks=c(0:executionStatus$AmountOfFeatures))+
    scale_x_continuous(breaks=c(0:nSystems))+
    labs(x = "Quantidade de sistemas", y = "Quantidade de caracteristicas atendidas") +
    geom_text(aes(label=SystemName),hjust=1.2, angle = 90) +
    geom_text(data=subset(coveredSystems, NewFeature > 0),aes(label=SystemName),hjust=1.2, angle = 90, colour = "red") +
    geom_segment(aes(x=0,xend=tail(coveredSystems, n=1)$NumberOfSystems, y=executionStatus$AmountOfFeatures, yend=executionStatus$AmountOfFeatures))


  ggsave(here("R/results", executionStatus$Tecnique, paste(folderName, "/coverageGraph.png", sep="")),
         width = 20, height = 20, units = "cm")
}
