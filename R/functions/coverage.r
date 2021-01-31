amountSystemsCoverage <- function(closest_systems_candidates) {

  dt <- head(closest_systems_candidates, n=1)
  #dt$SystemName = NULL
  dt[1, "FeaturesCovered"] <- 0
  dt[1, "NumberOfSystems"] <- 0
  dt[1, "NewFeature"] <- 0
  dt[1, "TotalFeatures"] <- length(names(closest_systems_candidates)) - 1
  dt[dt == 1] <- 0
  dt[dt == 2] <- 0

  dt[1, "SystemName"] <- ""

  columNames <- names(closest_systems_candidates)

  #newTableEnty <- tail(dt, n=1)

  for(i in 2:nrow(closest_systems_candidates)) {

    newTableEnty <- tail(dt, n=1)

    cont_new_features <- 0
    newTableEnty["NewFeature"] <- 0

    for(col in columNames) {

      if(col != "SystemName") {
        if(!is.na(closest_systems_candidates[i, col]) && closest_systems_candidates[i, col] > 0 && newTableEnty[col] < 1) {
          newTableEnty[col] <-  newTableEnty[col] + 1
          cont_new_features <- cont_new_features + 1
          newTableEnty["NewFeature"] <- 1
        }
      }
    }

    newTableEnty["NumberOfSystems"] <- newTableEnty["NumberOfSystems"] + 1
    newTableEnty["FeaturesCovered"] <- newTableEnty["FeaturesCovered"] + cont_new_features
    newTableEnty["SystemName"] <- closest_systems_candidates[i, "SystemName"]

    dt <- rbind(dt, newTableEnty)

  }

  dt

}

getCoveredFeatures <- function(cv) {

  c_names <- names(cv)
  featuresCovered <- vector()
  lastRow <- tail(cv, n=1)

  for(col in c_names) {
    if(lastRow[col] > 0 && col != "SystemName" && col != "FeaturesCovered" && col != "NumberOfSystems" && col != "TotalFeatures" && col != "NewFeature") {
      featuresCovered <- append(featuresCovered, col)
    }
  }

  featuresCovered

}

getFeaturesNotCovered <- function(cv) {

  c_names <- names(cv)
  featuresNotCovered <- vector()
  lastRow <- tail(cv, n=1)

  for(col in c_names) {
    if(lastRow[col] < 1 && col != "NewFeature") {
      featuresNotCovered <- append(featuresNotCovered, col)
    }
  }

  featuresNotCovered

}

getAmountOfNecessarySystems <- function(cv, numberOfAllFeatures) {

  amountOfSystems <- -1

  for(i in 2:nrow(cv)) {
    if(cv[i, "FeaturesCovered"] >= numberOfAllFeatures && amountOfSystems < 0) {
      amountOfSystems <- cv[i, "NumberOfSystems"]
      break;
    }
  }

  amountOfSystems


}

calculateSystemsCoverage <- function (data) {

  data$coverage <- 0
  data$coveragePercent <- 0

  columnsNames <- as.array(names(data))

  totalCoverage <- length(columnsNames) - 3;



  for(i in 1:nrow(data)) {

    contCoverage <- 0

    for(col in columnsNames) {

      if(col != "SystemName" && col != "coverage") {
        if(data[i, col] > 0) {
          contCoverage <- contCoverage + 1
        }
      }
    }


    data[i,"coverage"] <- contCoverage
    data[i,"coveragePercent"] <- (contCoverage * 100) / totalCoverage

  }


  data

}

calculateCoverageStatus <- function(closest_systems_candidates, executionStatus, folderName) {


  converage <- calculateSystemsCoverage(closest_systems_candidates)
  converage <- converage[,c("SystemName",  "coverage", "coveragePercent")]

  converage <- converage[rev(order(converage$coverage)),]

  closest_systems_candidates <- closest_systems_candidates[match(converage$SystemName, closest_systems_candidates$SystemName),]


  coveredSystems <- amountSystemsCoverage(closest_systems_candidates)

  amountOfFeatures <- length(coveredSystems) - 5
  executionStatus$AmountOfFeatures <- amountOfFeatures


  # checking covered features
  featuresCoverred <- getCoveredFeatures(coveredSystems)
  cat(length(featuresCoverred), "features covered:", featuresCoverred)
  executionStatus$AmountOfCoveredFeatures <- length(featuresCoverred)
  executionStatus$FeaturesCovered <- paste(featuresCoverred,collapse=",")
  executionStatus$PercentOfCoveredFeatures <- (length(featuresCoverred) * 1 / amountOfFeatures)


  cat("\n\n")

  # checking not covered features
  featuresNotCovered <- getFeaturesNotCovered(coveredSystems)
  cat(length(featuresNotCovered), "features not covered:", featuresNotCovered)
  executionStatus$AmountOfNotCoveredFeatures <- length(featuresNotCovered)
  executionStatus$FeaturesNotCovered <- paste(featuresNotCovered,collapse=",")
  executionStatus$PercentOfNotCoveredFeatures <- (length(featuresNotCovered) * 1 / amountOfFeatures)


  if(!is.null(featuresNotCovered))
    executionStatus$AllFeaturesWereCovered <- FALSE
  else
    executionStatus$AllFeaturesWereCovered <- TRUE

  cat("\n\n")

  # checking amount of necessary systems
  numberOfNeededSystems <- getAmountOfNecessarySystems(coveredSystems, amountOfFeatures)
  if(numberOfNeededSystems < 0) {numberOfNeededSystems <- executionStatus$amountOfCandidatesRecomendation
  cat("more than", numberOfNeededSystems, " systems are needed to comprises all features")
  } else {
    cat(numberOfNeededSystems, " systems are needed to comprises all features")
  }
  executionStatus$AmountOfNeededSystems <- numberOfNeededSystems



  # save
  write_excel_csv2(coveredSystems, here("R/results", executionStatus$Tecnique, paste(folderName, "/coverage.csv", sep="")));
  write_excel_csv2(converage, here("R/results", executionStatus$Tecnique, paste(folderName, "/coverage_by_system.csv", sep="")));

  # covered map
  generateCoverageMap(coveredSystems, executionStatus, folderName, closest_systems_candidates)

  # generate HeatMap
  generateHeatMap(closest_systems_candidates, executionStatus, folderName)

  executionStatus

}
