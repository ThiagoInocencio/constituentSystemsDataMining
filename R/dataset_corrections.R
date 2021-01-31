# install.packages("magrittr")
# install.packages("dplyr")
# install.packages("mlr")
# install.packages("corrplot")
# install.packages("caret")
# install.packages("stringr")
# install.packages("rlist")

#library(magrittr)
library(dplyr)
library(mlr)
library(corrplot)
library(caret)
library(pca3d)
library(ggbiplot)
library(stringr)

library(rlist)

sortCharString <- function(char) {
  vec <- strsplit(char, "/")[[1]]
  vec <- unique(vec)
  vec <- unlist(vec, use.names=FALSE)
  sorted <- list.sort(vec)
  newStr <- paste(sorted, collapse = "/")
  newStr
}

fixProgrammingLan <- function(char) {
  listLan <- strsplit(char, "/")[[1]]

  pLanguages <- list("c++", "php", "javascript", "java", "jsp", "unix-shell", "tcl", "perl", "perl", "python", "oberon", "pascal", "object-pascal", "lazarus", "common-lisp", "lisp","c", "basic", "assembly", "delphi-kylix", "haskell", "standard-ml", "r", "free-pascal", "fortran", "apl", "visual-basic-.net", "algol-68", "c#", "ada",  "lua", "matlab", "visual-basic", "visual-basic-for-application-(vba)")

  setA <- unique(pLanguages)
  setB <- unique(listLan)

  intersection <- intersect(setA, setB)

  if(length(intersection) == 0) {
    return(NA)
  } else {

    listLan <-  unlist(intersection, use.names=FALSE)

    newProgralen <- paste(listLan, collapse = "/")

    newProgralen

    return (newProgralen)
  }

}

data <- read.csv('R/datasets/dataset.csv', header = TRUE, stringsAsFactors = FALSE)

newData <- data

for(i in 1:nrow(newData)) {
  row <- newData[i,]
  newData[i,]$Category <- sortCharString(row$Category)
  newData[i,]$OperationSystems <- sortCharString(row$OperationSystems)
  newData[i,]$Audience <- sortCharString(row$Audience)

  translations <- sortCharString(row$Translations)

  if(!is.na(translations)) newData[i,]$Translations <- translations
  else newData[i,]$Translations <- tolower(translations)

  newData[i,]$OperationSystems <- sortCharString(row$OperationSystems)
  newData[i,]$ProgrammingLan <- sortCharString(row$ProgrammingLan)
  newData[i,]$UserInterface <- sortCharString(row$UserInterface)
  newData[i,]$License <- sortCharString(row$License)
}

for(i in 1:nrow(newData)) {
  row <- newData[i,]
  newData[i,]$ProgrammingLan <- fixProgrammingLan(row$ProgrammingLan)
  # do stuff with row
}

newData$Language  <- NULL

write.csv(newData,'R/datasets/dataset.csv', row.names = FALSE)
