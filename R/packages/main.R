installPackages <- function(){
  install.packages("magrittr")
  install.packages("rjson")
  install.packages("rlang")
  install.packages("vctrs")
  install.packages("tidyjson")
  install.packages("jsonlite")
  install.packages("tidyverse")
  install.packages("stats")
  install.packages("ggplot2")
  install.packages("conflicted")
  install.packages("readxl")
  install.packages("purrrlyr")
  install.packages("dbscan")
}

loadPackages <- function() {
  library(conflicted)
  conflict_prefer("here", "here")
  library(jsonlite)
  library(magrittr)
  library("tidyverse")
  library(tidyverse)
  library(fpc)
  library(cluster)
  library(httr)
  library(rvest)
  library(dplyr)
  library(purrr)
  library(plyr)
  library(magrittr)
  library(stringr)
  #library(xlsx)
  library(Hmisc)
  library(magrittr) # needs to be run every time you start R and want to use %>%
  library(dplyr)    # alternatively, this also loads %>%
  library("readxl")
  library(purrrlyr)
  library(dbscan)
}

loadRankingPackages <- function() {
  library(stats)
  library(ggplot2)
}

