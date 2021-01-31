spreadOperationSystems <- function(system) {
  listLan <- strsplit(system$OperationSystems, "/")[[1]]

  # creating new columns
  for(i in listLan) {
    system[paste("OPS.", i, sep = "")] <- 1
  }

  # removing column OperationsSystems
  system$OperationSystems <- NULL

  system
}

spreadAudience <- function(system) {
  listLan <- strsplit(system$Audience, "/")[[1]]

  # creating new columns
  for(i in listLan) {
    system[paste("AUD.", i, sep = "")] <- 2
  }

  # removind column Audience
  system$Audience <- NULL

  system
}

spreadCategory <- function(system) {
  listLan <- strsplit(system$Category, "/")[[1]]

  # creating new columns
  for(i in listLan) {
    system[paste("CAT.", i, sep = "")] <- 2
  }

  # removing column Category
  system$Category <- NULL

  system
}

spreadTranslations <- function(system) {
  listLan <- strsplit(system$Translations, "/")[[1]]

  # creating new columns
  for(i in listLan) {
    system[paste("TRANS.", i, sep = "")] <- 1
  }

  # removing column Translations
  system$Translations <- NULL

  system
}

spreadProgrammingLanguage <- function(system) {
  listLan <- strsplit(system$ProgrammingLan, "/")[[1]]

  # creating new columns
  for(i in listLan) {
    system[paste("PROG.", i, sep = "")] <- 1
  }

  # removing column ProgrammingLan
  system$ProgrammingLan <- NULL

  system
}

spreadUserInterface <- function(system) {
  listLan <- strsplit(system$UserInterface, "/")[[1]]

  # creating new columns
  for(i in listLan) {
    system[paste("UINT.", i, sep = "")] <- 1
  }

  # removing column UserInterface
  system$UserInterface <- NULL

  system
}



spreadColumns <- function(idealSystem) {
  idealSystem <- spreadOperationSystems(idealSystem)
  idealSystem <- spreadAudience(idealSystem)
  idealSystem <- spreadCategory(idealSystem)
  idealSystem <- spreadTranslations(idealSystem)
  idealSystem <- spreadProgrammingLanguage(idealSystem)
  idealSystem <- spreadUserInterface(idealSystem)
  idealSystem
}


processDataBase <-function(idealSystem, base) {

  idealSystem <- spreadColumns(idealSystem)

  constituenColumnNames <- as.array(names(idealSystem))

  # columns names
  operationSystemColumns <- str_detect(constituenColumnNames, regex("OPS.", ignore_case = FALSE))
  audienceColumns <- str_detect(constituenColumnNames, regex("AUD.", ignore_case = FALSE))
  categoryColumns <- str_detect(constituenColumnNames, regex("CAT.", ignore_case = FALSE))
  translationsColumns <- str_detect(constituenColumnNames, regex("TRANS.", ignore_case = FALSE))
  programmingLanguageColumns <- str_detect(constituenColumnNames, regex("PROG.", ignore_case = FALSE))
  userInterfaceColumns <- str_detect(constituenColumnNames, regex("UINT.", ignore_case = FALSE))


  # columns features
  opSystems <- str_remove(constituenColumnNames[operationSystemColumns], "OPS.")
  audiences <- str_remove(constituenColumnNames[audienceColumns], "AUD.")
  categories <- str_remove(constituenColumnNames[categoryColumns], "CAT.")
  translations <- str_remove(constituenColumnNames[translationsColumns], "TRANS.")
  programmingLanguages <- str_remove(constituenColumnNames[programmingLanguageColumns], "PROG.")
  userInterfaces <- str_remove(constituenColumnNames[userInterfaceColumns], "UINT.")
  status <- idealSystem$status
  License <- idealSystem$License
  #LastUpdate <- idealSystem$LastUpdate

  idealSystem$status <- TRUE
  idealSystem$License <- TRUE
  #idealSystem$LastUpdate <- TRUE

  # cloning the idealSystem dataset
  spreadedDT <- idealSystem


  for(i in 2:nrow(base)) {

    # creating a new row for the system
    newElement <- head(spreadedDT, n=1)

    # chaging name
    newElement["SystemName"] <- base[i,"SystemName"]


    if(base[i, "status"] == status) {
      newElement["status"] <- 1
    } else {
      newElement["status"] <- 0
    }

    if(base[i, "License"] == License) {
      newElement["License"] <- 1
    } else {
      newElement["License"] <- 0
    }

    #if(base[i, "LastUpdate"] == LastUpdate) {
    #  newElement["LastUpdate"] <- TRUE
    #} else {
    #  newElement["LastUpdate"] <- FALSE
    #}


    # spreading operation system columns
    newSystemOperationSystems <- strsplit(base[i,"OperationSystems"], "/")[[1]]
    for(op in opSystems) {

      if(is.element(op, newSystemOperationSystems)) {
        newElement[paste("OPS.", op, sep = "")] <- 1
      } else {
        newElement[paste("OPS.", op, sep = "")] <- 0
      }
    }


    # spreading audience columns
    newSystemAudiences <- strsplit(base[i,"Audience"], "/")[[1]]
    for(aud in audiences) {

      if(is.element(aud, newSystemAudiences)) {
        newElement[paste("AUD.", aud, sep = "")] <- 2
      } else {
        newElement[paste("AUD.", aud, sep = "")] <- 0
      }
    }


    # spreading categories columns
    newSystemCategories <- strsplit(base[i,"Category"], "/")[[1]]
    for(categ in categories) {

      if(is.element(categ, newSystemCategories)) {
        newElement[paste("CAT.", categ, sep = "")] <- 2
      } else {
        newElement[paste("CAT.", categ, sep = "")] <- 0
      }
    }


    # spreading translations columns
    newSystemTranslations <- strsplit(base[i,"Translations"], "/")[[1]]
    for(transl in translations) {

      if(is.element(transl, newSystemTranslations)) {
        newElement[paste("TRANS.", transl, sep = "")] <- 1
      } else {
        newElement[paste("TRANS.", transl, sep = "")] <- 0
      }
    }


    # spreading programming languages columns
    newSystemProgrammingLanguages <- strsplit(base[i,"ProgrammingLan"], "/")[[1]]
    for(program in programmingLanguages) {

      if(is.element(program, newSystemProgrammingLanguages)) {
        newElement[paste("PROG.", program, sep = "")] <- 1
      } else {
        newElement[paste("PROG.", program, sep = "")] <- 0
      }
    }


    # spreading user interface columns
    newSystemUserInterfaces <- strsplit(base[i,"UserInterface"], "/")[[1]]
    for(userInter in userInterfaces) {

      if(is.element(userInter, newSystemUserInterfaces)) {
        newElement[paste("UINT.", userInter, sep = "")] <- 1
      } else {
        newElement[paste("UINT.", userInter, sep = "")] <- 0
      }
    }

    # assigning the new element to the spreaded Data frame
    spreadedDT <- rbind(spreadedDT, newElement)

  }

  #View(spreadedDT)

  #for(i in 1:nrow(spreadedDT)) {
  #  for(cname in colnames(spreadedDT)) {
  #    if(cname != "SystemName") {
  #      if(str_detect(cname, regex("AUD.", ignore_case = FALSE)) ||
  #         str_detect(cname, regex("CAT.", ignore_case = FALSE))) {
  #        if(spreadedDT[i,cname] == TRUE)
  #          spreadedDT[i,cname] <- 2
  #        else
  #          spreadedDT[i,cname] <- 0
  #      } else if(spreadedDT[i,cname] == TRUE) {
  #          spreadedDT[i,cname] <- 1
  #      } else {
  #        spreadedDT[i,cname] <- 0
  #      }
  #    }
  #  }
  #}

  #View(spreadedDT)
  spreadedDT

}
