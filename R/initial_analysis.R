data <- read.csv('R/datasets/dataset.csv', header = TRUE, stringsAsFactors = FALSE)

# a. Quantidade de atributos que o dataset possui:
ncol(data)

# b. Quais atributos:
paste(names(data),collapse=", ")

# c. Quantidade de Exemplos que o dataset possui:
nrow(data)

# d. Sumário dos exemplos:
summary(data)


# Total de valores ausentes no dataset (Considerando que a base contém valores ausentes com NA e também com string vazia):
missingStatus <- as.numeric(sum(is.na(data$status)))

missingSystemName <- as.numeric(sum(is.na(data$SystemName)))
missingOperationSystems <- as.numeric(sum(is.na(data$OperationSystems)))
missingAudience <- as.numeric(sum(is.na(data$Audience)))
missingCategory <- as.numeric(sum(is.na(data$Category)))
missingLanguage <- as.numeric(sum(is.na(data$Language)))
missingTranslations <- as.numeric(sum(is.na(data$Translations)))
missingProgrammingLan <- as.numeric(sum(is.na(data$ProgrammingLan)))
missingUserInterface <- as.numeric(sum(is.na(data$UserInterface)))
missingLastUpdate <- as.numeric(sum(is.na(data$LastUpdate)))
missingLicense <- as.numeric(sum(is.na(data$License)))
setLen <- dim(data)[1]

missing.data <- data.frame(
  columnName = c("SystemName", "Status", "OperationsSystems", "Audience", "Category",  "Translations", "ProgrammingLan", "UserInterface", "LastUpdate", "License"),
  missingValues = c(missingSystemName, missingStatus, missingOperationSystems, missingAudience, missingCategory,  missingTranslations, missingProgrammingLan, missingUserInterface, missingLastUpdate, missingLicense),
  percentMissingValues = c(
    (missingSystemName * 100) / setLen,
    (missingStatus * 100) / setLen,
    (missingOperationSystems * 100) / setLen,
    (missingAudience * 100) / setLen,
    (missingCategory * 100) / setLen,
    (missingTranslations * 100) / setLen,
    (missingProgrammingLan * 100) / setLen,
    (missingUserInterface * 100) / setLen,
    (missingLastUpdate * 100) / setLen,
    (missingLicense * 100) / setLen
  )
)

missing.data

par(mar = c(7, 4, 1, 1) + 2)
barplot(missing.data$percentMissingValues, names.arg=missing.data$columnName, las=2,ylab="Percentage of Missing values", col = rainbow(11))

# a. Identificadores (valor único p cada linha)<br/>
duplicateObjects <- unique(data)
n_duplicateObjects <- as.numeric(nrow(data)) - as.numeric(nrow(duplicateObjects))
n_duplicateObjects

duplicatedObject <- data[duplicated(data),]

uniqueSystemName <- length(unique(data$SystemName))
uniqueStatus <- length(unique(data$status))
uniqueOperationSystems <- length(unique(data$OperationSystems))
uniqueAudience <- length(unique(data$Audience))
uniqueCategory <- length(unique(data$Category))
#uniqueLanguage <- length(unique(data$Language))
uniqueTranslations <- length(unique(data$Translations))
uniqueProgrammingLan <- length(unique(data$ProgrammingLan))
uniqueUserInterface <- length(unique(data$UserInterface))
uniqueLastUpdate <- length(unique(data$LastUpdate))
uniqueLicense <- length(unique(data$License))


uniqueValues <- data.frame(
  columnName = c("SystemName", "Status", "OperationsSystems", "Audience", "Category",  "Translations", "ProgrammingLan", "UserInterface", "LastUpdate", "License"),
  uniqueValues = c(uniqueSystemName, uniqueStatus, uniqueOperationSystems, uniqueAudience, uniqueCategory, uniqueTranslations, uniqueProgrammingLan, uniqueUserInterface, uniqueLastUpdate, uniqueLicense
))

# c. Atributos q tem muitos valores ausentes (removida, ou modelada)
newData <- data[!is.na(data$status),]
newData <- newData[!is.na(newData$SystemName),]
newData <- newData[!is.na(newData$OperationSystems),]
newData <- newData[!is.na(newData$Audience),]
newData <- newData[!is.na(newData$Category),]
newData <- newData[!is.na(newData$Translations),]
newData <- newData[!is.na(newData$ProgrammingLan),]
newData <- newData[!is.na(newData$UserInterface),]
newData <- newData[!is.na(newData$LastUpdate),]
newData <- newData[!is.na(newData$License),]

# quantidade de sistemas sem valores ausentes
nrow(newData)












