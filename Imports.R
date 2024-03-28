#This file consists of all imports done for the dashbaord. If new tables are added, a new function needs to be implemented here.
#However, this is not true for the addition of a new year. In this case the import works just well. The tables have to have the same structure and names as the ones from the years before.

column_classes <- c(Menge="numeric")

#This function imports the energy dataframe. Therefore, it has always to be called in the same way except the originating year.
#The function gets the yera from the user selection and uses it to import the csv file that has the according year in its name.
ImportEnergyData <- function(year){
  filepath = paste('./',year,'/Energie_',year,'.csv',sep="")
  file = read.csv(filepath, header=TRUE, sep = ';', colClasses=column_classes, dec=",")
  return(file)
}

#Same goes here for material
ImportMaterialData <- function(year){
  filepath = paste('./',year,'/Materialien_',year,'.csv',sep="")
  file = read.csv(filepath, header=TRUE, sep = ';', colClasses=column_classes, dec=",")
  return(file)
}

#Same for mobility
ImportMobilityData <- function(year){
  filepath = paste('./',year,'/Mobilitaet_',year,'.csv',sep="")
  file = read.csv(filepath, header=TRUE, sep = ';', colClasses=column_classes, dec=",")
  return(file)
}

#Same for material
ImportEmissionFactors <- function(year){
  filepath = paste('./',year,'/Emissionsfaktoren_',year,'.csv',sep="")
  file = read.csv(filepath, header=TRUE, sep = ';', colClasses=column_classes, dec=",")
  return(file)
}

#Same for waste
ImportWasteData <- function(year){
  filepath = paste('./',year,'/Abfall_',year,'.csv',sep="")
  file = read.csv(filepath, header=TRUE, sep = ';', colClasses=column_classes, dec=",")
  return(file)
}