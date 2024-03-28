#This file contains all fuctions necessary to filter the table containing all energy data for the specific energy type. Therefore, it is essential for the EnergyPlots.R file. 

##Function to filter dataframe according to user input. Needs the unfiltered data and the place (either campus, entire university or building number). Gives a dataframe with only the selected place.
filter_EnergyData <- function(unfiltered_data, place){
  filtered_data <- unfiltered_data %>%
    filter(Campus == place | Gebäude == place)
  return(filtered_data)
}

##Function to summarize data according to user input. Only needs unsummarized dataframe, gives summarized dataframe. 
summarize_EnergyData <- function(unsummarized_data) {
  summarized_data <- unsummarized_data %>%
    group_by(Art, Zeitraum, Jahr) %>%
    summarize(Menge = sum(Menge))
  
  return(summarized_data)
}

#Uses the original energy dataframe and filters all water values out. A dataframe then is returned that has all energy data excpt the values for water..
filter_water <- function(whole_data){
  filtered_data <- whole_data %>%
    filter(!(Art %in% c('Wasser', 'Brauchwasser', 'Trinkwasser')))  
  return(filtered_data)
}

#This function gets the original energy dataframe and creates one consisting of only the water data.
filter_Onlywater <- function(whole_data){
  filtered_data <- whole_data %>%
    filter(Art %in% c('Wasser', 'Brauchwasser', 'Trinkwasser'))  
  return(filtered_data)
}


##Function to select only entries related to electricity
electricity_data <- function(selection){
  strom <- filter(selection, Art == "AV Strom" | Art == "SV Strom" | Art == "Strom" )
  return(strom)
}

##Function to select only entries related to heating
heating_data <- function(selection){
  waerme <- filter(selection, Art == "Erdöl" | Art == "Erdgas" | Art == "Fernwärme"| Art == "Wärme")  
  return(waerme)
}

##Function to select only entries related to cooling
cooling_data <- function(selection){
  kaelte <- filter(selection, Art == "Kälte")
  return(kaelte)
}

##Function to select only entries related to water
water_data <- function(selection){
  wasser <-filter(selection, Art %in% c("Wasser", "Brauchwasser", "Trinkwasser"))
  return(wasser)
}