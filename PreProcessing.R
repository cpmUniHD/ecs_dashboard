#In the Preprocessing file, the imported files are processed in a way that they can be handled consistently.
#For this purpose, the columns are assigned different forms. Everything that shall be used as text is defined as character, numbers as numeric and the year as exactly this.
#This is crucial, because R cannot calculate with characters, therefore it is important to make sure that all columns have the correct form.

#the original dataframe is used an all types of water are filtered. One dataframe is returned that has all water data.
GetWholeWater<- function(data){
    file = data %>%
    group_by(Campus, Einheit, Art, Gebäude) %>%
    filter(Art %in% c('Trinkwasser', 'Brauchwasser', 'Wasser')) %>%
    summarise(Menge = sum(Menge))%>%
    mutate(Zeitraum = 'Jahr')  
  return(file)
}

#The preprocessing of the energy data is a bit more complicated. Firstly, all columns are assigned special types, such as character or numeric. Only the Zeitraum is factorized which is important to show the months in the correct order.
#Also the year is added.
PreprocessEnergy <- function(data, year,german_month_abbreviations) {
  file <- bind_rows(data, GetWholeWater(data)) %>%
    mutate(Zeitraum = factor(Zeitraum, levels = german_month_abbreviations)) %>%
    mutate(Menge = as.numeric(Menge),
           Art = as.character(Art),
           Versorger = as.character(Versorger),
           Adresse = as.character(Adresse),
           Campus = as.character(Campus),
           Zeitraum = as.character(Zeitraum),
           Einheit = as.character(Einheit),
           Gebäude = as.character(Gebäude),
           Jahr = year,
           Baujahr = as.character(Baujahr),
           Nettogrundfläche = as.numeric(Nettogrundfläche),
           Sanierung = as.character(Sanierung))%>%
    replace(is.na(.), 0)
  return(file)
}

#The mobility dataframe is only changed so that all columns have the correct type.
PreprocessMobility <- function(data, year) {
  file <- mutate(data, Jahr = year, Pkm = as.numeric(Pkm), Art = as.character(Art)) %>%
    replace(is.na(.), 0)
  return(file)
}

#same goes for material
PreprocessMaterial <- function(data, year) {
  file <- mutate(data, Jahr = year, Stück = as.numeric(Stück), Art = as.character(Art)) %>%
    replace(is.na(.), 0)
  return(file)
}

#same for emissions
PreprocessEmission <- function(data, year) {
  file <- mutate(data, Jahr = year, Emissionsfaktor = as.numeric(Emissionsfaktor), Art = as.character(Art)) %>%
    replace(is.na(.), 0)
  return(file)
}
#same for waste
PreprocessWaste <- function(data, year) {
  file <- mutate(data, Jahr = year, Tonnen = as.numeric(Tonnen), Art = as.character(Art)) %>%
    replace(is.na(.), 0)
  return(file)
}