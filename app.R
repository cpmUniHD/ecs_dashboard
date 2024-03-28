#Klimaschutz und Nachhaltigkeit an der Universitaet Heidelberg
#This is the code for a web application to display the CO2 balance of Heidelberg University.
#The basis of this code are the tables stored in folders of the respective year. The strucuture of the tables have to be kept at all times, otherwise this code will not work or errors in visualization will occur.
#This code was created by Zarah Janda from 1.10.2023 until 31.3.2024.

#This file is the app file containing the shiny construct. All other files are imported into this file and if you want to test the dashboard, you need to use the 'Run App' buttom from this file in the upper right corner.
#Therefore, this file depends on all the other smaller files, listed in detail under the 'File Import' part.
#Within this file, only few functions are defined, and those that are only serve the purpose to get the import and process the according documents.
#Broadly speaking consists this file of the user interface (ui) part and the server part. Within the ui part, the title panel is defined and the tab panels are called.
#Within the server part all outputs are called using functions.

# Load R packages: These packages collectively provide a comprehensive set of tools for building interactive Shiny applications, handling dates and times, creating visually appealing plots, and manipulating and visualizing data in R.
library(shiny)
#The shiny package is the core of the Shiny web application framework in R. It allows you to build interactive web applications directly from R.
library(shinythemes)
# Shinythemes provides additional themes and styling options for Shiny applications, allowing you to customize the appearance of your Shiny apps.
#library(lubridate)
# Lubridate is a package for working with dates and times in R. It provides convenient functions for parsing, manipulating, and formatting dates and times.
library(ggplot2)
# ggplot2 is a popular plotting package in R for creating static, dynamic, and interactive visualizations. It is based on the grammar of graphics.
library(tidyverse)
#Tidyverse is a collection of R packages, including dplyr, ggplot2, tidyr, and others, designed to work together seamlessly for data manipulation and visualization.
library(scales)
#Scales is a package that provides tools for scaling and formatting plots in ggplot2. It allows you to customize the appearance of axes, legends, and other plot elements.
#library(dplyr)
library(png)
#The png package provides functions for reading and writing PNG (Portable Network Graphics) images in R. It is needed to load the University logo
library(shinyWidgets)
#shinyWidgets extends Shiny with additional UI components and features, allowing you to enhance the interactivity and appearance of your Shiny applications.
library(plotly)
# Plotly is a versatile plotting library that supports interactive plots. The plotly package in R allows you to create interactive plots with features like zooming and hovering.
#library(tidyr )
library(conflicted)
#The conflicted package helps manage namespace conflicts in R. It can be useful when multiple packages define functions with the same name.
#library(dplyr)
library(shinyjs)
#shinyjs is a package that provides additional JavaScript functions for use in Shiny applications. It allows you to manipulate the DOM (Document Object Model) and add custom JavaScript behavior to your Shiny apps.
library(shinyBS)
#shinyBS is a package that extends Shiny with Bootstrap features. It provides additional Bootstrap components and functions for Shiny applications.
library(datetimeutils)
#popular package for creating interactive tables in Shiny applications. DT allows you to display and interact with tabular data.


#File imports
source('tab_BackgroundInfo.R')
source('tab_ContactInfo.R')
source('tab_Dashboard.R')
source('Imports.R')
source('PreProcessing.R')
source('FunctionsEnergy.R')
source('EnergyPlots.R')
source('MobilityPlots.R')
source('WastePlots.R')
source('MaterialPlots.R')


#This line is specifying a preference for the filter function from the plotly package.
conflicts_prefer(plotly::filter)

#Import of all necessary dataframes
energie_2019 = ImportEnergyData('2019')
energie_2020 = ImportEnergyData('2020')
energie_2021 = ImportEnergyData('2021')
energie_2022 = ImportEnergyData('2022')

mobilitaet_2019 = ImportMobilityData('2019')
mobilitaet_2020 = ImportMobilityData('2020')
mobilitaet_2021 = ImportMobilityData('2021')
mobilitaet_2022 = ImportMobilityData('2022')

material_2019 = ImportMaterialData('2019')
material_2020 = ImportMaterialData('2020')
material_2021 = ImportMaterialData('2021')
material_2022 = ImportMaterialData('2022')

emission_2019 = ImportEmissionFactors('2019')
emission_2020 = ImportEmissionFactors('2020')
emission_2021 = ImportEmissionFactors('2021')
emission_2022 = ImportEmissionFactors('2022')

waste_2019 = ImportWasteData('2019')
waste_2020 = ImportWasteData('2020')
waste_2021 = ImportWasteData('2021')
waste_2022 = ImportWasteData('2022')

#Preproseccing of the energy dataframes
german_month_abbreviations <- c('Jahr', month.abb.de.din1355.1)

energie_2019 <- PreprocessEnergy(energie_2019, 2019, german_month_abbreviations)
energie_2020 <- PreprocessEnergy(energie_2020, 2020, german_month_abbreviations)
energie_2021 <- PreprocessEnergy(energie_2021, 2021, german_month_abbreviations)
energie_2022 <- PreprocessEnergy(energie_2022, 2022, german_month_abbreviations)


#Since I want to show either the Jahr or the months, I create two sub dataframes for each df
## For energie_2019
energie_2019_jahr <- filter(energie_2019, Zeitraum == 'Jahr')
energie_2019_months <- filter(energie_2019, Zeitraum != 'Jahr')
## For energie_2020
energie_2020_jahr <- filter(energie_2020, Zeitraum == 'Jahr')
energie_2020_months <- filter(energie_2020, Zeitraum != 'Jahr')
## For energie_2021
energie_2021_jahr <- filter(energie_2021, Zeitraum == 'Jahr')
energie_2021_months <- filter(energie_2021, Zeitraum != 'Jahr')
## For energie_2022
energie_2022_jahr <- filter(energie_2022, Zeitraum == 'Jahr')
energie_2022_months <- filter(energie_2022, Zeitraum != 'Jahr')


mobilitaet_2019 <- PreprocessMobility(mobilitaet_2019, 2019)
mobilitaet_2020 <- PreprocessMobility(mobilitaet_2020, 2020)
mobilitaet_2021 <- PreprocessMobility(mobilitaet_2021, 2021)
mobilitaet_2022 <- PreprocessMobility(mobilitaet_2022, 2022)

material_2019 <- PreprocessMaterial(material_2019, 2019)
material_2020 <- PreprocessMaterial(material_2020, 2020)
material_2021 <- PreprocessMaterial(material_2021, 2021)
material_2022 <- PreprocessMaterial(material_2022, 2022)

emission_2019 <- PreprocessEmission(emission_2019, 2019)
emission_2020 <- PreprocessEmission(emission_2020, 2020)
emission_2021 <- PreprocessEmission(emission_2021, 2021)
emission_2022 <- PreprocessEmission(emission_2022, 2022)

waste_2019 <- PreprocessWaste(waste_2019, 2019)
waste_2020 <- PreprocessWaste(waste_2020, 2020)
waste_2021 <- PreprocessWaste(waste_2021, 2021)
waste_2022 <- PreprocessWaste(waste_2022, 2022)

# Here, the Shiny Part starts.
##First, the user interface (UI) needs to be defined. This is the left part of the website, where the user can define, at which data he/she wants to look at
ui <- fluidPage(tags$head(tags$style(HTML('* {font-family: "Trade Gothic LT Std"};'))),
  useShinyjs(),
  useShinyjs(),
  useShinyjs(),
  useShinyjs(),
  useShinyjs(),
  theme = shinytheme("journal"),
  titlePanel(
    #Here, the title and the logo is loaded and presented
    div("Energie, Klimaschutz und Nachhaltigkeit an der Universität Heidelberg",
                 img(height = 150, width = 200, src = "blank.png"),
                 img(height = 150, width = 300, src = "logo.png"))),
  #The Navbar Page is everything below the title panel. 
                navbarPage(
                  "",
                  #The first tabPanel is the Dashbaord as such. It consists of the sidebarPanel, where the user can define the information of interest and the main Panel, where the data is presented then
                  tab_Dashboard,
                  tab_BackgroundInfo,
                  tab_ContactInfo)
  ) 

#This function reacts to the selection of the user. According to the year, that was selected, the dataframe for mobility is chosen.
#If a new year is added, it has to be considered here. Take care to also add the new dataframe in the overview.
getMobilityData<- function(selection){
  res <- switch(selection,
                "2019" = mobilitaet_2019,
                "2020" = mobilitaet_2020, 
                "2021" = mobilitaet_2021,  
                "2022" = mobilitaet_2022,
                "Gesamt" = bind_rows(mobilitaet_2019, mobilitaet_2020, mobilitaet_2021, mobilitaet_2022)
  )
  return(res)
}

#Same as for mobility but for material
getMaterialData<- function(selection){
  res <- switch(selection,
                "2019" = material_2019,
                "2020" = material_2020, 
                "2021" = material_2021,  
                "2022" = material_2022,
                "Gesamt" = bind_rows(material_2019, material_2020, material_2021, material_2022)
  )
  return(res)
}

#Same for waste data
getWasteData <- function(selection){
  res <- switch(selection,
                "2019" = waste_2019,
                "2020" = waste_2020,
                "2021" = waste_2021,
                "2022" = waste_2022,
                "Gesamt" = bind_rows(waste_2019, waste_2020, waste_2021, waste_2022)
  )
  
  return(res)
}

#same for emissionfactors
getEmissionDataFrame <- function(selection){
  res <- switch(selection,
                "2019" = emission_2019,
                "2020" = emission_2020,
                "2021" = emission_2021,
                "2022" = emission_2022,
                "Gesamt" = bind_rows(emission_2019, emission_2020, emission_2021, emission_2022)
  )
  
  return(res)
}



#Same for energy but with the difference that a dataframe is chosen in which only the yearly values are stored.
getEnergyDataFrame_year <- function(selection){
  res <- switch(selection,
                "2019" = energie_2019_jahr,
                "2020" = energie_2020_jahr, 
                "2021" = energie_2021_jahr,  
                "2022" = energie_2022_jahr,
                "Gesamt" = bind_rows(energie_2019_jahr, energie_2020_jahr, energie_2021_jahr, energie_2022_jahr)
  )
  return(res)
}

#And here, only the monthly values are chosen. The function as such, however, stays the same.
getEnergyDataFrame_month <- function(selection){
  res <- switch(selection,
                "2019" = energie_2019_months,
                "2020" = energie_2020_months, 
                "2021" = energie_2021_months,  
                "2022" = energie_2022_months,
                "Gesamt" = bind_rows(energie_2019_months, energie_2020_months, energie_2021_months, energie_2022_months)
  )
  return(res)
}


#Here, the server part starts
server <- function(input, output, session) {
  
#This code is needed to show the Gebaeude choices of the according year. Meaning, that if the year 2020 is chosen, the Gebaeude of the table energie_2020 are portrayed and can be selected, and not in general, all Gebaeude  
  shinyjs::useShinyjs()
  observeEvent(input$shiny_year, {
    
    gebaude_choices <- numeric(0)
    if (input$shiny_year == "2019") {
      gebaude_choices <- unique(energie_2019$Gebäude)
    } else if (input$shiny_year == "2020") {
      gebaude_choices <- unique(energie_2020$Gebäude)
    } else if (input$shiny_year == "2021") {
      gebaude_choices <- unique(energie_2021$Gebäude)
    } else if (input$shiny_year == "2022") {
      gebaude_choices <- unique(energie_2022$Gebäude)
    }else if (input$shiny_year == "Gesamt") {
      gebaude_choices <- unique(bind_rows(energie_2019, energie_2020, energie_2021, energie_2022)$Gebäude)
    }  
    updateSelectInput(session, "subcategories_E", choices = c(subcategories_Energie, gebaude_choices))
  })
  
#Now we start with the plots we want to design.
#The first part is for the plots showing the consumptions for the entire year
  
 output$EnergyYear <- renderPlotly({
   selection_data = getEnergyDataFrame_year(input$shiny_year)
   plotEnergyYear(selection_data, input$shiny_year, input$shiny_category, input$subcategories_E)
})



output$WaterYear <- renderPlotly({
 selection_data = getEnergyDataFrame_year(input$shiny_year)
 plotWaterYear(selection_data, input$shiny_year, input$shiny_category, input$subcategories_E)
 
})

output$plotCO2 <- renderPlotly({
  selection_data = getEnergyDataFrame_year(input$shiny_year)
  emission_data = getEmissionDataFrame(input$shiny_year)
  plotCO2(selection_data, input$shiny_year, input$shiny_category, input$subcategories_E, emission_data)
  
})

output$plotCO2ePerSquareM <- renderPlotly({
  selection_data = getEnergyDataFrame_year(input$shiny_year)
  emission_data = getEmissionDataFrame(input$shiny_year)
  plotCO2ePerSquareM(selection_data, input$shiny_year, input$shiny_category, input$subcategories_E, emission_data)
  
})

output$TableYears <- renderTable({
  selection_data = getEnergyDataFrame_year(input$shiny_year)
  tableYearConstruction(selection_data, input$shiny_year, input$shiny_category, input$subcategories_E)
})


output$ElectricityMonth <- renderPlotly({
    selection_data = getEnergyDataFrame_month(input$shiny_year)
    plotElectricity(selection_data, input$shiny_year, input$shiny_category, input$subcategories_E)
    
    
})
  
output$HeatingMonth <- renderPlotly({
  selection_data = getEnergyDataFrame_month(input$shiny_year)
  plotHeating(selection_data, input$shiny_year, input$shiny_category, input$subcategories_E)
  
  
})  

output$CoolingMonth <- renderPlotly({
  selection_data = getEnergyDataFrame_month(input$shiny_year)
  plotCooling(selection_data, input$shiny_year, input$shiny_category, input$subcategories_E)
  
  
}) 
  
output$WaterMonth <- renderPlotly({
  selection_data = getEnergyDataFrame_month(input$shiny_year)
  plotWater(selection_data, input$shiny_year, input$shiny_category, input$subcategories_E)
  
  
}) 
  

output$BusinessTrips <- renderPlotly({
  selection_data <- getMobilityData(input$shiny_year)
  plotBusinessTrips(selection_data, input$shiny_year, input$shiny_category, input$subcategories_M)
  
  
}) 

output$BusinessTripsCO2 <- renderPlotly({
  selection_data <- getMobilityData(input$shiny_year)
  emission_data = getEmissionDataFrame(input$shiny_year)
  plotBusinessTripsCO2(selection_data, input$shiny_year, input$shiny_category, input$subcategories_M, emission_data)
  
  
}) 
  
output$TrainVsFlights <- renderPlotly({
  selection_data <- getMobilityData(input$shiny_year)
  emission_data = getEmissionDataFrame(input$shiny_year)
  plotTrainVsPlane(selection_data, input$shiny_year, input$shiny_category, input$subcategories_M, emission_data)
  
  
})


output$CommutePkm <- renderPlotly({
  selection_data <- getMobilityData(input$shiny_year)
  plotCommutePkm(selection_data, input$shiny_year, input$shiny_category, input$subcategories_M)
  
})  
output$CommuteCO2 <- renderPlotly({
  selection_data <- getMobilityData(input$shiny_year)
  emission_data = getEmissionDataFrame(input$shiny_year)
  plotCommuteCO2(selection_data, input$shiny_year, input$shiny_category, input$subcategories_M, emission_data)
  
  
}) 
output$CommuteCO2PerCapita <- renderPlotly({
  selection_data <- getMobilityData(input$shiny_year)
  emission_data = getEmissionDataFrame(input$shiny_year)
  plotCommuteCO2PerCapita(selection_data, input$shiny_year, input$shiny_category, input$subcategories_M, emission_data)

  
}) 

output$CarPoolPkm <- renderPlotly({
  selection_data <- getMobilityData(input$shiny_year)
  plotCarPoolPkm(selection_data, input$shiny_year, input$shiny_category, input$subcategories_M)
  
})

output$CarPoolCO2 <- renderPlotly({
  selection_data <- getMobilityData(input$shiny_year)
  emission_data = getEmissionDataFrame(input$shiny_year)
  plotCarPoolCO2(selection_data, input$shiny_year, input$shiny_category, input$subcategories_M, emission_data)
  
  
})
output$MobilityOverview <- renderPlotly({
  selection_data <- getMobilityData(input$shiny_year)
  emission_data = getEmissionDataFrame(input$shiny_year)
  plotMobilityOverview(selection_data, input$shiny_year, input$shiny_category, input$subcategories_M, emission_data)
  
  
})
output$MobilityOverviewCO2 <- renderPlotly({
  selection_data <- getMobilityData(input$shiny_year)
  emission_data = getEmissionDataFrame(input$shiny_year)
  plotMobilityOverviewCO2(selection_data, input$shiny_year, input$shiny_category, input$subcategories_M, emission_data)
  
  
})
output$Waste <- renderPlotly({
  selection_data <- getWasteData(input$shiny_year)
  plotWaste(selection_data, input$shiny_year, input$shiny_category)
})

output$WasteCO2 <- renderPlotly({
  selection_data <- getWasteData(input$shiny_year)
  emission_data = getEmissionDataFrame(input$shiny_year)
  plotWasteCO2(selection_data, input$shiny_year, input$shiny_category, emission_data)
})
  
output$Paper <- renderPlotly({
  selection_data <- getMaterialData(input$shiny_year)
  plotPaper(selection_data, input$shiny_year, input$shiny_category)
  
  
})

output$Material <- renderPlotly({
  selection_data <- getMaterialData(input$shiny_year)
  plotMaterial(selection_data, input$shiny_year, input$shiny_category)
  
  
})

output$MaterialCO2 <- renderPlotly({
  selection_data <- getMaterialData(input$shiny_year)
  emission_data = getEmissionDataFrame(input$shiny_year)
  plotMaterialCO2(selection_data, input$shiny_year, input$shiny_category, emission_data)
  
})

}

# Create Shiny object = Shiny app function
shinyApp(ui = ui, server = server)