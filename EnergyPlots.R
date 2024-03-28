#EnergyPlots
#Within this file all functions are defined that serve the analysis of the energy data. 
#For this purpose, One big plot containing all energy types measured in MwH is defined and one for water consumption. Also, the pie Chart displyaing the CO2e emissions is created, a plot showing the CO2e per square m for the according building and a table with the year of construction.
#In the later part, the plots for the monthly view are defined.Here, electricity, heating, cooling and water all have one inidividual plot. 

source('FunctionsEnergy.R')


plotEnergyYear <- function(data, shiny_year, shiny_category, subcategories_E) {
  validate(
    need((shiny_year != "") && (shiny_category != "") && (subcategories_E != ""),
         "Wählen Sie bitte Zeitraum, Kategorie und Unterkategorie aus.")
  )
  
  validate(
    need(nrow(data) > 0, "")
  )
  
  if (subcategories_E == "Universität") {
    summarized_data <- summarize_EnergyData(data)
    summarized_data <- filter_water(summarized_data)
  } else {
    filtered_data <- filter_EnergyData(data, subcategories_E)
    summarized_data <- summarize_EnergyData(filtered_data)
    summarized_data <- filter_water(summarized_data)
  }
  
  validate(
    need(nrow(summarized_data) > 0, "Für Ihre Auswahl liegen uns zurzeit leider nicht die entsprechenden Daten vor.")
  )
  
  if (shiny_year == "Gesamt") {
    summarized_data$Art <- factor(summarized_data$Art, levels = c('Strom', 'Kälte', 'Wärme', 'Gas', 'Öl'))
    art_colors <- c('Strom' = 'black', 'Kälte' = 'blue', 'Wärme' = 'red', 'Öl' = '#A6739B', 'Gas' = 'lightgreen')
    
    e <- ggplot(summarized_data, aes(x = Jahr, y = Menge, fill = Art)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = art_colors) +
      labs(x = "Energieträger", y = "Menge in MWh")
  } else {
    summarized_data$Art <- factor(summarized_data$Art, levels = c('Strom', 'Kälte', 'Wärme', 'Gas', 'Öl'))
    art_colors <- c('Strom' = 'black', 'Kälte' = 'blue', 'Wärme' = 'red', 'Öl' = '#A6739B', 'Gas' = 'lightgreen')
    
    e <- ggplot(summarized_data, aes(x = Art, y = Menge, fill = Art)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = art_colors) +
      theme(legend.position = 'none')+
      labs(x = "Energieträger", y = "Menge in MWh")
  }
  
  # Convert the ggplot object to plotly
  plotly_output_e <- ggplotly(e)
  return(plotly_output_e)
}


plotWaterYear <- function(data, shiny_year, shiny_category, subcategories_E) {
  validate(
    need((shiny_year != "") && (shiny_category!= "") &&(subcategories_E!= ""),
         "")
  )

  validate(
    need(nrow(data) > 0, "")
  )
  validate(
    need(!(subcategories_E == 'Notstrom 2100' | subcategories_E == 'Notstrom 6205'| subcategories_E == 'Notstrom 6329'| subcategories_E == 'Notstrom 6227'| subcategories_E == 'Notstrom 6270'| subcategories_E == 'Notstrom 6346'), "Für Notstromaggregate gibt es ausschließlich Daten zum Ölverbrauch")
  )
  
  
  if(subcategories_E == "Universität"){
    
    summarized_data <- summarize_EnergyData(data)
    
    summarized_data <- filter_Onlywater(summarized_data)
    
    
  }
  
  else{
    
    filtered_data <- filter_EnergyData(data, subcategories_E)
    
    summarized_data <- summarize_EnergyData(filtered_data)
    
    summarized_data <- filter_Onlywater(summarized_data)
  }
  
  validate(
    need(nrow(summarized_data) > 0, "Für Ihre Auswahl gibt es zurzeit leider nicht die entsprechenden Daten.")
  )
  
  if(shiny_year == "Gesamt"){  
    summarized_data$Art <- factor(summarized_data$Art, levels = c('Brauchwasser', 'Trinkwasser'))
    art_colors <- c('Brauchwasser' = 'lightskyblue2', 'Trinkwasser' = 'lightskyblue4')
    
    water <- ggplot(summarized_data, aes(x = Jahr, y = Menge, fill = Art)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = art_colors) +
      theme(legend.position = 'none')+
      labs(x = "Energieträger", y = "Menge in m3") 
    
  }
  else{
    summarized_data$Art <- factor(summarized_data$Art, levels = c('Brauchwasser', 'Trinkwasser'))
    art_colors <- c('Brauchwasser' = 'lightskyblue2', 'Trinkwasser' = 'lightskyblue4')
    
    water <- ggplot(summarized_data, aes(x = Art, y = Menge, fill = Art)) +
      geom_bar(stat = "identity")+
      scale_fill_manual(values = art_colors) +
      theme(legend.position = 'none')+
      labs(x = "Energieträger", y = "Menge in m3") 
  }
  
  # Convert the ggplot object to plotly
  plotly_output_water <- ggplotly(water)
  
  plotly_output_water
  
}

plotCO2 <- function(data, shiny_year, shiny_category, subcategories_E, emission) {
  
  
  validate(
    need((shiny_year != "") && (shiny_category!= "") &&(subcategories_E!= ""),
         "")
  )
    
    data <- data %>%
      mutate(Art = case_when(
        Art %in% c('Wärme', 'Öl', 'Gas') & Versorger == 'Stadtwerke Heidelberg Energie GmbH' ~ 'Fernwärme (Stadtwerke Heidelberg)',
        Art %in% c('Wärme', 'Öl', 'Gas') & Versorger %in% c('Klinik EnergieVersorgungs-Service mbH', 'E.ON Energy Solutions GmbH', 'Universitätsklinikum Heidelberg', 'Pfalzwerke AG') ~ 'Fernwärme (E.ON)',
        Art %in% c('Wärme', 'Öl', 'Gas')& Versorger == 'Haisch Mineralöle' ~ 'Erdöl',
        Art %in% c('Wärme', 'Öl', 'Gas') & Versorger == 'Energie Südbayern GmbH' ~ 'Erdgas',
        TRUE ~ Art  # if none of the conditions are met, keep the original 'Art' value
      ))
    
    
    
    
    if (subcategories_E == "Universität"){
      
      filtered_data <- data %>%
        filter(Zeitraum == 'Jahr')
      
      
      summarized_data <- filtered_data %>%
        group_by(Art) %>%
        summarize(Menge = sum(Menge))
      
    }
    
    else{
      
      filtered_data <- data %>%
        filter(Campus == subcategories_E | Gebäude == subcategories_E)%>%
        filter(Zeitraum == 'Jahr')
      
      # Group and summarize the data based on 'Art'
      summarized_data <- filtered_data %>%
        group_by(Art) %>%
        summarize(Menge = sum(Menge))
      
    }
    validate(
      need(nrow(summarized_data) > 0, "")
    )
    
    summarized_data <- summarized_data %>%
      mutate(Art = ifelse(Art %in% c('AV Strom', 'SV Strom'), 'Strom', Art)) %>%
      group_by(Art) %>%
      summarize(Menge = sum(Menge), .groups = 'drop')
    
    merged_data <- merge(summarized_data, emission, by = 'Art', all.x = TRUE)
    
    # Check if there are any missing values after the merge
    if (any(is.na(merged_data$Emissionsfaktor))) {
      stop("There are missing values in the 'Emissionsfaktor' column after the merge.")
    }
    
    # Create a new column in 'summarized_data' by multiplying 'Menge' with 'Emissionsfaktor'
    merged_data$Emissions_total <- merged_data$Menge * merged_data$Emissionsfaktor
    

    if(subcategories_E == 'Notstrom 2100' | subcategories_E == 'Notstrom 6205'| subcategories_E == 'Notstrom 6329'| subcategories_E == 'Notstrom 6227'| subcategories_E == 'Notstrom 6270'| subcategories_E == 'Notstrom 6346'){
      
      merged_data = merged_data
      
    }
    
    else{

      
      graue_energie_value <- 0.3333 * sum(merged_data$Emissions_total)
      graue_energie_row <- data.frame(Art = 'Graue Energie', Menge = 0, Emissionsfaktor = 6, Jahr = shiny_year, Emissions_total = graue_energie_value)
      merged_data <- rbind(merged_data, graue_energie_row)
    }
    
    art_levels <- c('Brauchwasser', 'Trinkwasser', 'Strom', 'Wasser', 'Kälte', 'Fernwärme (Stadtwerke Heidelberg)', 'Fernwärme (E.ON)', 'Erdöl', 'Erdgas', 'Graue Energie')
    
    # Use factor() with specified levels
    merged_data$Art <- factor(merged_data$Art, levels = art_levels)
    
    # Specify colors using the correct order of levels
    art_colors <- c('Brauchwasser' = 'lightskyblue2', 'Trinkwasser' = 'lightskyblue4', 'Strom' = 'black', 'Wasser' = 'lightskyblue', 'Kälte' = 'blue', 'Fernwärme (Stadtwerke Heidelberg)' = 'red', 'Fernwärme (E.ON)' = 'orange', 'Erdöl' = '#A6739B', 'Erdgas' = 'lightgreen', 'Graue Energie' = 'gray')
    
    # Create a new column for colors
    merged_data$Art_Color <- art_colors[merged_data$Art]
    
    # Plotly pie chart with correct colors
    CO2 <- plot_ly(merged_data, labels = ~Art, values = ~Emissions_total, type = 'pie',
                   marker = list(colors = ~Art_Color),
                   text = ~paste(Art, ": ", round(Emissions_total, 2)),
                   hoverinfo = 'text')
    
  #}
  
    CO2 <- CO2 %>%
      plotly::layout(
        title = paste("Emissionen in Tonnen CO2 e", subcategories_E, shiny_year),
        legend = list(x = 1, y = 0.5, traceorder = 'normal', orientation = 'v')
      )
  
  
  
  CO2
  
}


tableYearConstruction <- function(data, shiny_year, shiny_category,subcategories_E){
  validate(
    need((shiny_year != "") && (shiny_category!= "") &&(subcategories_E!= ""),
         "")
  )
  
  validate(
    need(nrow(data) > 0, "")
  )
   if(shiny_category == "Energie"){
  if(subcategories_E != "Universität" && subcategories_E != "Campus Bergheim" && subcategories_E != "Campus Altstadt" && subcategories_E != "Campus INF"){
    
    filtered_data <- data %>%
      filter(Gebäude == subcategories_E)
    # Selecting 'Baujahr' and 'Letzte Sanierung' columns
    result_df <- filtered_data %>%
      slice(1) %>%
      mutate(Baujahr = ifelse(Baujahr == 0, 'Wert nicht vorhanden', Baujahr),
             Sanierung = ifelse(Sanierung == 0, 'Wert nicht vorhanden', Sanierung)) %>%
      select("Baujahr", "Sanierung")
    
    return(result_df)
    
    
  }
   }
  
  
} 

plotCO2ePerSquareM <- function(data, shiny_year, shiny_category, subcategories_E, emission) {
  
  validate(
    need((shiny_year != "") && (shiny_category!= "") &&(subcategories_E!= ""),
         "")
  )
  data <- data %>%
    mutate(Art = case_when(
      Art %in% c('Wärme', 'Öl', 'Gas') & Versorger == 'Stadtwerke Heidelberg Energie GmbH' ~ 'Fernwärme (Stadtwerke Heidelberg)',
      Art %in% c('Wärme', 'Öl', 'Gas') & Versorger %in% c('Klinik EnergieVersorgungs-Service mbH', 'E.ON Energy Solutions GmbH', 'Universitätsklinikum Heidelberg', 'Pfalzwerke AG') ~ 'Fernwärme (E.ON)',
      Art %in% c('Wärme', 'Öl', 'Gas')& Versorger == 'Haisch Mineralöle' ~ 'Erdöl',
      Art %in% c('Wärme', 'Öl', 'Gas') & Versorger == 'Energie Südbayern GmbH' ~ 'Erdgas',
      TRUE ~ Art  # if none of the conditions are met, keep the original 'Art' value
    ))
  
  if (shiny_category == "Energie" && shiny_year != 'Gesamt' && subcategories_E != 'Universität' && subcategories_E != 'Campus INF'&& subcategories_E != 'Campus Altstadt' && subcategories_E != 'Campus Bergheim' && subcategories_E != 'Notstrom 2100' && subcategories_E != 'Notstrom 6205' && subcategories_E != 'Notstrom 6270'&& subcategories_E != 'Notstrom 6329'&& subcategories_E != 'Notstrom 6346'&& subcategories_E != 'Notstrom 6227') {
    
      
      filtered_data <- data %>%
        filter(Gebäude == subcategories_E)%>%
        filter(Zeitraum == 'Jahr')

      # Group and summarize the data based on 'Art'
      summarized_data <- filtered_data %>%
        mutate(Art = ifelse(Art %in% c('AV Strom', 'SV Strom'), 'Strom', Art)) %>%
        group_by(Art) %>%
        summarize(Menge = sum(Menge), Nettogrundfläche = first(Nettogrundfläche))
      

    validate(
      need(nrow(summarized_data) > 0, "")
    )

    merged_data <- merge(summarized_data, emission, by = 'Art', all.x = TRUE)
    
    merged_data$Emissions_total <- merged_data$Menge * merged_data$Emissionsfaktor

    graue_energie_value <- 0.3333 * sum(merged_data$Emissions_total)
    graue_energie_row <- data.frame(Art = 'Graue Energie', Menge = 0, Nettogrundfläche = first(merged_data$Nettogrundfläche), Emissionsfaktor = 6, Jahr = shiny_year, Emissions_total = graue_energie_value)
    merged_data <- rbind(merged_data, graue_energie_row)
    

    filtered_data_Uni <- data %>%
      filter(Zeitraum == 'Jahr') %>%
      group_by(Gebäude) %>%
      mutate(Nettogrundfläche = ifelse(row_number() == 1, Nettogrundfläche, 0)) %>%
      ungroup()
    
    summarized_data_uni <- filtered_data_Uni %>%
      mutate(Art = ifelse(Art %in% c('AV Strom', 'SV Strom'), 'Strom', Art)) %>%
      group_by(Art) %>%
      summarize(Menge = sum(Menge),
                Nettogrundfläche = sum(Nettogrundfläche))
    
    merged_data_uni <- merge(summarized_data_uni, emission, by = 'Art', all.x = TRUE)
    merged_data_uni$Emissions_total <- merged_data_uni$Menge * merged_data_uni$Emissionsfaktor
    
    graue_energie_value_uni <- 0.3333 * sum(merged_data_uni$Emissions_total)
    graue_energie_row_uni <- data.frame(Art = 'Graue Energie', Menge = 0, Nettogrundfläche = 0, Emissionsfaktor = 6, Jahr = shiny_year, Emissions_total = graue_energie_value_uni)
    merged_data_uni <- rbind(merged_data_uni, graue_energie_row_uni)
    

    selected_data_building <- merged_data %>%
      summarize(Emissions_total = sum(Emissions_total),
                Nettogrundfläche = first(Nettogrundfläche[Nettogrundfläche > 0])) %>%
      mutate(TonsCO2PerSquare = Emissions_total / Nettogrundfläche)
    
    selected_data_uni <- merged_data_uni %>%
      summarise(Emissions_total = sum(Emissions_total),
                Nettogrundfläche = sum(Nettogrundfläche)) %>%
      mutate(TonsCO2PerSquare = Emissions_total / Nettogrundfläche)
    

      if(selected_data_building$Nettogrundfläche == 0 | (is.na(selected_data_building$Nettogrundfläche))){
        #Should not do anything if this is the case. If tried otherwise, Code doesn't work.
      }

    else{
    # Create the new dataframe
    result_df <- data.frame(
      Site = c('MeanUniversity', subcategories_E),
      TonsCO2PerSquare = c(selected_data_uni$TonsCO2PerSquare, selected_data_building$TonsCO2PerSquare)
    )
    
    result_df$Site <- factor(result_df$Site, levels = c('MeanUniversity', subcategories_E))
    colors <- c('#CD5B45','lightsteelblue3')
    
    # Create a bar plot
    bar_plot <- ggplot(result_df, aes(x = Site, y = TonsCO2PerSquare, fill = Site)) +
      geom_bar(stat = "identity", position = "dodge", color = "black") +
      scale_fill_manual(values = colors) +  # Assign colors
       labs(title = "Vergleich Tonnen CO2 e pro Quadratmeter",
           x = "",
           y = "Tonnen CO2 e pro Quadratmeter") +
      scale_x_discrete(labels = c('MeanUniversity' = 'Universitärer Durchschnitt', subcategories_E)) +
      theme_minimal()+
      theme(legend.position = 'none')+
      labs(x = paste("Die Datenstrukturen zu den jeweiligen Gebäuden ist noch zu vereinheitlichen,
      sodass eine Vergleichbarkeit derzeit nur unter Vorbehalt gegeben ist."))
   
    plotly_output_CO2PerSquare <- ggplotly(bar_plot)
    
  }
  
  
  }
}


plotElectricity <- function(data, shiny_year, shiny_category, subcategories_E) {
  
  validate(
    need((shiny_year != "") && (shiny_category!= "") &&(subcategories_E!= ""),
         "")
  )
  
  validate(
    need(nrow(data) > 0, "")
  )
  
  validate(
    need(!(shiny_year == "Gesamt"), "Ihre Auswahl gibt es nur in der jährlichen Darstellung.")
  ) 
  validate(
    need(!(subcategories_E == "Universität" | subcategories_E == "Campus INF" | subcategories_E == "Campus Altstadt" | subcategories_E == "Campus Bergheim"), "Für Ihre Auswahl liegen uns leider keine monatlichen Daten vor.")
  ) 
  
  filtered_data <- filter_EnergyData(data, subcategories_E) 
  
  summarized_data <- summarize_EnergyData(filtered_data) 
  
  
  validate(
    need(nrow(summarized_data) > 0, "Für Ihre Auswahl liegen uns leider keine monatlichen Daten vor.")
  )
  
  
  strom <- electricity_data(summarized_data)
  validate(
    need(nrow(strom) > 0, "Für Ihre Auswahl liegen uns leider keine monatlichen Daten vor.")
  )
  strom$Zeitraum <- factor(strom$Zeitraum, levels = month.abb.de.din1355.1, ordered = TRUE)
  
  s <- ggplot(strom, aes(x = Zeitraum, y = Menge, fill = Art)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("AV Strom" = "grey55", "SV Strom" = "black", "Strom"="black")) +
    labs(title = paste("Stromverbrauch", subcategories_E, shiny_year),
         x = "Zeitraum",
         y = "Strommenge in MWh")
  
  # Convert the ggplot object to plotly
  ggplotly(s)
  # Convert the ggplot object to plotly
  plotly_output_s <- ggplotly(s)
  
  # Hide plot if empty
  if (nrow(strom) == 0) {
    shinyjs::runjs('$("#plot1").hide();')
  } else {
    shinyjs::runjs('$("#plot1").show();')
  }
  
  plotly_output_s
  
}


plotHeating <- function(data, shiny_year, shiny_category, subcategories_E) {
  
  validate(
    need((shiny_year != "") && (shiny_category!= "") &&(subcategories_E!= ""),
         "")
  )  
  validate(
    need(nrow(data) > 0, "")
  )
  
  validate(
    need(!(shiny_year == "Gesamt"), "")
  )       
  validate(
    need(!(subcategories_E == "Universität" | subcategories_E == "Campus INF" | subcategories_E == "Campus Altstadt" | subcategories_E == "Campus Bergheim"), "")
  ) 
  
  filtered_data <- filter_EnergyData(data, subcategories_E)
  
  
  summarized_data <- summarize_EnergyData(filtered_data)
  
  validate(
    need(nrow(summarized_data) > 0, "")
  )
  
  
  wärme <- heating_data(summarized_data)
  
  validate(
    need(nrow(wärme) > 0, "Für Ihre Auswahl liegen uns leider keine monatlichen Daten vor.")
  )   
  wärme$Zeitraum <- factor(wärme$Zeitraum, levels = month.abb.de.din1355.1, ordered = TRUE)
  
  # Now, create your ggplot
  w <- ggplot(wärme, aes(x = Zeitraum, y = Menge, fill = "Art")) +
    geom_bar(stat = "identity", fill = "red") +
    labs(title = paste("Wärmeverbrauch", subcategories_E, shiny_year),
         x = "Zeitraum",
         y = "Wärme in MWh")
  
  
  
  # Convert the ggplot object to plotly
  plotly_output_w <- ggplotly(w)
  
  # Hide plot if empty
  if (nrow(wärme) == 0) {
    shinyjs::runjs('$("#plot2").hide();')
  } else {
    shinyjs::runjs('$("#plot2").show();')
  }
  
  plotly_output_w
  
}


plotCooling <- function(data, shiny_year, shiny_category, subcategories_E) {
  
  validate(
    need((shiny_year != "") && (shiny_category!= "") &&(subcategories_E!= ""),
         "")
  )

  validate(
    need(nrow(data) > 0, "")
  )
  
  
  validate(
    need(!(shiny_year == "Gesamt"), "")
  )       
  
  validate(
    need(!(subcategories_E == "Universität" | subcategories_E == "Campus INF" | subcategories_E == "Campus Altstadt" | subcategories_E == "Campus Bergheim"), "")
  ) 
  
  
  
  filtered_data <- filter_EnergyData(data, subcategories_E)
  
  summarized_data <- summarize_EnergyData(filtered_data) 
  
  
  validate(
    need(nrow(summarized_data) > 0, "")
  )
  
  kälte <- cooling_data(summarized_data)
  validate(
    need(nrow(kälte) > 0, "Für Ihre Auswahl liegen uns leider keine monatlichen Daten vor.")
  )    
  kälte$Zeitraum <- factor(kälte$Zeitraum, levels = month.abb.de.din1355.1, ordered = TRUE)
  
  # Create the ggplot
  k <- ggplot(kälte, aes(x = Zeitraum, y = Menge, fill = Art)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(title = paste("Kälteverbrauch", subcategories_E, shiny_year),
         x = "Zeitraum",
         y = "Menge in MWh")
  
  
  
  # Convert ggplot to plotly
  plotly_output_k <- ggplotly(k)
  
  # Hide plot if empty
  if (nrow(kälte) == 0) {
    shinyjs::runjs('$("#plot3").hide();')
  } else {
    shinyjs::runjs('$("#plot3").show();')
  }
  
  plotly_output_k
  
  
}

plotWater <- function(data, shiny_year, shiny_category, subcategories_E) {
  
  validate(
    need((shiny_year != "") && (shiny_category!= "") &&(subcategories_E!= ""),
         "")
  )
  validate(
    need(nrow(data) > 0, "")
  )
  
  
  validate(
    need(!(shiny_year == "Gesamt"), "")
  )       
  
  validate(
    need(!(subcategories_E == "Universität" | subcategories_E == "Campus INF" | subcategories_E == "Campus Altstadt" | subcategories_E == "Campus Bergheim"), "")
  ) 
  
  
  
  filtered_data <- filter_EnergyData(data, subcategories_E) 
  
  summarized_data <- summarize_EnergyData(filtered_data)
  
  
  validate(
    need(nrow(summarized_data) > 0, "")
  )
  
  
  
  wasser <- water_data(summarized_data)
  validate(
    need(nrow(wasser) > 0, "Für Ihre Auswahl liegen uns leider keine monatlichen Daten vor.")
  )    
  wasser$Zeitraum <- factor(wasser$Zeitraum, levels = month.abb.de.din1355.1, ordered = TRUE)
  
  
  wa <- ggplot(wasser, aes(x = Zeitraum, y = Menge, fill = Art)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Wasser" = "lightskyblue", "Brauchwasser" = "lightskyblue2", "Trinkwasser" = "lightskyblue4")) +
    labs(title = paste("Wasserverbrauch", subcategories_E, shiny_year),
         x = "Zeitraum",
         y = "Wassermenge in m3")
  
  
  
  # Convert ggplot to plotly
  plotly_output_wa <- ggplotly(wa)
  
  # Hide plot if empty
  if (nrow(wasser) == 0) {
    shinyjs::runjs('$("#plot4").hide();')
  } else {
    shinyjs::runjs('$("#plot4").show();')
  }
  
  plotly_output_wa
  
  
}

