#Within this file, mobility is analysed. There are three parts to it: business trips, commuting and the car pool.
#For each one, it is differentiated between Pkm and CO2e emissions.

#This function is used to create a plot showing all business trips. It gets the mobility dataframe and filters according to the category Dienstreisen.
#If the overview was colected, again the year is on the x-axis, otherwise it is the type of business trip.
plotBusinessTrips <- function(data, shiny_year, shiny_category, subcategories_M) {
  
  if(shiny_category == "Mobilität"){
    art_colors <- c('MIV' = '#B9D3EE', 'Fähre' = 'darkseagreen1', 'Fernbus' = 'paleturquoise3', 'ÖPNV' = 'mistyrose2', 'Regionalverkehr' = 'lightsalmon1', 'Fernzüge' = 'navajowhite', 'Mittelstreckenflug' = 'thistle2', 'Kurzstreckenflug' = 'palevioletred2', 'Langstreckenflug' = 'plum3')
    
    if(subcategories_M == 'Dienstreisen'){

      
      filtered_data <- data[data$Kategorie == "Dienstreisen", ]

      art_levels <- c('MIV', 'Fähre', 'Fernbus', 'ÖPNV', 'Regionalverkehr', 'Fernzüge', 'Kurzstreckenflug', 'Mittelstreckenflug', 'Langstreckenflug')
      
      # Use factor() with specified levels
      filtered_data$Art <- factor(filtered_data$Art, levels = art_levels)
      
      # Specify colors using the correct order of levels
      filtered_data$Art_Color <- art_colors[filtered_data$Art]
      
      
      if(shiny_year == 'Gesamt'){
        gg_plot <- ggplot(filtered_data, aes(x = Jahr, y = Pkm, fill = Art)) +
          geom_bar(stat = "identity")+
          scale_fill_manual(values = art_colors)+
          theme(legend.position = "none")+
          theme(axis.text = element_text(color = "black", size = 10, angle=45))+
          theme(axis.title.x=element_blank())+ 
          ggtitle(paste("Personenkilometer", shiny_year)) +
          ylab("Pkm")+
          scale_y_continuous(labels = label_number(accuracy = 1))
       

      }
      else{
        
        gg_plot <- ggplot(filtered_data, aes(x = Art, y = Pkm, fill = Art)) +
          geom_bar(stat = "identity")+
          scale_fill_manual(values = art_colors)+
          theme(legend.position = "none")+
          theme(axis.text = element_text(color = "black", size = 10, angle=45))+
          theme(axis.title.x=element_blank())+ 
          ggtitle(paste("Personenkilometer", shiny_year)) +
          ylab("Pkm")+
          scale_y_continuous(labels = label_number(accuracy = 1))
        
      }
    }
  }
}



#Here, the same is done but the emissions are calculated. Therefore, it needs also the emission dataframe.
plotBusinessTripsCO2 <- function(data, shiny_year, shiny_category, subcategories_M, emissions) {
  
  if(shiny_category == "Mobilität"){
    
    if(subcategories_M == 'Dienstreisen'){
      
      filtered_data <- data[data$Kategorie == "Dienstreisen", ]
      merged_data <- merge(filtered_data, emissions, by = c('Jahr', 'Art'), all.x = TRUE)
      merged_data$Emission <- merged_data$Pkm * merged_data$Emissionsfaktor
      
      art_levels <- c('MIV', 'Fähre', 'Fernbus', 'ÖPNV', 'Regionalverkehr', 'Fernzüge', 'Kurzstreckenflug', 'Mittelstreckenflug', 'Langstreckenflug')
      
      # Use factor() with specified levels
      merged_data$Art <- factor(merged_data$Art, levels = art_levels)
      
      # Specify colors using the correct order of levels
      art_colors <- c('MIV' = '#B9D3EE', 'Fähre' = 'darkseagreen1', 'Fernbus' = 'paleturquoise3', 'ÖPNV' = 'mistyrose2', 'Regionalverkehr' = 'lightsalmon1', 'Fernzüge' = 'navajowhite', 'Mittelstreckenflug' = 'thistle2', 'Kurzstreckenflug' = 'palevioletred2', 'Langstreckenflug' = 'plum3')
      
      # Create a new column for colors
      merged_data$Art_Color <- art_colors[merged_data$Art]
      
      
      if(shiny_year == 'Gesamt'){
        gg_plot <- ggplot(merged_data, aes(x = Jahr, y = Emission, fill = Art)) +
          geom_bar(stat = "identity")+
          scale_fill_manual(values = art_colors)+
          theme(axis.text = element_text(color = "black", size = 10, angle=45))+
          theme(axis.title.x=element_blank())+ 
          ggtitle(paste("Emissionen", shiny_year)) +
          ylab("Tonnen CO2 e")+
          scale_y_continuous(labels = label_number(accuracy = 1))
        
      }
      else{
      
        gg_plot <- ggplot(merged_data, aes(x = Art, y = Emission, fill = Art)) +
          geom_bar(stat = "identity")+
          scale_fill_manual(values = art_colors)+
          theme(legend.position = "none")+
          theme(axis.text = element_text(color = "black", size = 10, angle=45))+
          theme(axis.title.x=element_blank())+ 
          ggtitle(paste("Emissionen", shiny_year)) +
          ylab("Tonnen CO2 e")+
          scale_y_continuous(labels = label_number(accuracy = 1))
      }
      plot <- ggplotly(gg_plot)
      
      return(plot)
    }
  }
}

#This plot shows the scenario that the business trips taken by train would have been taken by train. Therefore it needs the distance and the emissions.
plotTrainVsPlane <- function(data, shiny_year, shiny_category, subcategories_M, emissions) {
  
  if (shiny_category == "Mobilität") {
    
    if (subcategories_M == 'Dienstreisen') {
      
      filtered_data <- data[data$Kategorie == "Dienstreisen", ]
      
      merged_data <- merge(filtered_data, emissions, by = c('Jahr', 'Art'), all.x = TRUE)
      merged_data$Emission <- merged_data$Pkm * merged_data$Emissionsfaktor
      merged_data$Emission <- as.numeric(merged_data$Emission)
      merged_data$Pkm <- as.numeric(merged_data$Pkm)
  
      
      
      if(shiny_year == 'Gesamt'){
        new_row <- data.frame(
          Art = "IfTrain",
          Emissionsfaktor = as.numeric(0.000031),
          Pkm = sum(merged_data$Pkm[merged_data$Art %in% c("Kurzstreckenflug", "Mittelstreckenflug")])
        )
        new_row$Emission <- new_row$Pkm * new_row$Emissionsfaktor
        
        # Create a data frame from emission_values
        emission_values <- data.frame(
          Art = c("IfTrain", "SumFlights"),
          Emission = c(new_row$Emission, sum(merged_data$Emission[merged_data$Art %in% c("Kurzstreckenflug", "Mittelstreckenflug")]))
        )
        
        # Ensure Emission is numeric
        emission_values$Emission <- as.numeric(emission_values$Emission)
        
      }
      
      else{
        new_row <- data.frame(
          Art = "IfTrain",
          Emissionsfaktor = merged_data$Emissionsfaktor[merged_data$Art == "Fernzüge"],
          Pkm = sum(merged_data$Pkm[merged_data$Art %in% c("Kurzstreckenflug", "Mittelstreckenflug")])
        )
        
      new_row$Emission <- new_row$Pkm * new_row$Emissionsfaktor
      
      
      # Create a data frame from emission_values
      emission_values <- data.frame(
        Art = c("IfTrain", "SumFlights"),
        Emission = c(new_row$Emission, sum(merged_data$Emission[merged_data$Art %in% c("Kurzstreckenflug", "Mittelstreckenflug")]))
      )
      
      # Ensure Emission is numeric
      emission_values$Emission <- as.numeric(emission_values$Emission)

     }
      
      emission_values$Art <- factor(emission_values$Art, levels = c("SumFlights", "IfTrain"))
      
      # Define colors for each level
      colors <- c("SumFlights" = "thistle2", "IfTrain" = "navajowhite")
      
      
      # Create ggplot object
      gg_plot <- ggplot(emission_values, aes(x = Art, y = Emission, fill = Art)) +
        geom_bar(stat = "identity") +
        ggtitle(paste("Szenario: Emissionsvergleich", shiny_year)) +
        ylab("Tonnen CO2 e")+
        scale_x_discrete(labels = c("IfTrain" = "Selbe Strecke Fernzüge", "SumFlights" = "Summe Kurz- u. Mittelstreckenflüge")) +
        theme_minimal()+
        scale_fill_manual(values = colors) +  # Assign specific colors
        theme(axis.title.x=element_blank())+ 
        theme(legend.position = "none")
      #conflicts_prefer(plotly::layout)
      # Convert ggplot to Plotly
      plot <- ggplotly(gg_plot)
      
      return(plot)
      
    }
  }
}

#As done for the business trips, a plot for commuting is created here.
plotCommutePkm <- function(data, shiny_year, shiny_category, subcategories_M) {
  
  if (shiny_category == "Mobilität") {
    
    if (subcategories_M == 'Pendeln') {
      
      filtered_data <- data[data$Kategorie %in% c("Pendeln Beschäftigte", "Pendeln Studierende"), ]
      
      art_levels <- c('MIV', 'ÖPNV', 'ÖPFV')
      
      # Use factor() with specified levels
      filtered_data$Art <- factor(filtered_data$Art, levels = art_levels)
      
      # Specify colors using the correct order of levels
      art_colors <- c('MIV' = '#B9D3EE',  'ÖPNV' = 'mistyrose2', 'ÖPFV' = 'navajowhite')
      
      # Create a new column for colors
      filtered_data$Art_Color <- art_colors[as.character(filtered_data$Art)]
      
    
      if(shiny_year == 'Gesamt'){
        
        category_colors  <- c("#CDCDC1", "#CD8C95")
      
        plot <- ggplot(filtered_data, aes(x = Jahr, y = Pkm, fill = Kategorie)) +
          geom_bar(stat = "identity") +
          ggtitle(paste(" Personenkilometer Pendeln", shiny_year)) +
          ylab("Pkm")+
          theme_minimal()+
          theme(legend.position = "none")+
          scale_fill_manual(values = category_colors) +
          scale_y_continuous(labels = label_number(accuracy = 1))
        
      }
      
      else{
      
      
      plot <- ggplot(filtered_data, aes(x = Kategorie, y = Pkm, fill = Art)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = art_colors) +
        ggtitle(paste("Personenkilometer Pendeln", shiny_year)) +
        ylab("Pkm")+
        theme_minimal()+
        scale_y_continuous(labels = label_number(accuracy = 1))+
        theme(legend.position = "none")
      
      }
      
      plotly_output_e <- ggplotly(plot)
      return(plotly_output_e)
    }
  }
}

#Again, the emissions are calculated. Therefore, the emission dataframe is necessary again.
plotCommuteCO2 <- function(data, shiny_year, shiny_category, subcategories_M, emissions) {
  
  if(shiny_category == "Mobilität"){
    
    if (subcategories_M == 'Pendeln') {
      
      filtered_data <- data[data$Kategorie %in% c("Pendeln Beschäftigte", "Pendeln Studierende"), ]
      
      merged_data <- merge(filtered_data, emissions, by = c('Jahr', 'Art'), all.x = TRUE)
      merged_data$Emission <- merged_data$Pkm * merged_data$Emissionsfaktor
      merged_data$Emission <- as.numeric(merged_data$Emission)
      merged_data$Pkm <- as.numeric(merged_data$Pkm)
      
      
      art_levels <- c('MIV', 'ÖPNV', 'ÖPFV')
      
      # Use factor() with specified levels
      merged_data$Art <- factor(merged_data$Art, levels = art_levels)
      
      # Specify colors using the correct order of levels
      art_colors <- c('MIV' = '#B9D3EE', 'ÖPNV' = 'mistyrose2', 'ÖPFV' = 'navajowhite' )
      
      # Create a new column for colors
      merged_data$Art_Color <- art_colors[merged_data$Art]
      if(shiny_year == 'Gesamt'){
        
        category_colors  <- c("#CDCDC1", "#CD8C95")
        
        plot <- ggplot(merged_data, aes(x = Jahr, y = Emission, fill = Kategorie)) +
          geom_bar(stat = "identity") +
          ggtitle(paste("Emissionen Pendeln", shiny_year)) +
          ylab("Tonnen CO2 e")+
          theme_minimal()+
          scale_fill_manual(values = category_colors) +
          scale_y_continuous(labels = label_number(accuracy = 1))
        
      }
      
      else{
      
      
      
      plot <- ggplot(merged_data, aes(x = Kategorie, y = Emission, fill = Art)) +
        geom_bar(stat = "identity") +
        ggtitle(paste("Emissionen Pendeln", shiny_year)) +
        ylab("Tonnen CO2 e")+
        theme_minimal()+
        scale_fill_manual(values = art_colors) +
        scale_y_continuous(labels = label_number(accuracy = 1))
      
      }
      
      plotly_output_e <- ggplotly(plot)
      return(plotly_output_e)
    }
    
  }
}

#This plot now has an additional information, namely the co2e per capita for students and working university members.
#Therefore, the amount of students and workers are necessary, stored in the column Personenzahl.
plotCommuteCO2PerCapita <- function(data, shiny_year, shiny_category, subcategories_M, emissions) {
  
  if (shiny_category == "Mobilität") {
    
    if (subcategories_M == 'Pendeln') {
      
      filtered_data <- data[data$Kategorie %in% c("Pendeln Beschäftigte", "Pendeln Studierende"), ]
      
      merged_data <- merge(filtered_data, emissions, by = c('Jahr', 'Art'), all.x = TRUE)
      merged_data$Emission <- merged_data$Pkm * merged_data$Emissionsfaktor
      merged_data$Emission <- as.numeric(merged_data$Emission)
      merged_data$Pkm <- as.numeric(merged_data$Pkm)
      
      
      sum_emission <- merged_data %>%
        filter(Kategorie == 'Pendeln Beschäftigte') %>%
        group_by(Jahr) %>%
        summarize(Emission = sum(Emission),
                  Personenzahl = first(Personenzahl))
      
      # Create a new row with the calculated values
      new_row <- data.frame(
        Kategorie = 'Pendeln Beschäftigte',
        Art = 'Beschäftigte',
        Pkm = 0,
        Emissionsfaktor = 0,
        Personenzahl = sum_emission$Personenzahl,
        Emission = sum_emission$Emission,
        Jahr = sum_emission$Jahr
      )
      
      merged_data <- bind_rows(merged_data, new_row)
      
      sum_emission_stud <- merged_data %>%
        filter(Kategorie == 'Pendeln Studierende') %>%
        group_by(Jahr) %>%
        summarize(Emission = sum(Emission),
                  Personenzahl = first(Personenzahl))  # Assuming 'Personenzahl' is constant for the category
      
      # Create a new row with the calculated values
      new_row_stud <- data.frame(
        Kategorie = 'Pendeln Studierende',
        Art = 'Studierende',
        Pkm = 0,
        Emissionsfaktor = 0,
        Personenzahl = sum_emission_stud$Personenzahl,
        Emission = sum_emission_stud$Emission,
        Jahr = sum_emission$Jahr
        
      )
      merged_data <- bind_rows(merged_data, new_row_stud)
      
      
      filtered_data <- merged_data %>%
        filter(Art %in% c('Studierende', 'Beschäftigte'))
      
      filtered_data$PerCapita <- filtered_data$Emission / filtered_data$Personenzahl
      
      category_colors  <- c("#CDCDC1", "#CD8C95")
      
      if(shiny_year == 'Gesamt'){
        
        plot <- ggplot(filtered_data, aes(x = Jahr, y = PerCapita, fill = Art)) +
          geom_bar(stat = "identity") +
          ggtitle(paste("Pro Kopf Emissionen Pendeln", shiny_year)) +
          ylab("Tonnen CO2 e")+
          theme_minimal()+
          scale_fill_manual(values = category_colors)
        }
      

      
      
      else{
        
        
      # Create the plot using the filtered data
      plot <- ggplot(filtered_data, aes(x = Art, y = PerCapita, fill = Art)) +
        geom_bar(stat = "identity") +
        ggtitle(paste("Pro Kopf Emissionen Pendeln", shiny_year)) +
        ylab("Tonnen CO2 e")+
        theme_minimal() +
        scale_fill_manual(values = category_colors) +
        theme(legend.position = "none") +
        labs(x = paste("Anzahl Beschäftigte:", new_row$Personenzahl, ", Anzahl Studierende:", new_row_stud$Personenzahl))      
        
      }
      
      plotly_output_e <- ggplotly(plot)
      return(plotly_output_e)
      
    }
  }
}


#Here, only the information about the car pool is shown. The workflow as such is very similar, only other types are filtered.

plotCarPoolPkm <- function(data, shiny_year, shiny_category, subcategories_M) {
  
  if (shiny_category == "Mobilität") {
    
    if (subcategories_M == 'Fuhrpark') {
      
      filtered_data <- data[data$Kategorie %in% c("Fuhrpark"), ]
      
      art_levels <- c('Elektroantrieb', 'Benzinantrieb', 'Dieselantrieb')
      
      # Use factor() with specified levels
      filtered_data$Art <- factor(filtered_data$Art, levels = art_levels)
      
      # Specify colors using the correct order of levels
      art_colors <- c('Elektroantrieb' = '#CDBA96', 'Benzinantrieb' = 'tan3', 'Dieselantrieb' ='#CD6839')
      
      if(shiny_year == 'Gesamt'){
        plot <- ggplot(filtered_data, aes(x = Jahr, y = Pkm, fill = Art)) +
          geom_bar(stat = "identity") +
          ggtitle(paste("Personenkilometer Fuhrpark", shiny_year)) +
          ylab("Pkm")+
          theme_minimal() +
          scale_fill_manual(values = art_colors) +
          theme(legend.position = "none") 

        
      }
      
      else{
      
      plot <- ggplot(filtered_data, aes(x = Art, y = Pkm, fill = Art)) +
        geom_bar(stat = "identity") +
        ggtitle(paste("Personenkilometer Fuhrpark", shiny_year)) +
        ylab("Pkm")+
        theme_minimal() +
        scale_fill_manual(values = art_colors) +
        theme(legend.position = "none") 
      
    }
      # Adjust y-axis labels if needed
      #plot = plot + scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = "M"))
      
      plotly_output_e <- ggplotly(plot)
      return(plotly_output_e)
    }
  }
}

#Accordingly is done to calculated the emissions caused by the car pool.
plotCarPoolCO2 <- function(data, shiny_year, shiny_category, subcategories_M, emissions) {
  
  if(shiny_category == "Mobilität"){
    
    if (subcategories_M == 'Fuhrpark') {
      
      filtered_data <- data[data$Kategorie %in% c("Fuhrpark"), ]
      
      merged_data <- merge(filtered_data, emissions, by = c('Jahr', 'Art'), all.x = TRUE)
      merged_data$Emission <- merged_data$Pkm * merged_data$Emissionsfaktor
      merged_data$Emission <- as.numeric(merged_data$Emission)
      merged_data$Pkm <- as.numeric(merged_data$Pkm)
      
      
      art_levels <- c('Elektroantrieb', 'Benzinantrieb', 'Dieselantrieb')
      
      # Use factor() with specified levels
      merged_data$Art <- factor(merged_data$Art, levels = art_levels)
      
      # Specify colors using the correct order of levels
      art_colors <- c('Elektroantrieb' = '#CDBA96', 'Benzinantrieb' = 'tan3', 'Dieselantrieb' ='#CD6839')
      
      # Create a new column for colors
      merged_data$Art_Color <- art_colors[merged_data$Art]
      if(shiny_year == 'Gesamt'){
        plot <- ggplot(merged_data, aes(x = Jahr, y = Emission, fill = Art)) +
          geom_bar(stat = "identity") +
          ggtitle(paste("Emissionen Fuhrpark", shiny_year)) +
          ylab("Tonnen CO2 e")+
          theme_minimal() +
          scale_fill_manual(values = art_colors) 
        
      }
      
      else{
      
      plot <- ggplot(merged_data, aes(x = Art, y = Emission, fill = Art)) +
        geom_bar(stat = "identity") +
        ggtitle(paste("Emissionen Fuhrpark", shiny_year)) +
        ylab("Tonnen CO2 e")+
        theme_minimal() +
        scale_fill_manual(values = art_colors) +
        theme(legend.position = "none") 
      }
      plotly_output_e <- ggplotly(plot)
      return(plotly_output_e)
    }
    
  }
}

#Here, the overview plot for mobility is created. All types of mobility are kept and portrayed either for all years or for each year individually.
plotMobilityOverview <- function(data, shiny_year, shiny_category, subcategories_M, emissions) {
  
  if(shiny_category == "Mobilität"){
    
    if (subcategories_M == 'Übersicht') {
      
      merged_data <- merge(filtered_data, emissions, by = c('Jahr', 'Art'), all.x = TRUE)
      merged_data$Emission <- merged_data$Pkm * merged_data$Emissionsfaktor
      merged_data$Emission <- as.numeric(merged_data$Emission)
      merged_data$Pkm <- as.numeric(merged_data$Pkm)
      
      if(shiny_year == 'Gesamt'){
        plot <- gg_plot <- ggplot(merged_data, aes(x = Jahr, y = Pkm, fill = Art)) +
          geom_bar(stat = "identity") +
          ylab("Personenkilometer")+
          theme_minimal()
        }
      
      else{
      
      plot <- ggplot(merged_data, aes(x = Kategorie, y = Pkm, fill = Art)) +
        geom_bar(stat = "identity") +
        #scale_fill_manual(values = art_colors) +
        labs(x = "Kategorie", y = "Pkm")+
        theme(legend.position = "none")
      
      plot = plot + scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = "M"))
      }
      
      plotly_output_e <- ggplotly(plot)
      return(plotly_output_e)
    }
    
  }
}

#The same is done here but with the emissions caused by mobility.
plotMobilityOverviewCO2 <- function(data, shiny_year, shiny_category, subcategories_M, emissions) {
  
  if(shiny_category == "Mobilität"){
    
    if (subcategories_M == 'Übersicht') {
      
      merged_data <- merge(data, emissions, by = c('Jahr', 'Art'), all.x = TRUE)
      merged_data$Emission <- merged_data$Pkm * merged_data$Emissionsfaktor
      merged_data$Emission <- as.numeric(merged_data$Emission)
      merged_data$Pkm <- as.numeric(merged_data$Pkm)
      
      # Assign individual colors to each category
      category_colors <- c("lightblue", "darkolivegreen3", "#CDCDC1", "#CD8C95")
      
      
      if(shiny_year == 'Gesamt'){
        plot <- gg_plot <- ggplot(merged_data, aes(x = Jahr, y = Emission, fill = Kategorie)) +
          geom_bar(stat = "identity") +
          ggtitle(paste("Emissionen durch Dienstreisen", shiny_year)) +
          ylab("Tonnen CO2 e")+
          theme_minimal()+
          scale_fill_manual(values = category_colors) +
          scale_y_continuous(labels = label_number(accuracy = 1))
        
      }
      
      else{
        plot <- gg_plot <- ggplot(merged_data, aes(x = Kategorie, y = Emission, fill = Kategorie)) +
          geom_bar(stat = "identity") +
          ggtitle(paste("Emissionen durch Dienstreisen", shiny_year)) +
          ylab("Tonnen CO2 e")+
          theme_minimal()+
          scale_fill_manual(values = category_colors) +
          scale_y_continuous(labels = label_number(accuracy = 1))+
          theme(legend.position = "none")
        
        
      
      }
      plotly_output_e <- ggplotly(plot)
      return(plotly_output_e)
    }
    
  }
}
