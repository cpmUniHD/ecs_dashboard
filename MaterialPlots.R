#This file is used to analyse the material data, differentiating between paper and IT-equipment.
#If other categories are introduced, for example research equipement, this needs to be added here. Otherwise, it will not be displayed.

#This plot takes the dataframe, the year the user selected and the category so it can create a plot for the paper consumption.
#Therefore, the dataframe is filtered in a way that only toilet and different types of copy paper are kept. If the overview was selected a plot is created with the years on the x-axis. Otherwise, the paper type is portrayed on the x-axis.
plotPaper <- function(data, shiny_year, shiny_category){
  validate(
    need((shiny_year != "") && (shiny_category != ""),
         "Wählen Sie bitte Zeitraum, Kategorie und Unterkategorie aus.")
  )
  
  filtered_data <- data %>%
    filter(Art %in% c("Kopierpapier (Eco Label)", "Toilettenpapier", "Kopierpapier (Keine Angabe)", "Kopierpapier (Blauer Engel)"),
           Stück > 0)  # Add a filter for Stück > 0
  
  # Define custom colors for each Art
  custom_colors <- c("Toilettenpapier" = "#EED8AE", 
                     "Kopierpapier (Eco Label)" = "darkseagreen2", 
                     "Kopierpapier (Keine Angabe)" = "mistyrose3", 
                     "Kopierpapier (Blauer Engel)" = "palevioletred2")
  
if(shiny_year == 'Gesamt'){
    ggplot(filtered_data, aes(x = Jahr, y = Stück, fill = Art)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = custom_colors) +  # Use custom colors
    scale_y_continuous(labels = label_number(accuracy = 1))+
    labs(title = "", x = "", y = "Blattzahl")+
    ggtitle(paste("Papierkonsum ", shiny_year))
    
  
} 
  
  else{ 
    ggplot(filtered_data, aes(x = Art, y = Stück, fill = Art)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = custom_colors) +  # Use custom colors
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
    scale_y_continuous(labels = label_number(accuracy = 1))+
    labs(title = "", x = "", y = "Blattzahl")+
    ggtitle(paste("Papierkonsum ", shiny_year))
    
  }
  
  
}

#The same approach is used for other material which are basically it-devices. Therefore, all paper types all excluded. Again, if the overview was selected, the year is shown on the x-axis. Otherwise, its the type of it-device.

plotMaterial <- function(data, shiny_year, shiny_category){
  validate(
    need((shiny_year != "") && (shiny_category != ""),
         "")
  )
  
  excluded_data <- data %>%
    filter(!(Art %in% c("Kopierpapier (Eco Label)", "Toilettenpapier", "Kopierpapier (Keine Angabe)", "Kopierpapier (Blauer Engel)")),
           Stück > 0)  # Add a filter for Stück > 0
  
  # # Define custom colors for each Art
  custom_colors <- c("Monitore" = "#AEEEEE",
                     "Handys" = "rosybrown1",
                     "Tablets" = "#87CEFF",
                     "Notebooks" = "#CDC9C9",
                     "Desktop-Pcs" = "#CDB5CD",
                     "Festplatten" = "#EE6A50",
                     "Servers" = "lightsteelblue1")
  
  if(shiny_year == 'Gesamt'){
    ggplot(excluded_data, aes(x = Jahr, y = Stück, fill = Art)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = custom_colors) +  # Use custom colors
      scale_y_continuous(labels = label_number(accuracy = 1))+
      labs(title = "", x = "", y = "Anzahl")+
      ggtitle(paste("Materialkonsum ", shiny_year))
    
    
  }
  
  
  else{
  # Create a barplot using ggplot2 with custom colors
  ggplot(excluded_data, aes(x = Art, y = Stück, fill = Art)) +
    geom_bar(stat = "identity") +
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
    scale_fill_manual(values = custom_colors) +  # Use custom colors
    labs(title = "", x = "", y = "Anzahl")+
    ggtitle(paste("Materialkonsum ", shiny_year))
    
  }
}

#This functions creates the pie chart showing all emissions according to the material type. For this purpose, it additionally needs the emission dataframe. Then, it calculated the emissions
#It is important to always have the exact same names in the emission and the amount dataframe. Otherwise the calculation will not work.
plotMaterialCO2<- function(data, shiny_year, shiny_category, emissions){
  merged_data <- merge(data, emissions, by = c('Jahr', 'Art'), all.x = TRUE)
  merged_data$Emission <- merged_data$Stück * merged_data$Emissionsfaktor
  
  art_levels <- c("Desktop-Pcs", "Festplatten", "Handys", "Kopierpapier (Blauer Engel)", "Kopierpapier (Eco Label)", "Kopierpapier (Keine Angabe)", "Monitore", "Notebooks", "Servers", "Tablets", "Toilettenpapier")  
  art_colors <- c("Desktop-Pcs" = "#CDB5CD",
                  "Festplatten" = "#EE6A50",
                  "Handys" = "rosybrown1",
                  "Kopierpapier (Blauer Engel)" = "palevioletred2",
                  "Kopierpapier (Eco Label)" = "darkseagreen2",
                  "Kopierpapier (Keine Angabe)" = "mistyrose3",
                  "Monitore" = "#AEEEEE",
                  "Notebooks" = "#CDC9C9",
                  "Servers" = "lightsteelblue1",
                  "Tablets" = "#87CEFF",
                  "Toilettenpapier" = "#EED8AE")
  
  # Use factor() with specified levels
  merged_data$Art <- factor(merged_data$Art, levels = art_levels)  
  
  merged_data$Art_Color <- art_colors[merged_data$Art]
  
  CO2 <- plot_ly(merged_data, labels = ~Art, values = ~Emission, type = 'pie',
                 marker = list(colors = ~Art_Color),
                 text = ~paste(Art, ": ", round(Emission, 2)),
                 hoverinfo = 'text')
  
  CO2 <- CO2 %>%
    plotly::layout(title = paste("Emissionen in Tonnen CO2 e", shiny_year),
                   showlegend = FALSE)  
}

