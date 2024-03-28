#This file contains the analysis for the waste data. Again, the emissionfactors and the according table are necessary.
#Within this code, it is distinguished between three types of waste: 'Restmüll', 'Biomüll' and 'Altmüll'.
#Important: if other types are added, emissionfactors need to be added, too. Otherwise it will crash.

#Here, the plot for different types of waste is created. Therefore, the dataframe, year and category are given.
#Since the waste dataframe is comparably easy to manage, the function is very straightforward.
plotWaste<- function(data, shiny_year, shiny_category) {
  
  if(shiny_category == "Abfallentsorgung"){
    #If new waste type is added, it needs to be implemented here.
    art_colors = c("Restmüll" = "snow4", "Altpapier" = "lightskyblue", "Biomüll" = "peachpuff3")
    art_levels = c("Restmüll", "Biomüll", "Altpapier")
    
    data$Art <- factor(data$Art, levels = art_levels)
    data$Art_Color <- art_colors[data$Art]
    
    
    if(shiny_year == 'Gesamt'){
      gg_plot <- ggplot(data, aes(x = Jahr, y = Tonnen, fill = Art)) +
        geom_bar(stat = "identity")+
        scale_fill_manual(values = art_colors)+
        theme(legend.position = "none")+
        ggtitle(paste("Tonnen nach Abfallart", shiny_year)) +
        ylab("Tonnen")+
        scale_y_continuous(labels = label_number(accuracy = 1))
      
    }
    else{
    gg_plot <- ggplot(data, aes(x = Art, y = Tonnen, fill = Art)) +
      geom_bar(stat = "identity")+
      scale_fill_manual(values = art_colors)+
      theme(legend.position = "none")+
      ggtitle(paste("Tonnen nach Abfallart", shiny_year)) +
      ylab("Tonnen")+
      scale_y_continuous(labels = label_number(accuracy = 1))
    
    }
    
  }
}  

#Now again the emissions are calculated. Therefore the emissions are necessary again.  
plotWasteCO2 <- function(data, shiny_year, shiny_category, emissions) {
  
  if(shiny_category == "Abfallentsorgung"){
    
    art_colors = c("Restmüll" = "snow4", "Altpapier" = "lightskyblue", "Biomüll" = "peachpuff3")
    art_levels = c("Restmüll", "Biomüll", "Altpapier")
    data$Art <- factor(data$Art, levels = art_levels)
    data$Art_Color <- art_colors[data$Art]
    
      merged_data <- merge(data, emissions, by = c('Jahr', 'Art'), all.x = TRUE)
      merged_data$Emission <- merged_data$Tonnen * merged_data$Emissionsfaktor
      
      if(shiny_year == 'Gesamt'){
        gg_plot <- ggplot(merged_data, aes(x = Jahr, y = Emission, fill = Art)) +
          geom_bar(stat = "identity")+
          scale_fill_manual(values = art_colors)+
          theme(legend.position = "none")+
          ggtitle(paste("Emissionen nach Abfallart", shiny_year)) +
          ylab("Tonnen CO2 e")+
          scale_y_continuous(labels = label_number(accuracy = 1))
        
      }
      else{
        
        gg_plot <- ggplot(merged_data, aes(x = Art, y = Emission, fill = Art)) +
          geom_bar(stat = "identity")+
          scale_fill_manual(values = art_colors)+
          theme(legend.position = "none")+
          ggtitle(paste("Emissionen nach Abfallart", shiny_year)) +
          ylab("Tonnen CO2 e")+
          scale_y_continuous(labels = label_number(accuracy = 1))
      }
      plot <- ggplotly(gg_plot)
      
      return(plot)
    
  }
}
  
