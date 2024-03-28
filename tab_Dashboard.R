#Theis file define the user options. If the dashbaord is extended regarding an additional year or (sub)category, it needs to be implemented here.
#By using conditional panels, the plots are only shown when the according category is selected.
#Within each fluidRow the parameters for the plots are defined. If a plot is too wide or small, this can be changed here.
#Furthermore, the waste plots can be displayed by removing the '#' in front of them. 
#The text for the scenario analysis is also implemented here.

year <- c("","2019", "2020", "2021", "2022", "Gesamt")

category <- c("","Energie", "Mobilität", "Materialeinsatz"#, "Abfallentsorgung"
              )

subcategories_Energie <- c("","Universität", "Campus Altstadt", "Campus Bergheim", "Campus INF")
subcategories_Mobilitaet <- c("","Übersicht", "Dienstreisen", "Pendeln", "Fuhrpark")

tab_Dashboard <- tabPanel("Interaktives Dashboard",
                          sidebarPanel(
                            selectInput("shiny_year", "Zeitraum wählen:", choices = year),
                            selectInput("shiny_category", "Kategorie wählen:", choices = category),
                            #These conditional panels allow the selection of the user to define which subcategories are presented  
                            conditionalPanel(
                              condition = "input.shiny_category == 'Energie'",
                              selectInput(inputId = "subcategories_E", label = "Unterkategorie wählen:", choices = subcategories_Energie)
                            ),
                            conditionalPanel(
                              condition = "input.shiny_category == 'Mobilität'",
                              selectInput(inputId = "subcategories_M",
                                          label = "Unterkategorie wählen:",
                                          choices = subcategories_Mobilitaet)
                            ),
                            conditionalPanel(
                              condition = "input.shiny_year != 'Gesamt' && input.shiny_category == 'Energie'",
                              fluidRow(
                                column(5,
                                       radioButtons("AuswahlDarstellung", "Darstellung",
                                                    c("Jährlich" = "Jahres Darstellung",
                                                      "Monatlich" = "Monatliche Darstellung")))
                              )
                            )
                            
                          ),
                          #In the main panel, the plots are embedded
                          mainPanel(
                            conditionalPanel(
                              condition = "input.shiny_category == 'Energie' & input.AuswahlDarstellung == 'Jahres Darstellung'",
                              fluidRow(
                                column(width=7, plotlyOutput("EnergyYear", height ="auto", width = "auto"),  style = "margin-bottom: 50px;"),
                                column(width=5, plotlyOutput("WaterYear", height ="auto", width = "auto"),  style = "margin-bottom: 50px;")
                              ),
                              fluidRow(
                                column(width=2, plotlyOutput("", height ="750px", width = "800px"),  style = "margin-bottom: 50px;"),
                                column(width=9, plotlyOutput("plotCO2", height ="750px", width = "800px"),  style = "margin-bottom: 50px;")
                                ),
                              fluidRow(
                                column(width=8, plotlyOutput("plotCO2ePerSquareM", height ="auto", width = "auto"),  style = "margin-bottom: 50px;"), 
                                column(width=3, tableOutput("TableYears"))                                                               
                                )
                            ),
                            conditionalPanel(
                              condition = "input.shiny_category == 'Energie' & input.AuswahlDarstellung == 'Monatliche Darstellung'",
                              fluidRow(
                                column(width=6, plotlyOutput("ElectricityMonth", height ="auto", width = "auto")),
                                column(width=6, plotlyOutput("HeatingMonth", height ="auto", width = "auto"))
                              ),
                              fluidRow(
                                
                                column(width=6, plotlyOutput("CoolingMonth", height ="auto", width = "auto")),
                                column(width=6, plotlyOutput("WaterMonth", height ="auto", width = "auto"))
                              ),
                            ),
                            conditionalPanel(
                              condition = "input.shiny_category == 'Mobilität' & input.subcategories_M == 'Dienstreisen'",
                              fluidRow(
                                column(width = 6, plotlyOutput("BusinessTrips", height = "auto", width = "auto")),
                                column(width = 6, plotlyOutput("BusinessTripsCO2", height = "auto", width = "auto"))
                              ),
                              fluidRow(
                                column(width = 6, plotlyOutput("TrainVsFlights", height = "auto", width = "auto")),
                                column(width=6, h4("Erläuterung"),
                                       p("Der Graph 'Szenario: Emissionsvergleich' stellt eine Möglichkeit zur Emissionsreduktion an der Universität dar. Er visualisiert die Emissionen verursacht durch Kurz- und Langstreckenflüge im Vergleich zu den hypothehtischen Emissionen für dieselbe Strecke, wenn sie durch Fernzüge zurückgelegt worden wäre."), 
                                       p("Kurzstreckenflüge sind solche, deren Distanz unter 600 km, Mittelstreckenflüge jene, deren Distanz zwischen 600 und 1000 km liegen."))
                                )
                              ),
                            conditionalPanel(
                              condition = "input.shiny_category == 'Mobilität' & input.subcategories_M == 'Pendeln'",
                              fluidRow(
                                column(width=6, plotlyOutput("CommutePkm", height ="auto", width = "auto")),
                                column(width=6, plotlyOutput("CommuteCO2", height ="auto", width = "auto"))
                              ),
                              fluidRow(
                                column(width=6, plotlyOutput("CommuteCO2PerCapita", height ="auto", width = "auto"))                              
                              ),
                            ),
                            
                            conditionalPanel(
                              condition = "input.shiny_category == 'Mobilität' & input.subcategories_M == 'Fuhrpark'",
                              fluidRow(
                                column(width=6, plotlyOutput("CarPoolPkm", height ="auto", width = "auto")),
                                column(width=6, plotlyOutput("CarPoolCO2", height ="auto", width = "auto"))
                              ),
                              fluidRow(
                                #column(width=6, plotlyOutput("CommuteCO2PerCapita", height ="auto", width = "auto"))                              
                              ),
                            ),
                            conditionalPanel(
                              condition = "input.shiny_category == 'Mobilität' & input.subcategories_M == 'Übersicht'",
                              #fluidRow(
                              #  column(width=12, plotlyOutput("MobilityOverview", height ="auto", width = "auto")),
                                #column(width=6, plotlyOutput("CarPoolCO2", height ="auto", width = "auto"))
                             # ),
                              fluidRow(
                                column(width=12, plotlyOutput("MobilityOverviewCO2", height ="auto", width = "auto"))                              
                              ),
                            ),
                            # conditionalPanel(
                            #   condition = "input.shiny_category == 'Abfallentsorgung'",
                            #   fluidRow(
                            #   column(width=6, plotlyOutput("Waste", height ="auto", width = "auto")),
                            #   column(width=6, plotlyOutput("WasteCO2", height ="auto", width = "auto"))
                            #    ),
                            # ),
                            conditionalPanel(
                              condition = "input.shiny_category == 'Materialeinsatz'",
                              fluidRow(
                                column(width=12, plotlyOutput("Paper", height ="auto", width = "auto")),),
                              fluidRow(
                                column(width=12, plotlyOutput("Material", height ="auto", width = "auto")),
                              ),                              fluidRow(
                                column(width=12, plotlyOutput("MaterialCO2", height ="auto", width = "auto")),
                              ),
                            ),
                            )
)
