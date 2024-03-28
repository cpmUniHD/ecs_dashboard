#The Background tab contains solely text. In the string category_Infos the selectable values are defined. By using conditional panels, the according information is only displayed when the category is selected by the user.

category_Infos<- c("Generelles","Energie", "Mobilität", "Materialeinsatz", "Abfallentsorgung")


tab_BackgroundInfo <- tabPanel("Hintergrundinformationen",
         sidebarPanel(
           selectInput("shiny_category_Infos", "Kategorie wählen:", choices = category_Infos)
         ),
         mainPanel(
           conditionalPanel(
             condition = "input.shiny_category_Infos == 'Generelles'",
             h3("Generelle Hintergrundinformationen"),
             p("Die Universität Heidelberg unterstützt die klimapolitischen Aktivitäten des Landes Baden-Württemberg und verfolgt das Ziel, über eine schrittweise Minderung die Netto-Treibhausgasneutralität („Klimaneutralität“) bis zum Jahr 2040 zu erreichen."),
             p("Zu diesem Zweck erstellt die Universität auf jährlicher Basis eine Bilanzierung der durch ihren Betrieb verursachten Treibhausgase, beginnend mit dem Jahr 2019", a("(Link zum Klimaschutzkonzept).", href = "https://www.uni-heidelberg.de/md/zentral/universitaet/beschaeftigte/service/bau/nachhaltigkeit/klimaschutzkonzept_unihd_092023_final_a4.pdf).")),
             p("Dieses interaktive Dashboard wurde erstellt, um Interessierten einen niedrigschwelligen Zugriff auf die wesentlichen Ergebnisse zu ermöglichen."),
             p("Über den Fotoapparat Button kann jede Graphik im png Format heruntergeladen werden. Sollten Fragen oder Probleme auftauchen, finden Sie einige Informationen in den weiteren Abschnitten oder Sie kontaktieren uns."),
             ),
           conditionalPanel(
             condition = "input.shiny_category_Infos == 'Energie'",
             h3("Handlungsfeld Energieversorgung"),
             p("Die Kategorie Energie umfasst die Strom-, Kälte-, Wärme- und Wasserversorgung sowie die “Graue Energie”, welche für die Erstellung der universitär genutzten Gebäude aufgewendet wurde."),
             p("Die Energiedaten werden jährlich von der Universität erhoben. Da die Datenlage nicht gleich über alle Gebäude verteilt ist und es noch Datenlücken gibt, soll die Darstellung zunächst vorallem einen ersten Eindruck vermitteln."),
             p("Im Folgenden sind weitere Informationen und Bilanzgrenzen für die Modellierung der THG-Bilanz für diese erläutert."),
             h3("Strom"),
             p("Die Universität bezieht seit 2017 zu 100 % zertifizierten Ökostrom, die bisher gekauften Herkunftsnachweise lassen jedoch eine Reinvestition, der der Kundengelder in den Ausbau der erneuerbaren Energieinfrastruktur durch den Anbieter nicht erkennen. Folglich wird die Berechnung der CO2e-Emissionen bzgl. des universitären Stromverbrauchs der jeweils jährliche bundesdeutsche Strommix zugrunde gelegt."),
            
             h3("Wärme"),
             p("Der Großteil der universitär genutzten Gebäude wird über ein Fern- bzw. Nahwärmenetz versorgt. Einzel- und Randstandorte werden vereinzelt über Nahwärme, Erdgas und Heizöl versorgt."),

             h3("Kälte"),
             p("Fernkälte wird nur am Standort Neuenheimer Feld bezogen, welche ebenso wie Wärme vor Ort im Blockheizkraftwerk erzeugt bzw. in einem zentralen Kältespeicher vorgehalten wird."),
             
             h3("Wasser"),
             p("Wasserverbrauch verursacht CO2e-Emissionen durch die Wassergewinnungsprozesse und vor allem durch die Behandlung des Abwassers in Kläranlagen. Hierbei wird zwischen Frischwasser, Brauchwasser und Abwasser unterschieden."),
             p("Je nach Standort erfolgt die Warmwasserbereitstellung über dezentrale, elektrische Warmwasserbereiter oder das zentrale Warmwassernetz eines Gebäudes."),
            
             h3("Graue Energie"),
             p("Die Emissionen, die mit der Herstellung von Baumaterialien und Gebäuden verbunden sind, werden als „graue Emissionen“ bezeichnet. Der im Zuge von Baumaßnahmen entstehende CO2e-Ausstoß ist sehr komplex zu bilanzieren und verlangt genaue Kenntnis hinsichtlich der Baudurchführung und des verwendeten Materials. Folglich können insbesondere für Bestandsgebäude nur grobe Abschätzungen getroffen werden."),
             p("Auf Basis der Abschätzung des Bundesamts für Bauwesen und Raumordnung werden in Deutschland 100 Mio. t CO2e durch vorgelagerte Lieferketten bei der Herstellung, Errichtung und Modernisierung von Wohn- und Nichtwohngebäuden sowie durch direkte Emissionen der Bauwirtschaft verursacht. Diese freigesetzten Emissionen entsprechen ca. 25% der gesamten gebäudebezogenen Emissionen (graue Energie), die übrigen 75% entfallen auf den Gebäudebetrieb."),
              ),
           conditionalPanel(
             condition = "input.shiny_category_Infos == 'Mobilität'",
             h3("Pendelverhalten"),
             p("Das Pendelverhalten beruht Großteils auf individuellen Entscheidungen von Einzelpersonen, zu welchen keine repräsentativen Untersuchungsergebnisse vorliegen bzw. lediglich modellhafte Annahmen auf Grundlage der Entfernung des Arbeits-/Studienorts zum Wohnort getroffen werden können."),
            h3("Dienstreisen"),
             p("Unter Dienstreisen werden alle durch die Reisekostenabrechnung erfassten Vorgänge von Beschäftigten und Studierenden zusammengefasst. "),
             ),
           conditionalPanel(
             condition = "input.shiny_category_Infos == 'Materialeinsatz'",
             h3("Forschungsgeräte"),
             p("Die Herstellung von wissenschaftlichen Geräten ist mit entsprechenden Produktionskosten sowie einem ökologischen Fußabdruck verbunden. Den jährlich beschafften Forschungsgeräten lassen sich aufgrund unzureichender Daten bisher keine Emissionen zuordnen. Perspektivisch soll die Datenerfassung im Rahmen der Beschaffung ausgebaut werden, sodass die CO2e-Emissionen ermittelt werden können."),
             
             h3("Betriebsmaterialien"),
             p("Die Universität Heidelberg kauft jährlich zahlreiche Möbelstücke, elektronische Geräte u.ä. sowie diverse Verbrauchsmaterialien für den Betrieb von Forschung und Lehre."),
             p("Wie bei den Forschungsgeräten können diesen Materialien nach aktueller Datenlage keine Emissionen zugeordnet werden."),
             p("Perspektivisch soll die Datenerfassung auch für diesen Bereich erweitert werden."),

           ),
           conditionalPanel(
             condition = "input.shiny_category_Infos == 'Abfallentsorgung'",
             h3("Datenauswertung"),
             p("Im Bereich Abfallentsorgung war die Datenauswertung nur für die Abfallart Restmüll durchführbar. Ursache sind fehlende Daten zu den Entsorgungsprozessen sowie zu den Kompensationswerten von Abfallarten, welche qualitativ wiederverwertet werden."),
             p("Die Emissionen des Restmülls werden ausschließlich dem Emissionsfaktor der thermischen Behandlung zugeordnet."),
            )
           
         )
)