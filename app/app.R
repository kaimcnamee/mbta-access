# Milestone 8

library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(shinythemes)


# Read in ACS demographic data, MBTA lines, MBTA station nodes, and MBTA station
# icons.

combined <- readRDS("data.RDS")
mbta <- readRDS("mbta.RDS")
mbta_nodes <- readRDS("mbta_nodes.RDS")
mbtaIcon <- makeIcon("mbta-icon.png", iconWidth = 8, iconHeight = 8)

# Subset counties data by racial/ethnic group to separate Leaflet layers.

counties_map <- combined %>% 
    arrange(tractid) %>% 
    filter(distance <= 3218) %>% 
    mutate(rank = percent_rank(med_income) * 100)

counties_white <- counties_map %>% 
    filter(variable == "White")

counties_black <- counties_map %>%
    filter(variable == "Black")

counties_asian <- counties_map %>%
    filter(variable == "Asian")

counties_hispanic <- counties_map %>%
    filter(variable == "Hispanic")

# Define a Leaflet color palette to code each demographic's population
# proportion in each tract.

pal <- colorNumeric(palette = "plasma", domain = counties_map$percent, n = 100)

# Define UI for application

ui <- 
    
    # Create a page for the Leaflet map. Use HTML and CSS to style the elements
    # of the page. leafletOutput("map1") generates the map, while
    # plotOut("tract") generates the demographic bar chart that appears in the
    # information panel.
    
    navbarPage(
    "Racial Geography of the MBTA",
    theme = shinytheme("yeti"),
    tabPanel("Map", 
             div(class = "map", tags$head(includeCSS("styles.css")),
                leafletOutput("map1", width = "100%", height = "100%"),
                absolutePanel(id = "info", class = "panel panel-default", 
                              style = "color: white", bottom = 25, left = 50, 
                              width = 300, fixed = TRUE, draggable = FALSE, 
                              height = "auto", 
                              h3("Race on the T"),
                              p("This map illustrates the racial geography of 
                                Boston's public transit system."),
                              p("Select a base layer with the control panel on 
                                the right to view demographic breakdowns by 
                                census tract."),
                              p("Click on any census tract, transit line, or 
                                station to learn more."),
                              p("Data includes all tracts within 3218 meters 
                                (2 miles) from the nearest MBTA station."),
                              br(),
                            plotOutput("tract", height = 250)
                           )
             )
    ),
    
    # Add a model page to explain the model and findings from the analysis.
    # fluidPage columns create a flexible, dynamic 3 column layout, where the
    # blank outer columns pad the content in the middle column. Display the
    # plots and tables as images and style with CSS.
    
    tabPanel("Model", 
             fluidPage(
                 column(2),
                 column(8,
                        br(),
                        h3("Analyzing the data"),
                        p("To model the relationship between census tract 
                          demographics and MBTA accessibility, I combined the 
                          2018 American Community Survey data with the MBTA’s 
                          geographic data. Using the coordinates of census tract 
                          centroids (the population-weighted center of a census 
                          tract), I created a table containing every centroid 
                          and the distances to each MBTA station. The final data 
                          set I used for my analysis contained demographic 
                          information, and distance to the nearest MBTA station, 
                          for every census tract. I then created a new variable 
                          to indicate the most prevalent racial/ethnic group 
                          present in each census tract."),
                        p("To model the data and make predictions, I used a 
                          linear regression model. With distance as the outcome 
                          variable, I defined the predictors as the percentage 
                          of the total population made up of white, Black, 
                          Asian, and Hispanic residents, and the majority group. 
                          For each analysis, the effects of the predictor 
                          variables on distance were subtle — this is indicated 
                          by the overlapping distributions and confidence 
                          intervals in the graphics and tables below."),
                        p("I conducted my analysis on two sets of the data — one 
                          filtered to include tracts up to 1609 meters (1 mile), 
                          and one filtered to include tracts up to 3218 meters 
                          (2 miles). This is to limit the analysis to 
                          Massachusetts residents who are most likely to use the 
                          MBTA."),
                        p("Starting with tracts up to 1609m from the nearest 
                          station, I used the model to create a posterior 
                          probability distribution to predict the expected 
                          distance to the nearest station by tracts’ majority 
                          demographic:"),
                        br(),
                        img(src = "pe_1.png", style = "width: 100%"),
                        br(),
                        p("I also created a posterior probability distribution 
                        to predict individual census tracts’ distance to the 
                        nearest station:"),
                        br(),
                        img(src = "pp_1.png", style = "width: 100%"),
                        br(),
                        p("Expanding the analysis to include tracts up to 3218m 
                          changes the predicted expected distances:"),
                        br(),
                        img(src = "pe_2.png", style = "width: 100%"),
                        br(),
                        p("And the predictions for individual tracts:"),
                        br(),
                        img(src = "pp_2.png", style = "width: 100%"),
                        h3("Tables"),
                        p("See the below table for the results of the regression 
                          for tracts up to 1609m from the nearest station. The 
                          results indicate that the proportion of Black 
                          residents in census tract has a greater impact on 
                          distance than the proportion of other groups. The 
                          median distance for majority Hispanic tracts is 
                          greater than all other groups. For all cases, the 95% 
                          confidence intervals indicate a substantial amount of 
                          overlap in results, suggesting the differences between
                          groups are subtle."),
                        br(),
                        div(class = "flex", 
                            img(src = "tbl_1.png", style = "width: 40%")),
                        br(),
                        p("See the below table for the results of the regression 
                          for tracts up to 1318m from the nearest station. The
                          relative results of the regression are similar: the 
                          median impact on distance is greatest for the 
                          proportion of Black residents, but the confidence 
                          intervals indicate even greater overlap. Here, the 
                          median distance for mostly Asian tracts is higher 
                          than all other groups, but the negative affect of the 
                          proportion of Asian residents is much stronger than 
                          other groups."),
                        br(),
                        div(class = "flex", 
                            img(src = "tbl_2.png", style = "width: 40%")),
                        br(),
                        p("The differing regression results show that the degree 
                          to which the data is filtered can have a significant 
                          impact on the distribution of results: by including 
                          tracts farther from MBTA stations, the analysis picks 
                          up more homogenous suburbs like Newton and 
                          Somerville."),
                        br(),
                        h3("Limitations"),
                        p("This analysis is limited in two substantial ways. 
                        First census tracts could be an imprecise way to 
                        calculate the distance to the nearest MBTA station — a 
                        more accurate way to measure distances to the nearest 
                        station might be to calculate the distance between 
                        households and train stations. The smallest geographical 
                        unit of measurement for the ACS is block groups, so 
                        further analysis could yield different results."),
                        p("Second, distances in this analysis are in absolute 
                        terms and do not reflect the real distance a commuter 
                        might have to travel to get to a station. The method 
                        used to calculate the distance in this project ignores 
                        the layout of streets and the presence of buildings, 
                        geographic features, etc. that might affect how far a 
                        commuter actually travels to a station."),
                        br()
                        ),
                 column(2)
             )
    ),
    
    # Add an about page to explain the motivations and background of the
    # project.
    
    tabPanel("About", 
             fluidPage(
                 # img(class = "bg", src = "metro-boston-2.jpg"),
                 column(2),
                 column(8, br(),
                        h3("Motivation"),
                        p("The Boston Globe once called Boston the most racist 
                          city in America. This project explores what that means 
                          for the city’s public transit system."),
                        p("Amid a growing 
                          national conversation around systemic racism, the role 
                          structural forces play in determining individuals’ 
                          life outcomes has become particularly important: city 
                          planning is one example of this phenomenon. Combined 
                          with housing segregation, the construction of transit 
                          lines, highways, bridges, and more can have a 
                          significant impact on shaping the geography of urban 
                          areas. Initially inspired by an article about highway 
                          construction and its impact on marginalized 
                          communities in Los Angeles, I focused my project on 
                          the greater Boston area, where I currently live. By 
                          examining the racial geography of the city’s public 
                          transit system, I hope to shed light on some of the 
                          systemic inequities present."), br(),
                        h3("The data"),
                        p("The project uses demographic and geospatial data from 
                          the Census, the 2018 American Community Survey, and 
                          Massachusetts Bay Transportation Authority. I focus my 
                          analysis on the rapid transit system, the most widely 
                          used MBTA service, and the bus lines the organization 
                          has identified as key routes: the red, green, orange, 
                          blue, and silver lines. Using geographic coordinates 
                          of census tract centroids (the population weighted 
                          center of a census tract), I calculated the distance 
                          to each tract’s nearest MBTA station  — this is the 
                          data I used to build my statistical model."),
                        br(),
                        h3("About me"),
                        p("I'm Kai McNamee. I'm currently and undergrad at 
                          Harvard studying Governmennt and Economics. This is my 
                          final project for Gov 50: Data Fall 2020. You can find 
                          the code and data that went into this project on", 
                          tags$a(href="https://github.com/kaimcnamee/mbta-access", 
                                 "GitHub.")),
                        p(tags$a(href="mailto:kaimcnamee@college.harvard.edu", 
                                 "kaimcnamee@college.harvard.com")),
                        br()
                 ),
                 column(2)
             )
    )
)
    
    



# Define server logic

server <- function(input, output) {
    
    # Define the map output for the Map tab. 
    
    output$map1 <- renderLeaflet({
        
        # Create the map base layer with CartoDB tiles. Define MapPane groups
        # and use zIndex to specify layer order.
        
        leaflet() %>% 
            addMapPane(name = "polygons", zIndex = 1) %>% 
            addMapPane(name = "lines", zIndex = 2) %>% 
            addMapPane(name = "nodes", zIndex = 3) %>% 
            addMapPane(name = "info", zIndex = 4) %>% 
            addProviderTiles(provider = "CartoDB.Positron",  
                             options = leafletOptions(pane = "polygons")) %>%
            
            # Add tract polygon layers separated by racial/ethnic group. Define
            # unique layerId's for each polygon layer -- this will define an
            # output value when users click each census tract (Note: if layers
            # of the same type, ie polygons, have the same layerId, they'll
            # overwrite each other. To get around this, add a unique letter to
            # the front of each ID specification). Define the data to display
            # when users click census tracts.
            
            addPolygons(data = counties_white,
                        popup = ~ paste0(name),
                        highlightOptions = highlightOptions(fillOpacity = 0.3),
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        color = ~ pal(percent),
                        layerId = paste0("w", counties_white$name),
                        group = "White",
                        options = leafletOptions(pane = "polygons")) %>%
            
            addPolygons(data = counties_black, 
                        popup = ~ paste0(name),
                        highlightOptions = highlightOptions(fillOpacity = 0.3),
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        color = ~ pal(percent),
                        layerId = paste0("b", counties_black$name),
                        group = "Black",
                        options = leafletOptions(pane = "polygons")) %>%
            
            addPolygons(data = counties_asian, 
                        popup = ~ paste0(name),
                        highlightOptions = highlightOptions(fillOpacity = 0.3),
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        color = ~ pal(percent),
                        layerId = paste0("a", counties_asian$name),
                        group = "Asian",
                        options = leafletOptions(pane = "polygons")) %>%
            
            addPolygons(data = counties_hispanic, 
                        popup = ~ paste0(name),
                        highlightOptions = highlightOptions(fillOpacity = 0.3),
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        color = ~ pal(percent),
                        layerId = paste0("h", counties_hispanic$name),
                        group = "Hispanic",
                        options = leafletOptions(pane = "polygons")) %>%
            
            # Add a legend corresponding to population percentage for each map
            # layer.
            
            addLegend(data = counties_map, "bottomright",
                      pal = pal,
                      values = ~ percent,
                      title = "Percentage",
                      opacity = 1) %>% 
            
            # Add MBTA lines to the map.
            
            addPolylines(data = mbta, group = "Lines",
                         popup = ~ route,
                         options = leafletOptions(pane = "lines"),
                         weight = 2,
                         opacity = 0.8,
                         color = "white") %>%
            
            # Add MBTA markers for T stations. Hide icons by default.
            
            addMarkers(data = mbta_nodes, icon = mbtaIcon, group = "Stations",
                       popup = ~ station,
                       options = leafletOptions(pane = "nodes")) %>%
            hideGroup("Stations") %>% 
            
            # Add layer controls so users can toggle between and compare
            # racial/ethnic groups.
            
            addLayersControl(baseGroups = c("White", "Black", "Asian", 
                                            "Hispanic"),
                             overlayGroups = c("Lines", "Stations"),
                             options = layersControlOptions(collapsed = FALSE))
        })

    # Define the plot output for the info box on the Map tab.
    
    output$tract <- renderPlot({
        
        # Define the default plot that appears before users click on a census
        # tract. The initial if() condition checks if no tract has been clicked
        # yet. Here, I chose an arbitrary default tract and plotted its
        # demographics. Clean up the plot's theme, title, and labels.
        
            if(length(input$map1_shape_click$id) == 0){
                selected <- counties_map %>%
                    filter(name == "Census Tract 3537, Middlesex County")
                print(ggplot(data = selected, aes(x = variable, y = percent)) + 
                          geom_col(fill = "gray") +
                          labs(title = "Census Tract 3537, Middlesex County",
                               subtitle = paste0("Closest station: ", 
                                                selected$station, " (",
                                                round(selected$distance), "m)"),
                               x = " ",
                               y = " ") +
                          theme_minimal() +
                          scale_y_continuous(
                              labels = scales::percent_format(scale = 1)) +
                          theme(plot.title = element_text(size = 12, 
                                                          face = "bold"),
                                plot.subtitle = element_text(size = 10),
                                text = element_text(color = "white"),
                                axis.text = element_text(color = "white"), 
                                panel.background = element_blank(),
                                plot.background = element_blank(),
                                panel.grid = element_blank()))
                          
            
            }
             
        # Plot demographics of the user-selected tract. The object
        # input$map1_shape_click$id stores the Id value of the most recently
        # clicked census tract. Since the map code adds a unique letter for each
        # layer, use sub() to remove it. Clean up the plot's theme, title, and
        # labels.
            
            else if(length(input$map1_shape_click$id) > 0){
                selected <- counties_map %>%
                  filter(name == sub('.', '', input$map1_shape_click$id))
                print(ggplot(data = selected, aes(x = variable, y = percent)) + 
                        geom_col(fill = "gray") +
                          labs(title = sub('.', '', input$map1_shape_click$id),
                               subtitle = paste0("Closest station: ", 
                                                 selected$station, " (",
                                                 round(selected$distance), 
                                                 "m)"),
                               x = " ",
                               y = " ") +
                          theme_minimal() +
                          scale_y_continuous(
                              labels = scales::percent_format(scale = 1)) +
                          theme(plot.title = element_text(size = 12, 
                                                          face = "bold"),
                                plot.subtitle = element_text(size = 10),
                                text = element_text(color = "white"),
                                axis.text = element_text(color = "white"), 
                                panel.background = element_blank(),
                                plot.background = element_blank(),
                                panel.grid = element_blank()))
        }
    }, bg = "transparent")
        
    
}
    
# Run the application 
shinyApp(ui = ui, server = server)
