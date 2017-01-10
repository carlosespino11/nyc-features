library(shiny)
library(leaflet)
library(maptools)
library(spatialEco)
library(dplyr)


# setwd("~/Documents/Columbia/Exploratory Data Analysis and Visualization/HW3/Edav-P3-ggteam/ce2330/")


park = read.csv("park.csv", as.is = TRUE)
police = read.csv("police.csv", as.is = TRUE)
restaurant = read.csv("restaurant.csv", as.is = TRUE)
school = read.csv("school.csv", as.is = TRUE)

nycShape = readShapePoly("cb_2014_36_tract_500k/cb_2014_36_tract_500k.shp")
nycData = read.csv("ct_nyc_clean.csv")

nycShape@data = left_join(nycShape@data, nycData, by = c("AFFGEOID" = "Id")) 
nycShape = sp.na.omit(nycShape, col.name = "Id2", margin = 1)

pal <- function(x) {colorBin("YlGnBu", x)}

iconHeight = 20 
iconWidth = 20 

parkIcon <- makeIcon(
  iconUrl = "Pine-512.png",
  iconWidth = iconWidth, iconHeight = iconHeight
)

policeIcon <- makeIcon(
  iconUrl = "noun_4404.png",
  iconWidth = iconWidth, iconHeight = iconHeight
)

restaurantIcon <- makeIcon(
  iconUrl = "red_food_restaurant_pixel_perfect_pika_kitchen-512.png",
  iconWidth = iconWidth, iconHeight = iconHeight
)

schoolIcon <- makeIcon(
  iconUrl = "538411-education_512x512.png",
  iconWidth = iconWidth, iconHeight = iconHeight
)

namesMap = list("Median Age" = "median_age", "Married" = "married", 
                "High School and Higher" = "high_school_and_higher", 
                "Mean Income" = "mean_income", "Unemployment Rate" = "Unemployment_rate",
                "Median Rent" = "median_rent")

ui <- bootstrapPage(
  tags$head(
    includeCSS("styles.css")
  ),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10, id = "controls", class = "panel panel-default",
                fixed = TRUE, draggable = TRUE,height = "100px", width =  "250px",
                selectInput("feature", "Feature",
                            choices = names(namesMap))
  ),
  absolutePanel(top = 10, left = 35,headerPanel("NYC Explorer"))
)

server <- function(input, output, session) {
  # Reactive expression to subset data
  selectedFeature <- reactive({
    selected = input$feature
    return(list(feature = namesMap[[selected]], name = selected))
    
  })
  
  output$map <- renderLeaflet({
    # Aaspects of the map that  won't need to change dynamically
    leaflet(nycShape) %>% 
      addProviderTiles("CartoDB.Positron") %>%
      addMarkers(lng = ~lng, lat = ~lat, icon = parkIcon, popup= ~name, 
                 clusterOptions = markerClusterOptions(),
                 group = "Parks",
                 data = park) %>%
      addMarkers(lng = ~lng, lat = ~lat, icon = policeIcon, popup= ~Name, 
                 clusterOptions = markerClusterOptions(),
                 group = "Police Stations",
                 data = police) %>%
      addMarkers(lng = ~lng, lat = ~lat, icon = restaurantIcon, popup= ~name, 
                 clusterOptions = markerClusterOptions(),
                 group = "Restaurants",
                 data = restaurant)%>%
      addMarkers(lng = ~lng, lat = ~lat, icon = schoolIcon, popup= ~SCHOOLNAME, 
                 clusterOptions = markerClusterOptions(),
                 group = "Schools",
                 data = school)%>%
      # addPolygons(color = ~pal(median_age)(median_age),stroke = TRUE, weight = 1,
      #             popup = ~paste("Median Age",median_age, sep=": "), fillOpacity = 0.7,
      #             group = "Median Age") %>%
      # addPolygons(color = ~pal(married)(married),stroke = TRUE, weight = 1,
      #             popup = ~paste("Married",married, sep=": "), fillOpacity = 0.7,
      #             group = "Married") %>%
      # addPolygons(color = ~pal(high_school_and_higher)(high_school_and_higher),stroke = TRUE, weight = 1,
      #             popup = ~paste("High School and Higher",high_school_and_higher, sep=": "), fillOpacity = 0.7,
      #             group = "High School and Higher") %>%
      # addPolygons(color = ~pal(mean_income)(mean_income),stroke = TRUE, weight = 1,
      #             popup = ~paste("Mean Income",mean_income, sep=": "), fillOpacity = 0.7,
      #             group = "Mean Income") %>%
      # addPolygons(color = ~pal(Unemployment_rate)(Unemployment_rate),stroke = TRUE, weight = 1,
      #             popup = ~paste("Unemployment Rate",Unemployment_rate, sep=": "), fillOpacity = 0.7,
      #             group = "Unemployment Rate") %>%
      # addPolygons(color = ~pal(median_rent)(median_rent),stroke = TRUE, weight = 1,
      #             popup = ~paste("Median Rent",median_rent, sep=": "), fillOpacity = 0.7,
      #             group = "Median Rent") %>%
      # addLegend(pal = pal(nycShape$married),
      #           values = ~median_age,
      #           position = "bottomright",
      #           title = "Married") %>% %>%
      addLayersControl(
        overlayGroups = c("Parks", "Police Stations", "Restaurants", "Schools"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    
  })
  observe({
    feature = unlist(nycShape@data[selectedFeature()[["feature"]]])
    featureName = selectedFeature()[["name"]]
    
    # notSelected = names(namesMap)[!names(namesMap) %in% featureName]
    leafletProxy("map") %>%
      clearControls() %>%
      # hideGroup(notSelected[1]) %>%
      # hideGroup(notSelected[2]) %>%
      # hideGroup(notSelected[3]) %>%
      # hideGroup(notSelected[4]) %>%
      # hideGroup(notSelected[5]) %>%
      # showGroup(featureName) %>%
      addPolygons(data = nycShape, color = pal(feature)(feature),stroke = TRUE, weight = 1,
                  popup = paste(featureName,feature, sep=": "), fillOpacity = 0.7)%>%
      addLegend(pal = pal(feature),
                values = feature,
                position = "bottomright",
                title = featureName)

    })

}

shinyApp(ui, server)



