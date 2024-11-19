library(shiny)
library(leaflet)
library(dplyr)
library(gt)
source("geocodedaddresses.R")


# Define UI
ui <- fluidPage(
  # Application Title
  titlePanel(" "),
  
  sidebarLayout(
    # Sidebar for Filters
    sidebarPanel(
      h4("Filter Businesses and Projects"),
      selectInput(
        "country",
        "Country:",
        choices = c("All", sort(unique(responses$country_of_business_project))),
        selected = "All"
      ),
      selectInput(
        "project",
        "Business or Project Type:",
        choices = c("All", sort(unique(responses$business_or_project))),
        selected = "All"
      ),
      selectInput(
        "category",
        "Category:",
        choices = c("All", sort(unique(responses$category_of_business_or_project))),
        selected = "All"
      ),
      helpText("Use the filters above to explore Sudanese and diaspora-owned businesses and projects.")
    ),
    
    # Main Content
    mainPanel(
      tabsetPanel(
        # Map Tab
        tabPanel(
          "Map",
          leafletOutput("businessMap", height = 600)
        ),
        # Table Tab
        tabPanel(
          "Table",
          gt_output("businessTable")
        )
      )
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  # Reactive filtering based on user inputs
  filteredData <- reactive({
    data <- responses
    if (input$country != "All") {
      data <- data %>% filter(country_of_business_project == input$country)
    }
    if (input$project != "All") {
      data <- data %>% filter(business_or_project == input$project)
    }
    if (input$category != "All") {
      data <- data %>% filter(category_of_business_or_project == input$category)
    }
    data
  })
  
  # Update filter options dynamically
  observe({
    updateSelectInput(
      session,
      "project",
      choices = c("All", sort(unique(filteredData()$business_or_project)))
    )
    updateSelectInput(
      session,
      "category",
      choices = c("All", sort(unique(filteredData()$category_of_business_or_project)))
    )
  })
  
  # Render Leaflet Map
  output$businessMap <- renderLeaflet({
    data <- filteredData()
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        radius = 6,
        color = "blue",
        stroke = TRUE,
        fillOpacity = 0.8,
        label = ~name_of_business_project,
        popup = ~paste0(
          "<b>", name_of_business_project, "</b><br>",
          "Category: ", category_of_business_or_project, "<br>",
          "Location: ", city_location_of_business_project, ", ", country_of_business_project, "<br>",
          ifelse(!is.na(business_project_website),
                 paste0("<a href='", business_project_website, "' target='_blank'>Website</a>"), "")
        )
      )
  })
  
  # Render GT Table
  output$businessTable <- render_gt({
    filteredData() %>%
      select(
        "Business Name" = name_of_business_project,
        "Website" = business_project_website,
        "Category" = category_of_business_or_project,
        "City" = city_location_of_business_project,
        "Country" = country_of_business_project
      ) %>%
      gt() %>%
      tab_header(
        title = md("**Sudanese & Diaspora Owned Businesses**"),
        subtitle = md("Filtered results based on your selections.")
      ) %>%
      fmt_missing(columns = everything(), missing_text = "Not Available")
  })
}
shinyApp(ui, server)

