library(shiny)
library(agricolaeplotr)
library(ggplot2)
library(leaflet)
library(sf)
library(sp)

full_control_positions <- function(design,
                                   x = "col",
                                   y = "row",
                                   factor_name = "trt",
                                   labels = "plots",
                                   width = 1,
                                   height = 1,
                                   space_width = 0.95,
                                   space_height = 0.85,
                                   reverse_y = FALSE,
                                   reverse_x = FALSE,
                                   way_x=0,
                                   way_y=0,
                                   shift_x=0,
                                   dist_x=1,
                                   dist_y=1,
                                   shift_y=0,
                                   start_origin=FALSE) {

  test_string(x)
  test_string(y)

  test_input_reverse(reverse_x)
  test_input_reverse(reverse_y)

  test_input_extend(height)
  test_input_extend(width)

  test_input_extend(space_height)
  test_input_extend(space_width)

  test_input_shift(shift_x)
  test_input_shift(shift_y)

  table <- design

  if(start_origin == TRUE){
    shift_x <- width * -0.5 + (width * -0.5 * (1-space_width)) ## makes zero
    shift_y <- height * -0.5 + (height * -0.5 * (1-space_height)) ## makes zero

    table[, x]  <- as.numeric(table[, x] )

    for (i in way_x ){
      table[, x] <- ifelse(table[, x] > (i + (match(i,way_x) - 1)), table[, x] + dist_x, table[, x])
      print(design)
    }

    table[, x]  <- table[, x] * width + shift_x

    table[, y] <- as.numeric(table[, y])
    for (i in way_y ){
      table[, y] <- ifelse(table[, y] > (i + (match(i,way_y) - 1)), table[, y] + dist_y, table[, y])
    }
    table[, y] <- table[, y] * height + shift_y
  }
  else{
    table[, x]  <- as.numeric(table[, x] )
    for (i in way_x ){
      table[, x] <- ifelse(table[, x] > (i + (match(i,way_x) - 1)), table[, x] + dist_x, table[, x])
      print(design)
    }
    table[, x]  <- table[, x] * width + shift_x

    table[, y] <- as.numeric(table[, y])
    for (i in way_y ){
      table[, y] <- ifelse(table[, y] > (i + (match(i,way_y) - 1)), table[, y] + dist_y, table[, y])
    }
    table[, y] <- table[, y] * height + shift_y
  }

  if (reverse_y == TRUE) {
    table[, y] <- abs(table[, y] - max(table[, y])) +
      min(table[, y])
  }
  if (reverse_x == TRUE) {
    table[, x]  <- abs(table[, x]  - max(table[, x] )) +
      min(table[, x] )
  }
  plt <- ggplot(table, aes_string(x = x, y = y)) +
    geom_tile(aes_string(fill = factor_name),
              width = width * space_width, height = height *
                space_height) + theme_bw() + theme(line = element_blank()) +
    geom_text(aes_string(label = labels), colour = "black")

  plt

  return(plt)
}

#### general data #####

ui <- fluidPage(

  # App title ----

  titlePanel("GUI for a Quick Placement of Agricultural Experiments"),



  # Sidebar layout with input and output definitions ----

  sidebarLayout(



    # Sidebar panel for inputs ----

    sidebarPanel(


      # Input: Select a dataset ----

      fileInput("file", "Upload CSV File", accept = ".csv"),

      textInput("x", "X Coordinates", value = "ROW"),

      textInput("y", "Y Coordinates", value = "COLUMN"),

      numericInput("width", "Width of Plot", value = 5),

      numericInput("height", "Height of Plot", value = 5),

      numericInput("space_width", "Space Width", value = 0.5),

      numericInput("space_height", "Space Height", value = 0.5),

      checkboxInput("reverse_y", "Reverse Order in Row Direction", value = FALSE),

      checkboxInput("reverse_x", "Reverse Order in Column Direction", value = FALSE),

      textInput("factor_name", "Factor Name", value = "TREATMENT"),

      textInput("labels", "Labels", value = "TREATMENT"),

      numericInput("shift_x", "Shift in Units in X-axis", value = 0),

      numericInput("shift_y", "Shift in Units in Y-axis", value = 0),

      selectizeInput(
        inputId = "way_x","Shift in Units in X-axis",
        multiple = TRUE,choices = NULL, selected = NULL,
        options = list(create = TRUE)
      ),

      selectizeInput(
        inputId = "way_y","Shift in Units in Y-axis",
        multiple = TRUE,choices = NULL, selected = NULL,
        options = list(create = TRUE)
      ),

      numericInput("dist_x", "Shift in Plots in X-axis", value = 1),

      numericInput("dist_y", "Shift in Plots in Y-axis", value = 1),

      checkboxInput("start_origin", "Start at Origin (0|0)", value = FALSE),

      numericInput("northing", "Northing", value = 5939183.21, min = 0),

      numericInput("rotation", "Rotation", value = 40, min = 0),

      numericInput("easting", "Easting", value = 3454206.89 , min = 0),

      selectInput("maplayer",
                  "Which layer do you prefer?",
                  choices = c("OpenStreetMap.DE", "Esri.WorldImagery")
      ),


      actionButton("plotButton", "Generate Plot")

      ,width = 3),


    mainPanel(

      leafletOutput("plot",width = "100%",height = 1400)




    )

  )

)



# Define server logic to summarize and view selected dataset ----

server <- function(session, input, output) {

  ### plotting section

  dataset <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })

  generatePlot <- eventReactive(input$plotButton, {
    # Call the full_control_positions function with user inputs
    plot <- full_control_positions(design = dataset(), x = input$x, y = input$y,
                                   width = input$width, height = input$height,
                                   space_width = input$space_width, space_height = input$space_height,
                                   reverse_y = input$reverse_y, reverse_x = input$reverse_x,
                                   factor_name = input$factor_name, labels = input$labels,
                                   way_x = as.numeric(input$way_x), way_y = as.numeric(input$way_y),
                                   shift_x = input$shift_x, shift_y = input$shift_y,
                                   dist_x = input$dist_x, dist_y = input$dist_y,
                                   start_origin = input$start_origin)



    polygons_list <-  make_polygons(plot, north = input$northing,
                                    east = input$easting)

    polygons_list <- sf::as_Spatial(polygons_list)

    row.names(polygons_list) <- rownames(polygons_list@data)

    polygons_list <-
      sp::elide(polygons_list,
                rotate = input$rotation,
                center = apply(bbox(polygons_list), 1, mean))

    plt2 <-
      leaflet() %>% addTiles() %>% addPolygons(
        data = polygons_list,
        weight = 1,
        fillColor = polygons_list@data$fill,
        opacity = 1,
        fillOpacity = 1,
        label = c(polygons_list@data$plot_nr)
      ) %>% addProviderTiles(input$maplayer)



    return(plt2)
  })

  # Render the plot
  output$plot <- renderLeaflet({
    generatePlot()
  })
}

# Create Shiny app ----

shinyApp(ui, server)
###### debugging zone ########

