library(shiny)
library(agricolae)
library(ggplot2)
library(DBI)
library(sf)
library(leaflet)
library(RPostgres)
library(agricolaeplotr)
library(jsonlite)
library(sp)
library(DT)
library(qrcode)
library(jsonify)
library(shinycssloaders)
#cit.list <- c("base", names(utils::sessionInfo()$otherPkgs))
#for(i in 1:length(cit.list)){
# print(c(cit.list[i],packageDescription(cit.list[i], fields=c("License","Version"))))
#}

# generate_qr_labels <- function(input_df, output_file="labelscorrected",experiment_name="My Experiment", img_width = 65, img_height = 32, page_width = 210, page_height = 297) {
#   # Define layout constants
#   cols <- floor(page_width / img_width)  # Number of columns
#   rows <- floor(page_height / img_height)  # Number of rows
#
#
#   total_images <- nrow(input_df)
#   images_per_page <- cols*rows
#
#   num_pages <- ceiling(total_images / images_per_page)
#
#   input_df$trial <- experiment_name
#
#   input_df <- data.frame(lapply(input_df,as.factor))
#   # Convert mm to inches
#   mm_to_inches <- function(mm) {
#     return(mm / 25.4)
#   }
#  print("Here")
#   for (page in seq_len(num_pages)) {
#     start_idx <- (page - 1) * images_per_page + 1
#     end_idx <- min(page * images_per_page, total_images)
#
#     # Set up plotting device (e.g., PDF or PNG)
#     file_name <- sprintf(paste0(output_file,"_%02d.pdf"), page)  # Auto-numerated filename
#
#     # Open a PDF to save output
#     pdf(file_name, width = mm_to_inches(page_width), height = mm_to_inches(page_height))
#
#     # Set up a blank plot for the whole A4 page
#     par(mar = c(0, 0, 0, 0))  # Remove margins
#     plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(0, page_width), ylim = c(0, page_height))
#
#     df_chunk <- input_df[c(start_idx:end_idx),]
#     # Loop through data and draw labels in a grid
#     for (i in 1:nrow(df_chunk)) {
#       # Calculate row and column position
#       row <- ((i - 1) %% rows) + 1
#       col <- ((i - 1) %/% rows) + 1
#
#       # Convert to plot coordinates (in mm)
#       x_left <- (col - 1) * img_width
#       x_right <- x_left + img_width
#       y_bottom <- page_height - (row * img_height)
#       y_top <- y_bottom + img_height
#
#       # Draw a rectangle for the label boundary
#       rect(x_left, y_bottom, x_right, y_top, border = "white", lwd = 1)
#       # Generate QR code
#       qr_text <- jsonify::to_json(df_chunk[i,])
#       qr <- 1- qrcode::qr_code(qr_text, ecl = "Q")
#
#       # Draw QR code on the left side
#       qr_x_left <- x_left + 0
#       qr_x_right <- qr_x_left + 26
#       qr_y_bottom <- y_bottom + 0
#       qr_y_top <- qr_y_bottom + 26
#       rasterImage(qr, qr_x_left, qr_y_bottom, qr_x_right, qr_y_top)
#
#       # Add text on the right side
#       text_x <- x_right - 7  # Align text to the right
#       text_y_top <- y_top - 8
#
#       line_spacing <- 5
#
#       for (j in seq_along(df_chunk)) {
#         text_y_position <- text_y_top - (j - 1) * line_spacing  # Adjust vertical spacing
#         text(text_x, text_y_position, paste(colnames(df_chunk)[j], ":", df_chunk[i, j]),
#              cex = 1.2, font = 2, adj = 1)
#       }
#     }
#
#     # Close PDF output
#     dev.off()
#   }
#
#   files <- Sys.glob(paste0(output_file,"*.pdf"))
#   qpdf::pdf_combine(input = files, output = paste0(output_file,".pdf"))
# }



ui <- fluidPage(
  tags$head(
    tags$link(rel = "icon", type = "image/png", sizes = "48x48", href = "Logo.png"),
    tags$script(
      '


@media (max-width: 1999px) {
  .sidepanel {
    float: none;
    width: 100%;
  }

  .maincontent {
    float: none;
    width: 100%;
  }
};
      '
    ),

    tags$style(HTML("
      .nav-tabs {
        border-bottom: 2px solid #ddd;
      }
      .nav-tabs > li > a {
        background-color: #FFFFFF;
        color: black;
        font-size: 18px;
        padding: 15px 30px;
        border-color: #000000;
        border-radius: 1px;
        transition: background-color 0.3s ease;
        margin-right: 1px;
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.1);
      }
      .nav-tabs > li > a:hover {
        background-color: #E57738;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background-color: #FFFFFF;
        color: black;
        border: none;
        outline: none;
        box-shadow: 0 0 0 3px rgba(0, 123, 255, 0.5);
      }
      .tab-content {
        padding: 20px;
        border: 1px solid #ddd;
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      }
      .btn-custom {
        background-color: #FFFFFF;
        color: black;
        font-size: 16px;
        padding: 10px 20px;
        border-color: black;
        border-radius: 5px;
        transition: background-color 0.3s ease;
        margin: 2px;
      }
      .btn-custom:hover {
        background-color: #000000;
        color: white;
      }
      .btn-custom:focus {
      background.color: #000000;
      color: white;
      text-decoration: underline;
      outline: none;
      }

      .input-label {
        font-weight: bold;
      }

      .logout-button {
          top: 5px;
          right: 5px;
          padding: 8px 16px;
          font-size: 12px;
      }

      .nav-tabs > li > a {
        background-color: #FFFFFF;
        color: black;
        font-size: 16px;
        padding: 10px 20px;
        border-color: black;
        border-radius: 5px;
        transition: background-color 0.3s ease;
        margin: 2px;
      }
      .nav-tabs > li.active > a {
        background-color: #000000;
        color: white;
      }

      .nav-tabs > li.active > a:hover {
        background-color: #000000;
        color: white;
      }

      .nav-tabs > li.active > a:visited {
        background-color: #d3d3d3;
        color: black;
      }

    "))
  ),

  HTML('<meta name="viewport" content="width=device-width, initial-scale=1">'),
  div(
    class = "top-header",
    style = "position: relative; height: 60px;",
    #title=div(img(src='Logo.png', style="width: 120px; height: 50px;"))
    titlePanel("Field Experiment Designer", windowTitle = "Field Experiment Designer" ),

  ),
  # auth0::logoutButton("Log Out",style = "position: absolute; top: 5px; right: 5px; z-index:10000;", class="btn-custom", icon("right-from-bracket")),


  sidebarLayout(
    sidebarPanel(tags$style(".well {background-color: #FFFFFF; border-color: black}"),
                 conditionalPanel(
                   condition = "input.tabs_a=='1'",
                   tags$label("Upload a JSON file containing previous experimental designs", class="input-label"),
                   fileInput("file", "Upload JSON File", accept = ".json"),
                   tags$label("Choose your planned experimental design", class="input-label"),
                   selectInput(
                     "design",
                     NULL,
                     choices = c(
                       "Complete Factorial Design" = "design.ab" ,
                       "Split Plot Design" = "design.split",
                       "Alpha design type (0,1)" = "design.alpha",
                       "Randomized Balanced Incomplete Block Designs (BIB)" = "design.bib",
                       "Completely Randomized Design" = "design.crd",
                       "Cyclic designs" = "design.cyclic",
                       "Augmented block design" = "design.dau",
                       "Graeco - latin square design" = "design.graeco",
                       "Lattice designs" = "design.lattice",
                       "Latin Square Design" = "design.lsd",
                       "Randomized Complete Block Design" = "design.rcbd",
                       "Strip Plot Design" = "design.strip",
                       "Youden Plot Design" = "design.youden",
                       "Special Design" = "custom_spec"
                     ),
                     selected = "design.ab"
                   ),
                   conditionalPanel("input.design !='custom_spec'",
                   tags$label("Choose a seed number (automatic generated)", class="input-label"),
                   numericInput(
                     "seed",
                     NULL,
                     value = sample(0:2147483647, 1),
                     min = 0,
                     max = 2147483647
                   )),
                   conditionalPanel(
                     condition = "input.design == 'design.ab'",
                     tags$label("Choose the number of factors", class="input-label"),
                     numericInput(
                       "numIndividuals",
                       NULL,
                       value = 3,
                       min = 2,
                       max = 5
                     ),
                     uiOutput("fields")
                   ),
                   conditionalPanel(
                     condition = "input.design == 'design.ab' || input.design == 'design.split'",
                     tags$label("Choose your factorial design", class="input-label"),
                     selectInput(
                       "factorial_design",
                       NULL,
                       choices = c("rcbd", "crd", "lsd"),
                       selected = "crd"
                     )
                   ),
                   conditionalPanel("input.design !='custom_spec'",
                   tags$label("Choose a Method to randomize", class="input-label"),
                   selectInput(
                     "kind",
                     NULL,
                     choices = c(
                       "Wichmann-Hill",
                       "Marsaglia-Multicarry",
                       "Super-Duper",
                       "Mersenne-Twister",
                       "Knuth-TAOCP",
                       "Knuth-TAOCP-2002",
                       "default"
                     ))
                   ),
                   conditionalPanel(
                     condition = "input.design != 'design.graeco' && input.design != 'design.lsd' && input.design != 'design.lattice' && input.design != 'custom_spec'" ,
                     tags$label("Number of replications", class="input-label"),
                     numericInput(
                       "r",
                       NULL,
                       value = 3,
                       min = 1,
                       step = 1
                     )
                   ),
                   conditionalPanel(
                     condition = "input.design == 'design.alpha' || input.design == 'design.bib' || input.design == 'design.cyclic'",
                     tags$label("Number of Blocks", class="input-label"),
                     numericInput("k", NULL, value = 2, min = 1)
                   ),
                   conditionalPanel(
                     condition = "input.design == 'design.bib'",
                     tags$label("Maximum replications", class="input-label"),
                     numericInput(
                       "maxrep",
                       NULL,
                       value = 20,
                       min = 1,
                       step = 1
                     )
                   ),
                   conditionalPanel(
                     condition = "input.design == 'design.lattice'",
                     checkboxInput(
                       "simple",
                       "Lattice kind (unchecked = triple, checked = simple)",
                       value = FALSE
                     )
                   ),
                    conditionalPanel("input.design !='custom_spec'",tags$label("Choose Leading Numeration of plots", class="input-label"),
                                                        selectInput("serie", NULL, choices = c(1, 2, 3)),
                                                        checkboxInput("randomization", "Randomization", value = TRUE))
,
                   conditionalPanel(
                     condition = "input.design == 'design.dau' || input.design == 'design.graeco' || input.design == 'design.split' || input.design == 'design.strip'",
                     uiOutput("trt_duo")
                   ),
                   conditionalPanel(
                     condition = "input.design == 'design.split' || input.design == 'design.youden' || input.design == 'design.lsd' || input.design == 'design.rcbd'",
                     checkboxInput("first", "Randomize first replication", value = TRUE)
                   ),
                   conditionalPanel(
                     condition = "input.design == 'design.alpha' || input.design == 'design.bib' || input.design == 'design.crd' || input.design == 'design.lattice' || input.design == 'design.lsd' || input.design == 'design.cyclic' || input.design == 'design.rcbd' || input.design == 'design.youden'",
                     uiOutput("trt_single")
                   ),
                   conditionalPanel(
                     condition = "input.design == 'design.crd'",
                     checkboxInput("continue", "Continuous numbering of plot", value = FALSE)
                   ),
                   conditionalPanel(
                     condition = "input.design == 'design.cyclic'",
                     checkboxInput("rowcol", "TRUE: row-column design", value = FALSE)
                   ),
                   conditionalPanel(
                     condition = "input.design == 'custom_spec'",
                     numericInput("seed","Seed",213463,0,149324847),
                     numericInput("size","Size",2,1,149324847)
                   )

                 ),
                 conditionalPanel(
                   condition = "input.tabs_a == '2' || input.tabs_a == '3'",
                   tags$label("Width [m]", class="input-label"),
                   numericInput("width", NULL, value = 2, min = 0),
                   tags$label("Height [m]", class="input-label"),
                   numericInput("height", NULL, value = 2, min = 0),
                   tags$label("Relative Space Width [%]", class="input-label"),
                   numericInput("space_width", NULL, value = 0.05, min = 0, max = 1, step = 0.05),
                   tags$label("Relative Space Height [%]", class="input-label"),
                   numericInput("space_height", NULL, value = 0.1, min = 0, max = 1, step = 0.05),
                   conditionalPanel(
                     condition = "(input.design == 'design.split' && input.factorial_design == 'crd') || (input.design == 'design.ab' && input.factorial_design == 'crd') || input.design == 'design.crd'",
                     tags$label("Number of Columns", class="input-label"),
                     numericInput("ncols", NULL, value = 5, min = 1)
                   ),
                   conditionalPanel(
                     condition = "input.design == 'design.ab'",
                     uiOutput("choosen_factor")
                   ),
                   conditionalPanel(condition = "input.tabs_a == '2'",
                   tags$label("Northing (EPSG:31467)", class="input-label"),
                   numericInput("northing", NULL, value = 5939183.21, min = 0),
                   tags$label("Easting (EPSG:31467)", class="input-label"),
                   numericInput("easting", NULL, value = 3454206.89, min = 0),
                   tags$label("Rotation in Degree", class="input-label"),
                   numericInput("rotation", NULL, value = 0, min = -180, max = 180)),
                   checkboxInput("reverse_x", "Revert X Axis", value = FALSE),
                   checkboxInput("reverse_y", "Revert Y Axis", value = FALSE),
                   selectInput("colorOption","Color Option", choices = LETTERS[1:8], selected="D"),
                   selectInput("background","Background Color", choices = c("white","yellow","blue","green"), selected="white"),
                   conditionalPanel(
                     condition = "input.design == 'design.ab'",
                     uiOutput("choose_factor")
                   )
                 ),
                 conditionalPanel(
                   condition = "input.design == 'design.split' && (input.tabs_a == '2' || input.tabs_a == '3')",
                   checkboxInput("swap_factor", "Plot Split Factor", value = TRUE)
                 ),
                 conditionalPanel(
                   condition = "input.tabs_a=='3'",
                   fluidPage(
                     downloadButton(
                       'downloadpdf',
                       'Download PDF of the current design',
                       class = "btn-custom",
                       icon = icon("file-pdf")
                     )
                   )
                 ),

                 conditionalPanel(
                   condition = "input.tabs_a == '2'",
                   checkboxInput("rowcol", "TRUE: row-column design", value = FALSE),
                   tags$label("Which Layer Do You Prefer?", class="input-label"),
                   selectInput("maplayer", NULL, choices = c("OpenStreetMap.DE", "Esri.WorldImagery"))
                 ),
                 conditionalPanel(
                   condition = "input.tabs_a == 'database'",
                   downloadButton('downloadData', 'Download Data', class = "btn-custom", icon = icon("table")),
                   downloadButton('downloadParams', 'Download Model Parameter', class = "btn-custom", icon = icon("cog")),
                   downloadButton("exportgeojson", label = "Download GEOJSON", icon = icon("globe"), class = "btn-custom"),
                   tags$hr(),
                   tags$label("Username", class="input-label"),
                   textInput("user", NULL, value = "username"),
                   tags$label("Password", class="input-label"),
                   passwordInput("password", NULL, value = "password"),
                   tags$label("Host of DB", class="input-label"),
                   textInput("host", NULL, value = "127.0.0.1"),
                   tags$label("Port for Connection", class="input-label"),
                   numericInput("port", NULL, value = 7432, min = 0, max = 100000, step = 1),
                   tags$label("DB Name for Connection", class="input-label"),
                   textInput("dbname", NULL, value = "postgresDB"),
                   tags$label("DB Table Name or Filename", class="input-label"),
                   textInput("tablename", NULL, value = "experiment_data"),
                   actionButton('saveSQL', 'Send Design to Postgres', class = "btn-custom", icon = icon("database")),
                   # downloadButton('ISOBUS', 'Send Design to ISOBUS', class = "btn-custom", icon = icon("tractor"))
                 ),
                 conditionalPanel(
                   condition = "input.tabs_a == 'cloud'",
                   tags$label("Username", class="input-label"),
                   textInput("user", NULL, value = "username"),
                   tags$label("Password", class="input-label"),
                   passwordInput("password", NULL, value = "password"),
                   tags$label("Host of DB", class="input-label"),
                   textInput("host", NULL, value = "127.0.0.1"),
                   tags$label("Port for Connection", class="input-label"),
                   numericInput("port", NULL, value = 7432, min = 0, max = 100000, step = 1),
                   tags$label("DB Name for Connection", class="input-label"),
                   textInput("dbname", NULL, value = "postgresDB"),
                   tags$label("DB Table Name or Filename", class="input-label"),
                   textInput("tablename", NULL, value = "experiment_data"),
                   actionButton('cloudUpload', 'Send Design to Cloud Database', class = "btn-custom", icon = icon("cloud"))
                 ),
                conditionalPanel(condition = "input.tabs_a=='summary'",
                 selectInput("experimentparts","Focus of Summary",choices=c("Net Plot"="net_plot","Gross Plot"="gross_plot","Field"="field","Experiment"="experiment","Full"="all"), selected ="all"),
                 numericInput("digits", "Number of Significant Digits:", value = 3, min = 0)
                 ),
                 # conditionalPanel(
                 #   condition = "input.tabs_a == 'sampleLabels'",
                 #   textInput("experiment_name", "Experiment Name:", value = "My Experiment"),
                 #   textInput("output_file", "Output File Name:", value = "labelscorrected"),
                 #   numericInput("img_width", "Image Width:", value = 65, min = 1),
                 #   numericInput("img_height", "Image Height:", value = 32, min = 1),
                 #   numericInput("page_width", "Page Width:", value = 210, min = 1),
                 #   numericInput("page_height", "Page Height:", value = 297, min = 1),
                 #   downloadButton('makeLabels', 'Create Labels', class = "btn-custom", icon = icon("qrcode"))),

                 width = 3
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs", id = "tabs_a",
        tabPanel("Create Experiment", DTOutput("oid1") %>% withSpinner(color="black"), value = "1",icon = icon("gear")),
        tabPanel("Map Plot", plotOutput("doe_plot_map") %>% withSpinner(color="black") , value = "3",icon = icon("map")),
        tabPanel("Experiment Plot", leafletOutput("doe_plot") %>% withSpinner(color="black") , value = "2",icon = icon("location")),
        tabPanel("Data Export", verbatimTextOutput("sql_db"), value = "database", icon = icon("download")),
        tabPanel("Cloud", value = "cloud", icon = icon("cloud")),
        tabPanel("Flightmap", value = "flightmap", icon = icon("plane"),
                 tags$iframe(src = "https://maptool-dipul.dfs.de/?language=de",allow="geolocation",
                             width = "100%", height = "600px", frameborder = "0")%>% withSpinner(color="black")),
        tabPanel("Summary",DTOutput("oid2") %>% withSpinner(color="black"),value="summary",icon=icon("clipboard")),
        tabPanel("About",value="about",icon=icon("info-circle")),
        #tabPanel("Label",value="sampleLabels",icon=icon("qrcode"))
        #tabPanel("Nature Conservation", value = "flightmap", icon = icon("frog",style = "color: rgb(255,0,0)"),
        #         tags$iframe(src = "https://maptool-dipul.dfs.de/?language=de",allow="geolocation",
        #                     width = "100%", height = "600px", frameborder = "0"))


      ),
      tags$script(HTML(
        "$(document).on('keydown', function(e) {
      if (e.altKey) {
        switch(e.key) {
          case '1': Shiny.setInputValue('nav_key', '1', {priority: 'event'}); break;
          case '3': Shiny.setInputValue('nav_key', '2', {priority: 'event'}); break;
          case '2': Shiny.setInputValue('nav_key', '3', {priority: 'event'}); break;
          case '4': Shiny.setInputValue('nav_key', 'database', {priority: 'event'}); break;
          case '5': Shiny.setInputValue('nav_key', 'cloud', {priority: 'event'}); break;
          case '6': Shiny.setInputValue('nav_key', 'flightmap', {priority: 'event'}); break;
          case '7': Shiny.setInputValue('nav_key', 'summary', {priority: 'event'}); break;
          case '8': Shiny.setInputValue('nav_key', 'about', {priority: 'event'}); break;
        }
      }
    });"
      ))
    )
  )



)

# Define server logic to summarize and view selected data set ----

server <- function(session, input, output) {

  observeEvent(input$nav_key, {
    updateTabsetPanel(session, "tabs_a", selected = input$nav_key)
  })


  observeEvent(input$file, {
    req(input$file)
    json_data <- fromJSON(input$file$datapath)
    updateSelectInput(session, "design", selected = json_data$design)
    updateNumericInput(session, "numIndividuals", value = json_data$numIndividuals)
    updateNumericInput(session, "r", value = unique(json_data$r))
    updateNumericInput(session, "serie", value = json_data$serie)
    updateNumericInput(session, "seed", value = json_data$seed)
    updateSelectInput(session, "kind",  selected = json_data$kind)
    updateNumericInput(session, "northing", value = json_data$northing)
    updateNumericInput(session, "easting", value = json_data$easting)
    updateNumericInput(session, "rotation", value = json_data$rotation)
    updateCheckboxInput(session, "first", value = json_data$first)
    updateCheckboxInput(session, "randomization", value = json_data$randomization)
    updateNumericInput(session, "k", value = json_data$k)
    updateNumericInput(session, "maxrep", value = json_data$maxrep)
    updateSelectizeInput(session, "treatment_single", choices = json_data$trt, selected = json_data$trt)
    updateSelectizeInput(session, "treatment_11", choices = json_data$trt1, selected = json_data$trt1)
    updateSelectizeInput(session, "treatment_12", choices = json_data$trt, selected = json_data$trt)
    updateSelectizeInput(session, "treatment1", choices = json_data$treatment1, selected = json_data$treatment1)
    updateSelectizeInput(session, "treatment2", choices = json_data$treatment2, selected = json_data$treatment2)
    updateSelectizeInput(session, "treatment3", choices = json_data$treatment3, selected = json_data$treatment3)
    updateSelectizeInput(session, "treatment4", choices = json_data$treatment4, selected = json_data$treatment4)
    updateSelectizeInput(session, "treatment5", choices = json_data$treatment5, selected = json_data$treatment5)
    updateNumericInput(session, "width", value = json_data$width)
    updateNumericInput(session, "height", value = json_data$height)
    updateNumericInput(session, "space_width", value = json_data$space_width)
    updateNumericInput(session, "space_height", value = json_data$space_height)
    updateCheckboxInput(session, "reverse_y", value = json_data$reverse_y)
    updateCheckboxInput(session, "reverse_x", value = json_data$reverse_x)
    updateCheckboxInput(session, "rowcol", value = json_data$rowcol)
    updateCheckboxInput(session, "colorOption", value = json_data$colorOption)
    updateCheckboxInput(session, "background", value = json_data$background)

  })

 #### GPS Server stuff goes here

  coords <- reactive({
    input$gps_coords
  })

  transform_coords <- reactive({
    if (!is.null(coords())) {
      wgs84 <- st_sfc(st_point(c(coords()$lon, coords()$lat)), crs = 4326)
      utm <- st_transform(wgs84, crs = 31467) # UTM Zone 33N
      utm_coords <- st_coordinates(utm)
      list(easting = utm_coords[2], northing = utm_coords[1])
    } else {
      list(easting = 0, northing = 0)
    }
  })

  observe({
    if (!is.null(coords())) {
      utm <- transform_coords()
      updateNumericInput(session, "easting", value = utm$easting)
      updateNumericInput(session, "northing", value = utm$northing)
    }
  })

  outdesign <- reactive({
    if((input$design == "design.ab") && (input$factorial_design == "crd")){
      trt <- switch(input$numIndividuals,
                    c(length(input$treatment1)),
                    c(length(input$treatment1),
                      length(input$treatment2)),
                    c(length(input$treatment1),
                      length(input$treatment2),
                      length(input$treatment3)),
                    c(length(input$treatment1),
                      length(input$treatment2),
                      length(input$treatment3),
                      length(input$treatment4)),
                    c(length(input$treatment1),
                      length(input$treatment2),
                      length(input$treatment3),
                      length(input$treatment4),
                      length(input$treatment5))
      )

      rst <- design.ab(
        trt,
        r = input$r,
        serie = as.numeric(input$serie),
        design = input$factorial_design,
        kinds = input$kind,
        seed = input$seed)

      endnumber <- dim(rst$book)[2]

      for( i in 3:endnumber){

        rst$book[,i] <- factor(rst$book[,i], labels=input[[paste0("treatment",i-2)]])
      }
      return(rst)
    }

    if((input$design == "design.ab") && (input$factorial_design == "lsd")){
      trt <- switch(input$numIndividuals,
                    c(length(input$treatment1)),
                    c(length(input$treatment1),
                      length(input$treatment2)),
                    c(length(input$treatment1),
                      length(input$treatment2),
                      length(input$treatment3)),
                    c(length(input$treatment1),
                      length(input$treatment2),
                      length(input$treatment3),
                      length(input$treatment4)),
                    c(length(input$treatment1),
                      length(input$treatment2),
                      length(input$treatment3),
                      length(input$treatment4),
                      length(input$treatment5))
      )

      rst <- design.ab(
        trt,
        r = input$r,
        serie = as.numeric(input$serie),
        design = input$factorial_design,
        kinds = input$kind,
        seed = input$seed)

      endnumber <- dim(rst$book)[2]

      for( i in 4:endnumber){

        rst$book[,i] <- factor(rst$book[,i], labels=input[[paste0("treatment",i-3)]])
      }
      return(rst)
    }

    if((input$design == "design.ab") && (input$factorial_design == "rcbd")){
      trt <- switch(input$numIndividuals,
                    c(length(input$treatment1)),
                    c(length(input$treatment1),
                      length(input$treatment2)),
                    c(length(input$treatment1),
                      length(input$treatment2),
                      length(input$treatment3)),
                    c(length(input$treatment1),
                      length(input$treatment2),
                      length(input$treatment3),
                      length(input$treatment4)),
                    c(length(input$treatment1),
                      length(input$treatment2),
                      length(input$treatment3),
                      length(input$treatment4),
                      length(input$treatment5))
      )
      rst <- design.ab(
        trt=trt,
        r = input$r,
        serie = as.numeric(input$serie),
        design = input$factorial_design,
        kinds = input$kind,
        seed = input$seed,
        first = input$first,
        randomization = input$randomization)

      endnumber <- dim(rst$book)[2]

      for( i in 3:endnumber){

        rst$book[,i] <- factor(rst$book[,i], labels=input[[paste0("treatment",i-2)]])
      }

      return(rst)
    }

    if(input$design == "design.youden"){
      trt <- input$treatment_single
      rst2 <- design.youden(trt = trt,
                            r = input$r,
                            serie = as.numeric(input$serie),
                            kinds = input$kind,
                            seed = input$seed,
                            first = input$first,
                            randomization = input$randomization)
      return(rst2)
    }

    if(input$design == "design.crd"){
      trt <- input$treatment_single
      rst3 <- design.crd(trt = trt,
                         r = input$r,
                         serie = as.numeric(input$serie),
                         kinds = input$kind,
                         seed = input$seed,
                         randomization = input$randomization)
      return(rst3)
    }

    if(input$design == "design.rcbd"){
      trt <- input$treatment_single
      rst3 <- design.rcbd(trt = trt,
                          r = input$r,
                          serie = as.numeric(input$serie),
                          kinds = input$kind,
                          seed = input$seed,
                          first = input$first,
                          continue = input$continue,
                          randomization = input$randomization)
      return(rst3)
    }

    if(input$design == "design.lsd"){
      trt <- input$treatment_single
      rst3 <- design.lsd(trt = trt,
                         serie = as.numeric(input$serie),
                         kinds = input$kind,
                         seed = input$seed,
                         randomization = input$randomization,
                         first = input$first)
      return(rst3)
    }


    if(input$design == "design.dau"){
      trt1 <- input$treatment_12
      trt2 <- input$treatment_11
      rst4 <- design.dau(trt1 = trt1,
                         trt2 = trt2,
                         r = input$r,
                         serie = as.numeric(input$serie),
                         kinds = input$kind,
                         seed = input$seed,
                         randomization = input$randomization)
      return(rst4)
    }

    if(input$design == "design.strip"){
      trt1 <- input$treatment_11
      trt2 <- input$treatment_12
      rst4 <- design.strip(trt1 = trt1,
                           trt2 = trt2,
                           r = input$r,
                           serie = as.numeric(input$serie),
                           kinds = input$kind,
                           seed = input$seed,
                           randomization = input$randomization)
      return(rst4)
    }

    if(input$design == "design.graeco"){
      trt1 <- input$treatment_11
      trt2 <- input$treatment_12
      rst5 <- design.graeco(trt1 = trt1,
                            trt2 = trt2,
                            serie = as.numeric(input$serie),
                            kinds = input$kind,
                            seed = input$seed,
                            randomization = input$randomization)
      return(rst5)
    }


    if(input$design == "design.alpha"){
      trt <- input$treatment_single
      rst6 <- design.alpha(trt = trt,
                           r = input$r,
                           k = input$k,
                           serie = as.numeric(input$serie),
                           kinds = input$kind,
                           seed = input$seed,
                           randomization = input$randomization)
      return(rst6)
    }

    if(input$design == "design.bib"){
      trt <- input$treatment_single
      rst6 <- design.bib(trt = trt,
                         r = input$r,
                         k = input$k,
                         serie = as.numeric(input$serie),
                         kinds = input$kind,
                         seed = input$seed,
                         maxRep = input$maxrep,
                         randomization = input$randomization)
      return(rst6)
    }

    if(input$design == "design.cyclic"){
      trt <- input$treatment_single
      rst6 <- design.cyclic(trt = trt,
                            r = input$r,
                            k = input$k,
                            rowcol = input$rowcol,
                            serie = as.numeric(input$serie),
                            kinds = input$kind,
                            seed = input$seed,
                            randomization = input$randomization)
      return(rst6)
    }

    if((input$design == "design.lattice") && ( input$simple == TRUE)){
      trt <- input$treatment_single
      rst6 <- design.lattice(trt = trt,
                             r = 2,
                             serie = as.numeric(input$serie),
                             kinds = input$kind,
                             seed = input$seed,
                             randomization = input$randomization)
      return(rst6)
    }

    if((input$design == "design.lattice") && (input$simple == FALSE)){
      trt <- input$treatment_single
      rst6 <- design.lattice(trt = trt,
                             r = 3,
                             serie = as.numeric(input$serie),
                             kinds = input$kind,
                             seed = input$seed,
                             randomization = input$randomization)
      return(rst6)
    }

    if((input$design == "design.split") && (input$factorial_design == "crd")){
      trt1 <- input$treatment_11
      trt2 <- input$treatment_12
      rst4 <- design.split(trt1 = trt1,
                           trt2 = trt2,
                           r = input$r,
                           design = input$factorial_design,
                           serie = as.numeric(input$serie),
                           kinds = input$kind,
                           seed = input$seed,
                           first = input$first,
                           randomization = input$randomization)
      return(rst4)
    }

    if((input$design == "design.split") && (input$factorial_design == "rcbd")){
      trt1 <- input$treatment_11
      trt2 <- input$treatment_12
      rst4 <- design.split(trt1 = trt1,
                           trt2 = trt2,
                           r = input$r,
                           design = input$factorial_design,
                           serie = as.numeric(input$serie),
                           kinds = input$kind,
                           seed = input$seed,
                           first = input$first,
                           randomization = input$randomization)
      return(rst4)
    }
    if((input$design == "design.split") && (input$factorial_design == "lsd")){
      trt1 <- input$treatment_11
      trt2 <- input$treatment_12
      rst4 <- design.split(trt1 = trt1,
                           trt2 = trt2,
                           r = input$r,
                           design = input$factorial_design,
                           serie = as.numeric(input$serie),
                           kinds = input$kind,
                           seed = input$seed,
                           first = input$first,
                           randomization = input$randomization)
      return(rst4)
    }



  })


  # print a summary

  plt <- reactive({
    if((input$design == "design.ab") && (input$factorial_design == "crd")){

      plt <- plot_design.factorial_crd(outdesign(),
                                       ncols = input$ncols ,
                                       nrows = ceiling(length(outdesign()$book[,1])/input$ncols),
                                       factor_name = input$choosen_factor,
                                       width = input$width,
                                       height = input$height,
                                       space_width = (1-input$space_width),
                                       space_height = (1-input$space_height),
                                       reverse_x = input$reverse_x,
                                       reverse_y = input$reverse_y)
    }

    if((input$design == "design.ab") && (input$factorial_design == "lsd")){
      plt <- (plot_design.factorial_lsd(design = outdesign(),
                                        factor_name = input$choosen_factor,
                                        width = input$width,
                                        height = input$height,
                                        space_width = (1-input$space_width),
                                        space_height = (1-input$space_height),
                                        reverse_y = input$reverse_y,
                                        reverse_x = input$reverse_x))
    }

    if((input$design == "design.ab") && (input$factorial_design == "rcbd")){
      plt <- (plot_design.factorial_rcbd(design = outdesign(),
                                         factor_name = input$choosen_factor,
                                         width = input$width,
                                         height = input$height,
                                         space_width = (1-input$space_width),
                                         space_height = (1-input$space_height),
                                         reverse_y = input$reverse_y,
                                         reverse_x = input$reverse_x))
    }

    if(input$design== "design.alpha"){

      plt <- (plot_alpha(design = outdesign(),
                         factor_name = "trt",
                         width = input$width,
                         height = input$height,
                         space_width = (1-input$space_width),
                         space_height = (1-input$space_height),
                         reverse_y = input$reverse_y,
                         reverse_x = input$reverse_x))
    }

    if(input$design == "design.bib"){

      plt <- (plot_bib(design = outdesign(),
                       factor_name = "trt",
                       width = input$width,
                       height = input$height,
                       space_width = (1-input$space_width),
                       space_height = (1-input$space_height),
                       reverse_y = input$reverse_y,
                       reverse_x = input$reverse_x))
    }

    if(input$design == "design.crd"){

      plt <- (plot_design_crd(design = outdesign(),
                              ncols = input$ncols ,
                              nrows = ceiling(length(outdesign()$book[,1])/input$ncols),
                              factor_name = "trt",
                              width = input$width,
                              height = input$height,
                              space_width = (1-input$space_width),
                              space_height = (1-input$space_height),
                              reverse_y = input$reverse_y,
                              reverse_x = input$reverse_x))
    }
    if(input$design == "design.cyclic"){

      plt <- (plot_cyclic(design = outdesign(),
                          factor_name = "trt",
                          width = input$width,
                          height = input$height,
                          space_width = (1-input$space_width),
                          space_height = (1-input$space_height),
                          reverse_y = input$reverse_y,
                          reverse_x = input$reverse_x))
    }
    if(input$design == "design.dau"){

      plt <- (plot_dau(design = outdesign(),
                       factor_name = "trt",
                       width = input$width,
                       height = input$height,
                       space_width = (1-input$space_width),
                       space_height = (1-input$space_height),
                       reverse_y = input$reverse_y,
                       reverse_x = input$reverse_x))
    }

    if(input$design == "design.lsd"){

      plt <- (plot_latin_square(design = outdesign(),
                                factor_name = "trt",
                                width = input$width,
                                height = input$height,
                                space_width = (1-input$space_width),
                                space_height = (1-input$space_height),
                                reverse_y = input$reverse_y,
                                reverse_x = input$reverse_x))
    }

    if(input$design == "design.rcbd"){

      plt <- (plot_rcdb(design = outdesign(),
                        factor_name = "trt",
                        width = input$width,
                        height = input$height,
                        space_width = (1-input$space_width),
                        space_height = (1-input$space_height),
                        reverse_y = input$reverse_y,
                        reverse_x = input$reverse_x))
    }

    if(input$design == "design.strip"){

      plt <- (plot_strip(design = outdesign(),
                         width = input$width,
                         factor_name_1 = "trt1",
                         factor_name_2 = "trt2",
                         height = input$height,
                         space_width = (1-input$space_width),
                         space_height = (1-input$space_height),
                         reverse_y = input$reverse_y,
                         reverse_x = input$reverse_x))
    }

    if(input$design == "design.youden"){

      plt <- (plot_youden(design = outdesign(),
                          factor_name = "trt",
                          width = input$width,
                          height = input$height,
                          space_width = (1-input$space_width),
                          space_height = (1-input$space_height),
                          reverse_y = input$reverse_y,
                          reverse_x = input$reverse_x))
    }

    if(input$design == "design.graeco"){

      plt <- (plot_graeco(design = outdesign(),
                          factor_name = "trt1",
                          width = input$width,
                          height = input$height,
                          space_width = (1-input$space_width),
                          space_height = (1-input$space_height),
                          reverse_y = input$reverse_y,
                          reverse_x = input$reverse_x))
    }

    if(input$design == "design.lattice" &&
       input$simple == TRUE){

      plt <- (plot_lattice_simple(design = outdesign(),
                                  factor_name = "trt",
                                  width = input$width,
                                  height = input$height,
                                  space_width = (1-input$space_width),
                                  space_height = (1-input$space_height),
                                  reverse_y = input$reverse_y,
                                  reverse_x = input$reverse_x))
    }

    if(input$design == "design.lattice" &&
       input$simple == FALSE){

      plt <- (plot_lattice_triple(design = outdesign(),
                                  factor_name = "trt",
                                  width = input$width,
                                  height = input$height,
                                  space_width = (1-input$space_width),
                                  space_height = (1-input$space_height),
                                  reverse_y = input$reverse_y,
                                  reverse_x = input$reverse_x))
    }

    if(input$design=="design.split" &&
       input$factorial_design=="crd"){
      plt <- (plot_split_crd(design = outdesign(),
                             nrows = ceiling(length(outdesign()$book[,1])/input$ncols),
                             ncols = input$ncols,
                             width = input$width,
                             height = input$height,
                             factor_name_1 = "trt1",
                             factor_name_2 = "trt2",
                             subplots = TRUE,
                             space_width = (1-input$space_width),
                             space_height = (1-input$space_height),
                             reverse_y = input$reverse_y,
                             reverse_x = input$reverse_x))

      if(input$swap_factor == FALSE){

        plt <- (plot_split_crd(design = outdesign(),
                               nrows = ceiling(length(outdesign()$book[,1])/input$ncols),
                               ncols = input$ncols,
                               width = input$width,
                               height = input$height,
                               factor_name_1 = "trt2",
                               factor_name_2 = "trt1",
                               subplots = TRUE,
                               space_width = (1-input$space_width),
                               space_height = (1-input$space_height),
                               reverse_y = input$reverse_y,
                               reverse_x = input$reverse_x))

      }
    }

    if(input$design=="design.split" &&
       input$factorial_design=="lsd"){
      plt <- (plot_split_lsd(design = outdesign(),
                             width = input$width,
                             height = input$height,
                             factor_name_1 = "trt1",
                             factor_name_2 = "trt2",
                             subplots = TRUE,
                             space_width = (1-input$space_width),
                             space_height = (1-input$space_height),
                             reverse_y = input$reverse_y,
                             reverse_x = input$reverse_x
      ))

      if(input$swap_factor == FALSE){
        plt <- (plot_split_lsd(design = outdesign(),
                               width = input$width,
                               height = input$height,
                               factor_name_1 = "trt2",
                               factor_name_2 = "trt1",
                               subplots = TRUE,
                               space_width = (1-input$space_width),
                               space_height = (1-input$space_height),
                               reverse_y = input$reverse_y,
                               reverse_x = input$reverse_x))
      }
    }

    if(input$design=="design.split" &&
       input$factorial_design=="rcbd"){
      plt <- (plot_split_rcbd(design = outdesign(),
                              width = input$width,
                              height = input$height,
                              factor_name_1 = "trt1",
                              factor_name_2 = "trt2",
                              subplots = TRUE,
                              space_width = (1-input$space_width),
                              space_height = (1-input$space_height),
                              reverse_y = input$reverse_y,
                              reverse_x = input$reverse_x
      ))

      if(input$swap_factor == FALSE){
        plt <- (plot_split_rcbd(design = outdesign(),
                                width = input$width,
                                height = input$height,
                                factor_name_1 = "trt2",
                                factor_name_2 = "trt1",
                                subplots = TRUE,
                                space_width = (1-input$space_width),
                                space_height = (1-input$space_height),
                                reverse_y = input$reverse_y,
                                reverse_x = input$reverse_x
        ))
      }
    }





    return(plt)
  })
  output$choosen_factor <- renderUI({
    numIndividuals <- as.integer(input$numIndividuals)
    selectInput(
      "choosen_factor",
      "Choose factor to plot the experiment",
      choices = LETTERS[1:numIndividuals]
    )
  })

  output$fields <- renderUI({
    numIndividuals <- as.integer(input$numIndividuals)
    lapply(1:numIndividuals, function(i) {
      selectizeInput(
        inputId = paste0("treatment", i),
        label = paste(
          "Type your intended levels of your Treatment,
          do not forget to include a control",
          i
        )
        ,
        choices = c("control")
        ,
        multiple = TRUE
        ,
        options = list(create = TRUE)
      )

    })
  })

  output$trt_duo <- renderUI({
    numIndividuals <- as.integer(2)
    lapply(1:numIndividuals, function(i) {
      selectizeInput(
        inputId = paste0("treatment_1", i),
        label = paste(
          "Type your intended levels of your Treatment",i,
          "do not forget to include a control if necessary"
        )
        ,
        choices = c("control")
        ,
        multiple = TRUE
        ,
        options = list(create = TRUE)
      )

    })
  })

  output$trt_single <- renderUI({
    selectizeInput(
      inputId = "treatment_single",
      label = "Type your intended levels of your Treatment,
               do not forget to include a control if necessary."
      ,
      choices = c("control")
      ,
      multiple = TRUE
      ,
      options = list(create = TRUE)
    )

  })

  output$oid1 <- renderDT({
    datatable(outdesign()$book, editable=FALSE)

  })

  output$oid2 <- renderDT({

    stats <- DOE_obj(plt())
    r <- to_table(stats,part = input$experimentparts, digits = input$digits)
    datatable(r, editable=FALSE)
  })

  output$doe_plot <- renderLeaflet({

    plt2 <-
      leaflet() %>% addTiles() %>% addPolygons(
        data = polygons_list(),
        weight = 1,
        fillColor = polygons_list()@data$fill,
        opacity = 1,
        fillOpacity = 1,
        label = c(polygons_list()@data$plot_nr)
      ) %>% addProviderTiles(input$maplayer)

    plt2


  })

  output$doe_plot_map <- renderPlot({
    plt3 <- plt() + theme_pres() + scale_fill_viridis_d(option = input$colorOption)+
    theme(panel.background = element_rect(fill = input$background))
    plt3
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("dataset-", Sys.Date(), input$design, ".csv", sep = "")
    },
    content = function(filename) {
      write.csv(outdesign()$book, file = filename)
    }
  )

  output$downloadParams <- downloadHandler(
    filename = function() {
      paste0("model_Parameter", Sys.Date(), input$design, ".json", sep = "")
    },
    content = function(filename) {
      param_list <- outdesign()$parameter
      param_list$northing <- input$northing
      param_list$easting <- input$easting
      param_list$rotation <- input$rotation
      param_list$design <- input$design
      param_list$first <- input$first
      param_list$randomization <- input$randomization
      param_list$numIndividuals <- input$numIndividuals
      param_list$r <- input$r
      param_list$k <- input$k
      param_list$maxrep <- input$maxrep
      param_list$kind <- input$kind
      param_list$treatment_11 <- input$treatment_11
      param_list$treatment_12 <- input$treatment_12
      param_list$treatment_13 <- input$treatment_13
      param_list$treatment_14 <- input$treatment_14
      param_list$treatment_15 <- input$treatment_15
      param_list$treatment1 <- input$treatment1
      param_list$treatment2 <- input$treatment2
      param_list$treatment3 <- input$treatment3
      param_list$treatment4 <- input$treatment4
      param_list$treatment5 <- input$treatment5
      param_list$trt <- input$treatment_single
      param_list$width <- input$width
      param_list$height <- input$height
      param_list$space_width <- input$space_width
      param_list$space_height <- input$space_height
      param_list$reverse_y <- input$reverse_y
      param_list$reverse_x <- input$reverse_x
      param_list$rowcol <- input$rowcol
      param_list$colorOption <- input$colorOption
      param_list$background <- input$background

      json_obj <- jsonlite::toJSON(param_list)
      write(json_obj, filename)
    }
  )


  observeEvent(
    input$cloudUpload,
    {dat <- layer_data(plt())[, -(6:9)]
    drv <- DBI::dbDriver(RPostgres::Postgres())
    con <-
      dbConnect(
        drv,
        dbname = input$dbname,
        user = input$user,
        password = input$password,
        host = input$host,
        port = input$port
      )
    dbWriteTable(
      conn = con,
      value = dat,
      name = input$tablename,
      overwrite = TRUE
    )
    #dbWriteTable(conn=con,value=df_hex,name=input$tablename,overwrite =TRUE)
    dbDisconnect(con)

    showModal(
      modalDialog(
        title = "Data are written to Cloud Database",
        paste0(
          "The experiment design is successfully
                       written to cloud."
        ),
        easyClose = TRUE,
        footer = NULL
      )
    )
    }
  )
  # observeEvent(input$ISOBUS, {
  #   sf_df <- st_as_sf(polygons_list())
  #   print(polygons_list())
  #   st_crs(sf_df) <- 4326
  #   sf_df <- st_transform(sf_df, crs = 4326)
  #
  #   xml_doc <- xml_new_root("ISO11783_WorkingSet",
  #                           .namespace = "http://www.w3.org/2001/XMLSchema-instance",
  #                           `xsi:noNamespaceSchemaLocation` = "ISO11783-10.xsd")
  #
  #   working_set <- xml_add_child(xml_doc, "WorkingSet")
  #   device <- xml_add_child(working_set, "Device")
  #   sf_df$id <- 1:nrow(sf_df)
  #   # Iterate over each row in the sf dataframe
  #   for (i in 1:nrow(sf_df)) {
  #     id <- sf_df$id[i]
  #     name <- sf_df$name[i]
  #     coords <- st_coordinates(sf_df[i, ])
  #
  #     device_element <- xml_add_child(device, "DeviceElement", id = as.character(id), name = name)
  #     spatial_data <- xml_add_child(device_element, "SpatialData")
  #     polygon <- xml_add_child(spatial_data, "Polygon", id = as.character(id), name = name)
  #     coordinate_list <- xml_add_child(polygon, "CoordinateList")
  #
  #     # Add coordinates to the XML
  #     for (j in 1:nrow(coords)) {
  #       xml_add_child(coordinate_list, "Coordinate",
  #                     latitude = as.character(coords[j, "Y"]),
  #                     longitude = as.character(coords[j, "X"]))
  #     }
  #   }
  #
  #
  #   # Save the XML to a file
  #   write_xml(xml_doc, paste0(input$tablename,".xml"))
  #
  #   showModal(
  #     modalDialog(
  #       title = "ISOBUS Export Success!",
  #       paste0(
  #         "The experiment design is successfully written to ",
  #         input$tablename,
  #         '.xml .'
  #       ),
  #       easyClose = TRUE,
  #       footer = NULL
  #     )
  #   )
  # })

  observeEvent(input$saveSQL, {
    dat <- layer_data(plt())[, -(6:9)]
    drv <- DBI::dbDriver("PostgreSQL")
    con <-
      dbConnect(
        drv,
        dbname = input$dbname,
        user = input$user,
        password = input$password,
        host = input$host,
        port = input$port
      )
    dbWriteTable(
      conn = con,
      value = dat,
      name = input$tablename,
      overwrite = TRUE
    )
    #dbWriteTable(conn=con,value=df_hex,name=input$tablename,overwrite =TRUE)
    dbDisconnect(con)

    showModal(
      modalDialog(
        title = "Data are written to PostgreSQL.",
        paste0(
          "The experiment design is successfully written to ",
          input$dbname,
          " in table ",
          input$tablename,
          '.'
        ),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })


  output$downloadpdf <- downloadHandler(
    filename = function() {
      paste0(
        "TreatmentPlan",
        Sys.Date(),
        input$design,
        "_",
        input$choosen_factor,
        ".pdf",
        sep = ""
      )
    },
    content = function(filename) {
      ggsave(
        filename = filename,
        device = "pdf",
        width = 130 ,
        height = 130,
        units = "mm"
      )
    }
  )

  polygons_list <- reactive({
    plt2 <- plt() + scale_fill_viridis_d(option = input$colorOption)
    polygons_list <-  make_polygons(plt2, north = input$northing,
                                    east = input$easting)

    polygons_list <- sf::as_Spatial(polygons_list)

    row.names(polygons_list) <- rownames(polygons_list@data)

    polygons_list <-
      sp::elide(polygons_list,
                rotate = input$rotation,
                center = apply(bbox(polygons_list), 1, mean))

  })

  output$exportgeojson <- downloadHandler(
    filename = function() {
      paste0("TreatmentPlan_", Sys.Date(), "_", input$design, "_", input$tablename, ".geojson")
    },
    content = function(file) {
      df_hex <- st_as_sf(polygons_list())  # Convert to sf object
      st_write(df_hex, file, driver = "GeoJSON")  # Save as GeoJSON
    }
  )

  output$package_list <- renderDT({
    pckg <- data.frame(installed.packages())
    pck_df <- pckg[pckg$Package %in% .packages(),c(3,10:12)]
    })


  observeEvent(input$tabs_a, {
    if (input$tabs_a == "about") {
      showModal(modalDialog(
        title = "Über diese Anwendung",

        # Liste der verwendeten R-Pakete
        h4("Verwendete R-Pakete:"),
        DTOutput(session$ns("package_list")),

        # Lizenzinfo
        h4("Lizenz:"),
        p("Diese Anwendung steht unter Lizenz."),

        # Link zur Website
        h4("Mehr Informationen:"),
        tags$a(href = "https://www.deine-webseite.de", "Besuche meine Website", target = "_blank"),

        easyClose = TRUE,
        footer = modalButton("Schließen")
      ))
    }
  })





  # output$makeLabels <- downloadHandler(
  #   filename = function() {
  #     paste0(input$output_file, ".pdf")
  #   },
  #   content = function(file) {
  #     generate_qr_labels(input_df = outdesign$book(),
  #                                     output_file = file,
  #                                     experiment_name = input$experiment_name,
  #                                     img_width = input$img_width,
  #                                     img_height = input$img_height,
  #                                     page_width = input$page_width,
  #                                     page_height = input$page_height)
  #   }
  # )
}



# Create Shiny app ----
shinyApp(ui, server)

