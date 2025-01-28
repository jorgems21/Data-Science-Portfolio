# Define UI
# ui <- fluidPage(
#   titlePanel("Nashville Housing Deal Predictor"),
#   sidebarLayout(
#     sidebarPanel(
#       numericInput("sale_price", "Sale Price:", value = 200000, min = 10000, max = 1000000, step = 1000),
#       numericInput("total_value", "Total Value:", value = 250000, min = 10000, max = 1000000, step = 1000),
#       selectInput("property_city", "City:", choices = unique(df$Property.City)),
#       numericInput("year_built", "Year Built:", value = 2000, min = 1900, max = 2023),
#       numericInput("bedrooms", "Bedrooms:", value = 3, min = 1, max = 10),
#       numericInput("full_bath", "Full Bath:", value = 2, min = 1, max = 5),
#       numericInput("half_bath", "Half Bath:", value = 1, min = 0, max = 5),
#       actionButton("predict", "Predict")
#     ),
#     mainPanel(
#       textOutput("prediction_result")
#     )
#   )
# )

ui <- page_fluid(
  theme = bs_theme(bootswatch = "morph"),
  
  h1("Nashville Real Estate Deal Analyzer"),
  
  navset_card_tab(
    # Overview Tab
    nav_panel(
      title = "Overview",
      card(
        card_header("About This Application"),
        markdown("
        ### Nashville Real Estate Deal Analyzer
        
        This application helps real estate investors analyze potential deals using:
        
        * Historical property data analysis
        * Deal prediction tools
        * Comprehensive reporting
        
        The data used in this application includes properties from major US cities with
        various characteristics including size, location, and pricing information.
        ")
      )
    ),
    
    # Descriptive Analysis Tab
    nav_panel(
      title = "Descriptive Analysis",
      layout_columns(
        card(
          card_header("Price Distribution by City"),
          plotOutput("price_dist_plot")
        ),
        card(
          card_header("Year Built vs Price"),
          plotOutput("year_price_plot")
        )
      ),
      card(
        card_header("Property Details Table"),
        DTOutput("property_table")
      )
    ),
    
    # Deal Prediction Tab
    nav_panel(
      title = "Deal Prediction",
      layout_sidebar(
        sidebar = sidebar(
          selectInput("property_city", "City:", choices = unique(df$Property.City)),
          numericInput("year_built", "Year Built:", value = 2000, min = 1950, max = 2023),
          numericInput("bedrooms", "Bedrooms:", value = 3, min = 1, max = 6),
          numericInput("full_bath", "Bathrooms:", value = 2, min = 1, max = 4),
          numericInput("half_bath", "Half Baths:", value = 0, min = 0, max = 2),
          #numericInput("pred_sqft", "Square Feet:", value = 2000, min = 800, max = 5000),
          actionButton("predict", "Analyze Deal", class = "btn-primary")
        ),
        card(
          card_header("Deal Analysis Results"),
          textOutput("prediction_result"),
          plotOutput("comparison_plot")
        )
      )
    ),
    
    # Report Generation Tab
    nav_panel(
      title = "Generate Report",
      card(
        card_header("Download Analysis Report"),
        downloadButton("download_report", "Download Report")
      )
    )
  )
)