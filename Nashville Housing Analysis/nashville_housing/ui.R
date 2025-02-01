# Define UI

ui <- page_fluid(
  theme = bs_theme(bootswatch = "litera"),
  
  h1("Nashville Real Estate Deal Analyzer"),
  #title = "Nashville Real Estate Deal Analyzer",
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
          card_header("Deal Success by City"),
          plotOutput("good_deal_rate_plot")
        )
      ),
      layout_columns(
        card(
          card_header("Property Age by Deal Success Rate"),
          plotOutput("age_by_deal_rate_plot")
        ),
        card(
          card_header("Property Age by Sale Price"),
          plotOutput("age_by_price_plot")
        )
      ),
      card(
        card_header("Key Summary Statistics Table"),
        DTOutput("summary_table")
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
          # Price inputs
          numericInput("sale_price", "Sales Price ($)", 
                       value = 300000, min = 0, step = 1000),
          numericInput("total_value", "Total Value ($)", 
                       value = 350000, min = 0, step = 1000),
          selectInput("property_city", "City:", choices = unique(df$Property.City)),
          numericInput("year_built", "Year Built:", value = 2000, min = 1950, max = 2023),
          numericInput("bedrooms", "Bedrooms:", value = 3, min = 1, max = 6),
          numericInput("full_bath", "Bathrooms:", value = 2, min = 1, max = 4),
          numericInput("half_bath", "Half Baths:", value = 0, min = 0, max = 2),
          #numericInput("pred_sqft", "Square Feet:", value = 2000, min = 800, max = 5000),
          #actionButton("predict", "Analyze Deal", class = "btn-primary")
        ),
        layout_columns(
          fill = FALSE,
          value_box(
            title = "ML Model Prediction",
            value = textOutput("probability"),
            showcase = bsicons::bs_icon("robot"),
            theme = "primary",
            full_screen = TRUE
          ),
          value_box(
            title = "Potential Savings",
            value = textOutput("savings"),
            showcase = bsicons::bs_icon("piggy-bank"),
            full_screen = TRUE
          )
        ),
        card(
          card_header("Deal Analysis Dashboard"),
          layout_columns(
            card(
              plotOutput("probability_gauge")
            ),
            card(
              "ML Model Insights",
              textOutput("insights"),
              hr(),
              "This prediction is based on a machine learning model trained on historical real estate data, considering multiple factors including market trends, property characteristics, and location impacts."
            )
          )
        ),
        card(
          card_header("Model Feature Importance"),
          plotOutput("feature_importance")
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