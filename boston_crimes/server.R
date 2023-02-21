# Define server logic required to draw a histogram
function(input, output, session) {

  
  clean_df_top10 <- filter(clean_df, OFFENSE_CODE_GROUP == top10crimes$OFFENSE_CODE_GROUP)
  
  data <- subset(clean_df_top10, OFFENSE_CODE_GROUP == "Simple Assault") #input$country
  
  clean_df_top10 %>%
    subset(OFFENSE_CODE_GROUP == "Simple Assault" && ) %>%
    group_by(YEAR, DISTRICT) %>%
    summarise(counts = n())
  
  output$test_plot <- renderPlot({})
}
