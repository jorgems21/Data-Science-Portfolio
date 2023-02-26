# Define server logic required to draw a histogram
function(input, output, session) {

# Set reactives
data <- reactiveValues()
data$general <- data.frame()
data$general_districts <- data.frame()

clean_df_top10 <- filter(clean_df, OFFENSE_CODE_GROUP == top10crimes$OFFENSE_CODE_GROUP)

observeEvent(input$run, {
  
  data$general <- clean_df_top10 %>%
    filter(OFFENSE_CODE_GROUP %in% input$crimes) %>%
    #filter(DISTRICT %in% input$districts) %>%
    group_by(OFFENSE_CODE_GROUP, YEAR) %>%
    summarise(counts = n())
  
  data$general_districts <- clean_df_top10 %>%
    filter(OFFENSE_CODE_GROUP %in% input$crimes) %>%
    filter(DISTRICT %in% input$districts) %>%
    group_by(YEAR, DISTRICT) %>%
    summarise(counts = n())
  
  
})

output$test_plot <- renderPlot({
  data$general %>%
    ggplot(aes(x=YEAR, y=counts, group = OFFENSE_CODE_GROUP, color = OFFENSE_CODE_GROUP)) +
    geom_line() +
    #geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
    #theme_ipsum() +
    ggtitle("Crime per Year")
})

output$test_plot_2 <- renderPlot({
  data$general_districts %>%
    ggplot(aes(x=YEAR, y=counts, group = DISTRICT, color = DISTRICT)) +
    geom_line() +
    #geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
    #theme_ipsum() +
    ggtitle("Crime per Year across Districts")
})
  
  
  test_df <- clean_df_top10 %>%
    filter(OFFENSE_CODE_GROUP %in% c("Simple Assault",
                                   "Larceny")) %>%
    filter(DISTRICT %in% c("Downtown", "Brighton")) %>%
    group_by(YEAR,DISTRICT ) %>%
    summarise(counts = n())
  # 
   test_df %>%
     ggplot(aes(x=YEAR, y=counts, group = DISTRICT, color = DISTRICT)) +
        geom_line() +
        #geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
        #theme_ipsum() +
        ggtitle("Crime per Year")
    
  # 
  # clean_df_top10 %>%
  #   filter(OFFENSE_CODE_GROUP %in% c("Simple Assault",	
  #                                    "Larceny")) %>%
  #   filter(DISTRICT %in%  c("Downtown", "Brighton")) %>%
  #   group_by(OFFENSE_CODE_GROUP, YEAR, DISTRICT) %>%
  #   summarise(counts = n()) %>%
  #   ggplot( aes(x=YEAR, y=counts, group = OFFENSE_CODE_GROUP, color = OFFENSE_CODE_GROUP)) +
  #   geom_line() +
  #   #geom_line( color="grey") +
  #   #geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  #   #theme_ipsum() +
  #   ggtitle("Crime per Year")
  

}
