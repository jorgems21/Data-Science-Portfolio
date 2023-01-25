library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggvis)
library(RColorBrewer)
library(DT)
library(tidyr)
library(reshape2)
library(ggthemes)
library(zoo)
library(RColorBrewer)
library(ggmap)
library(shinydashboard)
library(dplyr)


e <- CleanedCrimes #Cleaned crimes is the final cleaned dataset

offenseCodeGrp <- unique(e$OFFENSE_CODE_GROUP)
districts <- unique(e$NAMED_DISTRICT)
hours <- unique(e$HOUR)
streets <- c("WASHINGTON ST", "BLUE HILL AVE", "BOYLSTON ST", "HARRISON AVE" , 
             "MASSACHUSETTS AVE", "CENTRE ST", "TREMONT ST", "HYDE PARK AVE", 
             "COMMONWEALTH AVE", "TREMONT ST")

api_key <- "AIzaSyBrC0xqUtMTASlZozbSR51IJSSUCfTixPQ"
register_google(api_key)


ui = dashboardPage(
  dashboardHeader(title="Boston Crimes Relating to Public Safety"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem(tabName = "main1", "Crime over District", icon = icon("chart-bar")),
      menuItem(tabName = "main", "Crime by Hour", icon = icon("chart-bar")),
      menuItem(tabName = "main3", "Map", icon = icon("map")),
      menuItem(tabName = "main2", "Data Repository", icon = icon("table"))
      
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(
        tabName = "main",
        fluidRow(  
          column (5,
                  box (width=12,
                       title = "Density of Crimes Per Hour for Various Districts",
                       color = "blue", 
                       selectInput("variable", "Districts:", choices = sort(districts), multiple = TRUE),
                       hr(),
                  ),
                  box(width = 12,
                      title = "Type of Crime Over Hours(Shooting Incl.)",
                      color = "blue", ribbon = TRUE, title_side = "top left",
                      selectInput("dist", "Choose District:", choices = sort(districts)),
                      hr(),
                  ),
          ), 
          column (7,
                  box(width = 12,
                      title = "Density and Count by Hour",
                      color = "blue", ribbon = TRUE, title_side = "top right",
                      column(width = 12, height="auto",
                             plotOutput("lineplot"),
                             plotOutput("plotShooting")
                      )
                  ),
          ),
        ),
      ),
      
      tabItem(
        tabName = "main1",
        fluidRow(
          column(5,
            box(width = 15,
                title = "Number and Type of Crime by District",
                color = "green", ribbon = TRUE, title_side = "top right",
                height = "1000",
                checkboxGroupInput("checkGroup", 
                                   h3("Checkbox group"),
                                   choices = offenseCodeGrp,
                                   selected = 1),
            ),
          ),
          column(7,
                 box(width = 20,
                     title = "Rate of Incident over District", ribbon = TRUE, title_side = "top right",
                     column(width = 12, height = "auto", plotOutput("plot"))
                 ),
          ),
        ),
      ),

      tabItem(
        tabName = "main3",
        fluidRow(
          column (5,
            box(width = 15,
                title = "Violent crimes in Boston",
                color = "green", ribbon = TRUE, title_side = "top right",
                height = "1000",
                selectInput("District", "Please Select a District",
                            choices=sort(districts),
                            multiple=TRUE),
                selectInput("OffenseType", "Please Select an Offense Type",
                            choice=sort(offenseCodeGrp),
                            multiple=TRUE),
                selectInput("Time","Please Select a Time",
                            choice=hours,
                            multiple=TRUE),
                selectInput("Shooting", "Shooting Occurred?",
                            choice=c("Yes", "No"),
                            multiple=TRUE),
                selectInput("Street", "Please Select a Street",
                            choice=sort(streets),
                            multiple=TRUE),
            ),
          ),
          column(7,
                 box(width = 20,
                     title = "Map of Boston", ribbon = TRUE, title_side = "top right",
                     column(width = 12, height = "auto", plotOutput("map"))
                 ),
        ),
      ),
    ),
    tabItem(
      tabName = "main2",
      fluidRow(
        box(title = "Data Repository", color = "blue", ribbon = TRUE, 
            width = 12,status = "success", height = "575",
            solidHeader = T, dataTableOutput("crimetable")
        ),
      ),
    )
   ),
  ),
)

get_data <- function(input){
  
  filter_names <- c("District", "OffenseType", "Shooting", "Time", "Street")
  col_names_for_filters <- c("NAMED_DISTRICT", "OFFENSE_CODE_GROUP", "SHOOTING", "HOUR", "STREET")
  
  current_data <- e
  
  for (index in c(1:length(filter_names))) {
    cur_filter <- input[[filter_names[index]]]
    col_name <- col_names_for_filters[index]
    if (length(cur_filter) > 0) {
      current_data <- current_data[current_data[[col_name]] %in% cur_filter, ]
    }
  }
  return(current_data)
}

# Server 
server = shinyServer(function(input, output, session) {

  data <- reactive({
    req(input$variable)
    df <- e %>% filter(NAMED_DISTRICT %in% input$variable) %>% group_by(HOUR)
  })
  
  dataOp <- reactive({
    req(input$checkGroup)
    df <- e %>% filter(OFFENSE_CODE_GROUP %in% input$checkGroup) %>% group_by(NAMED_DISTRICT)
  })
  
  dataPltSht <- reactive({
    req(input$dist)
    df <- e %>% filter(NAMED_DISTRICT %in% input$dist)
  })
  
  output$plotShooting<- renderPlot({
    ggplot(dataPltSht(),aes(HOUR, fill=SHOOTING))+geom_bar(stat = "count")+
      scale_x_continuous(breaks = seq(0, 23, 1))+
      theme_bw()
  })
  
  output$plot <- renderPlot({
    g <- ggplot(dataOp(), aes(NAMED_DISTRICT, fill= OFFENSE_CODE_GROUP))
    g + geom_bar(stat = "count") + theme(axis.text.x = element_text(angle = 90)) + 
      xlab("Districts")+
      ylab("Number of Incidents")
  })
  
  output$lineplot <- renderPlot({
    ggplot(data(), aes(HOUR, fill = NAMED_DISTRICT))+
      geom_density(alpha = 0.3)+
      xlab("Hour of the day")+
      ylab("Density of incidents")+
      scale_x_continuous(breaks = seq(0, 23, 1))+
      theme_bw()
  })
  
  output$map <- renderPlot({
    data <- get_data(input)
    mapdata <- data%>%filter(!is.na(Lat),!is.na(Long))
    boston <- qmap(c(lon=mean(mapdata$Long), lat=mean(mapdata$Lat)), zoom=12)
    geom <- boston + geom_point(aes(x=Long, y=Lat), data=mapdata, size=3, alpha=0.2, color="cyan4")+
      theme_void()
    plot(geom, height = 400, width = 600 )
    
  })
  
  output$crimetable <- renderDT(datatable(e, options = list(searching = TRUE, pageLength = 5,
                                                        lengthMenu = c(5, 10, 15, 20), 
                                                        scrollX = T)))
})

shinyApp(ui=ui, server=server)