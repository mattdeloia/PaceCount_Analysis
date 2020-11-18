library(Rmisc)
library(foreign)
library(memisc)
library(tidyverse)
library(readxl)
library(lubridate)
library(chron)
library(shiny)
library(shinythemes)
library(shinydashboard)

# load and tidy data
dfa <- read_csv("pacecount_table_advanced.csv") %>% as.data.frame() %>% mutate(experience="advanced") 
dfb <- read_csv("pacecount_table_intermediate.csv") %>% as.data.frame() %>% mutate(experience="intermediate")
dfc <- read_csv("pacecount_table_beginner.csv") %>% as.data.frame() %>% mutate(experience="beginner")
df <- dfa %>% rbind(dfb) %>% rbind(dfc) %>% as.data.frame()

# Define UI for application 
ui <- 
    
    dashboardPage (
        dashboardHeader(title="Pace Count Estimator"),

    dashboardSidebar(
        sidebarMenu(
        menuItem("Estimator" , tabName = "estimator", icon = icon("dashboard")),
        menuItem("About", tabName = "about", icon = icon("info")) ),
        sliderInput("height", "Height (inches)",min = 58, max = 76, value = 70, step = 1),
        radioButtons(inputId = "experience", label =  "Experience Level", choices = list("beginner" = "beginner", "intermediate" = "intermediate", "advanced" = "advanced"), selected = "intermediate" ),
        helpText("BEGINNER - limited confidence and land navigation training experience."),
        helpText("INTERMEDIATE - a number of repititions of day and night land navigation and confidence in ones ability."),
        helpText("ADVANCED - years of experience and complete confidence in day and night navigation ability.")
        ),
    
    dashboardBody(
        tabItems (
                  tabItem("estimator",
                    fluidRow(
                        
                        box(title="Day PaceCount Estimates (100m)", status="primary", solidHeader = TRUE,
                            valueBoxOutput("day_pacecount1"),
                            valueBoxOutput("day_pacecount2")),
                        
                        box(title="Night PaceCount Estimates (100m)", status="primary", solidHeader = TRUE,
                            valueBoxOutput("night_pacecount1"),
                            valueBoxOutput("night_pacecount2")) 
                    ),
                    fluidRow(
                        wellPanel (title="Pacecount Plot", plotOutput("plot") )
                    )),
                    tabItem("about",
                            h1("About")) ) 
                    ))


# Define server logic required to draw a histogram
server <- function(input, output) {

    observe({
        showNotification("FOR DEMONSTRATION PURPOSE ONLY  //  PLEASE CLOSE SESSION UPON COMPLETION OF DEMO   //  PLEASE ADDRESS QUESTIONS TO MATTHEW.DELOIA@NGC.COM", duration = 10, type="error") })    
    
dfy <- reactive({
          df %>% filter(experience==input$experience) 
     })

dfx <- reactive({
     df %>% filter(experience==input$experience, height==input$height) 
     })

output$day_pacecount1 <- renderValueBox({
        valueBox(dfx()$day_noload , "Day Pacecount (No load)", icon=icon("walking"), color="green")
    })
    
output$day_pacecount2 <- renderValueBox({
     valueBox(dfx()$day_load, "Day Pacecount (with Rucksack)", icon=icon("hiking"), color="red")
})

output$night_pacecount1 <- renderValueBox({
     valueBox(dfx()$night_noload, "Night Pacecount (No load)", icon=icon("walking"), color="green")
})

output$night_pacecount2 <- renderValueBox({
     valueBox(dfx()$night_load, "Night Pacecount (with Rucksack)", icon=icon("hiking"), color="red")
})

#reactive dataframe
df_plot <- reactive (
     {dfy() %>% 
               gather (day_noload:night_load, key=Category, value=pacecount) %>% 
               separate(Category, into = c("day_night", "loadcarrying"), sep="_", remove=TRUE) 
     })

output$plot <- renderPlot ({
     
     df_plot () %>%  ggplot(aes(y=pacecount, x=height, color=day_night)) + 
          geom_point(size=3) + 
          geom_vline(xintercept = input$height, linetype="dashed", color="red") +
        geom_line(aes(linetype=loadcarrying, color=day_night), size=1) +
          geom_text(aes(x = input$height, y=80, label="your height", angle=15, hjust=-.7, vjust=-15), color="red") +
        ylim(60,100) +
          theme(text = element_text(size = 10))     +
        scale_linetype_manual(values=c("solid", "dashed")) +
          xlab("height (inches)") +
          ylab("Pace Count") +
          scale_color_manual (values=c("lightgreen", "blue")) })
}
 
# Run the application 
shinyApp (ui = ui, server = server)
