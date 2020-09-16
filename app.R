library(Rmisc)
library(foreign)
library(memisc)
library(tidyverse)
library(readxl)
library(lubridate)
library(chron)
library(shiny)

# load and tidy data
df <- read_xlsx("PaceCountData_1Sept.xlsx")

# remove outliers
df <- df %>% filter(ifelse(day_night=="day", pacecount<80, pacecount <90), length=="long")

#Lists
day_night <- c("day", "night")
load_noload <- c("load", "noload")

#Modeling
#Day/noload model
df3a <- df %>% filter(day_night=="day", loadcarrying=="noload", length=="long" ) %>% 
    na.omit()
summary(df3a)
lm1 <-  lm(pacecount ~ height + duration, data=df3a)
summary(lm1)
lm1_duration <- median(df3a$duration)

#Day/load model
df3b <- df %>% filter(day_night=="day", loadcarrying=="load", length=="long" )%>% na.omit()
lm2 <-  lm(pacecount ~ height + duration , data=df3b)
summary(lm2)
lm2_duration <- median(df3b$duration)

#Night/Noload model
df3c <- df %>% filter(day_night=="night", loadcarrying=="noload", length=="long" )%>% na.omit()
lm3 <-  lm(pacecount ~ height + duration , data=df3c)
summary(lm3)
lm3_duration <- median(df3c$duration)

#Night/load model
df3d <- df %>% filter(day_night=="night", loadcarrying=="load", length=="long" )%>% na.omit()
lm4 <-  lm(pacecount ~ height + duration , data=df3d)
summary(lm4)
lm4_duration <- median(df3d$duration)

#Data table based on height and model output
height <- c(62:76) 
height <- data.frame(height)
duration <- lm1_duration
pacecount_table1 <- data.frame(height, duration)
day_noload <- round(predict(lm1, pacecount_table1), 0)

duration <- lm2_duration
pacecount_table2 <- data.frame(height, duration)
day_load <- round(predict(lm2, pacecount_table2), 0)

duration <- lm3_duration
pacecount_table3 <- data.frame(height, duration)
night_noload <- round(predict(lm3, pacecount_table3),0)

duration <- lm4_duration
pacecount_table4 <- data.frame(height, duration)
night_load <- round(predict(lm4, pacecount_table4),0)

pacecount_table_final <- height %>% cbind(day_noload, day_load, night_noload, night_load)

# Define UI for application 
ui <- 
    
    dashboardPage (
        dashboardHeader(title="Pace Count Estimator"),

    dashboardSidebar(
       # sliderInput("pacecount", "Input: 100m Day Pacecount",min = 55, max = 80, value = 68, step = 1),
        sliderInput("height", "height",min = 60, max = 80, value = 70, step = 1),
        sliderInput("weight", "weight",min = 115, max = 250, value = 180, step = 5),
        checkboxGroupInput(inputId = "day_night", label =  "Day/Night filter", choices = day_night, selected = day_night ),
        checkboxGroupInput(inputId = "load_noload", label =  "Load/Noload filter", choices = load_noload, selected = load_noload )
        ),
    
    dashboardBody(
                   
                    fluidRow(
                        
                        box(title="Day PaceCount Estimates (100m)", status="primary", solidHeader = TRUE,
                            valueBoxOutput("day_pacecount1"),
                            valueBoxOutput("day_pacecount2")),
                        
                        box(title="Night PaceCount Estimates (100m)", status="primary", solidHeader = TRUE,
                            valueBoxOutput("night_pacecount1"),
                            valueBoxOutput("night_pacecount2")) 
                    ),
                    fluidRow(
                        wellPanel (title="Pacecount Plot", plotOutput("plot") ),
                    ),
                    ))


# Define server logic required to draw a histogram
server <- function(input, output) {

output$day_pacecount1 <- renderValueBox({
        valueBox(round(predict(lm1, data.frame(height=input$height, duration=lm1_duration)),0), "Day Pacecount (No load)", icon=icon("walking"), color="green")
    })
    
output$day_pacecount2 <- renderValueBox({
        valueBox(round(predict(lm2, data.frame(height=input$height, duration=lm2_duration)),0), "Day Pacecount (with Rucksack)", icon=icon("hiking"), color="red")
    })
    
output$night_pacecount1 <- renderValueBox({
        valueBox(round(predict(lm3, data.frame(height=input$height, duration=lm3_duration)),0), "Night Pacecount (No load)", icon=icon("walking"), color="green")
    })
    
output$night_pacecount2 <- renderValueBox({
        valueBox(round(predict(lm4, data.frame(height=input$height, duration=lm4_duration)),0), "Night Pacecount (with Rucksack)", icon=icon("hiking"), color="red")
    })
    
#reactive dataframe
df_plot <- reactive (
    {pacecount_table_final %>% 
            gather (day_noload:night_load, key=Category, value=pacecount) %>% 
            separate(Category, into = c("day_night", "loadcarrying"), sep="_", remove=TRUE) %>% 
            filter(day_night %in% input$day_night & loadcarrying %in% input$load_noload)
})

output$plot <- renderPlot ({
        
        df_plot () %>%  ggplot(aes(y=pacecount, x=height, color=loadcarrying)) + 
        geom_point() + 
        geom_vline(xintercept = input$height, linetype="dashed", color="darkgray") +
        geom_text(aes(x = input$height, y=80, label="your height", angle=15), color="blue") +
            theme(text = element_text(size = 10))     +
            xlab("Height (inches)") +
            ylab("100m Pace Count") +
            scale_color_manual (values=c("tomato", "green")) +
            facet_grid(.~day_night) 
               })
}
 
# Run the application 
shinyApp (ui = ui, server = server)
