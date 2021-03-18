#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#URL: https://tzwwangucdavis.shinyapps.io/Final_Project/

library(shiny)
library(nycflights13)
library(tidyverse)
library(dplyr)
library(httr)
library(jsonlite)
library(rvest)

main_url = "https://covid-api.mmediagroup.fr/v1/cases"
a = fromJSON((main_url))
countrylist = names(a) 



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Data of Covid-19 among the World"),

    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "Country",
                        label = "Select a country:",
                        choices = countrylist,
                        selected = "Global"),
            selectInput(inputId = "Status",
                        label = "Select one of the status:",
                        choices =c("Confirmed", "Deaths", "Recovered")),
        ),
        mainPanel(
            textOutput("summary_title"),
            tableOutput("summary"),
            plotOutput("dataPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    #get the total confirmed, recovered, death data of selected country
    caseData = reactive({
        r <- GET("https://covid-api.mmediagroup.fr/v1/cases?",
                 query = list(country = input$Country))
        json <- fromJSON(content(r, as = "text", encoding = "UTF-8"))
        global_covid_data <- as.data.frame(json$All)
        wanted_data = data.frame(Confirmed = global_covid_data$confirmed, 
                                 Recovered = global_covid_data$recovered,
                                 Death = global_covid_data$deaths)
        return(wanted_data)
    })
    #get every day data of selected status
    Country_History_Data <- reactive({
        r <- GET("https://covid-api.mmediagroup.fr/v1/history?",
                 query = list(country = input$Country, status = input$Status))
        History_Data <- fromJSON(content(r, as = "text", encoding = "UTF-8"))
        History <- data.frame(date = names(History_Data$All$dates), 
                        number = as.numeric(unlist(History_Data$All$dates)))
        History$time <- rev(1:nrow(History))
        return(History)
    })
    
    Table_Name = reactive({
        text = paste("Summary data of", input$Country)
        return(text)
        })
    output$summary_title = renderText({
        Table_Name()
    })
    output$summary = renderTable({
        caseData()
    })
    
    output$dataPlot <- renderPlot({
        ggplot(Country_History_Data(), aes(x = time, y = number)) + 
            geom_line() +
            ggtitle(paste0("The cumulative ", input$Status, " in ", 
                           input$Country, " from 2020-01-22 to today")) + 
                           ylab(paste0(input$Status, " count")) + xlab("Day")
            
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
