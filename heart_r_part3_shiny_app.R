
####
#Projekt: Czynniki wplywajace na wystepowanie choroby serca
#
#Autor: Zuzanna Kontna
#
#Studia Podyplomowe: Data Scientist. Analityk danych
#Uniwersytet WSB Merito Gdańsk
#kwiecien, 2023
####

# import bibliotek

library(shiny)
library(DT)
library("lubridate") 
library(randomForest)

#### Aplikacja Shiny

# import modelu
model_1 = readRDS('C:/Users/zuza2/OneDrive/Pulpit/podyplomowka/projekt/model_r.rda')

#---- UI -----

ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4('Projekt: Czynniki wplywajace na wystepowanie choroby serca'),
      h4('Autor: Zuzanna Kontna'),
      h5('Studia Podyplomowe: Data Scientist. Analityk danych'),
      h5('Uniwersytet WSB Merito Gdańsk'),
      h6('czerwiec 2023')
      
    ),
    
    mainPanel(
      
      h1('Sprawdz, czy masz zdrowe serce!'),
      hr(),
      br(),
      
      ###
      
      # cholesterol
      radioButtons(inputId = 'cholesterol_ch',
                   label = "Poziom cholesterolu:",
                   choices = c("W normie" = 1,
                               "Powyzej normy" = 2,
                               "Znacznie powyzej normy" = 3
                               )
                   ),
      
      # cisnienie skurczowe krwi
      sliderInput(inputId = "ap_hi_ch", 
                  label = "Cisnienie skurczowe krwi:", 
                  value = 120, 
                  min = 0, 
                  max = 360
                  ),
      
      # cisnienie rozkurczowe krwi
      sliderInput(inputId = "ap_lo_ch", 
                  label = "Cisnienie rozkurczowe krwi:", 
                  value = 90, 
                  min = 0, 
                  max = 370
                  ),
      
      # data urodzenia
      dateInput(inputId = "data_ur_ch", 
                label = "Data urodzenia",
                value = '2000-01-01',
                min = '1900-01-01',
                max = '2005-01-01',
                format = 'yyyy-mm-dd',
                weekstart = 1,
                language = 'pl'
                ),
      
      # wzrost
      numericInput(inputId = 'wzrost_ch', 
                   label = 'Wzrost (w centymetrach):',
                   value = 170,
                   min = 140,
                   max = 220, 
                   step = 1
                   ),
      
      # walidacja wzrostu
      htmlOutput("wzrost_alert"),
      
      # masa
      numericInput(inputId = 'masa_ch', 
                   label = 'Masa (w kilogramach):',
                   value = 65,
                   min = 20,
                   max = 300, 
                   step = 1
                   ),
      
      # walidacja masy
      htmlOutput("masa_alert"),
      
      # odswiezanie danych
      submitButton(text = 'Przelicz dane', 
                   icon = icon("refresh")
                   ),
      
      br(),
      
      # odpowiedz modelu
      uiOutput(outputId = "odpowiedz")
      #tags$head(tags$style("#odpowiedz{font-size: 16px; font-style: italic;}"))
      
      
    )
    
  )
  
)


#---- SERVER LOGICS -----

server <- function(input, output, session) {
  
  # walidacja wzrostu  
  input_received_wzrost <- reactive({
    if(input$wzrost_ch > 220){
      max
    }else{
      input$wzrost_ch
    }
  })
  textnote_wzrost <- reactive({
    if(input$wzrost_ch <= 220){
      paste('')
    }else{
      paste0("<font color=\"#e05959\"><b>", "Maksymalny wzrost w modelu to 220!", "</b></font>")
    }
  })
  output$wzrost_alert<-renderText({
    HTML(textnote_wzrost() )
  })
  
  # walidacja masy
  input_received_masa <- reactive({
    if(input$masa_ch > 300){
      max
    }else{
      input$masa_ch
    }
  })
  textnote_masa <- reactive({
    if(input$masa_ch <= 300){
      paste('')
    }else{
      paste0("<font color=\"#e05959\"><b>", "Maksymalna masa w modelu to 300!", "</b></font>")
    }
  })
  output$masa_alert<-renderText({
    HTML(textnote_masa() )
  })
  
  
  # predykcja
  
  #output$odpowiedz <- renderText({
   
  output$odpowiedz <- renderUI({ 
    df <- data.frame(ap_hi = c(input$ap_hi_ch),
                     ap_lo = c(input$ap_lo_ch),
                     age_years = c(floor(time_length(difftime(Sys.Date(), as.Date(input$data_ur_ch)), "years"))),
                     cholesterol = as.factor(c(input$cholesterol_ch)),
                     BMI = c(input$masa_ch/(input$wzrost_ch*input$wzrost_ch))
    )
    res <- predict(model_1, df, na.action = na.roughfix)
    
    
    
    HTML(ifelse(
      res == 0, 
      as.character(div(style="color: green; font-size: 16px;", paste0('Gratulacje, nie masz choroby serca!'))), 
      as.character(div(style="color: red; font-size: 16px", paste0('Ups! Masz chorobe serca. Zglos sie do lekarza!')))))
  
    
  })
  
}


#---- CALL APP ----
shinyApp(ui, server)

