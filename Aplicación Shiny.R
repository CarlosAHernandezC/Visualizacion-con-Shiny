library(shiny)
library(shinyjs) 
library(highcharter) 
library(DT)
library(dplyr)
library(dashboardthemes)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(tidyverse)
library(jsonlite)
library(forecast)
library(xts)
library(memoise)

# Función ---------------------------------------------------------------------------------------------------------

obtener_indicadores <- function(empresa) {url <- stringr::str_c("https://www.elmercurio.com/inversiones/json/json.aspx?categoria=", empresa, "&time=10&indicador=2")
df <- jsonlite::read_json(url)$Data %>% 
  stringr::str_split(";") %>% 
  dplyr::first() %>%
  I() %>% 
  readr::read_delim(delim = ",", col_names = c("fecha", "precio", "vol"))
df <- df %>% 
  mutate(
    fecha = lubridate::ymd_hms(fecha),
    anio = lubridate::year(fecha)
  )
return(df)
}

data_memoisada <- memoise(obtener_indicadores)


# Shinydashboard (heather, sidebar, body)------------------------


header <- dashboardHeader(title = "Informe de inversiones", titleWidth=300)
sidebar <- dashboardSidebar(width = 300,
                            sidebarMenu(
                              id='sidebar',
                              menuItem('Empresa',icon = icon("briefcase"), tabName = 'menu1', startExpanded = T,
                                       div(id = "sidebar1",
                                           selectInput(inputId = "sel_empresa",
                                                       label = "Seleccione empresa",
                                                       choices = c("NUEVAPOLAR", "SMU", "BESALCO", "COPEC", "FALABELLA","BSANTANDER", "CMPC", "CHILE", "SQM-B", "ENELAM", "CENCOSUD","BCI", "LTM", "ENELCHILE","SM-CHILE%20B", "CCU", "PARAUCO","ITAUCORP", "AGUAS-A", "COLBUN", "ENTEL", "ECL", "CONCHATORO","RIPLEY", "AESGENER","ANDINA-B", "SONDA", "CAP", "ILC","SALFACORP", "SECURITY", "VAPORES", "ENELGXCH", "ANTARCHILE","BANMEDICA", "EMBONOR-B", "FORUS", "IAM", "MASISA", "ORO%20BLANCO","SK", "SMSAAM")))),
                                                                  
                              menuItem('Años', tabName = 'menu2', icon = icon("sliders"), startExpanded = T,
                                       div(id = "sidebar2",
                                           sliderInput("slider", "Seleccione años", min = 2001, max = 2022, 
                                                       value = c(2001, 2022))))
                            ))


body <- dashboardBody(shinyDashboardThemes(theme = "blue_gradient"),
                      tabsetPanel(
                        tabPanel(title = "Gráfico", plotOutput("grafico")),
                        tabPanel(title = "Tabla de datos", DTOutput("tabla"))
                      ))
                      


# ui -server ------------------------------------------------------------------------------------------------------


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  output$grafico <- renderPlot({
    
    d <- data_memoisada(input$sel_empresa) %>% 
      filter(anio >= input$slider[1] & anio <=  input$slider[2])
    d %>% 
      group_by(anio) %>%
      summarise(mean(precio))
    ggplot(d)+
      geom_line(aes(fecha,precio))+
      labs(title = paste("Precio promedio de las acciones de",input$sel_empresa))+
      theme(plot.title = element_text(hjust = 0.5))
    })
    
  
    
  output$tabla <- renderDT({
    d <- data_memoisada(input$sel_empresa) %>% 
      filter(anio >= input$slider[1] & anio <=  input$slider[2])
    d %>%
      group_by(Año = anio) %>%
      summarise(Promedio = round(mean(precio),2))
  })
}

shinyApp(ui = ui, server = server)
