#rm(list=ls())
#setwd("C:/Users/nicol/OneDrive/Escritorio/shiny Cobre")
load(".RData")

library(plotly)
library(ggplot2)
library(shiny)
library(shinymanager)
library(timetk)
library(lubridate)
library(rsconnect)
library(DT)
library(ggrepel)
library(shinythemes)
library(rgdal)
library(maptools)
library(sp)
library(tmap)
library(treemap)
library(d3treeR)
library(shinydashboard)
library(tidyverse)
library(networkD3)


inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


#data.frame with credentials info
credentials <- data.frame(
    user = c("PronusAnalista", "Cobre", "JoseEchandia", "MarinoRamirez","1"),
    password = c("T0D0ENMINUSCULA!", "SARLAFT2022", "Integrity2021", "Marino","1"),
    stringsAsFactors = FALSE
)


# Define UI for application
ui <- secure_app(head_auth = tags$script(inactivity),
                 fluidPage(
                   tags$head(includeCSS("www/CSS.css"),
                             tags$style(
                               HTML("#dashboard{margin-bottom:50px;}")
                             )),
                   theme = shinytheme("united"),
                   headerPanel(
                     title=tags$a(fluidRow(
                       column(2),
                       column(4,tags$img(src='logo_horizontal-01.png', height = 131, width = 250), target="_blank"),
                       column(4,tags$img(src='cobre-logo.png', height = 115, width = 250), target="_blank"),
                       column(2)))),
                   titlePanel( h1("Resultados Modelo Transaccional - 09/2022", align = "center")),
                   navbarPage(title = "DEBUZZ",
                              
                              # Pagina Explicaci??n Modelo -----------------------------------------------
                              
                              tabPanel(icon("home"),
                                       titlePanel(h2("Introducci??n", align = "center")),
                                       br(),
                                       br(),
                                       fluidRow(column(2),
                                                column(8,p("Soldier es una metodolog??a de optimizaci??n empresarial basada en nuestra herramienta Debuzz, que ayuda a nuestros clientes en la identificaci??n de ineficiencias en su esquema operativo, as?? como su impacto en el flujo de caja. A trav??s de una combinaci??n entre tecnolog??a y an??lisis de expertos se identifican ineficiencias imperceptibles que son corregidas mejorando el resultado financiero de nuestros clientes.")),
                                                column(2)),
                                       br(),
                                       fluidRow(column(2),
                                                column(8, tags$img(src='debuzz_intro.png', height = 300, width = 850)),
                                                column(2)),
                                       br(),
                                       fluidRow(column(2),
                                                column(8, tags$img(src='etapas_servicio.png', height = 400, width = 850)),
                                                column(2)),
                                       br()
                              ),
                              
                              # Pagina Estadisticas Descriptivas ----------------------------------------
                              
                              tabPanel("Estad??sticas Descriptivas",icon = icon("chart-line", verify_fa = FALSE),
                                       h2("Hist??rico de egresos", align = "center"),
                                       fluidRow(column(1),
                                                column(5, plotOutput("egresos_historicos")),
                                                column(5, plotlyOutput("egresos_crecimiento")),
                                                column(1)),
                                       br(),
                                       h2("Por concepto del egreso", align = "center"),
                                       fluidRow(
                                         column(6, plotlyOutput("egreso_concepto")),
                                         column(6, plotlyOutput("egreso_concepto_um"))),
                                       br(),
                                       h2("Por Tipo id del egreso", align = "center"),
                                       fluidRow(
                                         column(2),
                                         column(8, plotlyOutput("egreso_tipo_id")),
                                         column(2)),
                                       br(),
                                       h2("Por caracter??sticas de los terceros", align = "center"),
                                       fluidRow(
                                         column(6, plotlyOutput("egreso_frecuencia")),
                                         column(6, plotlyOutput("egreso_recurrencia"))),
                                       br()),
                              
                              # Pagina Segmentacion -----------------------------------------------------
                              
                              tabPanel("Modelo Transaccional",icon = icon("calculator", verify_fa = FALSE),
                                       dashboardPage(title= "Modelo", skin= "red",
                                                     dashboardHeader(title="Modelo Transaccional"
                                                     ),
                                                     dashboardSidebar(
                                                       sidebarMenu(id="sidebarID",
                                                                   menuItem("Anomalias Concepto", tabName = "ano_concepto"),
                                                                   menuItem("Anomalias Tipo id", tabName = "ano_tipo_id"),
                                                                   menuItem("Anomalias Generales", tabName = "ano_general")
                                                       )
                                                     )
                                                     ,
                                                     dashboardBody(
                                                       tabItems(
                                                         tabItem(tabName = "ano_concepto",
                                                                 h2("Segmentaci??n por Concepto", align = "center"),
                                                                 br(),
                                                                 fluidRow(
                                                                   column(2),
                                                                   column(8, plotlyOutput("cluster_concepto")),
                                                                   column(2)),
                                                                 br(),
                                                                 fluidRow(column(12, dataTableOutput("tab_concepto")))
                                                         ),
                                                         tabItem(tabName = "ano_tipo_id",
                                                                 h2("Segmentaci??n por Tipo id", align = "center"),
                                                                 br(),
                                                                 fluidRow(
                                                                   column(2),
                                                                   column(8, plotlyOutput("cluster_tipo_id")),
                                                                   column(2)),
                                                                 br(),
                                                                 fluidRow(column(12, dataTableOutput("tab_tipo_id")))
                                                         ),
                                                         tabItem(tabName = "ano_general",
                                                                 h2("Resultados Generales", align = "center"),
                                                                 br(),
                                                                 fluidRow(column(12, dataTableOutput("tab_general")))
                                                         )
                                                       )
                                                     )
                                       )
                              ),
                              tabPanel("Cruces en Listas",icon = icon("code-branch", verify_fa = FALSE),
                                       h2("Cruces en Listas", align = "center"),
                                       fluidRow(column(1),
                                                column(5, plotOutput("cruce_mensual")),
                                                column(5, plotOutput("cruce_mensual_um")),
                                                column(1)),
                                       br(),
                                       h2("Distribuci??n de los Cruces en Listas", align = "center"),
                                       fluidRow(
                                         column(6, plotlyOutput("cruce_tipo")),
                                         column(6, plotlyOutput("cruce_reportados"))),
                                       br(),
                                       h2("Terceros con Coincidencias - ??ltimo Mes", align = "center"),
                                       fluidRow(column(12, dataTableOutput("tab_reportados"))),
                                       br()
                              )
                   )
                 )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    result_auth <- secure_server(check_credentials = check_credentials(credentials))

    output$res_auth <- renderPrint({
        reactiveValuesToList(result_auth)
    })
# Estadisticas descriptivas -----------------------------------------------

# Graficas   
    output$egresos_historicos = renderPlot({shiny_egresos_mensuales})
    output$egresos_crecimiento = renderPlotly({shiny_egresos_mensuales_creci})

    output$egreso_concepto_um = renderPlotly({shiny_egresos_distr_concepto_um})
    output$egreso_concepto = renderPlotly({shiny_egresos_distr_concepto})
    
    output$egreso_tipo_id = renderPlotly({shiny_area_tipo_id})
    
    output$egreso_frecuencia = renderPlotly({shiny_gph_frecuencia_um})
    output$egreso_recurrencia = renderPlotly({shiny_gph_recurrencia})


# Anomalias --------------------------------------------------------------  
  
    # Tipo Id
    
    output$cluster_tipo_id = renderPlotly({shiny_gph_ano_id})
    
    output$tab_tipo_id = renderDataTable({
      datatable(shiny_tab_ano_id,
                extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               buttons = list(
                                 list(extend = "pageLength"),
                                 list(extend = "copy"),
                                 list(extend = 'csv', filename = "Anomalias_tipo_id"),
                                 list(extend = 'excel', filename = "Anomalias_tipo_id"),
                                 list(extend = 'pdf', filename = "Anomalias_tipo_id"),
                                 list(extend = "print")),
                               pagelength = 5,
                               lengthMenu = list(c(5, 10, 100, -1),
                                                 c('5', '10', '100','All'))
                ))
    })
    

    # Concepto
    
    output$cluster_concepto = renderPlotly({shiny_gph_ano_concepto})
    
    output$tab_concepto = renderDataTable({
      datatable(shiny_tab_ano_concepto,
                extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               buttons = list(
                                 list(extend = "pageLength"),
                                 list(extend = "copy"),
                                 list(extend = 'csv', filename = "Anomalias_concepto"),
                                 list(extend = 'excel', filename = "Anomalias_concepto"),
                                 list(extend = 'pdf', filename = "Anomalias_concepto"),
                                 list(extend = "print")),
                               pagelength = 5,
                               lengthMenu = list(c(5, 10, 100, -1),
                                                 c('5', '10', '100','All'))
                ))
    })
    
    # Total Mes
    
    output$tab_general = renderDataTable({
      datatable(shiny_ano_generales_um,
                extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               buttons = list(
                                 list(extend = "pageLength"),
                                 list(extend = "copy"),
                                 list(extend = 'csv', filename = "Anomalias_generales"),
                                 list(extend = 'excel', filename = "Anomalias_generales"),
                                 list(extend = 'pdf', filename = "Anomalias_generales"),
                                 list(extend = "print")),
                               pagelength = 5,
                               lengthMenu = list(c(5, 10, 100, -1),
                                                 c('5', '10', '100','All'))
                ))
    })
    
# Cruces en Listas --------------------------------------------------------  

# Graficas
    output$cruce_tipo  = renderPlotly({shiny_tipo_cruce})
    output$cruce_mensual  = renderPlot({shiny_conteos_mensuales})
    output$cruce_mensual_um = renderPlot({shiny_conteo_ultimo_mes})
    output$cruce_reportados  = renderPlotly({shiny_terceros_reportados})
    
    
    output$tab_reportados = renderDataTable({
      datatable(shiny_terceros_reportados_tabla,
                extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               buttons = list(
                                 list(extend = "pageLength"),
                                 list(extend = "copy"),
                                 list(extend = 'csv', filename = "Terceros_reportados"),
                                 list(extend = 'excel', filename = "Terceros_reportados"),
                                 list(extend = 'pdf', filename = "Terceros_reportados"),
                                 list(extend = "print")),
                               pagelength = 5,
                               lengthMenu = list(c(5, 10, 100, -1),
                                                 c('5', '10', '100','All'))
                ))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
