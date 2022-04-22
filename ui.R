#Librerías
library(shiny)
library(shiny)
library(tidyverse) 
library(wesanderson) 
library(tidygraph) 
library(visNetwork) 
library(extrafont) 
library(echarts4r) 
library(highcharter)
library(sf)
library(leaflet) 
library(plotly)



# Datos
datos <- read_csv("data/datos_dos.csv")
datos$`Año de creación` <- as.factor(datos$`Año de creación`)


servicios <- read_csv("data/servicios_final.csv")
Encoding(servicios$Provincia) <- "latin1"
Encoding(servicios$Tipo) <- "latin1"
#servicios$Provincia <- iconv(servicios$Provincia, "latin1", "UTF-8",sub='')  
#servicios$tipo <- iconv(servicios$Tipo, "latin1", "UTF-8",sub='')  


servicios_argentina <- read_csv("data/servicios_argentina.csv")
Encoding(servicios_argentina$Tipo) <- "latin1"
#servicios_argentina$tipo <- iconv(servicios_argentina$Tipo, "latin1", "UTF-8",sub='')  

##Mapa
#datos_mapa <- st_read("https://raw.githubusercontent.com/melinaschamberger/Argentina/main/datos_mapa_exportado.geojson")
#datos_mapa <- datos_mapa %>% select(2,3,12,13)
#Encoding(datos_mapa$Barrio) <- "latin1"
#datos_mapa$Barrio <- iconv(datos_mapa$Barrio, "latin1", "UTF-8",sub='')  
#Encoding(datos_mapa$Provincia) <- "latin1"
#datos_mapa$Provincia <- iconv(datos_mapa$Provincia, "latin1", "UTF-8",sub='')



# UI
shinyUI(fluidPage(
  #list(tags$head(HTML('<link rel="icon", href="img/logo.png",
   #                     type="image/png" />'))),
  div(style="padding: 1px 0px; width: '100%'",
      titlePanel(
        title="", 
        windowTitle = "Barrios populares en Argentina"
      )
  ),
  
  
  
  # add css file
  includeCSS("www/styles.css"),
  # app title with customisation
  h1(toupper("Barrios populares en Argentina"), 
     align = "left",
     style = "font-family: 'Encode Sans', sans-serif, cursive;
              font-weight: 800; line-height: 1.1; padding-bottom: 5px; padding-top: 5px;
              color: #A9A9A9;
              background-color: #EFEFEF; margin-top: 0.5; margin-left: 0;border-radius: 6px;"), 
  # create 1 row with 3 columns
  fluidRow(
    # let column - app desciption and explanation
    column(width = 2,
           wellPanel(
             br(),
             p("La aplicación visualiza los resultados del último", strong("Relevamiento Nacional de Barrios Populares"), 
               "realizado por el Registro Nacional de Barrios Populares (RENABAP) en el año", strong("2021"),"."),
             br(),
             p("En la parte superior se presenta la", strong("distribución y los años de creación"), 
               "de los barrios de la jurisdiccción."),
             br(),
             p("Seguidamente encontrará información sobre la ",
               strong("vivienda y el acceso a servicios.")), 
             br(),
             p("Mientras que al final de la aplicación se explora la ",
               strong("situación laboral"), "de quienes habitan
               estos barrios."),
             br(),
             p("------------", align = "center"),
             br(),
             p("Fuente:", a("RENABAP (2021).", href = "https://www.argentina.gob.ar/desarrollosocial/renabap/informesyestadisticas")),
             p("Datos:", a("Datos.gob.ar", href = "https://datos.gob.ar/dataset/desarrollo-social-registro-nacional-barrios-populares")),
             br(),
             p(a("Linkedin", href = "https://www.linkedin.com/in/melina-schamberger/"), "|", 
               a("GitHub", href = "https://github.com/melinaschamberger/Barrios_populares")),
             br(),
             p(""),
             style = "font-family: 'Encode Sans', sans-serif, cursive; color: #696969; background-color: #CAEBF2;
                    border-radius: 6px;"
           )),
    # middle column - the viz!
    column(width = 8,
           fluidRow(
             column(2, 
                    h4("Barrios:"), style = "font-family: 'Encode Sans', sans-serif, cursive; color: #696969; 
                    background-color: #CAEBF2;
                    border-radius: 100px;"),
             column(2,
                    h4("Familias:"), style = "font-family: 'Encode Sans', sans-serif, cursive; color: #696969; 
                    background-color: #CAEBF2;
                    border-radius: 100px;")),
           fluidRow(
             column(2, 
                    wellPanel (
                      div(textOutput("barrios"),style = "font-size:125%; font-family: 'Encode Sans', sans-serif, cursive;"))),
             column(2,
                    wellPanel (
                      div(textOutput("familias"),style = "font-size:125%; font-family: 'Encode Sans', sans-serif, cursive;")))),
           fluidRow(
             #Columna izquierda en el centro
             column(6,
                    #background-color: yellow
                    style = "height:550px; background-color: #EFEFEF;
                     font-family: 'Encode Sans', sans-serif, cursive; color: #696969;",
                    h3(strong("Distribución y creación")),
                    h4("Barrios creados por año"),
                    br(),
                    echarts4rOutput("plot"),
                    h3(strong("Vivienda y acceso a servicios")),
                    selectInput(inputId = "servicio",
                                choices = c("Agua", "Cloaca", "Electricidad"),
                                label = "Servicio:",
                                selected = TRUE),
                    highchartOutput("plot_servicios"),
                    br(),
                    h3(strong("Población")),
                    h4(strong("Mercado de Trabajo")),
                    br(), 
                    h4("Situación laboral (%)"),
                    highchartOutput("plot_sit_laboral")
             ),
             #Columna derecho en el centro
             column(width = 6,
                    #background-color: red
                    style = "height:450px; 
                  background-color: #EFEFEF; 
                  font-family: 'Encode Sans', sans-serif, cursive; color: #696969;",
                    br(),
                    br(),
                    h4("Distribución geográfica de los barrios"),
                    br(),
                    #leafletOutput("mapa"),
                    highchartOutput("mapa"),
                    br(),
                    br(), 
                    br(),br(), br(), 
                    br(), br(), 
                    h4("Familias, según condición de propiedad habitada"),
                    plotlyOutput("plot_tipo_propiedad"),
                    br(),
                    br(),
                    br(),br(), 
                    h4("Motivos de no trabajo (%)"),
                    plotlyOutput("plot_motivo_no_trabaja")
             ))),
    
    # right column
    column(width = 2,
           wellPanel(
             br(),
             p("Seleccione para explorar:"),
             br(),
             selectInput(inputId = "provincia",
                         choices = c("Argentina", "Buenos Aires","Catamarca", 
                                     "Chaco", "Chubut", 
                                     "Ciudad Autónoma de Buenos Aires","Córdoba", 
                                     "Corrientes", "Entre Ríos", "Formosa", 
                                     "Jujuy","La Pampa" ,"La Rioja", "Mendoza",
                                     "Misiones","Neuquén", "Río Negro",
                                     "Salta","San Luis", "San Juan",
                                     "Santa Cruz","Santa Fe", "Santiago del Estero",
                                     "Tierra del Fuego" ,"Tucumán"),
                         label = "Jurisdicción:",
                         selected = TRUE),
             br(),
             #uiOutput("cuartoInput"),
             tags$button("Reiniciar", id="restart", 
                         type="button", class="btn btn-danger", onclick="history.go(0)"),
             br(),br(), br(),
             p("------------", align = "center"),
             br(), 
             p(h6(em("Nota: Parte de los gráficos visualizan datos de familias encuestadas, valores que
                        pueden variar al considerar el total de familias estimadas."))),
             style = "font-family:'Encode Sans', sans-serif, cursive; color: #696969; background-color: #CAEBF2;
                      border-radius: 6px;",
             br()))
  )    
))

