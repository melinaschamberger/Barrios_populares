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



# SERVER

# render network viz
shinyServer(function(input, output) {
  
  
# ----------------------------------------------------------------------------------------REACTIVIDAD
  

  Provincia_elegida <- reactive({
    if (input$provincia == "Argentina") {
      Provincia_elegida <- datos }
    else {Provincia_elegida <- datos %>% filter(Provincia %in% input$provincia)}
  })
  
  #botón del departamento no considerado
  #output$cuartoInput <- renderUI({
  #  selectInput("Dpto",
  #              choices = if (input$provincia == "Argentina"){
  #                          " "}
  #                        else {unique(Provincia_elegida()$Dptos)},
  #              label = "Departamento:")
  #                })
                  
  
  #reactivo para grafico de barrios creados
  creaciones <- reactive({
    Provincia_elegida() %>% 
    group_by(`Año de creación`) %>% 
    summarise(Total = n()) %>% 
    filter(!is.na(Total)) %>% 
    filter(!is.na(`Año de creación`))
    })
  #Total barrios kpi
    total_creados <- reactive({
      nrow(Provincia_elegida())
    })
  #Total flias kpi
    total_flias <- reactive({
      sum(Provincia_elegida()$`Familias estimadas`)
    })
    
    #Servicio elegido
    servicio_df <- reactive({
      if (input$provincia == "Argentina") {
        servicio_df <- servicios_argentina %>%
          filter (Servicio %in% input$servicio) %>% 
          arrange(-Cantidad) 
        }
      else {
        servicio_df <- servicios %>% 
          filter(Provincia %in% input$provincia) %>% 
          filter(Servicio %in% input$servicio) %>% 
          arrange(-Cantidad)
        }
    })
        
    #Reactivo del mapa
    
   #  mapa_prov_elegida <- reactive({
    #  if (input$provincia == "Argentina") {
    #    mapa_prov_elegida <- datos_mapa }
    #  else {mapa_prov_elegida <- datos_mapa %>% 
    #      filter(Provincia %in% input$provincia)
    #    }
    # })
    
   #Reactivo del reemplazo del mapa
    mapa_prov_elegida <- reactive({
      if (input$provincia == "Argentina") {
            mapa_prov_elegida <- datos %>% 
                                  group_by(Provincia) %>%  
                                  summarise(Total = n()) %>% 
                                  arrange(-Total) %>% 
                                  rename(Dptos = Provincia) }
      else {mapa_prov_elegida <- datos %>% 
                                  filter(Provincia %in% input$provincia) %>% 
                                  group_by(Dptos) %>% 
                                  summarise(Total = n()) %>% 
                                  arrange(-Total) }
    })
    
     
    #Reactivo de tipo de propiedad
    propiedad_df <- reactive({
      if (input$provincia == "Argentina") {
        propiedad_df <- datos %>%
          summarise(Alquilada = sum(tipo_propiedad_alquilada),
                    Prestada = sum(tipo_propiedad_prestada),
                    Propia = sum(tipo_propiedad_de_quienes_vieven_en_ella)) %>% 
          gather(key="Condicion",value="Valor") %>% 
          arrange(-Valor) %>% 
          ungroup()
      }
      else {
        propiedad_df  <- datos %>% 
          filter(Provincia %in% input$provincia) %>% 
          group_by(Provincia) %>% 
          summarise(Alquilada = sum(tipo_propiedad_alquilada),
                    Prestada = sum(tipo_propiedad_prestada),
                    Propia = sum(tipo_propiedad_de_quienes_vieven_en_ella)) %>% 
          gather(key="Condicion",value="Valor", -Provincia) %>% 
          arrange(-Valor)
      }
    })
    
    
    #Reactivo de situación laboral
    sit_laboral_df <- reactive({
      if (input$provincia == "Argentina") {
        previo_arg <- datos %>%
          summarise(NS_NC_A = sum(situacion_laboral_ns_nc),
                    En_negro_A = sum(situacion_laboral_empleado_en_negro),
                    Independiente_A = sum(situacion_laboral_trabajo_independiente_cooperativa_familiar),
                    Doméstico_sin_sueldo_A = sum(situacion_laboral_realiza_trabajos_del_hogar_sin_sueldo),
                    En_blanco_A = sum(situacion_laboral_empleado_en_blanco),
                    Jubilado_pensionado_A = sum(situacion_laboral_jubilado_pensionado),
                    No_aplica_A = sum(situacion_laboral_no_aplica),
                    No_trabaja_A = sum(situacion_laboral_no_trabaja), 
                    Total = situacion_laboral_ns_nc +
                      NS_NC_A +En_negro_A + Independiente_A +
                      Doméstico_sin_sueldo_A + En_blanco_A + Jubilado_pensionado_A +
                      No_aplica_A + No_trabaja_A,
                    'NS NC' = round(NS_NC_A/Total*100),
                    'Empleo no formal' = round(En_negro_A/Total*100),
                    'Independiente' = round(Independiente_A /Total*100),
                    'Doméstico sin sueldo' = round(Doméstico_sin_sueldo_A /Total*100),
                    'Empleo formal' = round(En_blanco_A /Total*100),
                    'Jubiladx o pensionadx' = round(Jubilado_pensionado_A /Total*100),
                    'No aplica' = round(No_aplica_A/Total*100),
                    'No trabaja' = round(No_trabaja_A/Total*100)) %>% 
          select (10: 17)
                    
        previo_arg <- previo_arg[1,]  
        sit_laboral_df <- previo_arg %>% 
                      gather(key="Situacion",value="Porcentaje") %>% 
                      arrange(-Porcentaje)
      }
      else {
        previo_prov <- datos %>%
          filter(Provincia %in% input$provincia) %>% 
          #group_by(Provincia) %>% 
          summarise(NS_NC_A = sum(situacion_laboral_ns_nc),
                    En_negro_A = sum(situacion_laboral_empleado_en_negro),
                    Independiente_A = sum(situacion_laboral_trabajo_independiente_cooperativa_familiar),
                    Doméstico_sin_sueldo_A = sum(situacion_laboral_realiza_trabajos_del_hogar_sin_sueldo),
                    En_blanco_A = sum(situacion_laboral_empleado_en_blanco),
                    Jubilado_pensionado_A = sum(situacion_laboral_jubilado_pensionado),
                    No_aplica_A = sum(situacion_laboral_no_aplica),
                    No_trabaja_A = sum(situacion_laboral_no_trabaja), 
                    Total = situacion_laboral_ns_nc +
                      NS_NC_A +En_negro_A + Independiente_A +
                      Doméstico_sin_sueldo_A + En_blanco_A + Jubilado_pensionado_A +
                      No_aplica_A + No_trabaja_A,
                    'NS NC' = round(NS_NC_A/Total*100),
                    'En negro' = round(En_negro_A/Total*100),
                    'Independiente' = round(Independiente_A /Total*100),
                    'Doméstico sin sueldo' = round(Doméstico_sin_sueldo_A /Total*100),
                    'En blanco' = round(En_blanco_A /Total*100),
                    'Jubilado pensionado' = round(Jubilado_pensionado_A /Total*100),
                    'No aplica' = round(No_aplica_A/Total*100),
                    'No trabaja' = round(No_trabaja_A/Total*100)) %>% 
          select (10: 17)
        
        previo_prov <- previo_prov[1,]  
        sit_laboral_df <- previo_prov %>% 
          gather(key="Situacion",value="Porcentaje") %>% 
          arrange(-Porcentaje)
      }
    })
    
    
    # Reactivo motivo no trabaja
    
    no_trabaja_df <- reactive({
      if (input$provincia == "Argentina") {
        previo_arg_mn <- datos %>%
          summarise(NS_NC_A = sum(motivo_no_trabaja_ns_nc),
                    Solo_estudia_A = sum(motivo_no_trabaja_solo_estudia),
                    Busca_trabajo_A = sum(motivo_no_trabaja_busca_trabajo),
                    Discapacidad_A = sum(motivo_no_trabaja_discapacidad),
                    No_estudia_no_busca_trabajo_A = sum(motivo_no_trabaja_no_estudia_ni_busca_trabajo),
                    Total = NS_NC_A +
                      Solo_estudia_A +
                      Busca_trabajo_A +
                      Discapacidad_A +
                      No_estudia_no_busca_trabajo_A,
                    'NS NC' = round((NS_NC_A/Total)*100),
                    'Sólo estudia' = round(Solo_estudia_A/Total*100),
                    'Busca trabajo' = round(Busca_trabajo_A/Total*100),
                    'Discapacidad' = round(Discapacidad_A/Total*100),
                    'No estudia, ni busca trabajo' = round(No_estudia_no_busca_trabajo_A/Total*100)
          ) %>% 
          select (7:11)
        
        no_trabaja_df <- previo_arg_mn %>% 
          gather(key="Motivo",value="Porcentaje") %>% 
          arrange(-Porcentaje)
      }
      else {
        previo_prov_mn <- datos %>%
          filter(Provincia %in% input$provincia) %>% 
          #group_by(Provincia) %>% 
          summarise(NS_NC_A = sum(motivo_no_trabaja_ns_nc),
                    Solo_estudia_A = sum(motivo_no_trabaja_solo_estudia),
                    Busca_trabajo_A = sum(motivo_no_trabaja_busca_trabajo),
                    Discapacidad_A = sum(motivo_no_trabaja_discapacidad),
                    No_estudia_no_busca_trabajo_A = sum(motivo_no_trabaja_no_estudia_ni_busca_trabajo),
                    Total = NS_NC_A +
                      Solo_estudia_A +
                      Busca_trabajo_A +
                      Discapacidad_A +
                      No_estudia_no_busca_trabajo_A,
                    'NS NC' = round((NS_NC_A/Total)*100),
                    'Sólo estudia' = round(Solo_estudia_A/Total*100),
                    'Busca trabajo' = round(Busca_trabajo_A/Total*100),
                    'Discapacidad' = round(Discapacidad_A/Total*100),
                    'No estudia, ni busca trabajo' = round(No_estudia_no_busca_trabajo_A/Total*100)
          ) %>% 
          select (7:11)
        
        no_trabaja_df <- previo_prov_mn %>% 
          gather(key="Motivo",value="Porcentaje") %>% 
          arrange(-Porcentaje)
        
      }
    })
    
    
    # ----------------------------------------------------------------------------------------FIN REACTIVIDAD
  
  #gráfico: creados por año
  output$plot <- renderEcharts4r({
    ts_base <- creaciones() %>% 
      e_charts(x = `Año de creación`) %>% 
      e_datazoom(
        type = "slider", 
        toolbox = FALSE,
        bottom = 5
      ) %>% 
      e_tooltip() %>% 
      #e_title("Barrios creados por año.") %>% 
      e_x_axis(`Año de creación`, axisPointer = list(show = TRUE)) %>% 
      e_line(Total, legend = FALSE) %>% e_theme("roma")
    ts_base
  })

    #kpi barrios creados
  output$barrios <- renderText(paste({
    total_creados()
  }))
  
  #kpi flias en barrios
  output$familias <- renderText(paste({
    total_flias()
  }))
  
  
  ###Mapa
#  output$mapa <- renderLeaflet({
#    leaflet(mapa_prov_elegida()) %>%
#      addProviderTiles(providers$CartoDB.Positron) %>%
      #setView(lng = -58, lat = -34, zoom = 5) %>%
#      addPolygons(fillColor = "#e63946",
#                  weight = 2,
#                  opacity = 2,
#                  color = "#e63946",
#                  dashArray = "3",
#                  fillOpacity = 0.9,
#                  highlightOptions = highlightOptions(
#                    weight = 5,
#                    color = "#666",
#                    dashArray = "",
#                    fillOpacity = 0.9,
#                    bringToFront = TRUE),
#                  label = (sprintf(
#                    "<strong>%s</strong><br/>%g Familias estimadas </sup>",
#                    mapa_prov_elegida()$Barrio, mapa_prov_elegida()$Familias.estimadas
#                  ) %>% lapply(htmltools::HTML)),
#                  labelOptions = labelOptions(
#                    style = list("font-weight" = "normal", padding = "3px 8px"),
#                    textsize = "15px",
#                    direction = "auto")) %>% 
#      addEasyButton(easyButton(
#        icon="fa-crosshairs", title="Locate Me",
#        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
# })
  
  ##Reemplazo del mapa
  output$mapa <- renderHighchart({
    grafico_mapa <-  mapa_prov_elegida() %>% 
      hchart("bar", hcaes(x = Dptos, y = Total), name = "Cantidad de barrios",
           color = '#4a4e69')  %>% hc_add_theme(hc_theme_gridlight()) %>% 
      hc_yAxis(
        title = list(text = "Barrios",
                     style = list(fontFamily = "Roboto", fontSize = "12")),
        labels = list(format = "{value}", 
                      style = list(fontFamily = "Roboto", fontSize = "12"))) %>% 
      hc_xAxis(title = list(text="", 
                            style = list(fontFamily = "Roboto", fontSize = "12")),
               labels = list(style = list(fontFamily = "Roboto", fontSize = "12")))  
      grafico_mapa
  })
    
  
  
  #Grafico servicios
  output$plot_servicios <- renderHighchart({
    grafico <- servicio_df() %>% 
      hchart("bar", hcaes(x = Tipo, y = Cantidad), name = "Cantidad de barrios", 
             color = '#d90429') %>% 
      hc_yAxis(
        title = list(text="Cantidad de barrios", 
                     style = list(fontFamily = "Roboto", fontSize = "12")),
                  labels = list(style = list(fontFamily = "Roboto", fontSize = "12"))) %>% 
      hc_xAxis(title = list(style = list(fontFamily = "Roboto", fontSize = "12")),
               labels = list(style = list(fontFamily = "Roboto", fontSize = "12")))
    grafico
  })
  
  #Gráfico tipo de propiedad
  output$plot_tipo_propiedad <-  renderPlotly({
    plot_ly(
      x = propiedad_df()$Condicion,
      y = propiedad_df()$Valor,
      name = "SF Zoo",
      type = "bar",
      marker = list(color = '#8D99AE',
                    line = list(color = '#EF233C',
                                width = 1.5))) %>% 
      layout(font=(list(
        family = "Encode Sans",
        size = 14)),
        plot_bgcolor  = '#EFEFEF',
        paper_bgcolor = '#EFEFEF'
        #fig_bgcolor   = '#EFEFEF'
        )
  })
  
  
  #Gráfico de situación laboral
  output$plot_sit_laboral <- renderHighchart({
    hchart(sit_laboral_df(), "column", hcaes(x =Situacion, y = Porcentaje), name = "Situación laboral",
           color = '#82c0cc')  %>% 
      hc_yAxis(
        title = list(text = "Porcentaje provincial",
                     style = list(fontFamily = "Roboto", fontSize = "12")),
        labels = list(format = "{value}%", 
                      style = list(fontFamily = "Roboto", fontSize = "12")), max = 100) %>% 
      hc_xAxis(title = list(text="Situación laboral informada", 
                            style = list(fontFamily = "Roboto", fontSize = "12")),
               labels = list(style = list(fontFamily = "Roboto", fontSize = "12")))
    
  })
  
  #Gráfico de motivo de no trabajo
  output$plot_motivo_no_trabaja <- renderPlotly({
    plot_ly(
      labels = no_trabaja_df()$Motivo,
      values = no_trabaja_df()$Porcentaje,
      name = "SF Zoo",
      type = "pie",
      textposition = 'inside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#222222'),
      hoverinfo = 'text',
      text = ~paste(no_trabaja_df()$Motivo, ':', no_trabaja_df()$Porcentaje, '%'),
      marker = list(colors = c('#e63946', '#8d99ae', '#a8dadc', '#457b9d', '#1d3557'),
                    line = list(color = '#e1deda', width = 1)),
      #The 'pull' attribute can also be used to create space between the sectors
      showlegend = FALSE) %>% 
      layout(font=(list(
        family = "Encode Sans",
        size = 14)),
        plot_bgcolor  = '#EFEFEF',
        paper_bgcolor = '#EFEFEF'
        #fig_bgcolor   = '#EFEFEF'
      )
  })
  
})

#shinyApp(ui = ui.R, server = server.R)