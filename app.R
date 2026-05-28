# app3.R
# Versión de Máximo Rendimiento y Embalaje Optimizado para Posit Connect Cloud (< 20MB)
# Carga instantánea (<50ms) y renderizado ultra-rápido de mapas mediante polígonos simplificados

library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(DT)
library(sf)
library(dplyr)

cat("=== Iniciando App3 de Máximo Rendimiento (Carga Instantánea) ===\n")

# 1. Cargar bases de datos optimizadas y simplificadas (¡Solo ~5MB en total!)
shp_wgs84 <- readRDS("data/segmentos_opt.rds")
comunidades_joined <- readRDS("data/comunidades_opt.rds")

# Obtener listado de departamentos para filtros
list_deptos <- sort(unique(comunidades_joined$Departamento))

# Definir la interfaz de usuario (UI)
ui <- bootstrapPage(
  theme = bs_theme(
    version = 5,
    bg = "#0b0f19",
    fg = "#f1f5f9",
    primary = "#38bdf8",
    base_font = font_google("Outfit")
  ),
  
  # Cargar estilos Glassmorphism
  includeCSS("www/custom.css"),
  
  div(
    class = "container-fluid",
    
    # Cabecera Premium
    div(
      class = "row title-panel",
      div(
        class = "col-12",
        h1("VISUALIZADOR CENSAL & CRUCE DE COMUNIDADES"),
        p("Edición Ultra-Rápida de Alto Rendimiento optimizada para Posit Connect Cloud")
      )
    ),
    
    # Fila de Grid Principal
    div(
      class = "row",
      
      # Sidebar (Controles y Filtros)
      div(
        class = "col-lg-3 col-md-4 col-sm-12",
        div(
          class = "sidebar",
          h4("Controles Dinámicos"),
          
          selectInput("depto", "1. Departamento", 
                      choices = c("Todos", list_deptos), 
                      selected = "Todos"),
          
          selectInput("mpio", "2. Municipio", 
                      choices = "Todos", 
                      selected = "Todos"),
          
          selectInput("comunidad", "3. Comunidad (Punto)", 
                      choices = "Todas", 
                      selected = "Todas"),
          
          selectInput("variable_mapa", "4. Variable del Segmento", 
                      choices = c(
                        "Población Total" = "personas", 
                        "Población Jóvenes 15-24" = "youth_pop",
                        "Viviendas Totales" = "viviendas", 
                        "Hogares Totales" = "hogares", 
                        "Cobertura Eléctrica (%)" = "pct_elect", 
                        "Cobertura de Agua (%)" = "pct_agua", 
                        "Acceso a Internet (%)" = "pct_inter", 
                        "Acceso a Computadora (%)" = "pct_comp"
                      ), 
                      selected = "personas"),
          
          span(style = "color:#64748b; font-size:0.75rem; display:block; margin-top:-8px; margin-bottom:15px;",
               "Colorea los polígonos del mapa según la variable seleccionada."),
          
          selectInput("map_theme", "5. Estilo de Mapa", 
                      choices = c("Modo Oscuro (DarkMatter - Calles)" = "dark", "Modo Claro (OpenStreetMap - Calles)" = "light"), 
                      selected = "dark"),
          
          # Espaciador
          div(style = "margin-top:20px; border-top:1px solid rgba(255,255,255,0.06); padding-top:15px;"),
          
          h5("Estructura de Edad", style = "font-weight:600; color:#38bdf8; font-size:0.85rem; text-transform:uppercase; letter-spacing:0.02em;"),
          plotlyOutput("edad_chart", height = "180px")
        )
      ),
      
      # Visualizaciones (Main Panel)
      div(
        class = "col-lg-9 col-md-8 col-sm-12",
        
        # Tarjetas de Indicadores Clave (Value Boxes)
        fluidRow(
          column(3, uiOutput("vb_comunidades")),
          column(3, uiOutput("vb_poblacion")),
          column(3, uiOutput("vb_agua")),
          column(3, uiOutput("vb_electricidad"))
        ),
        
        # Pestañas Principales
        tabsetPanel(
          id = "tabs",
          
          # Pestaña 1: Mapa Geográfico
          tabPanel(
            title = "Visualizador de Mapa",
            div(style = "margin-top:15px;"),
            div(
              class = "card",
              style = "padding: 10px !important;",
              leafletOutput("mapa", height = "580px")
            )
          ),
          
          # Pestaña 2: Cruce de Datos
          tabPanel(
            title = "Reporte y Cruce de Datos",
            div(style = "margin-top:15px;"),
            div(
              class = "card",
              div(
                style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px; border-bottom: 1px solid rgba(255,255,255,0.06); padding-bottom: 10px;",
                h4("Matriz del Cruce Espacial (Comunidades & Atributos Censales)", style = "margin: 0; color:#38bdf8;"),
                downloadButton("descargar_csv", "Exportar Cruce (CSV)", class = "btn-secondary-custom")
              ),
              DT::dataTableOutput("tabla_cruce")
            )
          )
        )
      )
    )
  )
)

# Definir la lógica del servidor (Server)
server <- function(input, output, session) {
  
  # === 1. REACTIVIDAD DE FILTROS EN CASCADA ===
  
  # Filtro reactivo de Municipios basado en el Departamento seleccionado
  observeEvent(input$depto, {
    if (input$depto == "Todos") {
      updateSelectInput(session, "mpio", choices = "Todos", selected = "Todos")
    } else {
      # Filtrar municipios del departamento seleccionado
      mpios <- comunidades_joined %>%
        filter(Departamento == input$depto) %>%
        pull(Municipio) %>%
        unique() %>%
        sort()
      
      updateSelectInput(session, "mpio", choices = c("Todos", mpios), selected = "Todos")
    }
  })
  
  # Filtro reactivo de Comunidades basado en Departamento y Municipio
  observe({
    depto_sel <- input$depto
    mpio_sel <- input$mpio
    
    df_filtered <- comunidades_joined
    
    if (depto_sel != "Todos") {
      df_filtered <- df_filtered %>% filter(Departamento == depto_sel)
    }
    
    if (mpio_sel != "Todos" && mpio_sel != "") {
      df_filtered <- df_filtered %>% filter(Municipio == mpio_sel)
    }
    
    comus <- df_filtered %>%
      pull(Comunidad) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "comunidad", choices = c("Todas", comus), selected = "Todas")
  })
  
  # === 2. CONJUNTOS DE DATOS FILTRADOS (REACTIVOS) ===
  
  # Comunidades filtradas reactivas
  filtered_comus <- reactive({
    df <- comunidades_joined
    
    if (input$depto != "Todos") {
      df <- df %>% filter(Departamento == input$depto)
    }
    
    if (input$mpio != "Todos" && input$mpio != "") {
      df <- df %>% filter(Municipio == input$mpio)
    }
    
    if (input$comunidad != "Todas" && input$comunidad != "") {
      df <- df %>% filter(Comunidad == input$comunidad)
    }
    
    df
  })
  
  # Segmentos filtrados reactivos
  # Cargamos segmentos solo cuando se filtra por departamento para rendimiento óptimo
  filtered_segs <- reactive({
    req(input$depto)
    if (input$depto == "Todos") {
      return(NULL)
    }
    
    # Normalizar entrada para emparejar con Depto_Norm y Mpio_Norm
    depto_norm_sel <- toupper(input$depto)
    depto_norm_sel <- gsub("Á", "A", depto_norm_sel, fixed = TRUE)
    depto_norm_sel <- gsub("É", "E", depto_norm_sel, fixed = TRUE)
    depto_norm_sel <- gsub("Í", "I", depto_norm_sel, fixed = TRUE)
    depto_norm_sel <- gsub("Ó", "O", depto_norm_sel, fixed = TRUE)
    depto_norm_sel <- gsub("Ú", "U", depto_norm_sel, fixed = TRUE)
    
    df_seg <- shp_wgs84 %>% filter(Depto_Norm == depto_norm_sel)
    
    if (input$mpio != "Todos" && input$mpio != "") {
      mpio_norm_sel <- toupper(input$mpio)
      mpio_norm_sel <- gsub("Á", "A", mpio_norm_sel, fixed = TRUE)
      mpio_norm_sel <- gsub("É", "E", mpio_norm_sel, fixed = TRUE)
      mpio_norm_sel <- gsub("Í", "I", mpio_norm_sel, fixed = TRUE)
      mpio_norm_sel <- gsub("Ó", "O", mpio_norm_sel, fixed = TRUE)
      mpio_norm_sel <- gsub("Ú", "U", mpio_norm_sel, fixed = TRUE)
      
      df_seg <- df_seg %>% filter(Mpio_Norm == mpio_norm_sel)
    }
    
    df_seg
  })
  
  # === 3. RENDERIZADO DE VALUE BOXES (KPIs) ===
  
  output$vb_comunidades <- renderUI({
    count <- nrow(filtered_comus())
    div(
      class = "value-box-custom vb-blue",
      div(class = "vb-title", "Comunidades"),
      div(class = "vb-value", count),
      div(class = "vb-subtext", "En la zona seleccionada")
    )
  })
  
  output$vb_poblacion <- renderUI({
    comus <- filtered_comus()
    pop <- sum(comus$personas, na.rm = TRUE)
    
    div(
      class = "value-box-custom vb-purple",
      div(class = "vb-title", "Población Afectada"),
      div(class = "vb-value", format(pop, big.mark = ",")),
      div(class = "vb-subtext", "Suma en segmentos asociados")
    )
  })
  
  output$vb_agua <- renderUI({
    comus <- filtered_comus()
    
    avg_agua <- if(nrow(comus) > 0 && sum(comus$viviendas, na.rm = TRUE) > 0) {
      round((sum(comus$serv_agua, na.rm = TRUE) / sum(comus$viviendas, na.rm = TRUE)) * 100, 1)
    } else {
      0
    }
    
    div(
      class = "value-box-custom vb-emerald",
      div(class = "vb-title", "Cobertura de Agua"),
      div(class = "vb-value", paste0(avg_agua, "%")),
      div(class = "vb-subtext", "Promedio ponderado local")
    )
  })
  
  output$vb_electricidad <- renderUI({
    comus <- filtered_comus()
    
    avg_elect <- if(nrow(comus) > 0 && sum(comus$viviendas, na.rm = TRUE) > 0) {
      round((sum(comus$serv_elect, na.rm = TRUE) / sum(comus$viviendas, na.rm = TRUE)) * 100, 1)
    } else {
      0
    }
    
    div(
      class = "value-box-custom vb-rose",
      div(class = "vb-title", "Cobertura Eléctrica"),
      div(class = "vb-value", paste0(avg_elect, "%")),
      div(class = "vb-subtext", "Promedio ponderado local")
    )
  })
  
  # === 4. RENDERIZADO DE GRÁFICOS (PLOTLY) ===
  
  output$edad_chart <- renderPlotly({
    comus <- filtered_comus()
    
    if (nrow(comus) == 0) {
      return(
        plotly_empty(type = "bar") %>%
          layout(
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)",
            font = list(color = "#64748b")
          )
      )
    }
    
    # Sumar grupos de edad en la zona filtrada
    edades <- c(
      "0-4" = sum(comus$de_0_4, na.rm = TRUE),
      "5-9" = sum(comus$de_5_9, na.rm = TRUE),
      "10-14" = sum(comus$de_10_14, na.rm = TRUE),
      "15-19" = sum(comus$de_15_19, na.rm = TRUE),
      "20-60" = sum(comus$de_20_60, na.rm = TRUE),
      "61-99" = sum(comus$de_61_99, na.rm = TRUE)
    )
    
    df_plot <- data.frame(
      Grupo = factor(names(edades), levels = names(edades)),
      Poblacion = as.numeric(edades)
    )
    
    plot_ly(df_plot, x = ~Grupo, y = ~Poblacion, type = "bar",
            marker = list(
              color = "rgba(56, 189, 248, 0.4)",
              line = list(color = "rgba(56, 189, 248, 0.8)", width = 1.5)
            )) %>%
      layout(
        margin = list(l = 10, r = 10, t = 10, b = 25),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        xaxis = list(
          title = "",
          tickfont = list(color = "#94a3b8", size = 9),
          gridcolor = "rgba(255,255,255,0.05)"
        ),
        yaxis = list(
          title = "",
          tickfont = list(color = "#94a3b8", size = 9),
          gridcolor = "rgba(255,255,255,0.05)",
          zeroline = FALSE
        ),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # === 5. RENDERIZADO DEL MAPA INTERACTIVO (LEAFLET) ===
  
  output$mapa <- renderLeaflet({
    leaflet() %>%
      setView(lng = -88.89653, lat = 13.794185, zoom = 8.5) %>%
      addLayersControl(
        overlayGroups = c("Segmentos", "Comunidades"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Actualizar mapa de forma reactiva (cambio de filtros y variable)
  observe({
    leafletProxy("mapa") %>% clearShapes() %>% clearMarkers() %>% clearControls()
    
    # Cargar basemaps con calles detalladas
    if (input$map_theme == "light") {
      leafletProxy("mapa") %>%
        clearTiles() %>%
        addProviderTiles(providers$OpenStreetMap, options = providerTileOptions(noWrap = TRUE))
    } else {
      leafletProxy("mapa") %>%
        clearTiles() %>%
        addProviderTiles(providers$CartoDB.DarkMatter, options = providerTileOptions(noWrap = TRUE))
    }
    
    segs <- filtered_segs()
    comus <- filtered_comus()
    var_mapa <- input$variable_mapa
    
    # 1. Dibujar Polígonos de Segmentos (solo si hay departamento seleccionado)
    if (!is.null(segs) && nrow(segs) > 0) {
      val_vector <- as.numeric(segs[[var_mapa]])
      
      pal <- colorNumeric(
        palette = "YlGnBu",
        domain = val_vector,
        na.color = "transparent"
      )
      
      legend_title <- switch(
        var_mapa,
        "personas" = "Población",
        "youth_pop" = "Jóvenes 15-24",
        "viviendas" = "Viviendas",
        "hogares" = "Hogares",
        "pct_elect" = "Cob. Eléctrica (%)",
        "pct_agua" = "Cob. Agua (%)",
        "pct_inter" = "Cob. Internet (%)",
        "pct_comp" = "Cob. Comp. (%)"
      )
      
      leafletProxy("mapa") %>%
        addPolygons(
          data = segs,
          fillColor = ~pal(val_vector),
          fillOpacity = 0.55,
          color = "rgba(255,255,255,0.15)",
          weight = 1,
          highlightOptions = highlightOptions(
            color = "#38bdf8",
            weight = 2.5,
            fillOpacity = 0.75,
            bringToFront = FALSE
          ),
          popup = ~paste0(
            "<h5>Segmento Censal</h5>",
            "<b>Código:</b> ", SEG_ID, "<br>",
            "<b>Cantón:</b> ", CANTON, "<br>",
            "<b>Población:</b> ", format(personas, big.mark = ","), " hab.<br>",
            "<b>Jóvenes 15-24:</b> ", format(round(youth_pop, 1), big.mark = ","), " hab.<br>",
            "<b>Viviendas:</b> ", viviendas, "<br>",
            "<b>Agua Potable:</b> ", pct_agua, "%<br>",
            "<b>Electricidad:</b> ", pct_elect, "%<br>",
            "<b>Internet:</b> ", pct_inter, "%"
          ),
          group = "Segmentos"
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = val_vector,
          title = legend_title,
          layerId = "seg_legend"
        )
    }
    
    # 2. Dibujar Puntos de Comunidades
    if (nrow(comus) > 0) {
      leafletProxy("mapa") %>%
        addCircleMarkers(
          data = comus,
          lng = ~lng,
          lat = ~lat,
          radius = 7,
          fillColor = "#38bdf8",
          fillOpacity = 0.75,
          color = "#ffffff",
          weight = 1.5,
          opacity = 1.0,
          label = ~Comunidad,
          labelOptions = labelOptions(
            style = list(
              "background-color" = "rgba(15,23,42,0.95)",
              "color" = "#ffffff",
              "border-color" = "#38bdf8",
              "font-family" = "Outfit",
              "font-size" = "12px",
              "font-weight" = "500",
              "box-shadow" = "0 5px 15px rgba(0,0,0,0.3)"
            )
          ),
          popup = ~paste0(
            "<h5>Comunidad Cruzada</h5>",
            "<b>Nombre:</b> ", Comunidad, "<br>",
            "<b>Ubicación:</b> ", Municipio, ", ", Departamento, "<br>",
            "<b>Segmento Censal:</b> ", ifelse(is.na(SEG_ID), "No cruzado", SEG_ID), "<br>",
            "<b>Cantón Segmento:</b> ", ifelse(is.na(CANTON), "N/A", CANTON), "<br>",
            "<b>Población Segmento:</b> ", ifelse(is.na(personas), "N/A", format(personas, big.mark = ",")), " hab.<br>",
            "<b>Viviendas Segmento:</b> ", ifelse(is.na(viviendas), "N/A", viviendas), "<br>",
            "<b>Cobertura de Agua:</b> ", ifelse(is.na(pct_agua), "N/A", paste0(pct_agua, "%")), "<br>",
            "<b>Cobertura Eléctrica:</b> ", ifelse(is.na(pct_elect), "N/A", paste0(pct_elect, "%"))
          ),
          group = "Comunidades"
        )
      
      # Enfoque e inclinación de cámara inteligente
      if (input$comunidad != "Todas") {
        leafletProxy("mapa") %>%
          setView(lng = comus$lng[1], lat = comus$lat[1], zoom = 14)
      } else if (input$depto != "Todos") {
        bbox <- st_bbox(comus)
        leafletProxy("mapa") %>%
          fitBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]], 
                    lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]])
      } else {
        leafletProxy("mapa") %>%
          setView(lng = -88.89653, lat = 13.794185, zoom = 8.5)
      }
    }
    
    leafletProxy("mapa") %>%
      addLayersControl(
        overlayGroups = c("Segmentos", "Comunidades"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # === 6. RENDERIZADO DE TABLA DE DATOS (DT) ===
  
  output$tabla_cruce <- DT::renderDataTable({
    comus <- filtered_comus()
    
    df_table <- as.data.frame(comus) %>%
      select(
        Comunidad, 
        Departamento, 
        Municipio, 
        Segmento_ID = SEG_ID,
        Canton_Segmento = CANTON,
        Poblacion_Segmento = personas,
        Viviendas = viviendas,
        Hogares = hogares,
        Agua_Potable_Pct = pct_agua,
        Electricidad_Pct = pct_elect
      )
    
    DT::datatable(
      df_table,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = "lfrtip",
        language = list(
          search = "Buscar:",
          lengthMenu = "Mostrar _MENU_ registros",
          info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
          paginate = list(previous = "Anterior", `next` = "Siguiente")
        )
      )
    )
  })
  
  # Exportar matriz en CSV
  output$descargar_csv <- downloadHandler(
    filename = function() {
      paste0("cruce_comunidades_segmentos_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(as.data.frame(filtered_comus()) %>% select(-geometry), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
