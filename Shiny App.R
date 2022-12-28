library(rstudioapi)     # setwd a carpeta actual
library(forcats)        # fct_infreq
library(stringr)        # substrings
library(imager)         # cargar imágenes
library(devtools)       
library(dplyr)          # mutaciones
library(scales)         # escalas de colores
library(ggplot2)        
library(plotly)         # interactividad para ggplot
library(rjson)
library(leaflet)
library(leaflet.extras)
library(igraph)
library(shiny)
library(shinydashboard)
library(shinythemes)    # tema del dashboard
library(bslib)          # tema personalizado
library(rsconnect)      # deployea la app

# Cambia el working directory a la carpeta actual
setwd(dirname(getActiveDocumentContext()$path))
# Créditos de pie de página
footer = c(hr, "Fabio G. Calo Dizy, 2022")
# Convierte nombres de meses a números para compararlos
mes_dict = list("Enero"="1", "Febrero"="2", "Marzo"="3", "Abril"="4", "Mayo"="5", "Junio"="6", 
                "Julio"="7", "Agosto"="8", "Septiembre"="9", "Octubre"="10", "Noviembre"="11", "Diciembre"="12")


####################################################################################################
######################################### CSS ######################################################
####################################################################################################

theme = bs_theme(
  # Color de fondo y color de texto
  bg="#444547", fg="#ffffff",
  # Controls the accent (e.g., hyperlink, button, etc) colors
  secondary="#48DAC6", primary="#fca380", # primary al parecer no sirve
  # Button colors
  success="#00BC8C", info="#3498DB", warning="#F39C12", danger="#E74C3C",
  # Fonts
  base_font=c("Grandstander", "sans-serif"),
  code_font=c("Courier", "monospace"),
  heading_font="'Helvetica Neue', Helvetica, sans-serif",
  # Can also add lower-level customization
  "input-border-color"="#fca380",
)

# ############ Personalización ##############
# Color de la barra de navegación
#theme = bs_add_variables(theme, "navbar-navbar-default-background-color" = "#DD4B39")
theme = bs_add_rules(theme, ".navbar.navbar-default { 
                                background-color: #DD4B39 !important;
                                font-weight: bold;
                            }
                            ")


####################################################################################################
#################################### Visualizaciones ###############################################
####################################################################################################


######################################## Metro ###########################################
contorno_cdmx = fromJSON(file="contorno_cdmx.json")
# Zonas con bajo acceso al metro
metro_zona_n = fromJSON(file="metro/metro_zona_norte.json")
metro_zona_s = fromJSON(file="metro/metro_zona_sur.json")
metro_zona_e = fromJSON(file="metro/metro_zona_este.json")
metro_zona_o = fromJSON(file="metro/metro_zona_oeste.json")
# Datos de los mapas
af_metro = read.csv("metro/afluencia_metro.csv", header=TRUE, encoding="UTF-8")
icono_metro = makeIcon(iconUrl="www/logo_metro.png", iconWidth=20, iconHeight=20)
icono_metro_sm = makeIcon(iconUrl="www/logo_metro.png", iconWidth=8, iconHeight=8)
metro_dict = list("1"="1", "2"="2", "3"="3", "4"="4", "5"="5", "6"="6", "7"="7", "8"="8", "9"="9", "A"="10", "B"="11", "12"="12")
metro_lineas_clrs = c("#F1669B", "#007AC0", "#541F00", "#7CC7BA", 
                      "#FADA2A", "#00A062", "#F37C30", "#E7262C",
                      "#C1AE1B", "#90298E", "#06854D", "#BC9C57")

# Mapa de las líneas del metro
metro_mapa = leaflet(options=leafletOptions(zoomControl=TRUE, zoomSnap=0, zoomDelta=20)) %>%
                addTiles()%>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addGeoJSONv2(contorno_cdmx, color="#777777", opacity=0.1, fill=TRUE) %>%
                setView(lng=-99.12, lat=19.41, zoom=12)

# Agrega las lineas
for (num_linea in c(sprintf("%d", seq(1:9)), "A", "B", "12")) {
    linea_json = fromJSON(file=paste0("metro/metro_linea", num_linea, ".json")) # línea json
    markers = read.csv(file=paste0("metro/metro_linea", num_linea, ".csv"), header=TRUE, encoding="UTF-8")
    
    metro_mapa = metro_mapa %>%
                    addGeoJSONv2(linea_json, color=metro_lineas_clrs[strtoi(metro_dict[[num_linea]])], opacity=1, fill=FALSE, group=toString(num_linea)) %>%
                    addMarkers(data=markers, label=markers$estacion, icon=icono_metro, group=toString(num_linea))
}

# Agrega el control de capas
metro_mapa = metro_mapa %>% addLayersControl(overlayGroups = c(sprintf("%d", seq(1:9)), "A", "B", "12"))


###################################### Metrobús ##########################################
# Zonas con bajo acceso al metro
mb_zona_n = fromJSON(file="mb/mb_zona_norte.json")
mb_zona_s = fromJSON(file="mb/mb_zona_sur.json")
mb_zona_e = fromJSON(file="mb/mb_zona_este.json")
mb_zona_o = fromJSON(file="mb/mb_zona_oeste.json")
# Datos de los mapas
af_mb = read.csv("mb/afluencia_mb.csv", header=TRUE, encoding="UTF-8")
icono_mb = makeIcon(iconUrl="www/logo_mb.png", iconWidth=13, iconHeight=13)
icono_mb_sm = makeIcon(iconUrl="www/logo_mb.png", iconWidth=8, iconHeight=8)
mb_lineas_clrs = c("#B5121B", "#893994", "#779A0B", "#F78F1E", 
                   "#09347A", "#E44599", "#00703C")

# Mapa de las líneas del metrobús
mb_mapa = leaflet(options=leafletOptions(zoomControl=TRUE, zoomSnap=0, zoomDelta=20)) %>%
                addTiles()%>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addGeoJSONv2(contorno_cdmx, color="#777777", opacity=0.1, fill=TRUE) %>%
                setView(lng=-99.12, lat=19.4, zoom=11.7)

# Agrega las lineas
for (num_linea in seq(1:7)) {
    linea_json = fromJSON(file=paste0("mb/mb_linea", num_linea, ".json")) # línea json
    markers = read.csv(file=paste0("mb/mb_linea", num_linea, ".csv"), header=TRUE, encoding="UTF-8")
    
    mb_mapa = mb_mapa %>%
                addGeoJSONv2(linea_json, color=mb_lineas_clrs[strtoi(num_linea)], opacity=1, fill=FALSE, group=toString(num_linea)) %>%
                addMarkers(data=markers, label=markers$estacion, icon=icono_mb, group=toString(num_linea))
}

# Agrega el control de capas
mb_mapa = mb_mapa %>% addLayersControl(overlayGroups = seq(1:7))


######################################### RTP #############################################
#TODO

######################################### Ecobici #############################################
#TODO

####################################### Privado ###########################################
# Mapa de vialidades
vial = fromJSON(file="priv/vialidades.json")
# Posición de los marcadores de cada calle
vial_markers = read.csv(file="priv/vialidades.csv", header=TRUE, encoding="UTF-8")
# Ícono vacío; hace que no se muestren los íconos default de leaflet sobre las calless
icono_vial = makeIcon(iconUrl="www/empty_icon.png", iconWidth=15, iconHeight=15)
# Ranking mundial de congestión
ranking = read.csv("priv/tomtom_ranking.csv", header=TRUE, encoding="UTF-8")
# Variación por día durante la pandemia
dif_porc_waze = read.csv("priv/dif_porc_transito_waze.csv", header=TRUE, encoding="UTF-8")
# Variación por semana durante la pandemia
dif_porc_tomtom = read.csv("priv/dif_porc_transito_tomtom.csv", header=TRUE, encoding="UTF-8")

# Crea el mapa de vialidades
priv_mapa_vial = leaflet(options=leafletOptions(zoomControl=TRUE, zoomSnap=0, zoomDelta=20)) %>%
                    addTiles()%>%
                    addProviderTiles(providers$CartoDB.Positron) %>%
                    addGeoJSONv2(contorno_cdmx, color="#777777", opacity=0.1, fill=TRUE) %>%
                    addGeoJSONv2(vial, color="#29cca6", opacity=1, fill=FALSE, weight=3) %>%
                    addMarkers(data=vial_markers, icon=icono_vial, label=vial_markers$nombre) %>%
                    setView(lng=-99.12, lat=19.39, zoom=11.3)


#################################### Contaminación ########################################
cont_cost = read.csv("cont/costos_contaminacion.csv", header=TRUE)


###################################### Misc. ##########################################
misc_zona_n = fromJSON(file="misc/misc_zona_norte.json")
misc_zona_s = fromJSON(file="misc/misc_zona_sur.json")
misc_zona_e = fromJSON(file="misc/misc_zona_este.json")
misc_zona_o = fromJSON(file="misc/misc_zona_oeste.json")


####################################################################################################
######################################### GUI ######################################################
####################################################################################################

Home = tabPanel("Home",
    fluidRow(
        column(width=1,
            img(src="side_img.PNG", width="350px") # las imágenes deben de estar en www
        ),
        column(width=8, offset=2,
            includeMarkdown("home.md")
        )
    ),
    footer[[1]](), # agrega espacio vacío
    footer[[2]], # agrega créditos
    footer[[1]]()
)

Metro = navbarMenu("Metro",
    tabPanel("Mapa",
        sidebarLayout(
            sidebarPanel(
                textOutput("metro_desc"),
            ),
            mainPanel(
                leafletOutput("metro_mapa", height=620, width=950),
            )
        ),
        footer[[1]](), # agrega espacio vacío
        footer[[2]], # agrega créditos
        footer[[1]]()
    ),
    tabPanel("Afluencia Promedio",
        sidebarLayout(
            sidebarPanel(
                textOutput("metro_af_desc"),
                hr(),
                sliderInput(inputId="metro_anio", label="Elige un rango de años", value=c(2010, 2022), min=2010, max=2022),
                sliderInput(inputId="metro_mes", label="Elige un rango de meses", value=c(1, 12), min=1, max=12),
                plotOutput(outputId="metro_barras_prom"),
            ),
            mainPanel(
                leafletOutput("metro_mapa_af", height=730, width=950),
            )
        ),
        footer[[1]](), # agrega espacio vacío
        footer[[2]], # agrega créditos
        footer[[1]]()
    ),
    tabPanel("Afluencia Total",
        sidebarLayout(
            sidebarPanel(
                sliderInput(inputId="metro_anio_", label="Elige un rango de años", value=c(2010, 2022), min=2010, max=2022),
            ),
            mainPanel(
                plotOutput(outputId="metro_barras_tot", height=620, width=950)
            )
        ),
        footer[[1]](), # agrega espacio vacío
        footer[[2]], # agrega créditos
        footer[[1]]()
    ),
    tabPanel("Zonas de Bajo Acceso",
        sidebarLayout(
            sidebarPanel(
                textOutput("metro_zonas_desc")
            ),
            mainPanel(
                leafletOutput("metro_mapa_zonas", height=730, width=950)
            )
        ),
        footer[[1]](), # agrega espacio vacío
        footer[[2]], # agrega créditos
        footer[[1]]()
    )
)

Metrobus = navbarMenu("Metrobús",
    tabPanel("Mapa",
        sidebarLayout(
            sidebarPanel(
                textOutput("mb_desc"),
            ),
            mainPanel(
                leafletOutput("mb_mapa", height=620, width=950),
            )
        ),
        footer[[1]](), # agrega espacio vacío
        footer[[2]], # agrega créditos
        footer[[1]]()
    ),
    tabPanel("Afluencia Promedio",
        sidebarLayout(
            sidebarPanel(
                textOutput("mb_af_desc"),
                hr(),
                sliderInput(inputId="mb_anio", label="Elige un rango de años", value=c(2005, 2022), min=2005, max=2022),
                sliderInput(inputId="mb_mes", label="Elige un rango de meses", value=c(1, 12), min=1, max=12),
                plotOutput(outputId="mb_barras_prom")
            ),
            mainPanel(
                leafletOutput("mb_mapa_af", height=730, width=950)
            )
        ),
        footer[[1]](), # agrega espacio vacío
        footer[[2]], # agrega créditos
        footer[[1]]()
    ),
    tabPanel("Afluencia Total",
        sidebarLayout(
            sidebarPanel(
                sliderInput(inputId="mb_anio_", label="Elige un rango de años", value=c(2005, 2022), min=2005, max=2022),
            ),
            mainPanel(
                plotOutput(outputId="mb_barras_tot", height=620, width=950)
            )
        ),
        footer[[1]](), # agrega espacio vacío
        footer[[2]], # agrega créditos
        footer[[1]]()
    ),
    tabPanel("Zonas de Bajo Acceso",
        sidebarLayout(
            sidebarPanel(
                textOutput("mb_zonas_desc")
            ),
            mainPanel(
                leafletOutput("mb_mapa_zonas", height=730, width=950)
            )
        ),
        footer[[1]](), # agrega espacio vacío
        footer[[2]], # agrega créditos
        footer[[1]]()
    )
)

# TODO: agregar RTP
# RTP = navbarMenu("RTP",
#     tabPanel("Mapa"
#     ),
#     tabPanel("Afluencia Promedio"
#     ),
#     tabPanel("Afluencia Total"
#     ),
#     tabPanel("Zonas de Bajo Acceso"
#     )
# )

Privado = navbarMenu("Transporte Privado",
    tabPanel("Vialidades Primarias",
        sidebarLayout(
            sidebarPanel(
                textOutput("priv_vial_desc"),
            ),
            mainPanel(
            leafletOutput("priv_mapa_vial", height=620, width=950),
            )
        ),
        footer[[1]](), # agrega espacio vacío
        footer[[2]], # agrega créditos
        footer[[1]]()
    ),
    tabPanel("Ranking Mundial de Tráfico",
        sidebarLayout(
            sidebarPanel(
                textOutput("priv_rank_desc"),
            ),
            mainPanel(
                plotlyOutput(outputId="priv_rank", height=620, width=950),
            )
        ),
        footer[[1]](), # agrega espacio vacío
        footer[[2]], # agrega créditos
        footer[[1]]()
    )
)

Pandemia = navbarMenu("Efectos de la Pandemia",
    tabPanel("Metro",
        sidebarLayout(
            sidebarPanel(
                textOutput("pand_metro_desc"),
            ),
            mainPanel(
                plotlyOutput(outputId="pand_metro", height=620, width=950),
            )
        ),
        footer[[1]](), # agrega espacio vacío
        footer[[2]], # agrega créditos
        footer[[1]]()
    ),
    tabPanel("Metrobús",
        sidebarLayout(
            sidebarPanel(
                textOutput("pand_mb_desc"),
            ),
            mainPanel(
                plotlyOutput(outputId="pand_mb", height=620, width=950),
            )
        ),
        footer[[1]](), # agrega espacio vacío
        footer[[2]], # agrega créditos
        footer[[1]]()
    ),
    tabPanel("Transporte Privado",
        sidebarLayout(
            sidebarPanel(
                textOutput("pand_priv_desc"),
            ),
            mainPanel(
                plotlyOutput(outputId="pand_priv", height=620, width=950),
            )
        ),
        footer[[1]](), # agrega espacio vacío
        footer[[2]], # agrega créditos
        footer[[1]]()
    ),
)

Contaminacion = navbarMenu("Contaminación",
    tabPanel("Costos de la Contaminación",
        sidebarLayout(
            sidebarPanel(
                textOutput("cont_cost_desc"),
            ),
            mainPanel(
                fluidRow(
                    column(width=6,
                        plotlyOutput(outputId="cont_cost_PIB", height=620, width=465),
                    ),
                    column(width=6,
                        plotlyOutput(outputId="cont_cost_CTADA", height=620, width=465),
                    )
                )
            )
        ),
        footer[[1]](), # agrega espacio vacío
        footer[[2]], # agrega créditos
        footer[[1]]()
    )
)

Misc = navbarMenu("Misc.",
    tabPanel("Zonas de Bajo Acceso al Transporte Público",
        sidebarLayout(
            sidebarPanel(
                textOutput("misc_zonas_desc"),
            ),
            mainPanel(
                leafletOutput("misc_mapa_zonas", height=730, width=950)
            )
        ),
        footer[[1]](), # agrega espacio vacío
        footer[[2]], # agrega créditos
        footer[[1]]()
    ),
    # tabPanel("Debug",
    #     sidebarLayout(
    #         sidebarPanel(
    #             textOutput("test"),
    #         ),
    #         mainPanel(
    #         )
    #     )
    # ),
)

UI = navbarPage("El Transporte en la CDMX", theme=theme,
  Home,
  Metro,
  Metrobus,
  #RTP,
  #Ecobici,
  Privado,
  Pandemia,
  Contaminacion,
  Misc
)


####################################################################################################
######################################## Server ####################################################
####################################################################################################


Server = function(input, output) {
    setwd(dirname(getActiveDocumentContext()$path))
    # Valor reactivo que guarda el id de la línea seleccionada
    clickedId = reactiveValues(id=0)
    # Valor reactivo que guarda la fecha introducida
    anio_inpt = reactiveValues(anio_i=0, anio_f=0, mes_i=0, mes_f=0)
    # Valor reactivo que guarda la afluencia promedio del transporte seleccionado
    af_prom = reactiveValues(af=tibble())
    
    # Para debuggear
    output$test = renderPrint({
        #clickedId$id
    })
    
    ######################################## Metro ###########################################
    #---------------- Mapa ----------------
    output$metro_desc = renderText({
        "A continuación se muestra un mapa con todas las estaciones del
        metro de la CDMX. Colócate sobre una estación para ver su nombre. Usa
        el ícono en la esquina superior derecha del mapa para mostrar o esconder
        ciertas líneas."
    })
    
    output$metro_mapa = renderLeaflet({
        metro_mapa
    })
    
    #---------------- Af Prom ----------------
    output$metro_af_desc = renderText({
        "Los puntos del mapa representan la afluencia promedio por día en el rango
        de fechas seleccionado (donde azul oscuro es el mín. y amarillo es el máx.). 
        Haz click sobre una línea para comparar sus estaciones."
    })
    
    output$metro_mapa_af = renderLeaflet({
        # Obtiene las fechas de inicio y fin ingresadas
        anio_inpt$anio_i = input$metro_anio[1]
        anio_inpt$anio_f = input$metro_anio[2]
        anio_inpt$mes_i = input$metro_mes[1]
        anio_inpt$mes_f = input$metro_mes[2]
        
        # Filtra el rango de fechas ingresado y agrupa por linea y estación, 
        # obteniendo el promedio de afluencia por estación
        af_prom$af = filter(af_metro, anio >= anio_inpt$anio_i, anio <= anio_inpt$anio_f, 
                         mes_dict[mes] >= anio_inpt$mes_i, mes_dict[mes] <= anio_inpt$mes_f) %>% 
                    group_by(linea, estacion) %>% summarise(af_prom_metro=sum(afluencia) / n())
        
        # Crea el leaflet
        metro_mapa_af = leaflet(options=leafletOptions(zoomControl=TRUE, zoomSnap=0, zoomDelta=1.3)) %>%
                addTiles()%>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addGeoJSONv2(contorno_cdmx, color="#777777", opacity=0.1, fill=TRUE, layerId="0") %>%
                setView(lng=-99.12, lat=19.39, zoom=11.9)
        
        # Agrega las líneas
        for (num_linea in c(sprintf("%d", seq(1:9)), "A", "B", "12")) {
            # Línea
            linea_json = fromJSON(file=paste0("metro/metro_linea", num_linea, ".json"))
            # Estaciones
            markers = read.csv(file=paste0("metro/metro_linea", num_linea, ".csv"), header=TRUE, encoding="UTF-8")
            # Agrega la columna de af_prom_metro
            markers$af_prom_metro = filter(af_prom$af, linea==paste0("Linea ", num_linea))$af_prom_metro 
            # Colores para diferenciar afluencia
            pal = colorNumeric(c("viridis"), 1:10, domain = markers$af_prom)
            
            metro_mapa_af = metro_mapa_af %>% 
                                addGeoJSONv2(linea_json, color=metro_lineas_clrs[strtoi(metro_dict[[num_linea]])], opacity=1, fill=FALSE, group=toString(num_linea), layerId=num_linea) %>%
                                addCircles(data=markers, label=markers$estacion, group=toString(num_linea), radius=170, stroke=FALSE, fill=TRUE, fillOpacity=1, fillColor = ~pal(af_prom_metro))
        }
        
        # Agrega control de capas
        metro_mapa_af = metro_mapa_af %>% addLayersControl(overlayGroups = c(sprintf("%d", seq(1:9)), "A", "B", "12"))
        # Renderiza el mapa
        metro_mapa_af
    })
    
    output$metro_barras_prom = renderPlot({
        if(clickedId$id == 0) {
            # Agrupa sólo por línea
            af = af_prom$af %>% group_by(linea) %>% summarise(af_prom_metro=sum(af_prom_metro) / n())
            # Reordena de menor a mayor
            af = af %>% arrange(af_prom_metro)
            # Hace que ggplot no ignore el nuevo orden
            af$linea = factor(af$linea, levels=af$linea)
            
            # Muestra el resumen de todas las líneas
            ggplot(af, aes(x=linea, y=af_prom_metro)) +
                geom_bar(stat="identity", fill="#D9576A") +
                ggtitle(paste0("Afluencia promedio por día del metro de la CDMX \n(",
                        anio_inpt$anio_i, "/", anio_inpt$mes_i, "-", anio_inpt$anio_f, "/", anio_inpt$mes_f, ")")) +
                xlab("Línea") + ylab("Afluencia promedio") +
                coord_flip() +
                theme(axis.text = element_text(face="bold"))
        } else {
            # Filtra la línea seleccionada
            af = filter(af_prom$af, linea==paste0("Linea ", clickedId$id))
            # Reordena de menor a mayor
            af = af %>% arrange(af_prom_metro)
            # Hace que ggplot no ignore el nuevo orden
            af$estacion = factor(af$estacion, levels=af$estacion)
            
            # Muestra el resumen de la línea seleccionada
            ggplot(af, aes(x=estacion, y=af_prom_metro)) +
                geom_bar(stat="identity", fill="#D9576A") +
                ggtitle(paste0("Afluencia promedio por día de la Línea ", clickedId$id, "\n(", 
                        anio_inpt$anio_i, "/", anio_inpt$mes_i, "-", anio_inpt$anio_f, "/", anio_inpt$mes_f, ")")) +
                xlab("Estación") + ylab("Afluencia promedio") +
                coord_flip() +
                theme(axis.text = element_text(face="bold"))
        }
    })
    
    #---------------- Af Tot ----------------
    output$metro_barras_tot = renderPlot({
        # Obtiene las fechas de inicio y fin ingresadas
        anio_inpt$anio_i = input$metro_anio_[1]
        anio_inpt$anio_f = input$metro_anio_[2]
        
        # Filtra los años seleccionados y agrupa por año y línea
        af_tot_por_anio = filter(af_metro, anio >= anio_inpt$anio_i & anio <= anio_inpt$anio_f) %>% 
                            group_by(anio, linea) %>% summarise(tot=sum(afluencia))
        
        # Reordena de menor a mayor
        orden = c(2, 1, 3, "B", 8, 9, 7, "A", 5, 12, 6, 4)
        
        # Hace que ggplot no ignore el nuevo orden
        af_tot_por_anio$linea = factor(af_tot_por_anio$linea, levels=paste("Linea", orden))
        
        # Afluencia del metro por línea y por año
        ggplot(af_tot_por_anio, aes(x=linea, y=tot, fill=factor(anio))) +
        geom_bar(stat="identity") +
        scale_y_continuous(labels=label_number(suffix=" millones", scale=1e-6),
                            breaks=trunc(seq(0, max(af_tot_por_anio$tot) + 1e10, by=1e8)/1e8)*1e8) +
        ggtitle("Afluencia total del metro en la CDMX por año") +
        xlab("Línea") + ylab("Afluencia total") +
        labs(fill="Año") +
        scale_fill_viridis_d()
    })
    
    #---------------- Zonas ----------------
    output$metro_zonas_desc = renderText({
        print("A continuación se muestra un mapa con las regiones de la CDMX
        que tienen un bajo nivel de acceso al metro. La forma de cada región
        es una aproximación de las zonas en donde la estación de metro más
        cercana está a 1h o más caminando.")
    })
    
    output$metro_mapa_zonas = renderLeaflet({
        # Crea una copia del mapa normal
        metro_mapa_zonas = metro_mapa
        
        # Aleja inicialmente el mapa más que el original
        metro_mapa_zonas = metro_mapa_zonas %>% 
                setView(lng=-99.12, lat=19.34, zoom=10.9)
        
        # Agrega las zonas
        metro_mapa_zonas = metro_mapa_zonas %>% 
                addGeoJSONv2(metro_zona_n, color="#ff1100", opacity=0.5, stroke=FALSE, fill=TRUE) %>%
                addGeoJSONv2(metro_zona_s, color="#00ffbb", opacity=0.5, stroke=FALSE, fill=TRUE) %>%
                addGeoJSONv2(metro_zona_e, color="#61ff00", opacity=1, stroke=FALSE, fill=TRUE) %>%
                addGeoJSONv2(metro_zona_o, color="#4400ff", opacity=0.5, stroke=FALSE, fill=TRUE)
        
        # Renderiza el mapa
        metro_mapa_zonas
    })
    
    ###################################### Metrobús ##########################################
    #---------------- Mapa ----------------
    output$mb_desc = renderText({
        "A continuación se muestra un mapa con todas las estaciones del
        metrobús de la CDMX. Colócate sobre una estación para ver su nombre. Usa
        el ícono en la esquina superior derecha del mapa para mostrar o esconder
        ciertas líneas."
    })
    
    output$mb_mapa = renderLeaflet({
        mb_mapa
    })
    
    #---------------- Af Prom ----------------
    output$mb_af_desc = renderText({
        "El color de cada línea representa la afluencia promedio por día en el rango
        de fechas seleccionado (donde azul oscuro es el mín. y amarillo es el máx.)."
    })
    
    output$mb_mapa_af = renderLeaflet({
        # Obtiene las fechas de inicio y fin ingresadas
        anio_inpt$anio_i = input$mb_anio[1]
        anio_inpt$anio_f = input$mb_anio[2]
        anio_inpt$mes_i = input$mb_mes[1]
        anio_inpt$mes_f = input$mb_mes[2]
        
        # Filtra el rango de fechas ingresado y agrupa por linea y estación, 
        # obteniendo el promedio de afluencia por estación
        af_prom$af = filter(af_mb, anio >= anio_inpt$anio_i, anio <= anio_inpt$anio_f, 
                            mes_dict[mes] >= anio_inpt$mes_i, mes_dict[mes] <= anio_inpt$mes_f) %>% 
                        group_by(linea) %>% summarise(af_prom_mb=sum(afluencia) / n())
        
        # Colores para diferenciar afluencia
        pal = colorNumeric(c("viridis"), 1:10, domain=af_prom$af$af_prom_mb) 
        
        # Crea el leaflet
        mb_mapa_af = leaflet(options=leafletOptions(zoomControl=TRUE, zoomSnap=0, zoomDelta=1.3)) %>%
                addTiles()%>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addGeoJSONv2(contorno_cdmx, color="#777777", opacity=0.1, fill=TRUE, layerId="0") %>%
                setView(lng=-99.12, lat=19.39, zoom=11.9)
        
        # Agrega las líneas
        for (num_linea in seq(1:7)) {
            # Línea
            linea_json = fromJSON(file=paste0("mb/mb_linea", num_linea, ".json"))
            # Estaciones
            markers = read.csv(file=paste0("mb/mb_linea", num_linea, ".csv"), header=TRUE, encoding="UTF-8")
            
            mb_mapa_af = mb_mapa_af %>% 
                            addGeoJSONv2(linea_json, color=pal(af_prom$af$af_prom_mb[num_linea]), opacity=1, fill=FALSE, group=toString(num_linea), layerId=num_linea) %>%
                            addMarkers(data=markers, label=markers$estacion, icon=icono_mb_sm, group=toString(num_linea))
                            #addCircles(data=markers, label=markers$estacion, group=toString(num_linea), radius=120, stroke=FALSE, fill=TRUE, fillOpacity=1, fillColor = "#E31B14")
        }
        
        # # Agrega control de capas
        mb_mapa_af = mb_mapa_af %>% addLayersControl(overlayGroups = seq(1:7))
        # Renderiza el mapa
        mb_mapa_af
    })
    
    output$mb_barras_prom = renderPlot({
        # Agrupa sólo por línea
        af = af_prom$af %>% group_by(linea) %>% summarise(af_prom_mb=sum(af_prom_mb) / n())
        # Reordena de menor a mayor
        af = af %>% arrange(af_prom_mb)
        # Hace que ggplot no ignore el nuevo orden
        af$linea = factor(af$linea, levels=af$linea)
        
        # Muestra el resumen de todas las líneas
        ggplot(af, aes(x=linea, y=af_prom_mb)) +
            geom_bar(stat="identity", fill="#D9576A") +
            scale_y_continuous(labels=label_number(suffix=" mil", scale=1e-3),
                                breaks=trunc(seq(0, max(af$af_prom_mb) + 1e6, by=5e4)/1e5)*1e5) +
            ggtitle(paste0("Afluencia promedio por día del metrobús de la CDMX \n(",
                    anio_inpt$anio_i, "/", anio_inpt$mes_i, "-", anio_inpt$anio_f, "/", anio_inpt$mes_f, ")")) +
            xlab("Línea") + ylab("Afluencia promedio") +
            coord_flip() +
            theme(axis.text = element_text(face="bold"))
    })
    
    #---------------- Af Tot ----------------
    output$mb_barras_tot = renderPlot({
        # Obtiene las fechas de inicio y fin ingresadas
        anio_inpt$anio_i = input$mb_anio_[1]
        anio_inpt$anio_f = input$mb_anio_[2]
        
        # Filtra los años seleccionados y agrupa por año y línea
        af_tot_por_anio = filter(af_mb, anio >= anio_inpt$anio_i & anio <= anio_inpt$anio_f) %>% 
                            group_by(anio, linea) %>% summarise(tot = sum(afluencia))
        
        # Reordena de menor a mayor
        orden = c(1, 2, 3, 6, 5, 4, 7)
        
        # Hace que ggplot no ignore el nuevo orden
        af_tot_por_anio$linea = factor(af_tot_por_anio$linea, levels=paste("Linea", orden))
        
        # Afluencia del metrobús por línea y por año
        ggplot(af_tot_por_anio, aes(x=linea, y=tot, fill=factor(anio))) +
        geom_bar(stat="identity") +
        scale_y_continuous(labels=label_number(suffix=" millones", scale=1e-6),
                            breaks=trunc(seq(0, max(af_tot_por_anio$tot) + 1e10, by=1e8)/1e8)*1e8) +
        ggtitle("Afluencia total del metrobús en la CDMX por año") +
        xlab("Línea") + ylab("Afluencia total") +
        labs(fill="Año") +
        scale_fill_viridis_d()
    })
    
    #---------------- Zonas ----------------
    output$mb_zonas_desc = renderText({
        print("A continuación se muestra un mapa con las regiones de la CDMX
        que tienen un bajo nivel de acceso al metrobús. La forma de cada región
        es una aproximación de las zonas en donde la estación de metrobús más
        cercana está a 1h o más caminando.")
    })
    
    output$mb_mapa_zonas = renderLeaflet({
        # Crea una copia del mapa normal
        mb_mapa_zonas = mb_mapa
        
        # Aleja inicialmente el mapa más que el original
        mb_mapa_zonas = mb_mapa_zonas %>% 
                setView(lng=-99.12, lat=19.36, zoom=11.1)
        
        # Agrega las zonas
        mb_mapa_zonas = mb_mapa_zonas %>% 
                addGeoJSONv2(mb_zona_n, color="#ff1100", opacity=0.5, stroke=FALSE, fill=TRUE) %>%
                addGeoJSONv2(mb_zona_s, color="#00ffbb", opacity=1, stroke=FALSE, fill=TRUE) %>%
                addGeoJSONv2(mb_zona_e, color="#61ff00", opacity=1, stroke=FALSE, fill=TRUE) %>%
                addGeoJSONv2(mb_zona_o, color="#4400ff", opacity=0.5, stroke=FALSE, fill=TRUE)
        
        # Renderiza el mapa
        mb_mapa_zonas
    })
    
    ######################################### RTP ############################################
    
    
    ####################################### Privado ##########################################
    #---------------- Vialidades ----------------
    output$priv_vial_desc = renderText({
        "A continuación se muestra un mapa con las vialidades primarias de la CDMX, de acuerdo con la SEMOVI. 
        Colócate sobre una línea para ver su nombre."
    })
    
    output$priv_mapa_vial = renderLeaflet({
        priv_mapa_vial
    })
    
    #---------------- Ranking ----------------
    output$priv_rank_desc = renderText({
        "TomTom hace un ranking cada año de las ciudades más congestionadas del mundo.
        El nivel de congestión representa cuánto tiempo pasas en el tráfico; por ejemplo,
        un nivel de congestión del 60% significa que tus viajes duran, en promedio, 60%
        que si no hubiera tráfico. Como puedes ver, la CDMX ganó el primer lugar del mundo
        tres años seguidos (2015-2017)."
    })
    
    output$priv_rank = renderPlotly({
        # Número de colores para los ranks
        n = 8
        # Define una paleta de colores para los ranks
        pal = colorNumeric(c("viridis"), 1:n+1, domain=1:n, reverse=TRUE)
        colors = c()
        
        # Pone los colores en una lista
        for (i in 1:n) {
            colors = append(colors, pal(i))
        }
        
        ggplot(ranking, aes(x=anio, y=congestion)) +
            geom_point(size=3.5, color="#8c4d19") +
            geom_line(size=1.5, color="#8c4d19") +
            geom_text(aes(label=lugar, colour=factor(lugar)), size=9, nudge_x=0.3, nudge_y=1.4, check_overlap=TRUE, show.legend = FALSE) +
            scale_x_discrete(limits=ranking$anio) +
            scale_colour_manual(values=colors) +
            theme(legend.position="none") + 
            ggtitle("Nivel de congestión y ranking global de la CDMX") +
            xlab("Año") + ylab("Congestión") +
            theme(axis.text = element_text(face="bold"))
    })
    
    ####################################### Pandemia ##########################################
    #---------------- Metro ----------------
    output$pand_metro_desc = renderText({
        "Esta gráfica representa la afluencia total del metro del 2010 al 2022. Como puedes ver,
        el uso del metro cayó drásticamente en el 2020. Los datos sólo cubren hasta agosto del 2022,
        por lo que podríamos ver un aumento respecto al 2021. Es posible que en uno o dos años más
        incluso regresemos a los niveles del 2019, a pesar de que la capacidad del metro no ha
        aumentado en este periodo y las unidades son cada vez más viejas."
    })
    
    output$pand_metro = renderPlotly({
        # Agrupa por año
        af_metro_por_anio = af_metro %>% group_by(anio) %>% summarise(tot = sum(afluencia))
        
        ggplot(af_metro_por_anio, aes(x=anio, y=tot)) +
            geom_point(size=3, color="#8c4d19") +
            geom_line(size=1.5, color="#8c4d19") +
            scale_x_discrete(limits=af_metro_por_anio$anio[c(FALSE, TRUE)]) + 
            scale_y_continuous(labels=label_number(suffix=" millones", scale=1e-6), 
                                breaks=trunc(seq(0, max(af_metro_por_anio$tot) + 1e8, by=1e8)/1e8)*1e8) +
            ggtitle("Afluencia del metro en la CDMX") +
            xlab("Año") + ylab("Afluencia total") +
            theme(axis.text = element_text(face="bold"))
    })
    
    #---------------- Metrobús ----------------
    output$pand_mb_desc = renderText({
        "Esta gráfica representa la afluencia total del metrobús del 2005 al 2022. Como puedes ver,
        el uso del metrobús cayó drásticamente en el 2020, pero empezó a recuperarse en el 2021. 
        Los datos sólo cubren hasta agosto del 2022, por lo que probablemnte veamos un aumento 
        respecto al 2021. Es posible que en uno o dos años más incluso regresemos a los niveles del 
        2019, a pesar de que la capacidad del metrobús no ha aumentado en este periodo."
    })
    
    output$pand_mb = renderPlotly({
        # Agrupa por año
        af_mb_por_anio = af_mb %>% group_by(anio) %>% summarise(tot = sum(afluencia))
        
        ggplot(af_mb_por_anio, aes(x=anio, y=tot)) +
            geom_point(size=3, color="#8c4d19") +
            geom_line(size=1.5, color="#8c4d19") +
            scale_x_discrete(limits=af_mb_por_anio$anio[c(FALSE, TRUE)]) + 
            scale_y_continuous(labels=label_number(suffix=" millones", scale=1e-6), 
                                breaks=trunc(seq(0, max(af_mb_por_anio$tot) + 1e8, by=5e7)/1e7)*1e7) +
            ggtitle("Afluencia del metrobús en la CDMX") +
            xlab("Año") + ylab("Afluencia total") +
            theme(axis.text = element_text(face="bold"))
    })
    
    #---------------- Privado ----------------
    output$pand_priv_desc = renderText({
        "Esta gráfica representa la variación del tráfico durante la pandemia, tomando la semana
        10 del 2020 como punto de partida. Aunque inicialmente el tráfico se desplomó, a partir de
        la semana 23 comenzó a crecer y, en la semana 72 (semana 20 del 2021) estaba menos del 30%
        debajo de la semana 10. No se tienen datos a partir de la semana 72, pero a finales del 2022
        posiblemente estemos por encima de la semana 10 del 2020."
    })
    
    output$pand_priv = renderPlotly({
        ggplot(dif_porc_tomtom, aes(x=semana, y=variacion)) +
            geom_point(size=3, color="#8c4d19") +
            geom_line(size=1.5, color="#8c4d19") +
            scale_x_discrete(limits=dif_porc_tomtom$semana[c(FALSE, FALSE, TRUE)]) + 
            #scale_y_continuous(labels=label_number(suffix=" millones", scale=1e-6), 
                                #breaks=trunc(seq(0, max(af_mb_por_anio$tot) + 1e8, by=5e7)/1e7)*1e7) +
            ggtitle("Variación del tráfico en la CDMX durante la pandemia (respecto a la semana 10)") +
            xlab("Semana") + ylab("Variación") +
            theme(axis.text = element_text(face="bold"))
    })
    
    #################################### Contaminación #######################################
    #---------------- Costos ----------------
    output$cont_cost_desc = renderText({
        "Aquí puedes ver el PIB nacional comparado con los Costos Totales por Agotamiento y
        Degradación Ambiental (CTADA). Las escalas son diferentes, pero podemos resaltar
        que ambos han aumentado a más del doble de lo que eran en el 2003. 
        En el 2020, el 55.6% del CTADA provino de medios de transporte."
    })
    
    output$cont_cost_PIB = renderPlotly({
        ggplot(cont_cost, aes(anio)) + 
            geom_line(aes(y=PIB), colour="#d18f26", size=1.7) +
            scale_x_discrete(limits=cont_cost$anio[c(FALSE, FALSE, TRUE)]) +
            scale_y_continuous(labels=label_number(suffix=" millones", scale=1e-6),  
                                breaks=trunc(seq(0, max(cont_cost$PIB) + 1e6, by=1e6)/1e6)*1e6) + 
            theme(legend.position="none") + 
            xlab("Año") + ylab("Pesos") + 
            ggtitle("PIB de México por año") 
    })
    
    output$cont_cost_CTADA = renderPlotly({
        ggplot(cont_cost, aes(anio)) + 
            geom_line(aes(y=CTADA), colour="#13ad96", size=1.7) + 
            scale_x_discrete(limits=cont_cost$anio[c(FALSE, FALSE, TRUE)]) +
            scale_y_continuous(labels=label_number(suffix=" millones", scale=1e-6),  
                                breaks=trunc(seq(0, max(cont_cost$CTADA) + 1e6, by=5e4)/1e4)*1e4) + 
            theme(legend.position="none", plot.title=element_text(size=15)) + 
            xlab("Año") + ylab("Pesos") + 
            ggtitle("Costos Totales por Agotamiento y \nDegradación Ambiental en México")    
    })
    
    #################################### Misc. #######################################
    #---------------- Zonas ----------------
    output$misc_zonas_desc = renderText({
        print("En este mapa se hayan las regiones de la CDMX que tienen un bajo nivel de acceso
        al transporte público (por ahora nos estamos restringiendo al metro y metrobús). La forma 
        de cada región es una aproximación de las zonas en donde la estación de metro o metrobús más
        cercana está a 1h o más caminando.")
    })
    
    output$misc_mapa_zonas = renderLeaflet({
        # Crea un mapa
        misc_mapa_zonas = leaflet(options=leafletOptions(zoomControl=TRUE, zoomSnap=0, zoomDelta=20)) %>%
                addTiles() %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addGeoJSONv2(contorno_cdmx, color="#777777", opacity=0.1, fill=TRUE) %>%
                setView(lng=-99.12, lat=19.36, zoom=11.1)
        
        # Agrega las líneas del metro
        for (num_linea in c(sprintf("%d", seq(1:9)), "A", "B", "12")) {
            linea_json = fromJSON(file=paste0("metro/metro_linea", num_linea, ".json")) # línea json
            markers = read.csv(file=paste0("metro/metro_linea", num_linea, ".csv"), header=TRUE, encoding="UTF-8")
            
            misc_mapa_zonas = misc_mapa_zonas %>%
                            addGeoJSONv2(linea_json, color=metro_lineas_clrs[strtoi(metro_dict[[num_linea]])], opacity=1, fill=FALSE, group=toString(num_linea)) %>%
                            addMarkers(data=markers, label=markers$estacion, icon=icono_metro_sm, group=toString(num_linea))
        }
        
        # Agrega las líneas del metrobús
        for (num_linea in seq(1:7)) {
            linea_json = fromJSON(file=paste0("mb/mb_linea", num_linea, ".json")) # línea json
            markers = read.csv(file=paste0("mb/mb_linea", num_linea, ".csv"), header=TRUE, encoding="UTF-8")
            
            misc_mapa_zonas = misc_mapa_zonas %>%
                        addGeoJSONv2(linea_json, color=mb_lineas_clrs[strtoi(num_linea)], opacity=1, fill=FALSE, group=toString(num_linea)) %>%
                        addMarkers(data=markers, label=markers$estacion, icon=icono_mb_sm, group=toString(num_linea))
        }
        
        # Agrega las zonas
        misc_mapa_zonas = misc_mapa_zonas %>% 
                addGeoJSONv2(misc_zona_n, color="#ff1100", opacity=0.5, stroke=FALSE, fill=TRUE) %>%
                addGeoJSONv2(misc_zona_s, color="#00ffbb", opacity=1, stroke=FALSE, fill=TRUE) %>%
                addGeoJSONv2(misc_zona_e, color="#61ff00", opacity=1, stroke=FALSE, fill=TRUE) %>%
                addGeoJSONv2(misc_zona_o, color="#4400ff", opacity=0.5, stroke=FALSE, fill=TRUE)
        
        # Renderiza el mapa
        misc_mapa_zonas
    })
    
    ##################################### Observables ########################################
    
    # Actualiza la gráfica de barras de la af prom del metro
    observeEvent(input$metro_mapa_af_geojson_click, { # geojson_click sólo funciona con las líneas del metro y el contorno de la CDMX
      # Almacena el id de la línea seleccionada
      click = input$metro_mapa_af_geojson_click
      
      if(clickedId$id != click$id) {
        # Selecciona el elemento si algo más estaba seleccionado
        clickedId$id = click$id
      } else {
        # Deselecciona el elemento si es la segunda vez que se le hace click
        # o si se hace click al contorno de la CDMX
        clickedId$id = 0
      }
    })
}

# Hot reload (nota: generalmente no funciona)
options(shiny.autoreload = TRUE)
shinyApp(UI, Server)
