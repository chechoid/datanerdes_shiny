library(shiny)
library(tidyverse)
library(lubridate)

# Levantamos datasets
calidad <- read_csv('data/calidad-de-aire-2019.csv') %>% 
    mutate(co_centenario=as.numeric(co_centenario),
           no2_centenario=as.numeric(no2_centenario),
           pm10_centenario=as.numeric(pm10_centenario),
           co_cordoba=as.numeric(co_cordoba),
           no2_cordoba=as.numeric(no2_cordoba),
           pm10_cordoba=as.numeric(pm10_cordoba),
           co_la_boca=as.numeric(co_la_boca),
           no2_la_boca=as.numeric(no2_la_boca),
           pm10_la_boca=as.numeric(pm10_la_boca))

#hacemos todo el procesamiento
centenario <- calidad %>% 
    select(fecha, hora, co_centenario, no2_centenario, pm10_centenario) %>%
    rename('co'=co_centenario,
           'no2'=no2_centenario,
           'pm10'=pm10_centenario) %>% 
    gather(key='medicion', value='valor', c(3:5)) %>% 
    mutate(observatorio='Centenario')

laboca <- calidad %>% 
    select(fecha, hora, co_la_boca, no2_la_boca, pm10_la_boca) %>% 
    rename('co'=co_la_boca,
           'no2'=no2_la_boca,
           'pm10'=pm10_la_boca) %>% 
    gather(key='medicion', value='valor', c(3:5)) %>% 
    mutate(observatorio='La Boca')

cordoba <- calidad %>% 
    select(fecha, hora, co_cordoba, no2_cordoba, pm10_cordoba) %>% 
    rename('co'=co_cordoba,
           'no2'=no2_cordoba,
           'pm10'=pm10_cordoba) %>% 
    gather(key='medicion', value='valor', c(3:5)) %>% 
    mutate(observatorio='Córdoba')

#esta es la base con la que vamos a trabajar
calidad_tidy <- rbind(centenario, cordoba, laboca)

# UI
ui <- fluidPage(
    #Título de la página
    titlePanel("Calidad del Aire en Buenos Aires"),
    hr(), #HTML: una línea horizontal
    
    # Sidebar: slider para los bins 
    sidebarLayout(
        sidebarPanel(
            #primer parámetro: el selector de observatorio
            selectInput(inputId = "obs", #id
                        label = "Observatorio",#lo que figura
                        choices = c('La Boca', 'Centenario', 'Córdoba'), #choices y values acá son lo mismo
                        selected = 'Centenario'), #cuál es el default?
            
            hr(),
            
            #segundo parámetro: un slider
            sliderInput(inputId = "size", 
                        label = "Tamaño del punto:", 
                        min = 1, max = 3, 
                        value = 1),
            
            hr(),
            
            #tercer parámetro: rango de fechas
            dateRangeInput(inputId = "fecha",
                           label="Fecha:",
                           start = "2019-01-01",
                           end = "2019-06-01",
                           min="2019-01-01",
                           max="2019-06-01")
        ),

        # El panel principal: ¿qué queremos ver?
        mainPanel(
           plotOutput('grafico') #id
           )
    )
)

# Server: donde ocurre la magia
server <- function(input, output) {
    
    subset_observatorio <- reactive({
        calidad_tidy %>% 
            filter(observatorio %in% input$obs)%>% 
            mutate(fecha=ymd(fecha)) %>%
            filter(., between(fecha, input$fecha[1], input$fecha[2])) %>% #importante
            group_by(week(fecha), observatorio, medicion) %>%
            filter(!is.na(valor)) %>% 
            summarise(valor=mean(valor, na.rm = TRUE)) %>% 
            ungroup()
    })
    
    obs_title <- reactive({unique(subset_observatorio()$observatorio)})
    plot_title <- reactive({paste0('Mediciones en Observatorio ', obs_title())})
    
    graf_plot <- reactive({
        
       ggplot(subset_observatorio())+
            geom_line(aes(x=`week(fecha)`, y=valor, group=medicion, color=medicion))+
            geom_point(aes(x=`week(fecha)`, y=valor), size=input$size)+
            labs(x='Semanas',
                 y='Valores',
                 title=plot_title(),
                 subtitle='Fuente: datos.buenosaires.gob.ar')+
            theme_minimal()
    }) 

    output$grafico <- renderPlot({
       print(graf_plot())
    })
    
}

# App: esta es la parte donde todo corre
shinyApp(ui = ui, server = server)
