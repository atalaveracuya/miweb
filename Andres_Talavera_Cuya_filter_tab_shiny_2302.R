#### Tarea  final ####

## Andrés Talavera Cuya 

# Referencias: 
# Clases Reportes en R 
# https://www.davidsolito.com/post/conditional-drop-down-in-shiny/

#### COnfiguraciones Iniciales #### 

# Limpiar memoria 
rm(list = ls())


# Cargando librerias 
library(shiny)
library(tidyverse)
library(shinythemes)
library(plotly)
library(DT)

# data 
amazon <- read.csv("https://raw.githubusercontent.com/atalaveracuya/amazon_prime_titles/main/amazon_prime_titles.csv",
                   sep = ",",
                   dec = ".",
                   stringsAsFactors = TRUE
)

#crear variables 
summary(amazon$release_year)

amazon$decada <- findInterval(amazon$release_year,c(1920,1930,1940,1950,1960,1970,1980,1990,2000,2010))


amazon<-amazon %>% 
  mutate(decada=as.factor(decada))
levels(amazon$decada)<-c("1920 - 1930","1931 - 1940","1941 - 1950","1951 - 1960",
                         "1961 - 1970","1971 - 1980","1981 - 1990","1991 - 2000",
                         "2001 - 2010", "2011 - 2021")


amazon <- amazon[,c("decada","type", "title", "rating", "duration", "release_year", "listed_in")]

amazon <- amazon %>% rename_at('decada', ~'Decada')
amazon <- amazon %>% rename_at('type', ~'Tipo')
amazon <- amazon %>% rename_at('title', ~'Título')
amazon <- amazon %>% rename_at('rating', ~'Rating')
amazon <- amazon %>% rename_at('duration', ~'Duración')
amazon <- amazon %>% rename_at('release_year', ~'Año')
amazon <- amazon %>% rename_at('listed_in', ~'Genero')

###### Definamos nuestra interfaz de usuario ############ 

  ui_examen = fluidPage(

    # Modifiquemos el tema :
    theme = shinytheme("sandstone"),
    
    # Titulo
    titlePanel("Búsqueda de películas y TV Show del dataset Amazon Prime"),
    
    # Agregar el componente para los input/output
    
    sidebarLayout( # Inicia el sidebarlayout
      
      
    sidebarPanel( # Inicia el sidebarPanel
      
      
      # Titulo al paquete de inputs
      h3("Características"),
      
      uiOutput('select_var1'),
      uiOutput('select_var2'),
      # Marketing
      br(), br(),
      h5("Curso Reportes en R",
         img(src = "https://raw.githubusercontent.com/robintux/Images4Colabs/master/EPC_Transparente.png",
             height = "30px")
      )
      
      
      ), # finaliza el sidebarPanel


    #### mainPanel : Definamos las salidas (output)  ####
    
    mainPanel( 
      tableOutput("table")
    )
  ) # Finaliza el sidebarlayout
  )
  
  ###### Interfaz del servidor de aplicaciones ############
  
  server_examen = function(input, output, session) {
    
    # Implementamos código "reactivo" para cada vez que se selecciona
    # el "tipo de contenido", "decada" y "género"
    
    tab <- reactive({ # Inicio de reactive 
      
      amazon %>% 
        filter(Tipo == input$var1) %>%
        filter(Decada == input$var2)
    }) #   # Fin de la expresión reactiva  
    
    
    # Primer input denderizado por el servidor 
    output$select_var1 <- renderUI({ 

  selectizeInput('var1', 'Tipo de contenido', choices = c("seleccione" = "", levels(amazon$Tipo)))
      
          })    # Fin del renderTable  
      

    # 2nd Input rendered by the server <--
    output$select_var2 <- renderUI({
      
      selectizeInput('var2', 'Decada', choices = c("seleccione", levels(amazon$Decada)))
      
    })

    
    output$table <- renderTable({ 
      
      tab()
    
    })
    
}   # Fin de  la interfaz de aplicaciones


# Ejecución 
shinyApp(ui = ui_examen, server = server_examen)  

