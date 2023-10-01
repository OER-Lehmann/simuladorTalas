### BASE DE DATOS PARA APLICACION EN SHINY. ------------------------------------

### /// ESTOY HACIENDO DE CUENTA QUE SE TRATA DE LAS PALOMAS DE LA FCEN /// ###

### Establezco una semilla de aleatoriedad para tener reproducibilidad.
#para que se mantenga la semilla tengo que correr todo el script a la vez, o ejecutarla
#si pido el comando aleatorio mas de una vez

set.seed(314)

### Defino coordenadas X e Y para las unidades experimentales.

## Las coordenadas en X estan distribuidas uniformemente.

x_coords <- runif(n = 500,
                  min = 0,
                  max = 100)

## Las coordenadas en Y estan distribuidas uniformemente.

y_coords <- runif(n = 500,
                  min = 0,
                  max = 100)

### Defino una variable cualitativa: fenologia.

# Voy a crear una relacion teorica de 3:2 (sin_frutos:con_frutos).

fenologia <- sample(x = c("sin_frutos", "con_frutos"),
               size = 500,
               replace = TRUE,
               prob = c(0.4, 0.6))

### Defino una variable cuantitativa continua: largo.

# 'largo' tiene una distribucion normal con media = 6.7 y desvio = 1.97.
set.seed(314)
altura <- rnorm(n = 500,
               mean = 6.7,
               sd = 1.97)
summary(altura)
#se corrige para que todos los arboles sean de mas de 1 metro
altura<-altura+1

# ...pero voy a hacer que los que tienen frutos sean un poco mas grandes.

altura[fenologia == "con_frutos"] <-
  altura[fenologia == "con_frutos"] +
  runif(n = sum(fenologia == "con_frutos"),
        min = 0.2,
        max = 0.5) #esto lo modifique de oscar, no sé si tiene sentido este min y max, quizas debería ser mas chicos

summary(altura)
### Defino una variable ordinal: estado_de_salud.

estado_de_salud <- sample(x = c("terminal", "enfermo", "sano"),
                          size = 500,
                          prob = c(0.1, 0.35, 0.55),
                          replace = TRUE)

### Defino una variable cuantitativa continua: DAP



#tendría que ver qué tiene sentido de relación altura y DAP. Dap tiene sentido entre
#10 cm y 100 cm ponele, o sea que un árbol de 2m podría tener 10 cm podria ser
#una pendiente de 5. DAP esta definido como altura*5 + e, donde 'e' es un error normal.


DAP<-(5*(altura)) + rnorm(500, 0, 10)


# Se corrige para que todos los valores sean positivos y mas de 10

DAP <- DAP + 13.4
summary(DAP)

###Maru. Intengo agregar una variable discreta

#plumas_mudadas<-sample(0:10, size=500, replace=TRUE)

#pero asi va a ser uniforme, estaria bueno que sea poisson
cantidad_fustes<-rpois(500, 0.5)

cantidad_fustes<-cantidad_fustes+1


### Creo el data frame.

poblacion_talas <- data.frame(unidad_experimental = 1:500,
                                fenologia,
                                estado_de_salud,
                                altura,
                                DAP,
                              cantidad_fustes)

coords <- data.frame(x_coords,
                     y_coords)

### Aplicacion. -----

library(shiny)

set.seed(NULL)

ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(numericInput(inputId = "n",
                                value = 50,
                                label = "n",
                                min = 1,
                                max = 500)),
  mainPanel(
    
    plotOutput(outputId = "scatterplot")
    

    
  )
  
))

server <- function (input, output) {
  
  output$scatterplot <- renderPlot(
    
    {
      
      muestra <- sample(x = seq_len(nrow(poblacion_talas)),
                        size = input$n,
                        replace = FALSE)
      
      no_muestra <- seq_len(nrow(poblacion_talas))[-muestra]
      
      muestra_talas <- poblacion_talas[muestra, ]
      
      save(file = "muestra_talas.R", muestra_talas)
      
      colores <- rep("black", 500)
      colores[muestra] <- "red"
      
      puntos <- rep(1, 500)
      puntos[muestra] <- 19
      
      plot(coords$x_coords, coords$y_coords,
           col = colores,
           pch = puntos,
           axes = FALSE,
           xlab = NA,
           ylab = NA)
      
    }
    
  )
  
}

app_muestreo <- shinyApp(ui = ui, server = server)
app_muestreo 


