#=====================================================#
#                                                     #
#   Simon Cortazar Alba - 202020930 - Version 4.2.1   #
#   Isabella Cruz -   202011925 - Version 4.2.2       #
#   Juan Sebastián Muñoz -  202021633 - Version 4.2.2 #
#                                                     #
#                 Problem set 3 - R                   #     
#                                                     #
#=====================================================#

# Nota: Para que el script corra de manera ininterrumpida con un solo run hace falta dirigirse al punto dos del taller y no instalar
# html tools. Aquí se hizo porque hacía falta su versión más reciente. Por tanto, si ya está actualizada, se puede borrar el comando
# con la instalación de dicho paquete, y el script correrá ininterrimpidamente con un solo run.

# 0. Prelimianres

rm(list=ls()) # Limpiamos el environment
require("pacman")
p_load(tidyverse, rio, 
      arrow, 
      broom, 
      mfx, 
      margins,  
      estimatr,                            # Descargamos las librerias necesarias para desarrolalr el primer punto del taller
      lmtest, 
      fixest,           #Usamos fixest para tener disponibles funciones que 
      modelsummary,     #que nos permitan hacer estimaciones con múltiples efectos fijos
      stargazer)       #Usamos stargazer para imprimir las tablas
                       #de los modelos que vamos a usar para las estimaciones 


# 1. Regresiones

  # Importamos base de datos

  data <- import("input/data_regresiones.rds") # importamos la base de datos

  # 1.1 Estimaciones - Tres modelos econométricos cuya var. dep. sea precio de la vivienda
  
  # Primero, vamos a transforamar la variable del precio de la vivienda a logaritmos para 
  # reducir las magnitudes de los precios que, al ser tan elevados, resulta dificil manejarlos.
  
  data <- data %>% 
    mutate(ln_precio=log(price), na.rm=T)
  
    unique(is.infinite(data$ln_precio)) # Notemos que hay valores que dan infinito cuando
                                        # hicimos la transformación, esto solo pasa para valores muy cercanos a cero
                                        # en este caso fueron tres valores en particular.
  
    data$ln_precio[is.infinite(data$ln_precio)] <- NA # Reemplazamos los valores donde hay infinito
                                                      # por NAs.
  
    # Modelo 1
  
  # Para este primer modelo resula interesante evaluar las diferencias en los precios de las viviendas
  # dependiendo del tipo de inmbueble; es decir, si es un apartamento o una casa. De este modo, construimos
  # una variable dummy que tome el valor de uno si la vivienda es una casa y cero si es un apartamento.
  # esto se puede lograr indicando el regresión que la variables es as.factor(), o también así:
  
  # data <- data %>% 
  # mutate(casa=ifelse(property_type=="Casa", 1, 0))
    
  # Notemos que esto se puede lograr especificando en las regresiones si property type es "factor"
  
  # Planteamos el modelo de regresión
  
  mco_1 <- lm(ln_precio ~ as.factor(property_type) + surface_total, data=data) # Explicamos el precio por el tipo de inmueble
                                                     # y la superficie de este.

    # Modelos 2 
  
  # Para este modelo vamos a estimar la diferencia en los precios de los diferentes tipos de viviendas 
  # teniendo en cuenta más controles que los anteriormente añadidos, como la cantidad de baños
  
  mco_2 <- lm(ln_precio ~ as.factor(property_type)  + surface_total + bathrooms, data=data)
  
  
    # Modelo 3
  
  # Para este modelo estimamos las diferencias en los precios para los diferentes tipos de viviendas 
  # teniendo en cuenta ahora, muchos más controles, como la  cantidad de cuartos.
  
  mco_3 <- lm(ln_precio ~ as.factor(property_type)  + surface_total + bathrooms + rooms, data=data)

# notemos que todos los modelos estimados denotan estimaciones donde cada vez se añaden más controles 
# para que, así, el coeficiente que encontremos de las diferencias promedios en los precios de los
# diferentes tipos de casas sea cada vez más limpio y evitemos sesgos por variables omitidas.
  
  
  # 1.2 Presentar resultados
  
    # Combinamos en una tabla los resultados de los COEFICIENTES estimados de los tres modelos
  
    
   print(msummary(list("Modelo 1"=mco_1, "Modelo 2"= mco_2, "Modelos 3"= mco_3), # Indicamos los modelos que vamos a incluir en la tabla
             coef_rename = c("Intercepto","Casa inmueble", "Área del imbueble", "Número de baños",
                             "Número de cuartos"), # Cambiamos los nombres de las variables para la exportación
             gof_map= "omit", 
             estimate = c("{estimate}{stars}"), # Se omiten todas las estadísticas de bondad de ajuste. Además del
                                                # coeficiente se reporta los errores estándar y las estrellas de 
                                                # significancia.
                                                 
             output="output/resultados_regresiones.xlsx")) # Exportamos de una vez ls tabla
   
   # Notemos al mismo tiempo que cumplimos de una vez el punto 1.3 al exportar la tabla de resultados de los coeficientes.
   # Además, imprimimos la tabla en la consola para visualizar los resultados.
    
    # Generamos un único gráfico que represente los tres modelos (coefplot)
    
    plot <- modelplot(list("Modelo 1"=mco_1, "Modelo 2"= mco_2, "Modelos 3"= mco_3), # Indicamos los modelos que vamos a incluir en la tabla
              conf_level = 0.95, #Indicamos el nivel de los intervalos de confianza
              coef_rename = c("Intercepto","Casa inmueble", "Área del imbueble", "Número de baños",  # Cambiamos los nombres de las variables para la exportación
                              "Número de cuartos")) + 
      theme_light() + 
      geom_vline(aes(xintercept = 0),color="black",linetype="dashed",width=1)+ # Ponemos una linea de x=0
      theme(axis.text = element_text(color = "black", size = 10)) + # Cambiamos las características de los ejes
      labs(y="",x= "Estimaciones") + # Cambiamos las etiquetas de los ejes
      ggtitle("Comparación de Modelos") + # Ponemos un título
      theme(
      plot.title = element_text(family = "Helvetica", face = "bold", size = (17)), # Editamos el título de la gráfica
      legend.title = element_text(face = "bold.italic", family = "Helvetica"), # Editamos las características del título
      legend.text = element_text(face = "italic", family = "Helvetica"), # Editamos las característsicas del texto del gráfico
      axis.title = element_text(family = "Helvetica", size = (10)), # Editamos los ejes
      axis.text = element_text(family = "Helvetica", size = (10)) # Editamos los ejes
      )
      
    # 1.3 Exportamos los resultados
    
    ggsave("output/plot_regresiones.png") # Exportamos el resultado

# ====================================================================================================== #
    
    
# 2. Datos espaciales
    
# 2.0 Preliminares
    
    install.packages("htmltools") # Instalamos una versión más reciente de este libreria apra correr leaflet
    library(htmltools) # Actualizamos htmltools para realizar el segundo ejercicio
    
rm(list=ls())
require(pacman)
p_load(tidyverse,rio,skimr,
       sf, 
       leaflet, 
       tmaptools,                                  # Descargamos las librerias necesarias para desarrolalr el primer punto del taller
       ggsn, 
       ggmap, 
       osmdata,
       dplyr) 
# En este ejercicio, usaremos específicamente osmdata, pues permite usar la información de mapas "raster" en el formato OpenStreetMap. 
# Igualmente, usamos ggmap para usar específicamente información espacial de GoogleMaps.
# Para poder analizar este formato de información, se usa tmaptools, que es justamente una librería que aporta herramientas para procesar información espacial.
# También usamos leaflet, que permite crear mapas interactivos. Esto es justamente lo que buscamos con las líneas que corresponden al apartado 2.2.
# Finalmente, se debe usar ggsn para poder añadir información visual relevante para los mapas, como escalas o símbolos para identificar el norte.
# Cabe mencionar que gran parte de las bibliotecas anteriormente discutidas requieren el uso de ggplot.
# 

# 2.1 Seleccione una ciudad de Colombia y descargue los restaurantes y parques.

  # Creamos un objeto que almacene el espacio geográfico

# Buscamos cual es la "key" del amenity del restaurante

  print(n=127, available_tags("amenity")) # El amenity del restaurante es "restaurant"
  
# Obtenemos es spacial box de medellín
  
  osm_rest <- opq(bbox=getbb("Medellín Colombia")) %>% 
        add_osm_feature(key="amenity", value="restaurant") # Dentro del spacial box de Medellín buscamos el amenity de restaurante 
                                                           # usando la llave que encontramos antes.
  
  osm_rest_sf <- osmdata_sf(osm_rest)     #  Editamos lo anterior para obtener los resultados en forma de datos sf, usando la librería sf.
  
  
  restaurant_sf <- osm_rest_sf$osm_points %>% select(osm_id, amenity) %>% # Solamente seleccionamos la id de Open Street map de los restaurantes
                                                                          # más el tipo de amenity.
                   mutate(type="restaurant")         # Creamos una variable de identificación por el tipo de sitio, que más adelante también crearemos a los demás lugares espaciales
                                                     # para después crear el mapa.
  
  restaurant_sf 

  # Ahora hacemos lo mismo para los parques
  
  osm_park<- opq(bbox=getbb("Medellín Colombia")) %>% 
    add_osm_feature(key="leisure", value="park")  # De la clase ya se sabía que la "key" era leisure y el valor "park"
  # Dentro del spacial box de Medellín buscamos el leisure de parque 
  # usando la llave que encontramos antes.
  
    osm_park_sf <- osmdata_sf(osm_park) #  Editamos lo anterior par aobtener los resultados en forma de datos sf.
                    
                    
    park_sf <- osm_park_sf$osm_polygons %>% select(osm_id, name) %>% # Solamente seleccionamos la id de Open Street map de los restaurantes
                                                                    # más el tipo de amenity.
      
               mutate(type="park") # Creamos una variable de identificación por el tipo de sitio, que más adelante serivrá para después crear el mapa.
    park_sf
  
  # Con el objeto de park y restaurante ya tenemos los objetos necesarios para el punto.
  
  
  # 2.2 Visualizamos el punto que acabamos de hacer.
    
  leaflet() %>% addTiles() %>% addPolygons(data=park_sf) # Para los parques // Visaulizamos los parques en OSM. 
  
  leaflet() %>% addTiles() %>% addCircleMarkers(data=restaurant_sf , col="darkblue") # Para los restaurantes // Visaulizamos los restaurantes en OSM
  
  # 2.3 Geocodificamos una dirección del punto 2.1
  
  direccion <- geocode_OSM("Calle 46 %23% 55-54, Medellín", as.sf = T) %>% # Sacamos los datos geoespaciales de una dirección cualquiera de Medellín, como la altuitud y longitud
                                                                           #Es decir, las coordenadas.
               mutate(type="direccion") # Creamos una variable de identificación por el tipo de sitio, que más adelante serivrá para después crear el mapa.
  direccion
  
  # 2.4 Exportamos el mapa
  
  # a. Importamos datos geoespaciales administrativos de Medellín
  
  osm_medellin<- opq(bbox=getbb("Medellín Colombia")) %>% # Dentro del spacial box de Medellín buscamos los datos de delimitación administrativa de la ciudad
                                                          # Es decir, buscamos los datos para encontrar las delimitaciones espaciales de Medellín por nivel administrativo
    add_osm_feature(key="boundary", value="administrative") 
  
  medellin_sf <- osmdata_sf(osm_medellin) # Lo transformamos en datos sf

  medellin_sf <- medellin_sf$osm_multipolygons %>% subset(admin_level==8) 
  
  # En esta última línea de código se sacaron todas las delimitaciónes "boundaries" que indicaran el nivel administrativo que mejor describiera las comunas
  # y barrios de medellín. En principio, para obtener a medellín con sus comunas hizo falta especificar el nivel 8, que según la wiki OSM tambié corresponde
  # a capitales departamentales (urbanas) más sus barrios o comunas. 
  

# Añadimos calles
  
  osm_layer <- get_stamenmap(bbox = as.vector(st_bbox(medellin_sf)), 
                             maptype="toner", source="osm", zoom=13)
  
  # Como vamos a incluir calles, obtenemos las calles de Medellín.
 
  
  mapa_completo <- ggmap(osm_layer) +  # Graficamos sobre el mada de las calles 
                    geom_sf(data=medellin_sf, alpha=0.3 , inherit.aes=F) + # Añadimos la forma de Medellín
                    scalebar(data = medellin_sf, dist = 5 , transform = T , dist_unit = "km") + # Añadimos una barra de escalas
                    north(data = medellin_sf,location = "topleft") +  # Añadimos la estrella del norte
                    theme_linedraw() # Cambiamos el tema para que el mapa se viera mejor
  
  mapa_completo # Aquí tenemos el mapa base sobre el cual trabajar e identificar el  restaurante, al dirección y los parques
  

  mapa_lugar    <-  mapa_completo + # Al mapa anterior le añadimos lo siguiente: 
                    geom_sf(data=restaurant_sf, aes(fill="restaurant"), colour="blue", inherit.aes=F, size=0.5) + # Le añadimos los restaurantes como puntos
                    geom_sf(data=park_sf, aes(fill="park"),colour="light green", inherit.aes=F, size=5) + # Le añadimos los parques que se van a rellenar de verde. Esto ya que especificamos fill y 
                                                                                                          # la geometría del parque son polígonos y no puntos
                    geom_sf(data=direccion, aes(fill="direccion"), colour="orange" , inherit.aes=F, size=3)+ # Añadimos la dirección que especificamos, la hacemos más grande para que se pueda ver
                    coord_sf() + #especificamos el sistema de coordenadas para gráficos que contienen datos espaciales sf
                    labs(title = "Mapa Medellín", # Añadimos título
                    subtitle = "Restaurantes, parques y una direccion", # Añadimos subtítulo
                    fill = "Lugares") # Cambiamos el título de la guía de colores

 mapa_lugar
 
 # Exportamos el mapa

 ggsave("output/mapa_amenities.png") # Guardamos el mapa en la carpeta de outputs

 # ====================================================================================================== #
 
 # 3. Web Scraping
 
  #. 3.0 - Preliminares
 
     rm(list=ls()) # Limpiamos el entorno
     require("pacman")
     p_load(rvest,tidyverse, data.frame, rio,
            wesanderson, wordcloud)             # Llamamos los paquetes que vamos a utilizar
 
#Para este ejercicio específico, usamos rvest que permite obtener información de una página de internet y traerla a R. 
#Para el lado estético, se usa wesanderson para obtener algunas paletas de color específicas,
#También usamos wordcloud, que usamos para generar "nubes de palabras", wordclouds.
     
     
  # 3.1 Leemos el siguiente link
     
     url_1 <-  "https://es.wikipedia.org/wiki/Departamentos_de_Colombia" # Creamos un objeto que contenga el string de la página
     browseURL(url_1) # Revisamos que se pueda acceder desde R a la página
 
     html_1 <- read_html(url_1) # Leemos el html y lo guardamos en un objeto que contenga el HTML de la página
     class(html_1) # Notamos que es de tipo xml_document
     
  # 3.2 Extraemos el título de la página
     
     html_1 %>% html_nodes(xpath = '//*[@id="firstHeading"]/span') %>% html_text()
     
# Sacamos el xpath '//*[@id="firstHeading"]/span' inspeccionando el título en la página de wikipedia. 
# Para que sea en formato texto utilizamos el html_text
    
 
 # 3.3 Extraemos la tabla de los departamentos de Colombia
     
     tabla <- html_1 %>% html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[3]') %>% html_table() 
     
# Sacamos el xpath '//*[@id="mw-content-text"]/div[1]/table[3]' inspeccionando la table en la página de wikipedia.      
# Para importarlo en formato tabla utilizamos el html_table
     
     
     export(tabla,"output/tabla_departamento.xlsx") # Exportamos la tabla en formato excel.
 
 # 3.4 Extraemos todos los párrafos de la página
     
     parrafos <- html_1 %>% html_elements("p") %>% 
                 html_text()                        # Extraemos todos los elementos p (párrafos) a R.
                                                    # para que sea en formato texto utilizamos el html_text
     
     parrafo_list=as.list(parrafos) # Convertimos el vector en una lista
     
     
     names(wes_palettes)
     colores <- wes_palette("Darjeeling1", type="continuous")     # Seleccionamos los colores para hacer la nube de palabras
    
      
      png("output/nube_palabras.png") # Indicamos que vamos a exportar en png la siguiente gráfica
      
      nube <- wordcloud(parrafo_list, scale=c(3.5,.8), random.order = F, color=colores, rot.per = 0.15, max.words = 75) # Creamos la nube
      
      title(main= "Mapa Colombia", cex.main=2.5) # Ponemos un título
      
      dev.off() # Indacamos que lo que está arriba es lo que se va a exportar
 


