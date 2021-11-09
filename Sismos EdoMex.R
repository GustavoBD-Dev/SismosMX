---
title: "R Notebook"
output: html_notebook
---

```{r}
library(sf)
library(ggplot2)
library(tidyverse)
library(tmap)
library(dplyr)
library(raster)
library(ggrepel)
library(gganimate)
# Lectura de los arhivos sho para los paises de Mexico y Estados unidos
MEX <- st_read('F:/UAEMex/Mineria de datos/Reportaje/MX/México_Estados.shp')
# lectura del archivo con datos de sismos
sismos <- read.csv('F:/UAEMex/Mineria de datos/Sismos/SSNMX.csv') # 230159 Datos

EDOMEX<-st_read('F:/UAEMex/Mineria de datos/15_mexico/15_mexico/conjunto_de_datos/15mun.shp')
st_crs(4326)

```

```{r}
view(MEX)
str(sismos)
```



Obtenemos el mapa del estado de mexico, aqui podemos ver las coordenadas de 
Latitud y Longitud para el estado de Mexico
```{r}
MX15 <- MEX[MEX$CODIGO == "MX15", ] #obtenemose el mapa del estado de mexico
ggplot() +  # Crea un objeto ggplot a partir de un objeto vacio
  geom_sf(data=MX15, fill = "greenyellow" , color = 'black') +
  ggtitle( "Estado de México") +
  xlab("Longitud") + ylab("Latitud") +
  theme_minimal()

```


Como en el mapa nos proporciona los rangos, realizamos el filtro a los datos 
del archivo sismos, obteniendo los datos que se encuentran dentro de los rangos.
una vez que tenemos los datos de sismos cerca del Estado de México, podemos 
ubicarlos en el mapa con sus coordenadas que se nos proporciona en el dataset.

```{r}
#sismos <- read.csv('F:/UAEMex/Mineria de datos/Sismos/SSNMX.csv') # 230159 Datos
# seleccionamos los meteoritos que nos interesa graficar
sismosMEX <- sismos %>%
  #filter(Magnitud != "no calculable") %>%
  drop_na() %>% # Quitamos los elementos que faltan (na´s) para la representación gráfica
  #filter(as.numeric(Magnitud) > 5.0) %>%
  filter(between(Longitud, -101, -98),
         between(Latitud, 18, 20.5)) #%>%
  #arrange() 
  # se utiliza para ordenar las filas de un data frame de acuerdo a una o varias columnas/variables

ggplot(data = (MEX[MEX$ESTADO == "México", ])) +
  geom_sf(fill = "greenyellow", 
          color = 'black', ) +
  geom_point(data = sismosMEX,
             aes(x=Longitud, y = Latitud), 
             colour = "red", 
             size = 1) + #stroke = F
  ggtitle( "Sismos con epicentro cerca del Estado de México") +
  xlab("Longitud") + ylab("Latitud") +
  theme_minimal()
```


```{r}
sismos <- read.csv('F:/UAEMex/Mineria de datos/Sismos/SSNMX.csv') # 230159 Datos
# Establecemos la escala minima a encontrar 
escalaMaxima <- 5.0
# seleccionamos los sismos que nos interesa graficar
sismosMexico <- sismos %>%
  filter(Magnitud != "no calculable") %>%
  filter(as.numeric(Magnitud) > escalaMaxima) %>%
  drop_na() 
  
ggplot(data = MEX) +
  geom_sf(fill = "navyblue", 
          color = "black") +
  geom_point(data = sismosMexico,
             mapping = aes(
               x = Longitud,
               y = Latitud), 
             color = "greenyellow", 
             size = 1) +
  ggtitle( "Sismos de México de 1912 a 2021 > 5.0 escala Richter") +
  xlab("Longitud") + ylab("Latitud") +
  theme_dark()
```


```{r}
#sismos <- read.csv('F:/UAEMex/Mineria de datos/Sismos/SSNMX.csv') # 230159 Datos
# seleccionamos los meteoritos que nos interesa graficar



E1 <- MEX[MEX$ESTADO == "Guanajuato",]
E2 <- MEX[MEX$ESTADO == "Querétaro",]
E3 <- MEX[MEX$ESTADO == "Hidalgo",]
E4 <- MEX[MEX$ESTADO == "Tlaxcala",]
E5 <- MEX[MEX$ESTADO == "Puebla",]
E6 <- MEX[MEX$ESTADO == "Morelos",]
E7 <- MEX[MEX$ESTADO == "Guerrero",]
E8 <- MEX[MEX$ESTADO == "Michoacán",]
E9 <- MEX[MEX$ESTADO == "Distrito Federal",]
E10 <- MEX[MEX$ESTADO == "México",]

estadosVecinos <- rbind(E1,E2,E3,E4,E5,E6,E7,E8,E9,E10)

mapa_1<- estadosVecinos  %>% mutate(centroid = map(geometry, st_centroid),
                         coords = map( centroid,st_coordinates),
                         coords_x=map_dbl(coords,1), 
                         coords_y = map_dbl(coords,2))


datosNuevos <- datosNuevos %<%
   filter(Magnitud != "no calculable")

sismosMEX <- sismos %>%
  filter(Magnitud != "no calculable") %>%
  filter(as.numeric(Magnitud) > 4.0) %>%
  drop_na() %>% # Quitamos los elementos que faltan (na´s) para la representación gráfica
  filter(between(Longitud, -101, -98),
         between(Latitud, 18, 20.5)) #%>%
  #arrange() 
  # se utiliza para ordenar las filas de un data frame de acuerdo a una o varias columnas/variables

ggplot(data = mapa_1) +
  geom_sf(fill = "greenyellow", 
          color = 'black', ) +
  geom_text(mapping = aes(coords_x, 
                          coords_y, 
                          label = ESTADO), 
            size = 4) + # size of chr
  geom_point(data = sismosMEX,
             aes(x=Longitud, y = Latitud), 
             colour = "red", 
             size = 1) + #stroke = F
  ggtitle( "Sismos con epicentro cerca del Estado de México") +
  xlab("Longitud") + ylab("Latitud") +
  theme_minimal()
```

Obtenemos los sismos a partir de de subtraccion de cadenas en la 
referencia de localización
```{r}
SISMOSEDOMEX <- sismos %>%
  filter(str_sub(sismos$Referencia.de.localizacion, start = -3)=="MEX")

SISMOSEDOMEX
ggplot(data = (MEX[MEX$ESTADO == "México", ])) +
  geom_sf(fill = "navyblue", 
          color = 'black', ) +
  geom_point(data = SISMOSEDOMEX,
             aes(x=Longitud, y = Latitud), 
             colour = "greenyellow", 
             size = 1) + #stroke = F
  ggtitle( "Sismos con epicentro cerca del Estado de México") +
  xlab("Longitud") + ylab("Latitud") +
  theme_dark()

SISMOSEDOMEX <- sismos %>%
  filter(str_sub(sismos$Referencia.de.localizacion, start = -3)=="MEX") %>% 
  filter(Magnitud != "no calculable")%>% # Obtenemos sismos con magnitud
  filter(as.numeric(Magnitud) > 2.5)

SISMOSEDOMEX
ggplot(data = (MEX[MEX$ESTADO == "México", ])) +
  geom_sf() +
  geom_point(data = SISMOSEDOMEX,
             aes(x=Longitud, 
                 y = Latitud,
                 color = Magnitud,
                 size = Magnitud), stroke=F)+ 
  ggtitle( "Sismos con epicentro cerca del Estado de México") +
  xlab("Longitud") + ylab("Latitud") +
  theme_dark()

```



```{r}
ISO3 <- c(" AGS", ", BC", " BCS", "CAMP", "CHIS", "CHIH", "CDMX", "COAH", " COL", 
          " DGO", " GTO", " GRO", " HGO", " JAL", " MEX", "MICH", " MOR", " NAY", 
          ", NL", " OAX", " PUE", " QRO", " QRO", ", QR", " SIN", " SON", " TAB", 
          "TAMS", "TLAX", " VER", " YUC", " ZAC")

NombreEstados <-c("Aguascalientes","Baja California","Baja California Sur","Campeche",
             "Coahuila de Zaragoza","Colima","Chiapas","Chihuahua","Ciudad Mexico",
             "Durango","Guanajuato","Guerrero","Hidalgo","Jalisco","Edo. de Mexico",
             "Michoacan","Morelos","Nayarit","Nuevo Leon","Oaxaca","Puebla",
             "Queretaro","Quintana Roo","San Luis Potosi","Sinaloa","Sonora",
             "Tabasco","Tamaulipas","Tlaxcala","Veracruz de Ignacio de la Llave",
             "Yucatán","Zacatecas")

# Calculamos el numero de sismos por estado
TotalSismos <- c() # Vector para agregar el total de sismos
for (var in ISO3){ # Iteramos en las abreviaciones ISO3
  SISMOSEDOMEX <- sismos %>% 
    # Filtramos por cada abreviacion del vector ISO3
    filter(str_sub(sismos$Referencia.de.localizacion, start = -4) == var)# %>%
    #filter(Magnitud != "no calculable")
  # Obtenemos el total de valores y lo agregamos al vector de totales 
  TotalSismos <- c(TotalSismos, count(SISMOSEDOMEX))
}

# Ordenamos los estados por el codigo de estado
NEWMEX <- MEX[order(MEX$CODIGO), ]
# agregar un nuevo atributo a la columna de atributos
NEWMEX[["Total_Sismos"]] <- TotalSismos
#Los centros de cada estado 
NEWMEX <- NEWMEX  %>% mutate(centroid = map(geometry, st_centroid),
                         coords = map( centroid,st_coordinates),
                         coords_x=map_dbl(coords,1), 
                         coords_y = map_dbl(coords,2)) 
#mapa con nombre
ggplot(data = NEWMEX)+
  #geom_sf(aes(fill=Total_Sismos, color = "black")) +
  geom_sf(aes(fill=ESTADO), color= "white", size = 0.3) +
  geom_text(mapping = aes(coords_x,coords_y,label=Total_Sismos),
            size= 3,  min.segment.lenght=0)+labs( x= "", y="" )+
ggtitle("Sismos por estados en México")+
  theme_dark()
```









```{r}
SISMOSEDOMEX <- sismos %>%
  filter(str_sub(sismos$Referencia.de.localizacion, start = -3)==" MEX")
  filter(str_sub(sismos$Referencia.de.localizacion, start = -3)==" MOR")
  filter(str_sub(sismos$Referencia.de.localizacion, start = -3)==" PUE")
  filter(str_sub(sismos$Referencia.de.localizacion, start = -3)==" HGO")
  filter(str_sub(sismos$Referencia.de.localizacion, start = -3)=="TLAX")
  filter(str_sub(sismos$Referencia.de.localizacion, start = -3)=="CDMX")
 
SISMOSEDOMEX
#ggplot(data = (MEX[MEX$ESTADO == "México", ])) +
ggplot(data = estadosVecinos) +
  geom_sf(fill = "navyblue", 
          color = 'black', ) +
  geom_point(data = SISMOSEDOMEX,
             aes(x=Longitud, y = Latitud), 
             colour = "greenyellow", 
             size = 1) + #stroke = F
  ggtitle( "Sismos con epicentro cerca del Estado de México") +
  xlab("Longitud") + ylab("Latitud") +
  theme_dark()

SISMOSEDOMEX <- sismos %>%
  filter(str_sub(sismos$Referencia.de.localizacion, start = -3)=="MEX") %>% 
  filter(Magnitud != "no calculable")%>% # Obtenemos sismos con magnitud
  filter(as.numeric(Magnitud) > 2.5)

SISMOSEDOMEX
ggplot(data = (MEX[MEX$ESTADO == "México", ])) +
  geom_sf() +
  geom_point(data = SISMOSEDOMEX,
             aes(x=Longitud, 
                 y = Latitud,
                 color = Magnitud,
                 size = Magnitud), stroke=F)+ 
  ggtitle( "Sismos con epicentro cerca del Estado de México") +
  xlab("Longitud") + ylab("Latitud") +
  theme_dark()
```









