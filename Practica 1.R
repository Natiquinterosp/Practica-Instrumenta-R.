## INICIO DE PRACTICA GUIADA ---------------------------------------------------

## En el siguiente script vamos a iniciar un analisis exploratorio y de visualizacion de un dataset que ya conocemos, delitos! La idea es poner a prueba todo lo que vimos en las clases y desafiarnos con preguntas que requieren una busqueda particular. Esta pensado para que exploremos libremente, intenten ser creativos y ambiciosos a la hora del analisis y sumen cualquier conocimiento adicional que puedan compartir con el resto. 

## Introduccion ----------------------------------------------------------------

## 1. Vamos a iniciar creando un proyecto nuevo en una carpeta para alojar todos los documentos de este trabajo. Recuerden poner un nombre acorde y setear el espacio de trabajo. 

## 2. Cargen todas las librerias y datasets a utilizar. Instalen las librerias que no tengan descargadas y luego importenlas en su espacio de trabajo.      


library(skimr)
library(tidyverse)
library(janitor)
library(hrbrthemes)
library(lubridate)
library(summarytools)
library(dplyr)
library(ggsci)
library(ggplot2)

delitos_2019 <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2019.csv")


delitos_2018 <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2018.csv")

delitos_2017 <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2017.csv")

## 3. Como pueden ver importamos las librerias desde una pagina web. Estas paginas suelen ser una gran fuente de datos cuando queremos inciar un analisis. Pueden revisarlo en el siguiente link: https://mapa.seguridadciudad.gob.ar/ 

## Limpieza de los datos -------------------------------------------------------

## 4. Unifica los tres datasets en uno solo de tal modo que quede uno debajo del otro. En el caso de que aparezca un error, que sucede con la variable franja horaria? Transformala y luego uni los datasets.  

delitos <- bind_rows(delitos_2019,
                     delitos_2018,
                     delitos_2017 %>% mutate(franja_horaria = as.double(franja_horaria)))

view(delitos)


## 5.Cuantas filas tiene cada dataset? Y cuantas columnas? 

head(delitos)

summary(delitos)

delitosbarrios

# id             fecha            franja_horaria  tipo_delito       
#Min.   :125088   Min.   :2017-01-01   Min.   : 0.00   Length:361958     
#1st Qu.:217073   1st Qu.:2017-10-07   1st Qu.:10.00   Class :character  
#Median :307563   Median :2018-06-30   Median :15.00   Mode  :character  
#Mean   :307563   Mean   :2018-06-28   Mean   :13.78                     
#3rd Qu.:398052   3rd Qu.:2019-03-19   3rd Qu.:19.00                     
#Max.   :488541   Max.   :2019-12-31   Max.   :23.00                     
#NA's   :43                        
 #subtipo_delito     cantidad_registrada     comuna          barrio         
 #Length:361958      Min.   :1           Min.   : 1.000   Leng


## 6. Que tipo de datos contiene el dataset? 

class(delitos$franja_horaria)

#[1] "numeric"


## 7. Cuantos valores faltantes se registran en cada variable? 

sum(is.na(delitos$franja_horaria))

#[1] 43


## 8. Que sucede con la variable cantidad registrada? Explora los valores unicos, cuales son los valores mas frecuentes y saca conclusiones al respecto. Puede que tengas que buscar sobre tablas de frecuencia.  

unique(delitos, cantidad_registrada = FALSE)

valoresfrecuentes <- freq(delitos$cantidad_registrada)

##Frequencies  
#delitos$cantidad_registrada  
#Type: Numeric  

#Freq     % Valid   % Valid Cum.     % Total   % Total Cum.
# 1   361929    99.99199       99.99199    99.99199       99.99199
#2       25     0.00691       99.99889     0.00691       99.99889
#3        3     0.00083       99.99972     0.00083       99.99972
#4        1     0.00028      100.00000     0.00028      100.00000
#<NA>        0                                0.00000      100.00000
#Total   361958   100.00000      100.00000   100.00000      100.00000


## 9. Cual es la relacion entre tipo de delito y subtipo de delito? Describir. Puede que tengas que buscar sobre tablas de contingencia


table(delitos$cantidad_registrada)

##1      2      3      4 
#361929     25      3      1 
 

## 10. Hace el grafico pertinente para mostrar los tipos de delitos existentes y sus frecuencias. No olvides incluir titulo, nombres a los ejes y colores.  

names(delitos)

#[1] "id"                  "fecha"               "franja_horaria"     
#[4] "tipo_delito"         "subtipo_delito"      "cantidad_registrada"
#[7] "comuna"              "barrio"              "lat"                
#[10] "long"  




selecciondelitos <- select(delitos, "tipo_delito", "subtipo_delito", "cantidad_registrada", "barrio")



# A tibble: 361,958 x 4
#tipo_delito           subtipo_delito  cantidad_registrada barrio          
#<chr>                 <chr>                         <dbl> <chr>           
# 1 Lesiones              Siniestro Vial                    1 Nueva Pompeya   
#2 Robo (con violencia)  NA                                1 Liniers         
#3 Lesiones              Siniestro Vial                    1 Chacarita       
#4 Hurto (sin violencia) Hurto Automotor                   1 Floresta        
#5 Robo (con violencia)  Robo Automotor                    1 Parque Patricios
#6 Robo (con violencia)  NA                                1 Boca            
#7 Lesiones              Siniestro Vial                    1 Liniers         
#8 Hurto (sin violencia) Hurto Automotor                   1 Villa Pueyrredón
#9 Robo (con violencia)  Robo Automotor                    1 Barracas        
#10 Robo (con violencia)  NA                                1 Almagro         
# ... with 361,948 more rows


library(ggplot2)

freqdelitos <- ggplot(selecciondelitos, aes(x = tipo_delito)) + geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) )

freqdelitos

## 11. Hace el grafico pertinente para mostrar como se distribuye la variable franja horaria. No olvides incluir titulo, nombres a los ejes y colores.  

library(tidyverse)
library(hrbrthemes)



#prueba

distribucion_delitos <- delitos %>% 
  group_by(tipo_delito, franja_horaria) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = round(n/sum(n) *100, 2))

distribucion_delitos


distribucion <- ggplot(data = delitos) +
  geom_histogram(mapping = aes(x=franja_horaria), color= "blue", fill="blue", bins=24)+
  theme_minimal()+
  labs(title="Frecuencia de Delitos según Franja Horaria",
       y="Delitos",
       x="Franja horaria")
  

## 12. Incorporaremos al grafico anterior una segmentacion por tipo de delito y un filtro para quedarnos con los delitos que hayan ocurrido especialmente en Puerto Madero. 



puerto_madero <-   filter(barrio == 'Puerto Madero') %>% 
  ggplot(data = delitos) + 
  geom_histogram(mapping = aes(x=franja_horaria), color= "blue", fill="blue", bins=24)+ 
  theme_minimal()+
  labs(title="Frecuencia de Delitos según Franja Horaria",
       y="Delitos",
       x="Franja horaria")
  


























