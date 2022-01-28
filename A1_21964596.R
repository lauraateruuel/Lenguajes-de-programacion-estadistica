# A1 LPE
#21964596-Laura Teruel

# Load packages
pacman::p_load(tidyverse, httr, xml2, jsonlite, leaflet, leaflet.extras, janitor)

# EJERCICIO a ------------------------------

# Cogemos los datos de la api
url = "https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/"
json <- fromJSON(url)
ds <- json$ListaEESSPrecio %>% as_tibble
# Guardamos los datos en csv
write_csv(ds, "gasolineras.csv")
# Limpie el dataset, ponga el formato correcto en los números, etc
glimpse(ds)
ds_clean <- ds %>% clean_names %>% 
  type_convert(locale = locale(decimal_mark = ","))
glimpse(ds_clean)

#cambio de forma manuel el tipo de dato del hidrógeno
ds_clean$precio_hidrogeno <-as.double(ds_clean$precio_hidrogeno)

#cree una columna nueva que deberá llamarse low-cost, y determine cuál es 
#el precio promedio de todos los combustibles a nivel comunidades autónomas 
#tanto para el territorio peninsular e insular, 
#esta columna deberá clasificar las estaciones por  lowcost y no lowcost, 
ds_lowcost <- ds_clean %>%
  group_by(idccaa) %>% 
  # Calculo la media por ccaa y clasifico en low cost o no low cost
  mutate(low_cost = (precio_gasoleo_a < mean(precio_gasoleo_a, na.rm=TRUE))) %>% 
  ungroup()

write_csv(ds_lowcost, "lowcost_21964596.csv")


# EJERCICIO b ---------------------------------
#cuántas gasolineras tiene la comunidad de Madrid (13) y la comunidad de Andalucía (1), 
#cuántas son low-cost, cuantas no lo son, 

#ANDALUCIA
#cuento las gasolineras en Andalucia
gasolinerasAnd <- ds_lowcost %>% filter(idccaa=="01") %>% count() #2207
#cuento las low cost en Andalucia
lowcostAnd <- ds_lowcost %>% filter(idccaa=="01" & low_cost) %>% count()#954
#MADRID
#cuento las gasolineras en Madrid
gasolinerasMad <- ds_lowcost %>% filter(idccaa=="13") %>% count() #793
#cuento las lowcost en Madrid
lowcostMad <- ds_lowcost %>% filter(idccaa=="13"& low_cost) %>% count()#239

#además, necesita saber cuál es el precio promedio, el precio más bajo y el
#más caro de los siguientes carburantes: gasóleo A, y gasolina 95 e Premium 
Precios <- function(combustible, provincia) {
  
  preciosProv <- ds_lowcost %>% filter(idccaa==!!provincia)
  return(c(min(preciosProv[[combustible]], na.rm=TRUE),
           max(preciosProv[[combustible]], na.rm=TRUE),
           mean(preciosProv[[combustible]], na.rm=TRUE)))
}

todasAndalucia <- c(gasolinerasAnd, lowcostAnd)
todasMadrid <- c(gasolinerasMad, lowcostMad)
preciosAndalucia <- c(Precios("precio_gasoleo_a", "01"),
                      Precios("precio_gasolina_95_e5", "01"),
                      Precios("precio_gasoleo_premium", "01"))
preciosMadrid <- c(Precios("precio_gasoleo_a", "13"),
                   Precios("precio_gasolina_95_e5", "13"),
                   Precios("precio_gasoleo_premium", "13"))

nombreVariable <- c("Total de gasolineras", "Gasolineras low cost",
                  "Precio minimo gasoleo A", "Precio maximo gasoleo A",
                  "Precio medio gasoleo A",
                  "Precio minimo gasolina 95 E5", "Precio maximo gasolina 95 E5",
                  "Precio medio gasolina 95 E5",
                  "Precio minimo gasoleo premium", "Precio maximo gasoleo premium",
                  "Precio medio gasoleo premium")
datosAnd <- c(todasAndalucia, preciosAndalucia)
datosMad <- c(todasMadrid, preciosMadrid)

# Conseguido el objetivo, deberá guardar este "archivo" en  una nueva tabla 
#llamada informe_CAM_expediente y deberá estar disponible también en su 
#repositorio con el mismo nombre en formato csv
informeB <- tibble(`Dato`=nombreVariable, `Andalucia`=unlist(datosAnd),
                           `Madrid`=unlist(datosMad))
write_csv(informeB, "informe_CAM_21964596.csv")

# EJERCICIO c --------------------------------------------------------------
# Por si la comunidad de Madrid no se adapta a sus requerimientos, el 
# empresario tambien quiere:

#conocer a nivel provincias, cuántas gasolineras son low-cost, cuantas no lo 
#son, cuál es el precio promedio, el precio más bajo y el más caro de los 
#siguientes carburantes: gasóleo A, y gasolina 95 e5 Premium , en todo el  
#TERRITORIO NACIONAL, exceptuando las grandes CIUDADES ESPAÑOLAS ("MADRID", 
#"BARCELONA", "SEVILLA" y "VALENCIA")
provinciasAdap <- c('MADRID', 'BARCELONA', 'SEVILLA', 'VALENCIA')

gasolinerasAdap <- ds_lowcost %>% filter( 
  (provincia %in% provinciasAdap) == FALSE)

todasAdap <- gasolinerasAdap %>% count() #9405
lowcostAdap <- gasolinerasAdap %>% filter(low_cost) %>% count() #3725
noLowcostAdap <- todasAdap - lowcostAdap #5680

minGasoleoA <- min(gasolinerasAdap$precio_gasoleo_a, na.rm=TRUE)
maxGasoleoA <- max(gasolinerasAdap$precio_gasoleo_a, na.rm=TRUE)
meanGasoleoA <- mean(gasolinerasAdap$precio_gasoleo_a, na.rm=TRUE)

minGasolina95 <- min(gasolinerasAdap$precio_gasolina_95_e5_premium, na.rm=TRUE)
maxGasolina95 <- max(gasolinerasAdap$precio_gasolina_95_e5_premium, na.rm=TRUE)
meanGasolina95 <- mean(gasolinerasAdap$precio_gasolina_95_e5_premium, na.rm=TRUE)

nombreVariableAdap <- c('Gasolineras low cost','Gasolineras no low cost',
                      'Precio minimo gasoleo A', 'Precio maximo gasoleo A',
                      'Precio medio gasoleo A', 'Precio minimo gasolina 95 E5 Premium',
                      'Precio maximo gasolina 95 E5 Premium',
                      'Precio medio gasolina 95 E5 Premium')

datosAdap <- c(lowcostAdap, noLowcostAdap, minGasoleoA, maxGasoleoA,
               meanGasoleoA, minGasolina95, maxGasolina95, meanGasolina95)

# Conseguido el objetivo, deberá guardar este "archivo" en  una nueva tabla 
#llamada informe_no_grandes_ciudades_expediente y deberá estar disponible 
#también en su repositorio con el mismo nombre en formato Excel
informeC <- tibble(`Dato`=nombreVariableAdap, `Valor`=unlist(datosAdap))
write_excel_csv(informeC, "informe_no_grandes_ciudades_21964596.csv")


# EJERCICIO d --------------------------
#gasolineras se encuentran abiertas las 24 horas exclusivamente, 
#genere una nueva tabla llamada no_24_horas sin la variable horario 
#(es decir no debe aparecer esta columna). 
no24horas <- ds_lowcost %>% filter(horario != 'L-D: 24H')
no_24_horas <- no24horas %>% select(-horario)

#Conseguido el objetivo, deberá guardar este "archivo" en  una nueva tabla 
#llamada no_24_horas y deberá estar disponible también en su repositorio 
#con el mismo nombre en formato Excel
write_excel_csv(no_24_horas, "no_24_horas.xls")

# EJERCICIO e -----------------------------
# Conseguido el objetivo, deberá guardar este "archivo" en  una nueva tabla 
#llamada no_24_horas y deberá estar disponible también en su repositorio con 
#el mismo nombre en formato Excel

#deberá añadir la población al dataset original creando una nueva columna 
#denominada población, esta información debe ser veraz y la más actualizada, 
#la población debe estar a nivel municipal (todo el territorio nacional)


#Genere el TopTen de municipios entre todo el territorio nacional excepto 
#el territorio insular,  donde no existan gasolineras 24 horas, agrupadas 
#entre low-cost y no low-cost, deberá guardar este "archivo" en  una nueva tabla
#llamada informe_top_ten_expediente y deberá estar disponible también en su 
#repositorio con el mismo nombre en formato csv.

