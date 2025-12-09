
devtools::load_all(path = "../morantviz/")

library(dplyr)
library(janitor)
library(sf)
library(stringi)

# 1. Cargar base ENVIPE
load("Insumo_mapa/BD_ENVIPE_2025.RData")

# Esto carga TPer_Vic2 directamente

#Creamos la g pero, en este caso, usando la base de envipe en lugar de la normal.
g <- Graficar$new(
  bd = TPer_Vic2,
  diccionario = dicc,
  colores = colores,
  color_principal = "pink",
  tema = tema_morant()
)

g$mapear_municipios(
  entidad = "12",  # Ajustamos entidad
  variable = "AP6_4_04", #Colocamos la variable a graficar
  valor = "1", #El resultado que queremos visualizar
  titulo_leyenda = "La intensidad del color muestra los municipios\nde menor a mayor participación", #Leyenda, no cambia
  tipo = "continuo",
  var_entidad = "CVE_ENT", #Variable que contiene las claves de entidad
  var_municipio = "CVE_MUN", #Variable que contiene las claves de
  low  = "grey80", #Color bajo
  high = "#611232" #Color alto
)


g$mapa

