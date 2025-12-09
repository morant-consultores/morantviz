# Mapa
# Este script muestra como fue generada la función para hacer mapas.

library(stringr)
library(tidyr)
library(ggalluvial)
library(encuestar)
library(morantviz)
library(survey)
library(dplyr)
library(ggplot2)
library(patchwork)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(googledrive)
library(readxl)
library(ggwordcloud)
library(sf)

devtools::load_all(path = "../morantviz/")


dicc <- tibble::tribble(~codigo, ~nombre, ~pregunta,
                        "conoce_pm_astiazaran", "Astiazarán", "Conoce o ha escuchado de (...)",
                        "conoce_pm_delrio", "Del Río", "Conoce o ha escuchado de (...)",
                        "conoce_pm_lia", "Lía Limón", "Conoce o ha escuchado de (...)",
                        "conoce_pm_javier", "Javier López Casarín", "Conoce o ha escuchado de (...)",
                        "opinion_pm_astiazaran", "Astiazarán","¿Cuál es su opinión sobre (...)?",
                        "opinion_pm_delrio", "Del Río","¿Cuál es su opinión sobre (...)?"
)


colores <- tibble::tribble(~respuesta, ~color,
                           "Sí", "#0c4c8a",
                           "No", "#ecf0f1",
                           "Buena", "#27ae60",
                           "Mala", "#c0392b",
                           "Muy buena", "#2ecc71",
                           "Muy mala", "#e74c3c",
                           "Regular", "#f1c40f",
                           "Ns/Nc", "#95a5a6")

g <- Graficar$new(bd = diseno_demo$variables,
                  diccionario = dicc,
                  colores = colores,
                  color_principal = "pink",
                  tema = tema_morant())

#Base de datos de delincuencia para este ejercicio (bajada de ENVIPE-INEGI)
bd_delincuencia <- "Insumo_mapa/BD_ENVIPE_2025.RData"
objs <- load(bd_delincuencia)

cdmx_del<- get("TPer_Vic2") |>
  dplyr::filter(CVE_ENT == "09") |> 
  dplyr::filter(AP6_4_04 =="1")



#### AQUI COMIENZA EL PROCESO QUE SE REPITE ####
#ESTO ES PREVIO A LA FUNCIÓN
path <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/SHP/2024/09 CIUDAD DE MEXICO/MUNICIPIO.shp"
shp_mun <- read_sf(path) |>
  janitor::clean_names()

bd_mapa_sup  <- cdmx_del |>
  mutate(
    nombre = stringi::stri_trans_general(
      toupper(NOM_MUN),
      id = "latin-ascii"
    )
  ) |>
  count(nombre)

bd_mapa_sup |>
  anti_join(shp_mun)

pre_mapa_mun_sup <- shp_mun |>
  left_join(bd_mapa_sup, by = "nombre") |>  ggplot(aes(fill = n)) +
  geom_sf() +
  scale_fill_gradient2(
    low = "#235b4e",
    high = "#611232",
    labels = as.numeric,
    name = "Total de respuestas",
    na.value = "gray55"
  ) +
  labs(
    caption = "La intensidad del color muestra los municipios de menor a mayor participación"
  ) +
  theme_void(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    plot.caption = element_text(hjust = 0.5, size = 9, color = "gray25")
  )

