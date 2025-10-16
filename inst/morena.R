############################ Cargamos las librerias y diseño ############################

library(readr)
library(readxl)

# Diseño
diseno_24 <- read_rds("/Users/saulnoguez/Documents/GitHub/enc_chihuahua_nov2024/R/resultado_final_nov24.rda")

# Diccionario
dicc <- read_xlsx("../enc_chihuahua_nov2024/Insumos/dicc_enc_chihuahua_nov2024.xlsx")

# diseno_24$variables |>
#   as_tibble() |>
#   select(contains("opinion"), contains("honesto"),
#          contains("cercano"), contains("conoce"),
#          contains("cumple"), contains("candidato"), contains("voto")) |>
#   select(contains(c("andrea", "cruz"))) |>
#   select(contains("estado"))

options(survey.lonely.psu="remove")

#Cresmos g
g <- Graficar$new(diseno = diseno_24, diccionario = dicc |> rename(codigo = llaves), colores = colores,color_principal = "pink",
             tema = tema_morant())


# Ahora será necesario hacer un cálculo para cada uno de los 8 atributos:
# Las siguientes son sus opciones de respuesta:
# Opinión positiva ; Buena, Regular, Mala, NS/NC
# Honestidad ; Mucho, Algo, Poco, Nada, NS/NC
# Cercano a la gente ; Mucho, Algo, Poco, Nada, NS/NC
# Conoce el Estado ; Mucho, Algo, Poco, Nada, NS/NC
# Cumple lo que dice ; Mucho, Algo, Poco, Nada, NS/NC
# Buen candidato ; Si, No, NS/NC 
# Diposición a votar ; Sï_votaría, Nunca_votaría, NS/NC
# Preferencia como candidato/a; nombre1_, nombre2_, Otro: , Ninguno, NS/NC

# Al tener distintas opciones de respuesta, es necesario agrupar en quienes tienen respuestas similares. Se procesan juntas Honestidad, Cercano, Conoce, Cumple. Se procesan individualmente: Opinion positiva, Buen candidato, Disposición a votar y Preferencia como candidato.

############################ Calculamos para Opinion_positiva ############################

# Creamos un cruce de opinion con el nombre de las personas
op <- "opinion_per1"
personajes <- c("cruz", "andrea")
vars <- paste(op, personajes, sep = "_")

# Usamos metodo contar variables
g$contar_variables(variables = vars,confint = F)

# Filtramos solo respuesta positiva
g$filtrar_respuesta(valor = "Buena")

# Pegamos diccionario
g$pegar_diccionario()
op_morena <- g$tbl |>
  mutate(nombre = "Opinión") |>
  select(codigo, bloque, pregunta, tipo_pregunta, pase, multirespuesta, respuestas,
         tema, respuesta, media, ee, nombre) |>
  mutate(atributo = NA_character_, puntos = NA_real_, personaje = NA)

############################ Calculamos para los 4 atributos que tienen iguales respuestas (Honestidad/Cercano_a_la_gente/Conoce_estado/Cumple) ############################

# Hacemos un tibble con las cuatro categorias que se procesan de igual manera, pero, asignando puntajes distintos

atributos <- tibble(
  atributo = c("caract_per_honesto", "caract_per_cercano", "caract_per_conocechi", "caract_per_cumple"),
  puntos = c(1.25, .25, .25, .25),
  nombre = c("Honestidad", "Cercano a la gente", "Conoce el Estado", "Cumple")
)

vars <- expand.grid(atributos$atributo, personajes) |>
  mutate(var = paste(Var1, Var2, sep = "_")) |>
  pull(var)

g$contar_variables(variables = vars, confint = F)
g$filtrar_respuesta(valor = c("Mucho", "Algo"))

g$tbl <- g$tbl |>
  mutate(media = if_else(respuesta == "Algo", media * .5, media)) |>
  summarise(media = sum(media), .by = codigo)

g$pegar_diccionario()

library(stringr)
frecuencia_atributos <- g$tbl |>
  mutate(
    atributo = str_match(codigo, paste(atributos$atributo, collapse = "|")) |> as.vector(),
    .before = 0
  ) |>
  left_join(atributos, join_by(atributo)) |>
  mutate(
    puntos = if_else(media == max(media, na.rm = TRUE), puntos, 0),
    tema = case_when(
      str_detect(codigo, "cruz") ~ "Cruz Pérez Cuéllar",
      str_detect(codigo, "andrea") ~ "Andrea Chávez"
    )
  ) |>
  group_by(atributo) |>
  mutate(puntos = if_else(media == max(media, na.rm = TRUE), puntos, 0)) |>
  ungroup()


########################### Calculamos los 3 atributos faltantes (Buen_candidato/Disposición_a_votar/Peferencia_como_candidato) ###########################

# ---------- Buen candidato ---------- #

# Creamos objeto buen candidato por tipo de candidato
buen_candidato <- "tipo_candidato"
vars <- paste(buen_candidato, personajes, sep = "_")

# Creamos un objeto para Buen candidato por personaje con contar_variables
g$contar_variables(vars, confint = F)
g$filtrar_respuesta(valor = "Sí")
g$pegar_diccionario()

# Objeto creado con el maximo obtenido para el candidato
buenC <- g$tbl |>
  mutate(puntos = if_else(media == max(media, na.rm = TRUE), 1, 0),
         nombre = "Buen candidato",
         atributo = NA_character_, personaje = NA) |>
  select(codigo, bloque, pregunta, tipo_pregunta, pase, multirespuesta, respuestas,
         tema, respuesta, media, ee, nombre, atributo, puntos, personaje)

# ---------- Disposición a votar  ---------- #

votaria <- "voto_fut"
vars <- paste(votaria, personajes, sep = "_")
g$contar_variables(vars, confint = F)

g$filtrar_respuesta(valor = "Sí votaría")
g$pegar_diccionario()

vot <- g$tbl |>
  mutate(puntos = if_else(media == max(media, na.rm = TRUE), 2, 0),
         nombre = "Votaría",
         atributo = NA_character_, personaje = NA) |>
  select(codigo, bloque, pregunta, tipo_pregunta, pase, multirespuesta, respuestas,
         tema, respuesta, media, ee, nombre, atributo, puntos, personaje)

# ---------- Preferencia como candidato/a  ---------- #

preferencia <- "candidato_gb_27"

g$contar_variables(preferencia, confint = F)
g$pegar_diccionario()

preferencia <- "candidato_gb_27"

g$contar_variables(preferencia, confint = F)
g$pegar_diccionario()

pref <- g$tbl |>
  # limpiamos no-respuestas
  filter(
    !respuesta %in% c("Ns/Nc", "Ninguno", "Otro"),
    !is.na(respuesta) & respuesta != ""
  ) |>
  # mapeo de personaje
  mutate(
    personaje = case_when(
      str_detect(respuesta, regex("Andrea", ignore_case = TRUE)) ~ "andrea",
      str_detect(respuesta, regex("Cruz",   ignore_case = TRUE)) ~ "cruz",
      TRUE ~ "otro"
    )
  ) |>
  filter(personaje != "otro") |>
  mutate(
    personaje = factor(personaje, levels = c("andrea", "cruz")),
    tema = case_when(
      personaje == "andrea" ~ "Andrea Chávez",
      personaje == "cruz" ~ "Cruz Pérez Cuéllar"
    )
  ) |>
  select(atributo, personaje, tema, media, puntos, nombre)

# ---------- Unión de todos los bloques (sin preferencia) ---------- #

todo <- op_morena |>
  bind_rows(frecuencia_atributos) |>
  bind_rows(buenC) |>
  bind_rows(pref) |>
  filter(!is.na(tema), !is.na(nombre), !is.na(media)) |>
  mutate(
    nombre = str_replace_all(nombre, "\n", " "),
    nombre = str_squish(nombre),
    nombre = factor(nombre, levels = c(
      "Opinión", "Honestidad", "Cercano a la gente",
      "Conoce el Estado", "Cumple", "Buen candidato", "Votaría", "Preferencia declarada"
    ))
  )

todo |>
  ggplot(aes(x = nombre, y = tema)) +
  geom_tile(aes(fill = media), show.legend = FALSE) +
  ggfittext::geom_fit_text(aes(label = scales::percent(media, accuracy = 1)),
                           contrast = TRUE) +
  scale_fill_gradient(low = "#fde0dd", high = "#A6032F") +
  labs(x = NULL, y = NULL, title = "Evaluación de atributos por candidato") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

######################### Gráfico con conocimiento ##########################

#Traer conocimiento
vars_conocimiento <- c( "conoce_per1_cruz", "conoce_per1_andrea")


g$contar_variables(variables = vars_conocimiento, confint = F)
g$filtrar_respuesta(valor = c("Sí lo conoce"))

g$tbl <- g$tbl |>
  summarise(media = sum(media), .by = codigo)

#Multiplicamos cada categoria por el

todo <- bind_rows(
  op_morena,
  frecuencia_atributos |> select(names(op_morena) %>% union(c("atributo","puntos","personaje"))),
  buenC,
  pref,
  vot
) |>
  filter(!is.na(tema), !is.na(nombre), !is.na(media)) |>
  mutate(
    nombre = str_replace_all(nombre, "\n", " "),
    nombre = str_squish(nombre),
    nombre = factor(nombre, levels = c(
      "Opinión", "Honestidad", "Cercano a la gente",
      "Conoce el Estado", "Cumple", "Buen candidato", "Votaría", "Preferencia declarada"
    ))
  )
