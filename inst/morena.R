# Función MORENA (para probar desde fuera)
################################### GRAFICO CONOCIMIENTO MORENA ###################################

graficar_morena <- function(tabla_larga,
                            orden_atributos,
                            etiquetas_atributos) {
  
  # -----------------------------
  # 0. Homologar nombres
  # -----------------------------
  tabla_larga <- tabla_larga |>
    mutate(
      atributo = nombre,
      tema = as.character(tema)
    )
  
  # -----------------------------
  # 1. QUITAR atributos no deseados
  # -----------------------------
  tabla_larga <- tabla_larga |>
    filter(atributo %in% orden_atributos)
  
  # -----------------------------
  # 2. Filtrar para graficar
  # -----------------------------
  base_plot <- tabla_larga |>
    mutate(
      atributo = factor(atributo, levels = orden_atributos),
      etiqueta = etiquetas_atributos[as.character(atributo)]
    )
  
  # -----------------------------
  # 3. Colores correctos (6 atributos)
  # -----------------------------
  colores_lineas <- c(
    "Honestidad" = "#880D1E",
    "Cercano a la gente" = "#DD2D4A",
    "Conoce el Estado" = "#004777",
    "Cumple" = "#F49CBB",
    "Buen candidato" = "#2274A5",
    "Votaría" = "#8ACDEA"
  )
  
  # -----------------------------
  # 4. Línea horizontal del conocimiento 
  # -----------------------------
  conocimiento_df <- tabla_larga |>
    distinct(tema, conocimiento)
  
  # -----------------------------
  # 5. GRAFICO FINAL
  # -----------------------------
  g <- ggplot(base_plot, aes(
    x = atributo,
    y = media,
    group = tema,
    color = atributo
  )) +
    geom_line(size = 1) +
    geom_point(size = 4) +
    
    # Línea de conocimiento por candidato
    geom_hline(
      data = conocimiento_df,
      aes(yintercept = conocimiento),
      inherit.aes = FALSE,
      color = "#c1121f",
      linewidth = 1
    ) +
    geom_text(
      data = conocimiento_df,
      aes(
        x = 1,
        y = conocimiento,
        label = "Conocimiento"
      ),
      color = "#c1121f",
      vjust = -0.6,
      hjust = 0,
      inherit.aes = FALSE,
      size = 4
    ) +
    
    facet_wrap(~tema, nrow = 1) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1)
    ) +
    scale_color_manual(
      values = colores_lineas,
      breaks = orden_atributos,
      labels = etiquetas_atributos
    ) +
    scale_x_discrete(
      breaks = orden_atributos,
      labels = etiquetas_atributos
    ) +
    tema_morant() +
    theme(
      axis.text.x = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      strip.text = element_text(size = 14)
    ) +
    labs(
      x = NULL,
      y = NULL,
      color = "Aspectos evaluados"
    )
  
  return(g)
}


####
####
analizar_morena <- function(personajes,
                            puntos,
                            vars_conocimiento,
                            diseno,
                            diccionario,
                            colores) {

  ###############################################################
  # 0) DICCIONARIO UNIFICADO Y LIMPIO
  ###############################################################
  diccionario <- diccionario |>
    janitor::clean_names() |>
    dplyr::rename(codigo = dplyr::matches("codigo|llaves")) |>
    dplyr::mutate(
      codigo = as.character(codigo) |> stringr::str_squish()
    )

  ###############################################################
  # 0.1) VALIDAR QUE VARIABLES EXISTEN EN EL DISEÑO
  ###############################################################
  vars_en_diseno <- names(diseno$variables)

  # Construir las mismas variables que usará la función
  pref_opinion <- puntos |> dplyr::filter(tipo == "opinion") |> dplyr::pull(prefijo)
  vars_opinion <- paste(pref_opinion, rep(personajes, each = length(pref_opinion)), sep = "_")

  atributos <- puntos |>
    dplyr::filter(tipo == "atributo") |>
    dplyr::select(prefijo, nombre, peso_atrib = puntos)

  vars_atrib <- expand.grid(atributos$prefijo, personajes) |>
    dplyr::mutate(var = paste(Var1, Var2, sep = "_")) |>
    dplyr::pull(var)

  pref_buenc <- puntos |> dplyr::filter(nombre == "Buen candidato") |> dplyr::pull(prefijo)
  vars_buenc <- paste(pref_buenc, personajes, sep = "_")

  pref_vot <- puntos |> dplyr::filter(nombre == "Votaría") |> dplyr::pull(prefijo)
  vars_vot <- paste(pref_vot, personajes, sep = "_")

  pref_pref <- puntos |> dplyr::filter(nombre == "Preferencia declarada") |> dplyr::pull(prefijo)
  vars_pref <- pref_pref

  vars_todas <- c(
    vars_opinion,
    vars_atrib,
    vars_buenc,
    vars_vot,
    vars_conocimiento,
    vars_pref
  )

  faltan_en_diseno <- setdiff(vars_todas, vars_en_diseno)
  if (length(faltan_en_diseno) > 0) {
    stop("Las siguientes variables no existen en diseno$variables:\n",
         paste(faltan_en_diseno, collapse = ", "))
  }

  faltan_en_dicc <- setdiff(vars_todas, diccionario$codigo)
  if (length(faltan_en_dicc) > 0) {
    warning(
      "Las siguientes variables no están en el diccionario$codigo:\n",
      paste(faltan_en_dicc, collapse = ", "),
      "\nSe podrán usar, pero no tendrán nombre legible en tablas."
    )
  }

  ###############################################################
  # 1) INSTANCIA GRAFICAR
  ###############################################################
  g <- Graficar$new(
    diseno      = diseno,
    diccionario = diccionario,
    colores     = colores,
    color_principal = "pink",
    tema = tema_morant()
  )
  
  # ... resto de tu función igual ...
}

########
########
