#Función para cargar shapes (Funciona para el metodo de generar mapas)
#Primero funcion que te carga los shapes
cargar_shp_entidad <- function(entidad,
                               base_path = "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/SHP/2024") {

  entidad2 <- sprintf("%02d", as.numeric(entidad))

  dirs <- list.dirs(base_path, recursive = FALSE, full.names = TRUE)

  carpeta_ent <- dirs[grepl(paste0("^", entidad2, " "), basename(dirs))]

  if (length(carpeta_ent) == 0) {
    stop(paste("No se encontró carpeta para la entidad", entidad))
  }

  shp_path <- file.path(carpeta_ent, "MUNICIPIO.shp")

  if (!file.exists(shp_path)) {
    stop(paste("No existe MUNICIPIO.shp en", carpeta_ent))
  }

  shp <- sf::read_sf(shp_path) |>
    janitor::clean_names() |>
    dplyr::mutate(
      municipio_tmp = sprintf("%02d", as.numeric(municipio))
    )

  return(shp)
}

#' Clase R6 Graficar
#'
#' Clase base para construir tablas, manipular respuestas y generar
#' gráficas a partir de encuestas con diseño muestral y diccionario.
#'
#' @docType class
#' @format R6Class object
#' @export
#' @import survey dplyr ggplot2 patchwork
#'
#' @examples
#' \dontrun{
#' g <- Graficar$new(
#'   diseno = encuesta_demo$muestra$diseno,
#'   diccionario = dicc,
#'   colores = colores,
#'   color_principal = "pink",
#'   tema = tema_morant()
#' )
#'
#' # Conocimiento en barras horizontales
#' g$
#'   contar_variables(
#'     variables = c("conoce_pm_astiazaran", "conoce_pm_delrio"),
#'     confint = FALSE
#'   )$
#'   filtrar_respuesta(valor = "Sí")$
#'   pegar_diccionario()$
#'   pegar_color()$
#'   envolver_etiquetas(columna = "nombre", ancho = 13)$
#'   reordenar_columna(columna = "nombre", tipo = "asc")
#'
#' g$graficar_barras_h(x = "nombre")
#' }

Graficar <- R6::R6Class(
  "Graficar",
  private = list(
    pesos = NULL
  ),
  public = list(
    #' @field tbl Tibble con los resultados procesados.
    tbl = NULL,
    #' @field grafica Objeto `ggplot` generado.
    grafica = NULL,
    #' @field diseno Objeto survey design (de `survey`).
    diseno = NULL,
    #' @field bd base de datos a analizar.
    bd = NULL,
    #' @field diccionario Diccionario de variables y etiquetas.
    diccionario = NULL,
    #' @field colores Tabla de colores por respuesta.
    colores = NULL,
    #' @field color_principal Color por defecto.
    color_principal = NULL,
    #' @field tema Tema de `ggplot` a aplicar en las gráficas.
    tema = NULL,
    #' @field mapa Objeto ggplot con el último mapa generado
    mapa = NULL,


    #' Inicializar objeto Graficar
    #'
    #' @param diseno Diseño muestral de la encuesta (`survey::svydesign`).
    #' @param diccionario Diccionario con nombres y etiquetas de variables.
    #' @param colores Tabla con correspondencia respuesta-color.
    #' @param color_principal Color por defecto para respuestas sin color asignado.
    #' @param tema Tema de `ggplot` a aplicar en las gráficas.
    #' @return Un objeto de clase `Graficar`.
    #' @examples
    #' g <- Graficar$new(diseno, diccionario, colores, "pink", tema_morant())
    #'
    initialize = function(diseno = NULL, bd = NULL, diccionario, colores, color_principal, tema) {
      if(!is.null(diseno)) {
        self$diseno <- diseno
        private$pesos <- T
      }
      if(!is.null(bd)) {
        self$bd <- bd |> tibble::as_tibble()
        private$pesos <- F
      }
      self$diccionario <- diccionario
      self$colores <- colores
      self$tema <- tema
      self$color_principal <- color_principal
    },

    #' Contar variables ponderadas
    #'
    #' Llama a `contar_vars_pesos` para obtener proporciones y medias.
    #'
    #' @param variables Vector de nombres de variables.
    #' @param confint Lógico; si calcular intervalos de confianza.
    #' @return La tabla interna (`self$tbl`) se actualiza.
    #' @examples
    #' g$contar_variables(c("conoce_pm_astiazaran", "conoce_pm_delrio"), confint = FALSE)

    contar_variables = function(variables, confint = F, pct = T){
      if(private$pesos){
        self$tbl <- contar_vars_pesos(
          variables = variables,
          confint = confint,
          diseno = self$diseno
        )
      }

      if(!private$pesos){
        self$tbl <- contar_vars(
          bd = self$bd,
          variables = variables,
          pct = pct
        )
      }

      invisible(self)
    },

    #' Contar variables ponderadas por grupos
    #'
    #' Llama a `contar_vars_porGrupos_pesos` para obtener proporciones y medias.
    #'
    #' @param variables Vector de nombres de variables.
    #' @param grupos Vector de grupos.
    #' @param confint Lógico; si calcular intervalos de confianza.
    #' @return La tabla interna (`self$tbl`) se actualiza.
    #' @examples
    #' g$contar_variables_porGruos(c("conoce_pm_astiazaran", "conoce_pm_delrio"), grupos = "region", confint = FALSE)
    #'
    contar_variables_porGrupos = function(variables, grupos, confint){
      if(private$pesos){
        self$tbl <- contar_vars_porGrupos_pesos(
          variables = variables,
          grupos = grupos,
          confint = confint,
          diseno = self$diseno
        )
      } else{
        self$tbl <- contar_vars_porGrupos(
          bd = self$bd,
          variables = variables,
          grupos = grupos
        )
      }

      invisible(self)
    },
    #' Contar variable multirespuesta
    #'
    #' Llama a `contar_variable_multirespuesta` para obtener proporciones de una variable multirespuesta con algún separador.
    #'
    #' @param variable Nombre de la variable multirespuesta
    #' @param sep Separador de las respuestas elegidas.
    #' @param confint Lógico; si calcular intervalos de confianza.
    #' @return La tabla interna (`self$tbl`) se actualiza.
    #' @examples
    #' g$contar_variable_multirespuesta(variable = "problema_inseguridad", sep = "|", confint = F)
    #'
    contar_variable_multirespuesta = function(variable, sep, confint){

      if(private$pesos){
        self$tbl <- contar_multirespuesta_pesos(diseno = self$diseno,
                                                variable = variable,
                                                sep = sep, confint = confint)
      } else{
        self$tbl <- contar_multirespuesta(bd = self$bd,
                                          variable = variable,
                                          sep = sep)
      }

      invisible(self)
    },
    calcular_pct = function(var = "n", grupo = "codigo"){
      self$tbl <- self$tbl |>
        mutate(pct = !!rlang::sym(var)/sum(!!rlang::sym(var)), .by = !!rlang::sym(grupo))

      invisible(self)
    },
    #' Filtrar respuestas específicas
    #'
    #' @param variable Nombre de la variable a filtrar.
    #' @param valor Valores a conservar.
    #' @return La tabla interna (`self$tbl`) se filtra.
    #' @examples
    #' g$filtrar_respuesta(variable = "respuesta", valor = "Sí")
    filtrar_respuesta = function(variable, valor){
      self$tbl <- self$tbl |>
        dplyr::filter(respuesta %in% !!valor)
      invisible(self)
    },


    #' Reordenar una columna
    #'
    #' Permite reordenar factores de forma manual, ascendente, descendente o por suma.
    #'
    #' @param columna Columna a reordenar.
    #' @param tipo Tipo de orden: `"manual"`, `"asc"`, `"desc"`, `"suma"`.
    #' @param freq Con respecto a qué columna es el criterio para ordenar
    #' @param ... Niveles en orden manual si `tipo = "manual"`.
    #' @return La tabla interna (`self$tbl`) se actualiza.
    #' @examples
    #' g$reordenar_columna("nombre", "asc")
    #' g$reordenar_columna("respuesta", "manual", c("Sí", "No"))
    reordenar_columna = function(columna, tipo, freq = "media",...){
      match.arg(tipo, choices = c("manual", "asc", "desc", "suma"))

      if(tipo == "manual"){
        self$tbl <- self$tbl |>
          dplyr::mutate(!!rlang::sym(columna) :=
                          forcats::fct_relevel(!!rlang::sym(columna), ...))
      }

      if(tipo %in% "desc"){
        self$tbl <- self$tbl |>
          dplyr::arrange(dplyr::desc(!!rlang::sym(freq))) |>
          dplyr::mutate(!!rlang::sym(columna) :=
                          forcats::fct_inorder(!!rlang::sym(columna)))
      }

      if(tipo == "asc"){
        self$tbl <- self$tbl |>
          dplyr::arrange(!!rlang::sym(freq)) |>
          dplyr::mutate(!!rlang::sym(columna) :=
                          forcats::fct_inorder(!!rlang::sym(columna)))
      }

      if(tipo == "suma"){
        self$tbl <- self$tbl |>
          dplyr::mutate(!!rlang::sym(columna) :=
                          forcats::fct_reorder(!!rlang::sym(columna), !!rlang::sym(freq), .fun = sum))
      }

      invisible(self)
    },

    #' Envolver etiquetas de texto
    #'
    #' @param columna Columna de etiquetas.
    #' @param ancho Número máximo de caracteres por línea.
    #' @examples
    #' g$envolver_etiquetas("nombre", ancho = 12)
    envolver_etiquetas = function(columna, ancho){
      self$tbl <- self$tbl |>
        dplyr::mutate(!!rlang::sym(columna) :=
                        stringr::str_wrap(!!rlang::sym(columna), width = ancho))
      invisible(self)
    },

    #' Pegar diccionario de variables
    #'
    #' Hace un `left_join` entre la tabla de resultados y el diccionario.
    #' @examples
    #' g$pegar_diccionario()
    pegar_diccionario = function(){
      self$tbl <- self$tbl |>
        dplyr::left_join(self$diccionario, dplyr::join_by(codigo))
      invisible(self)
    },

    #' Partir la categoría "Regular" en dos mitades
    #'
    #' Divide la proporción de "Regular" en dos (positivo/negativo).
    #'
    #' @param opcion Valor de la categoría Regular.
    #' @examples
    #' g$partir_regular("Regular")
    partir_regular = function(opcion, freq = "media"){
      self$tbl <- self$tbl |>
        dplyr::filter(respuesta != !!opcion) |>
        dplyr::bind_rows(
          c(1, -1) |>
            purrr::map_dfr(~{
              self$tbl |>
                dplyr::filter(respuesta == !!opcion) |>
                dplyr::mutate(!!rlang::sym(freq) := .x*!!rlang::sym(freq)/2)
            })
        ) |>
        dplyr::mutate(respuesta2 = dplyr::if_else(!!rlang::sym(freq) < 0 & respuesta == "Regular", "Regular2", respuesta))

      invisible(self)
    },

    #' Cambiar signo a medias de categorías negativas
    #'
    #' @param negativo Vector de respuestas negativas.
    #' @examples
    #' g$cambiarSigno_freq(c("Mala", "Muy mala"))
    cambiarSigno_freq = function(negativo, freq = "media"){
      self$tbl <- self$tbl |>
        dplyr::mutate(!!rlang::sym(freq) := dplyr::if_else(respuesta %in% !!negativo, -!!rlang::sym(freq), !!rlang::sym(freq)))
      invisible(self)
    },

    #' Etiquetar categoría Regular y porcentajes
    #'
    #' @param regular Valor de la categoría Regular.
    #' @examples
    #' g$etiquetar_regular("Regular").
    etiquetar_regular = function(regular, freq){
      self$tbl <- self$tbl |>
        dplyr::mutate(
          etiqueta = dplyr::case_when(
            !!rlang::sym(freq) < 0 & respuesta == !!regular ~ "",
            respuesta == !!regular ~ scales::percent(!!rlang::sym(freq)*2, accuracy = 1),
            TRUE ~ scales::percent(abs(!!rlang::sym(freq)), accuracy = 1)
          )
        )
      invisible(self)
    },

    #' Pegar colores
    #'
    #' Asigna colores a cada respuesta. Usa `color_principal` si falta.
    #' Volví a utilizar la función inicial de pegar_color()
    #' @examples
   # pegar_color = function(columna="respuesta"){
   #   self$tbl <- self$tbl |>
   #     columna_sym <- rlang::sym(columna)
   #     dplyr::left_join(self$colores, dplyr::join_by(!!columna_sym)) |>
   #     dplyr::mutate(color = dplyr::if_else(is.na(color), self$color_principal, color))
   #   invisible(self)
   # },

    pegar_color = function(columna = "respuesta") {
      columna_sym <- rlang::sym(columna)

      self$tbl <- self$tbl |>
        dplyr::left_join(self$colores, dplyr::join_by(!!columna_sym)) |>
        dplyr::mutate(
          color = dplyr::if_else(
            is.na(.data$color),
            self$color_principal,
            .data$color
          )
        )

      invisible(self)
    },


    #' Agregar saldo por grupo
    #'
    #' @param por Variable de agrupación.
    #' @examples
    #' g$agregar_saldo("nombre")
    agregar_saldo = function(por, freq = "media"){
      self$tbl <- self$tbl |>
        dplyr::mutate(saldo = sum(!!rlang::sym(freq)), .by = !!rlang::sym(por))
      invisible(self)
    },

    #' Graficar barras horizontales
    #'
    #' @param x Variable en el eje X.
    #' @return Objeto `ggplot`.
    #' @examples
    #' g$graficar_barras_h("nombre")
    graficar_barras_h = function(x, y = "media"){
      self$grafica <- ggplot2::ggplot(self$tbl, ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y))) +
        ggchicklet::geom_chicklet(ggplot2::aes(fill = color)) +
        ggplot2::geom_text(ggplot2::aes(label = scales::percent(!!rlang::sym(y))),
                           size = 5, hjust = -.1,
                           family = self$tema$text$family) +
        ggplot2::coord_flip() +
        ggplot2::labs(caption = self$tbl$pregunta[1]) +
        ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                    limits = c(0,1)) +
        ggplot2::scale_fill_identity() +
        self$tema
      return(self$grafica)
    },

    #' Graficar barras verticales
    #'
    #' @param y Variable en el eje y.
    #' @return Objeto `ggplot`.
    #' @examples
    #' g$graficar_barras_v("nombre")
    graficar_barras_v = function(x, y = "media"){
      self$grafica <- ggplot2::ggplot(self$tbl, ggplot2::aes(x= !!rlang::sym(x), y = !!rlang::sym(y)))+
        ggchicklet::geom_chicklet(ggplot2::aes(fill = color),width = 0.8 ) +
        ggplot2::geom_text( ggplot2::aes(label = scales::percent(media, accuracy = 1)),
      size = 5, vjust = -.1, family = self$tema$text$family) +
        ggplot2::labs(caption = self$tbl$pregunta[1])+
        ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1))+
        ggplot2::scale_fill_identity()+
        self$tema
        return(self$grafica)
    },
    #' Graficar dona o gauge
    #'
    #' @param x Variable en el eje x.
    #' @return Objeto `ggplot`.
    #' @
    #' g$graficar_gauge("nombre")

    graficar_gauge = function (){
      valor <- self$tbl |>
        dplyr::filter(respuesta %in% c("Sí", "Sí lo conoce")) |>
        dplyr::pull(media)

      self$grafica <- self$tbl |>
        ggplot2::ggplot(ggplot2::aes(x = "", y = media, fill = color)) +
        ggplot2::geom_col(width = 0.4) +
        ggplot2::coord_polar(theta = "y", start = 0) +
        ggplot2::scale_fill_identity() +
        ggplot2::theme_void() +
      ggplot2::annotate("text",
      x = 0,
      y = 0,
      label = base::paste0(scales::percent(valor, accuracy = 1)),
      size = 12, fontface = "bold", color = "black")
      return(self$grafica)
    },
    #' Graficar piramide
    #' Necesario hacer cruce de rango edad por sexo
    #' @param x Variable en el eje x.
    #' @return Objeto `ggplot`.
    #' @ examples
    #' g$contar_variables_porGrupos(variables = c("rango_edad"),grupos = c("sexo"), confint = F)
    #' g$graficar_piramide()
    graficar_piramide = function(cantidad_puntos = 30, tam_punto = 6,
      tam_texto_etiqueta_porcentaje = 6,
      separacion_texto = 1.5,
      espaciado =  c(1, 1),
      tam_texto_rango_edad = 6){

        #Parámetros fijos
      grupo_izquierda <- "F"
      grupo_derecha   <- "M"
      columna_categoria <- "respuesta"
      columna_grupo <- "sexo"
      columna_pct <- "media"
      fuente <- "Montserrat"
      titulo <- "Distribución poblacional por sexo"
      mostrar_etiquetas <- TRUE
      niveles_respuesta <- (base::unique(g$tbl$respuesta))
      colores_personalizados <- stats::setNames(c("#94D0CC", "#B49FCC"),
                                   c(grupo_izquierda, grupo_derecha))

      # --- 3. Expandir puntos ---
      bd_expandido <- self$tbl %>%
        dplyr::filter(.data[[columna_grupo]] %in% c(grupo_izquierda, grupo_derecha)) %>%
        dplyr::mutate(
          grupo = .data[[columna_grupo]],
          categoria = base::factor(.data[[columna_categoria]],
            levels = base::rev(niveles_respuesta %||% base::unique(.data[[columna_categoria]]))),
            puntos = base::round(.data[[columna_pct]] * cantidad_puntos)) %>%
        dplyr::filter(puntos > 0) %>%
        tidyr::uncount(puntos, .remove = FALSE) %>%
        dplyr::group_by(categoria, grupo) %>%
        dplyr::mutate(
          fila = dplyr::row_number(),
          x = dplyr::if_else(grupo == grupo_izquierda, -fila, fila),
          etiqueta = dplyr::if_else(
            fila == base::max(fila),
            base::paste0(base::round(100 * dplyr::first(.data[[columna_pct]]), 1), "%"),
            NA_character_)) %>%
        dplyr::ungroup()

      # --- 4. Etiquetas del centro ---
      etiquetas_centro <- bd_expandido %>%
        dplyr::distinct(categoria) %>%
        dplyr::mutate(x = 0, y = categoria)
      # --- 5. Colores --
      colores_usar <- colores_personalizados %||%
        stats::setNames(c("#94D0CC", "#B49FCC"), c(grupo_izquierda, grupo_derecha))

      x_max <- base::max(base::abs(bd_expandido$x)) + 4 # margen de 2 puntos de ancho

      g_piramide <- ggplot2::ggplot(bd_expandido, ggplot2::aes(x = x, y = categoria, color = grupo)) +
        ggplot2::geom_point(size = tam_punto, alpha = 0.8) +
        ggplot2::scale_color_manual(values = colores_usar, name = NULL) +
        ggplot2::scale_x_continuous(
          limits = c(-x_max, x_max),
          breaks = NULL,
          expand = c(0, 0)) +
        ggplot2::scale_y_discrete(position = "right", expand = expansion(mult = espaciado)) +
        ggplot2::geom_text(
          data = etiquetas_centro,
          aes(x = 0, y = y, label = y),
          inherit.aes = FALSE,
          size = tam_texto_rango_edad,
          hjust = 0.5,
          vjust = 0.5,
          family = fuente,
          color = "black") +
        ggplot2::geom_vline(xintercept = 0, color = "gray70", linetype = "dotted", linewidth = 0.3) +
        ggplot2::theme_minimal(base_family = fuente) +
        ggplot2::labs(title = " ", x = NULL, y = NULL) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 10, family = fuente),
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          legend.position = "top",
          legend.text = ggplot2::element_text(size = 9, family = fuente)) +
        ggplot2::geom_text(
          ggplot2::aes(
            label = etiqueta,
            hjust = dplyr::if_else(grupo == grupo_izquierda, 1.1, -0.1)),size = tam_texto_etiqueta_porcentaje,
            na.rm = TRUE,
            family = fuente,
            color = "black",
            nudge_x = dplyr::if_else(
              bd_expandido$grupo == grupo_izquierda, -separacion_texto, separacion_texto))
      return(g_piramide)
    },


    #' Graficar lollipops sin multirespuesta
    #'
    #' @param x Variable en el eje y.
    #' @return Objeto `ggplot`.
    #' @
    #' g$graficar_lollipops("respuesta")
    graficar_lollipops = function(x, y = "media"){
      self$grafica <- self$tbl |>
        ggplot2::ggplot(ggplot2::aes(x = stats::reorder(!!rlang::sym(x), !!rlang::sym(y)), y = !!rlang::sym(y), color = color)) +
        ggplot2::geom_segment(ggplot2::aes(xend = !!rlang::sym(x), y = 0, yend = !!rlang::sym(y)), linewidth = 1) +
        ggplot2::geom_point(size = 5) +
        ggplot2::geom_text(ggplot2::aes(label = scales::percent(!!rlang::sym(y), accuracy = 1.)),
      size = 6, hjust = -0.5, color = "black") +
        ggplot2::coord_flip() +
        ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(string = x, width = 60)) +
        ggplot2::scale_y_continuous(limits = c(0,1),labels = scales::percent_format())+
        ggplot2::scale_color_identity(guide = "none") +
        ggplot2::labs(title = " ")+
        self$tema+
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
          axis.text.y = ggplot2::element_text(size = 15, family = "Montserrat"),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          plot.background = ggplot2::element_rect(color = "transparent", fill = "transparent"),
          panel.background = ggplot2::element_rect(color = "transparent", fill = "transparent"),
          legend.background = ggplot2::element_rect(color = "transparent", fill = "transparent") )
      return(self$grafica)
    },


################################### Graficar Líneas  ###################################

#' Genera una gráfica de líneas para una variable
#'
#' Esta función toma la tabla `self$tbl` y construye una gráfica de líneas
#' donde el eje X corresponde a la variable `x`, el eje Y corresponde a la
#' métrica definida en `freq`, y las líneas se agrupan por la columna `codigo`.
#'
#' Además, se añaden puntos, etiquetas de porcentaje sobre los valores,
#' y se aplica el tema corporativo definido en la clase.
#'
#'  `x` Nombre de la columna que se usará en el eje X (ej. "respuesta").
#' `freq` Nombre de la columna numérica que define el eje Y
#'        (por defecto "media").
#'
#' @return La gráfica de líneas
#'

  graficar_lineas = function(
      x,
      freq = "media",
      color = "color"
      ){
        group = "codigo"

        aes_args <- aes(
          x = !!sym(x),
          y = !!sym(freq),
          group = !!sym(group),
          color = color   # 🔑 Usar columna self$tbl$color
        )

        self$grafica <- self$tbl |>
          ggplot(aes_args) +
          geom_line(linewidth = 1) +
          geom_point(size = 3) +
          geom_text(aes(label = scales::percent(media, accuracy = 1)),
                    size = 5, hjust = -.1,
                    family = self$tema$text$family,
                    vjust = -1, color = "black") +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                             limits = c(0,1)) +
          labs(caption = ifelse(is.na(self$tbl$pregunta[1]),
                                "Sin pregunta definida",
                                self$tbl$pregunta[1])) +
          self$tema

        return(self$grafica)
    },



################################### Grafica Sankey  ###################################

#' Genera un diagrama de Sankey a partir de la tabla de la clase
#'
#' Esta función toma la tabla `self$tbl` y construye un diagrama de Sankey
#' que muestra los flujos desde una variable de agrupación (`grupo`) hacia
#' las respuestas (`respuesta`), con pesos definidos por una métrica (`freq`).
#'
#' `grupo` Nombre de la columna que se usará como primer nodo (ej. "sexo").
#' `freq` Nombre de la columna numérica que define el grosor de los flujos
#'        (por defecto "media").
#'
#' @return La gráfica Sankey.

  graficar_sankey = function(grupo,freq = "media"){

    sankey_df <- self$tbl %>%
    select(grupo, respuesta, !!sym(freq))

    sankey_long <- sankey_df %>%
    make_long(grupo, respuesta, value = !!sym(freq))

    paleta <- setNames(self$colores$color, self$colores$respuesta)

    self$grafica <- ggplot(sankey_long,
       aes(x = x, next_x = next_x,
           node = node, next_node = next_node,
           value = value,
           fill = node)) +   # los flujos toman color del nodo
    geom_sankey(flow.alpha = 0.9, color = NA) +
    geom_sankey_label(aes(label = node), size = 3.5, color = "black") +
    scale_fill_manual(values = paleta, na.value = "grey90") +  # usa paleta, gris claro para extras
    scale_y_continuous(breaks = NULL) +
    labs(caption = ifelse(is.na(self$tbl$pregunta[1]),
                   "Sin pregunta definida",
                   self$tbl$pregunta[1]))+
    theme_void() +
    self$tema

    return(self$grafica)
  },

#############################



    #' Graficar Bloque
    #'
    #' Permite mostrar distintos tipos de métricas (media, n, porcentaje) dentro de cada bloque.
    #'
    #' @param freq Character. Indica la variable que se usará para determinar el tipo de los bloques.
    #'   Puede ser `"media"`, `"n"` o `"pct"`. Por defecto es `"media"`.
    #'
    #' @return
    #' Objeto `ggplot2::ggplot`. Se almacena en `self$grafica` y se retorna
    #' para permitir manipulaciones adicionales o guardado.
    graficar_bloque = function(freq = "media") {
      # Crear la gráfica
         # Crear símbolo una sola vez

      self$grafica <- ggplot(self$tbl, aes(area = !!rlang::sym(freq), fill = color)) +
        geom_treemap(color = "white", size = 2) +
        geom_treemap_text(
          aes(label = paste0(respuesta, " ", scales::percent(!!rlang::sym(freq), accuracy = 1))),
          color = "white",
          place = "centre",
          grow = TRUE,
          reflow = TRUE,
          family = self$tema$text$family,
          fontface = "bold",
          size = 10
        ) +
        scale_fill_identity() +
        labs(caption = self$tbl$pregunta[1]) +
        self$tema
      return(self$grafica)
    },
    #' Graficar barras divergentes
    #'
    #' Genera un gráfico divergente de opinión (positivas vs negativas).
    #'
    #' @param regular Valor Regular.
    #' @param positivas Vector de categorías positivas.
    #' @param negativas Vector de categorías negativas.
    #' @return Objeto `ggplot`.
    #' @examples
    #' g$graficar_barras_divergente("Regular",
    #'                              positivas = c("Buena", "Muy buena"),
    #'                              negativas = c("Mala", "Muy mala"))
    graficar_barras_divergente = function(regular, positivas, negativas, y = "media"){
      self$grafica <- self$tbl |>
        ggplot2::ggplot(ggplot2::aes(x = nombre, y = !!rlang::sym(y),
                                     group = factor(respuesta2, c(regular, paste0(regular,"2"), positivas, negativas)))) +
        ggchicklet::geom_chicklet(ggplot2::aes(fill = respuesta, color = respuesta)) +
        ggfittext::geom_fit_text(
          ggplot2::aes(label = etiqueta),
          size = 25,
          position = ggplot2::position_stack(.5, reverse = TRUE),
          vjust = .5, contrast = TRUE, show.legend = FALSE,
          family = self$tema$text$family
        ) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(
          values = self$tbl |> dplyr::distinct(respuesta, color) |> dplyr::pull(color) |>
            purrr::set_names(self$tbl |> dplyr::distinct(respuesta, color) |> dplyr::pull(respuesta))
        ) +
        ggplot2::scale_color_manual(
          values = self$tbl |> dplyr::distinct(respuesta, color) |> dplyr::pull(color) |>
            purrr::set_names(self$tbl |> dplyr::distinct(respuesta, color) |> dplyr::pull(respuesta))
        ) +
        self$tema +
        ggplot2::theme(legend.position = "bottom") +
        lemon::scale_y_symmetric(labels = function(x) scales::percent(abs(x), accuracy = 1)) +
        ggplot2::labs(fill = NULL, color = NULL)
      return(self$grafica)
    },


    #' Preparar datos para gráfico de waffle
    #' @param eje_y,            columna fija (eje Y)
    #' @param  valor           "valor",   valor resultante
    #' @param eje_x,            Eje X (ej. "region")

    #' @return Actualiza self$tbl con los datos preparados.
    generar_coordenadas = function(eje_y, eje_x, valor = "media") {
      stopifnot(!is.null(self$tbl))
      df <- self$tbl

      # Validación de columnas
      if(!all(c(eje_y, eje_x, valor) %in% names(df))){
        stop("Algunas columnas indicadas no existen en self$tbl.")
      }

      # ----------------Niveles para factores --------------------

      #niveles_x: toma los valores únicos de la columna eje_x para mantener el orden de los niveles en el eje X.
      #niveles_y: agrupa por eje_y,  extrae los valores de la columna eje_y

      niveles_x <- df %>% distinct(!!rlang::sym(eje_x)) %>% pull(!!rlang::sym(eje_x))
      niveles_y <- df %>%
        group_by(!!rlang::sym(eje_y)) %>%
        summarise(promedio = mean(!!rlang::sym(valor), na.rm = TRUE)) %>%
        pull(!!rlang::sym(eje_y))

      #----------------- Preparar tabla base con posiciones--------------------
      df <- df %>%
        mutate(
          !!rlang::sym(eje_x) := factor(stringr::str_wrap(!!rlang::sym(eje_x), 15), levels = niveles_x),
          !!rlang::sym(eje_y)  := factor(stringr::str_wrap(!!rlang::sym(eje_y), 35), levels = rev(niveles_y)),
          col     = as.numeric(!!rlang::sym(eje_x)),
          row     = as.numeric(!!rlang::sym(eje_y)),
          fill_id = !!rlang::sym(valor),
          .id = row_number()
        )

      #---------------- Definir forma squircle ----------------------------
      # Genera los puntos de un squircle (una mezcla entre cuadrado y círculo) que será la forma de cada celda.
      n_puntos <- 100; a <- 0.45; b <- 0.45; exp <- 9
      theta <- seq(0, 2*pi, length.out = n_puntos)
      base_shape <- tibble(
        x_unit = a * sign(cos(theta)) * abs(cos(theta))^(2/exp),
        y_unit = b * sign(sin(theta)) * abs(sin(theta))^(2/exp),
        vertex_id = seq_along(theta)
      )

      #------------------ Combinar celda con vértices del squircle --------------
      # tidyr::crossing(df, base_shape): crea una fila por cada combinación de celda y vértice → así cada celda tendrá 100 puntos para dibujar el squircle.
      # x y y: coordenadas finales de cada vértice sumando la posición de la celda.
      df_squircles <- tidyr::crossing(df, base_shape) %>%
        mutate(
          x = col + x_unit,
          y = row + y_unit
        ) %>%
        arrange(.id, vertex_id)

      # --------------- Crear tabla para etiquetas centradas -----------
      #Extrae información única para colocar las etiquetas en el centro de cada celda.
      #row_pos será usado en geom_text para centrar verticalmente la etiqueta.

      df_etiquetas <- df %>%
        dplyr::distinct(.id, !!rlang::sym(eje_x), !!rlang::sym(eje_y), col, row, fill_id) %>%
        dplyr::mutate(row_pos = row)

      # ----------------- Combinar ----------------------
      # Combina los datos de los polígonos con las etiquetas centradas.
      self$tbl <- df_squircles %>%
        left_join(df_etiquetas, by = c(".id", eje_x, eje_y,"col","row","fill_id"))

      invisible(self)
    },


    #' Graficar waffle
    #'
    #' Genera la visualización tipo squircle para mostrar porcentajes
    #'
    #' @param nombre_x        Etiqueta del eje X (horizontal)
    #' @param escala_color    Vector con colores para el gradiente. Debe tener dos elementos: low y high.
    #' @param eje_x           columna de self$tbl que será eje X
    #' @param eje_y           columna de self$tbl que será eje Y
    #'
    #' @return Objeto ggplot.
    graficar_waffle = function(
      nombre_x = NULL,
      escala_color = c(low = "#9d7ad240", high = "#9d7ad2"),
      eje_x = "grupo",
      eje_y = "base_y"
    ) {
      stopifnot(!is.null(self$tbl))

        df <- self$tbl

      # --- Gráfico principal ---
      self$grafica <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, group = .id)) +
        ggplot2::geom_polygon(ggplot2::aes(fill = fill_id), color = "white") +
        ggplot2::geom_text(
          ggplot2::aes(x = col, y = row_pos, label = scales::percent(fill_id, accuracy = 1)),
          size = 10, color = "black", family = self$tema$text$family,
          vjust = 0.5, hjust = 0.5
        ) +
        ggplot2::scale_fill_gradient(
          low = escala_color["low"],
          high = escala_color["high"],
        ) +
        ggplot2::scale_x_continuous(
          breaks = seq_along(levels(df[[eje_x]])),
          labels = levels(df[[eje_x]]),
          position = "top"
        ) +
        ggplot2::scale_y_continuous(
          breaks = seq_along(levels(df[[eje_y]])),
          labels = levels(df[[eje_y]])
        ) +
        ggplot2::labs(
          caption = caption,
          x = nombre_x,
        ) +
        self$tema +
        ggplot2::theme(
          axis.title.x = ggplot2::element_text(
            size = 14,
            face = "bold",
            hjust = 0.5,
            vjust = 2
          ),
          panel.grid = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(face = "bold", size = 12),
          axis.text.y = ggplot2::element_text(size = 14, face = "bold"),
          plot.title = ggplot2::element_text(face = "bold", size = 16, hjust = 0.5),
          plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5)
        )

      return(self$grafica)
    },


#' Quitar filas con NA.

  #' @return Actualiza `self$tbl` filtrando las observaciones no seleccionadas.
  #' @examples
  #' g$contar_variables(vars)$quitar_na)$graficar_lollipops("respuesta")
    quitar_na = function() {
    self$tbl <- self$tbl |> 
    dplyr::filter(!is.na(respuesta)) |> 
    dplyr::mutate(respuesta = forcats::fct_drop(respuesta))
  invisible(self)
},

# MAPAS
mapear_municipios = function(entidad = NULL,
                             variable = NULL,
                             valor = NULL,
                             base_persona = NULL,
                             var_entidad = "CVE_ENT",
                             var_municipio = "CVE_MUN",
                             titulo_leyenda = "Total",
                             tipo = "continuo",
                             low = "#e5f5f9",
                             high = "#611232") {



  # 1. Base a usar
  if (is.null(base_persona)) {
    base <- self$bd
  } else {
    base <- base_persona
  }

  # 2. Cargar shape automáticamente
  if (is.null(entidad)) stop("Debes especificar una entidad, ej. '09'")
  shp <- cargar_shp_entidad(entidad)  # <--- AQUÍ SE CARGA SOLO

  # 3. Filtrar base
  if (!is.null(var_entidad)) {
    base <- base |>
      dplyr::filter(.data[[var_entidad]] == entidad)
  }

  if (!is.null(variable) && !is.null(valor)) {
    base <- base |>
      dplyr::filter(.data[[variable]] == valor)
  }

  # 4. Clave municipal estandarizada
  base <- base |>
    dplyr::mutate(
      municipio_tmp = sprintf("%02d", as.numeric(.data[[var_municipio]]))
    )

  # 5. Resumen
  resumen <- dplyr::count(base, municipio_tmp)

  # 6. Unión con shape
  mapa <- shp |>
    dplyr::left_join(resumen, by = "municipio_tmp")

  # 7. Graficar
  p <- ggplot2::ggplot(mapa, ggplot2::aes(fill = n)) +
    ggplot2::geom_sf(color = "white", size = 0.1) +
    ggplot2::theme_void() +
    ggplot2::theme(
  legend.position = "bottom",
  legend.title.position = "top",
  legend.title = element_text(hjust = 0.5),   # Centrar título
  legend.text  = element_text(hjust = 0.5),   # Centrar texto
  legend.justification = "center",             # Centrar caja de leyenda
  legend.box.just = "center",                  # Centrar caja cuando hay varias líneas
  legend.box = "horizontal"                    # Mantener barra horizontal
)

if (tipo == "continuo") {

  # Escala continua (gradiente)
  p <- p +
    ggplot2::scale_fill_gradient(
      low = low,
      high = high,
      na.value = "gray80",
      name = titulo_leyenda,
      guide = ggplot2::guide_colourbar(
        title.position = "top",
        title.hjust = 0.5
      )
    )

} else if (tipo == "discreto") {

  # Crear categorías (quintiles por default)
  mapa$n_cat <- cut(
    mapa$n,
    breaks = quantile(mapa$n, probs = seq(0, 1, by = 0.2), na.rm = TRUE),
    include.lowest = TRUE
  )

  p <- ggplot2::ggplot(mapa, ggplot2::aes(fill = n_cat)) +
    ggplot2::geom_sf(color = "white", size = 0.1) +
    ggplot2::scale_fill_brewer(
      palette = "Reds",
      name = titulo_leyenda
    )
}


  # 8. Guardar resultado
  self$mapa <- p
  invisible(self)
},

analizar_morena = function(
  personajes,
  puntos,
  vars_conocimiento,
  nombres_completos = NULL,
  etiquetas_preferencia = NULL
) {

  ###############################################################
  # 0) DICCIONARIO UNIFICADO Y LIMPIO
  ###############################################################
  diccionario <- self$diccionario |>
    janitor::clean_names()

  col_codigo <- names(diccionario)[names(diccionario) %in%
                                     c("llaves","codigo","variable","var","id","columna","nombre_var")][1]

  if (is.na(col_codigo)) {
    stop("No se encontró ninguna columna de códigos en el diccionario.")
  }

  diccionario <- diccionario |>
    dplyr::rename(codigo = !!rlang::sym(col_codigo)) |>
    dplyr::mutate(
      codigo = stringr::str_squish(as.character(codigo))
    )

  self$diccionario <- diccionario

  ###############################################################
  # 0.1) NOMBRES COMPLETOS
  ###############################################################
  if (is.null(nombres_completos)) {
    nombres_completos <- stringr::str_to_title(personajes)
  }
  if (length(nombres_completos) != length(personajes)) {
    stop("nombres_completos debe tener la misma longitud que personajes.")
  }

  ###############################################################
  # 1) CONSTRUCCIÓN DE VARIABLES
  ###############################################################
  # Opinión positiva
  pref_opinion <- puntos |> 
    dplyr::filter(nombre == "Opinión positiva") |> 
    dplyr::pull(prefijo)

  vars_opinion <- paste(pref_opinion, personajes, sep = "_")

  # Atributos
  atributos <- puntos |> 
    dplyr::filter(tipo == "atributo") |> 
    dplyr::select(prefijo, nombre, peso_atrib = puntos)

  vars_atrib <- expand.grid(
    prefijo   = atributos$prefijo,
    personaje = personajes,
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(codigo = paste(prefijo, personaje, sep = "_")) |>
    dplyr::pull(codigo)

  # Buen candidato
  pref_buenc <- puntos |> 
    dplyr::filter(nombre == "Buen candidato") |> 
    dplyr::pull(prefijo)

  vars_buenc <- paste(pref_buenc, personajes, sep = "_")

  # Votaría
  pref_vot <- puntos |> 
    dplyr::filter(nombre == "Votaría") |> 
    dplyr::pull(prefijo)

  vars_vot <- paste(pref_vot, personajes, sep = "_")

  # Preferencia declarada
  pref_pref <- puntos |> 
    dplyr::filter(tipo == "preferencia") |> 
    dplyr::pull(prefijo)

  vars_pref <- pref_pref

  vars_todas <- c(
    vars_opinion,
    vars_atrib,
    vars_buenc,
    vars_vot,
    vars_conocimiento,
    vars_pref
  )

  ###############################################################
  # 2) VALIDAR VARIABLES EN DISEÑO
  ###############################################################
  vars_en_diseno <- names(self$diseno$variables)

  faltan <- setdiff(vars_todas, vars_en_diseno)
  if (length(faltan) > 0) {
    stop("Variables NO presentes en diseño: ", paste(faltan, collapse = ", "))
  }

  ###############################################################
  # 3) OPINIÓN POSITIVA
  ###############################################################
  self$contar_variables(vars_opinion, confint = FALSE)
  self$filtrar_respuesta(variable = NULL, valor = "Buena")
  self$pegar_diccionario()

  opinion <- self$tbl |> dplyr::mutate(nombre = "Opinión positiva")

  ###############################################################
  # 4) ATRIBUTOS
  ###############################################################
  self$contar_variables(vars_atrib, confint = FALSE)
  self$filtrar_respuesta(variable = NULL, valor = c("Mucho","Algo"))

  self$tbl <- self$tbl |>
    dplyr::mutate(
      media = dplyr::if_else(respuesta == "Algo", media * 0.5, media)
    ) |>
    dplyr::summarise(media = sum(media), .by = codigo)

  self$pegar_diccionario()

  frecuencia_atributos <- self$tbl |>
    dplyr::mutate(
      respuesta = "Mucho/Algo (ponderado)",
      prefijo = stringr::str_match(
        codigo,
        paste(atributos$prefijo, collapse = "|")
      ) |> as.vector()
    ) |>
    dplyr::left_join(atributos, by = "prefijo")

  ###############################################################
  # 5) BUEN CANDIDATO
  ###############################################################
  self$contar_variables(vars_buenc, confint = FALSE)
  self$filtrar_respuesta(variable = NULL, valor = "Sí")
  self$pegar_diccionario()

  buen_candidato <- self$tbl |>
    dplyr::mutate(nombre = "Buen candidato")

  ###############################################################
  # 6) VOTARÍA
  ###############################################################
  self$contar_variables(vars_vot, confint = FALSE)
  self$filtrar_respuesta(variable = NULL, valor = "Sí votaría")
  self$pegar_diccionario()

  votaria <- self$tbl |> dplyr::mutate(nombre = "Votaría")

  ###############################################################
  # 7) PREFERENCIA DECLARADA
  ###############################################################
  self$contar_variables(vars_pref, confint = FALSE)
  self$pegar_diccionario()

  respuestas_validas <- self$tbl$respuesta[
    purrr::map_lgl(
      self$tbl$respuesta,
      ~ any(stringr::str_detect(
        stringr::str_to_lower(.x),
        stringr::str_to_lower(personajes)
      ))
    )
  ] |> unique()

  self$filtrar_respuesta(variable = NULL, valor = respuestas_validas)

  preferencia <- self$tbl |>
    dplyr::mutate(
      nombre   = "Preferencia declarada",
      atributo = "preferencia",
      tema     = respuesta
    )

  ###############################################################
  # 8) UNIÓN GENERAL
  ###############################################################
  todo <- opinion |>
    dplyr::bind_rows(frecuencia_atributos |> dplyr::select(-peso_atrib)) |>
    dplyr::bind_rows(buen_candidato) |>
    dplyr::bind_rows(votaria) |>
    dplyr::bind_rows(preferencia) |>
    dplyr::filter(!is.na(nombre), !is.na(media)) |>
    dplyr::mutate(
      tema = dplyr::case_when(
        stringr::str_detect(codigo, paste0("_", personajes[1], "$")) ~ nombres_completos[1],
        stringr::str_detect(codigo, paste0("_", personajes[2], "$")) ~ nombres_completos[2],
        TRUE ~ tema
      )
    )

  ###############################################################
  # 9) CONOCIMIENTO
  ###############################################################
  conocimiento_tbl <- contar_vars_pesos(
    variables = vars_conocimiento,
    confint   = FALSE,
    diseno    = self$diseno
  ) |>
    dplyr::filter(respuesta == "Sí lo conoce") |>
    dplyr::summarise(conocimiento = sum(media), .by = codigo) |>
    dplyr::mutate(
      tema = dplyr::case_when(
        stringr::str_detect(codigo, paste0("_", personajes[1], "$")) ~ nombres_completos[1],
        stringr::str_detect(codigo, paste0("_", personajes[2], "$")) ~ nombres_completos[2],
        TRUE ~ codigo
      )
    ) |>
    dplyr::select(tema, conocimiento)

  todo_con_conocimiento <- todo |>
    dplyr::left_join(conocimiento_tbl, by = "tema") |>
    dplyr::mutate(
      conocimiento = tidyr::replace_na(conocimiento, 0),
      media_con_conocimiento = ifelse(
        nombre == "Preferencia declarada",
        media,
        media * conocimiento
      )
    )

  ###############################################################
  # 10) PUNTOS
  ###############################################################
  pesos <- puntos |> dplyr::select(nombre, peso = puntos)

  todo_con_conocimiento <- todo_con_conocimiento |>
    dplyr::left_join(pesos, by = "nombre") |>
    dplyr::group_by(nombre) |>
    dplyr::mutate(
      ganador = media_con_conocimiento == max(media_con_conocimiento, na.rm = TRUE),
      puntos  = ifelse(ganador, peso, 0)
    ) |>
    dplyr::ungroup()

  tabla_morena <- todo_con_conocimiento |>
    dplyr::group_by(tema) |>
    dplyr::mutate(puntaje_final = sum(puntos, na.rm = TRUE)) |>
    dplyr::ungroup()

  ###############################################################
  # 11) HEATMAP FINAL
  ###############################################################
  niveles_x <- c(
    "Opinión positiva","Honestidad","Cercano a la gente",
    "Conoce el Estado","Cumple","Buen candidato",
    "Votaría","Preferencia declarada"
  )

  totales <- tabla_morena |>
    dplyr::select(tema, puntaje_final) |> dplyr::distinct()

  tabla_final <- todo_con_conocimiento |>
    dplyr::select(tema, nombre, media_con_conocimiento) |>
    dplyr::left_join(totales, by = "tema") |>
    dplyr::mutate(
      nombre = factor(nombre, levels = c(niveles_x, "Puntaje final"))
    ) |>
    dplyr::bind_rows(
      totales |>
        dplyr::transmute(
          tema,
          nombre = factor("Puntaje final", levels = c(niveles_x, "Puntaje final")),
          media_con_conocimiento = NA_real_,
          puntaje_final
        )
    )

  grafico <- ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = dplyr::filter(tabla_final, nombre != "Puntaje final"),
      ggplot2::aes(x = nombre, y = tema, fill = media_con_conocimiento),
      width = 1, height = 1,
      show.legend = FALSE
    ) +
    ggfittext::geom_fit_text(
      data = dplyr::filter(tabla_final, nombre != "Puntaje final"),
      ggplot2::aes(
        x = nombre, y = tema,
        label = scales::percent(media_con_conocimiento, accuracy = 1)
      ),
      contrast = TRUE
    ) +
    ggplot2::geom_tile(
      data = dplyr::filter(tabla_final, nombre == "Puntaje final"),
      ggplot2::aes(x = nombre, y = tema),
      fill = "white", width = 1, height = 1
    ) +
    ggfittext::geom_fit_text(
      data = dplyr::filter(tabla_final, nombre == "Puntaje final"),
      ggplot2::aes(
        x = nombre, y = tema,
        label = scales::number(puntaje_final, accuracy = 0.01)
      ),
      color = "black"
    ) +
    ggplot2::scale_fill_gradient(
      low = "#fde0dd",
      high = "#A6032F"
    ) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid  = ggplot2::element_blank()
    )

  ###############################################################
  # 12) GRÁFICO DE CONOCIMIENTO (TECHO)
  ###############################################################
  orden_atributos <- c(
    "Honestidad",
    "Cercano a la gente",
    "Conoce el Estado",
    "Cumple",
    "Buen candidato",
    "Votaría"
  )

  etiquetas_atributos <- c(
    "Honestidad"          = "Honestidad",
    "Cercano a la gente"  = "Cercano a la gente",
    "Conoce el Estado"    = "Conoce\nel Estado",
    "Cumple"              = "Cumple",
    "Buen candidato"      = "Buen candidato",
    "Votaría"             = "Votaría"
  )

  tabla_conocimiento <- todo_con_conocimiento |>
    dplyr::filter(nombre %in% orden_atributos) |>
    dplyr::mutate(
      atributo = nombre,
      tema     = as.character(tema)
    )

  conocimiento_df <- tabla_conocimiento |> 
    dplyr::distinct(tema, conocimiento)

  grafico_conocimiento <- ggplot2::ggplot(
    tabla_conocimiento,
    ggplot2::aes(
      x     = factor(atributo, levels = orden_atributos),
      y     = media_con_conocimiento,
      group = tema,
      color = atributo
    )
  ) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(size = 4) +
    ggplot2::geom_hline(
      data = conocimiento_df,
      ggplot2::aes(yintercept = conocimiento),
      inherit.aes = FALSE,
      color = "#c1121f",
      linewidth = 1
    ) +
    ggplot2::geom_text(
      data = conocimiento_df,
      ggplot2::aes(
        x     = 1,
        y     = conocimiento,
        label = "Conocimiento"
      ),
      color = "#c1121f",
      vjust = -0.6,
      hjust = 0,
      size  = 4,
      inherit.aes = FALSE
    ) +
    ggplot2::facet_wrap(~tema, nrow = 1) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0,1)
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Honestidad"          = "#880D1E",
        "Cercano a la gente"  = "#DD2D4A",
        "Conoce el Estado"    = "#004777",
        "Cumple"              = "#F49CBB",
        "Buen candidato"      = "#2274A5",
        "Votaría"             = "#8ACDEA"
      )
    ) +
    tema_morant() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "bottom",
      strip.text = ggplot2::element_text(size = 14)
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      color = "Aspectos evaluados"
    )

  ###############################################################
  # SALIDA FINAL
  ###############################################################
  return(list(
    tabla                = tabla_morena,
    larga                = todo_con_conocimiento,
    grafico              = grafico,
    grafico_conocimiento = grafico_conocimiento
  ))
}




) 
) 



#' Clase R6 Encuesta
#'
#' Hereda de `Graficar` y permite generar saldos de opinión y gráficos
#' combinados (opinión, conocimiento, Ns/Nc).
#'
#' @docType class
#' @format R6Class object
#' @export
#'
#' @examples
#' \dontrun{
#' g <- Encuesta$new(
#'   diseno = encuesta_demo$muestra$diseno,
#'   diccionario = dicc,
#'   colores = colores,
#'   color_principal = "pink",
#'   tema = tema_morant()
#' )
#'
#
#' g$saldos_opinion(
#'   sufijo_opinion = "opinion_pm",
#'   cat_ns_nc = "Ns/Nc",
#'   sufijo_conoce = "conoce_pm",
#'   cat_conoce = "Sí",
#'   actores = c("astiazaran", "delrio"),
#'   positivas = c("Muy buena", "Buena"),
#'   negativas = c("Mala", "Muy mala"),
#'   regular = "Regular"
#' )
#' }

Encuesta <- R6::R6Class(
  "Encuesta",
  inherit = Graficar,
  public = list(
    #' Inicializa la clase Encuesta
    #'
    #' @inheritParams Graficar$initialize
    initialize = function(diseno = NULL, bd = NULL, diccionario, colores, color_principal, tema){
      super$initialize(diseno, bd, diccionario, colores, color_principal, tema)
    },

    ################################### Función máximo  ###################################

    #' Resalta el valor máximo de una métrica
    #'
    #' Esta función modifica la columna `color` de `self$tbl`, asignando
    #' un color especial (`col_max`) a la fila que contiene el valor máximo
    #' de la variable indicada en `freq`.

      color_maximo = function(col_max,freq="media") {

      self$tbl <- self$tbl |> mutate(color = dplyr::if_else(!!rlang::sym(freq) == max(!!rlang::sym(freq)), !!col_max, color))

      invisible(self)
      },

    ##############

    ################################### función de degradado continuo ###################################

    #' Asigna un degradado de colores continuo a una métrica
    #'
    #' Esta función aplica una escala de color continua a la columna indicada
    #' en `freq` (por defecto "media"). Cada valor recibe un color interpolado
    #' entre los colores definidos en `colores_base`.
    #'
    #' Opcionalmente, si se pasa un color en `col_max`, también se resalta el
    #' valor máximo con ese color (utilizando `self$color_maximo`).
    #'

    degradado_continuo = function(colores_base,col_max = "",freq='media') {


      escala_color <- scales::col_numeric(
      palette = colores_base,
      domain = range(self$tbl[[freq]], na.rm = TRUE))

      #  Asignar color continuo a cada valor de 'media'
      self$tbl <- self$tbl |>
        dplyr::mutate(color = escala_color(!!rlang::sym(freq)))

     # Color max
      if (col_max != "") {
        self$color_maximo(col_max, freq = freq)
      }

      invisible(self)
    }
  )
)
