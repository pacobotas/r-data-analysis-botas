# ==============================================================================
# FUNCIONES AUXILIARES: ANÁLISIS DE DATOS (PISA 2022)
# ==============================================================================

#' Auditar valores faltantes (NAs) en un vector o columna
#' @param x Vector a evaluar
#' @return Cantidad de valores NA
contar_nas <- function(x) {
  sum(is.na(x))
}

#' Calcular el porcentaje de valores faltantes
#' @param x Vector a evaluar
#' @return Porcentaje de NAs redondeado a 2 decimales
pct_nas <- function(x) {
  round(mean(is.na(x)) * 100, 2)
}

#' Asignar cuartiles socioeconómicos dentro de cada país
#' @param df Data frame con columnas country y la variable a segmentar
#' @param var_nombre Nombre de la variable numérica (por defecto "escs")
#' @return Data frame con la columna 'cuartil' añadida
agregar_cuartiles_pais <- function(df, var_nombre = "escs") {
  df %>%
    group_by(country) %>%
    mutate(cuartil = ntile(.data[[var_nombre]], 4)) %>%
    ungroup()
}

#' Calcular rendimiento promedio y cantidad de observaciones agrupadas
#' @param df Data frame filtrado
#' @param grupo_var Vector de nombres de variables de agrupación
#' @param puntaje_var Nombre de la variable de puntaje evaluada
#' @return Data frame resumido con promedio y n
calcular_resumen_rendimiento <- function(df, grupo_vars, puntaje_var) {
  df %>%
    group_by(across(all_of(grupo_vars))) %>%
    summarise(
      nota_promedio = mean(.data[[puntaje_var]], na.rm = TRUE),
      cantidad = n(),
      .groups = "drop"
    )
}

#' Calcular la tasa de resiliencia académica para el cuartil 1 (Q1)
#' @param df Data frame con columnas region, country, math y cuartil
#' @param umbral_puntaje Valor de corte (generalmente la media muestral)
#' @return Data frame ordenado con la tasa de alumnos resilientes (%)
calcular_tasa_resiliencia <- function(df, umbral_puntaje) {
  df %>%
    filter(cuartil == 1) %>%
    group_by(region, country) %>%
    summarise(
      total_q1 = n(),
      resilientes = sum(math > umbral_puntaje, na.rm = TRUE),
      tasa_resiliencia = (resilientes / total_q1) * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(tasa_resiliencia))
}
