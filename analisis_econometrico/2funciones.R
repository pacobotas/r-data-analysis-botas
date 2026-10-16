# ==============================================================================
# FUNCIONES AUXILIARES: ANÁLISIS ECONOMÉTRICO (GAPMINDER)
# ==============================================================================

#' Calcular el Error Cuadrático Medio Raíz (RMSE)
#' @param obs Vector de valores observados/reales
#' @param pred Vector de valores predichos por el modelo
#' @return Valor escalar del RMSE
calcular_rmse <- function(obs, pred) {
  sqrt(mean((obs - pred)^2, na.rm = TRUE))
}

#' Calcular la tasa de crecimiento porcentual interanual (Delta Y / Y)
#' @param x Vector numérico ordenado cronológicamente
#' @return Vector con las variaciones porcentuales (longitud length(x) - 1)
crecimiento_interanual <- function(x) {
  diff(x) / x[-length(x)]
}

#' Calcular el coeficiente de determinación R^2 de forma manual
#' @param obs Vector de valores observados (variable dependiente)
#' @param pred Vector de valores predichos / ajustados (y_hat)
#' @return Coeficiente de determinación R^2
calcular_r2_manual <- function(obs, pred) {
  scr <- sum((obs - pred)^2, na.rm = TRUE)
  stc <- sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
  1 - (scr / stc)
}
