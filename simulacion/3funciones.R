# ==============================================================================
# FUNCIONES AUXILIARES: SIMULACIÓN DE ENVIDO EN EL TRUCO
# ==============================================================================

#' Generar el mazo español de 40 cartas de Truco
#' @return data.frame con 40 cartas (numero y palo)
crear_mazo <- function() {
  palos <- c("espadas", "bastos", "oros", "copas")
  numeros <- c(1:7, 10:12)
  expand.grid(numero = numeros, palo = palos, stringsAsFactors = FALSE)
}

#' Simular la extracción aleatoria de 3 cartas de un mazo
#' @param mazo data.frame con las cartas disponibles
#' @return Lista con la mano extraída (3 cartas) y el mazo restante
sacar_al_azar <- function(mazo) {
  idx <- sample(nrow(mazo), size = 3, replace = FALSE)
  mano <- mazo[idx, ]
  mazo_restante <- mazo[-idx, ]
  return(list(mano = mano, mazo_restante = mazo_restante))
}

#' Calcular los tantos de envido de una mano de 3 cartas
#' @param mano data.frame con 3 filas (columnas numero y palo)
#' @return Valor numérico entero del envido (entre 0 y 33)
contar_tantos <- function(mano) {
  # Las figuras (10, 11 y 12) valen 0 para el envido; del 1 al 7 valen su número
  v_envido <- ifelse(mano[["numero"]] %in% c(10, 11, 12), 0, mano[["numero"]])
  palos_mano <- mano[["palo"]]
  
  conteo_palos <- table(palos_mano)
  palo_con_par <- names(conteo_palos[conteo_palos >= 2])
  
  if (length(palo_con_par) > 0) {
    # Al menos dos cartas del mismo palo
    palo_elegido <- palo_con_par[1]
    valores_mismo_palo <- v_envido[palos_mano == palo_elegido]
    
    if (length(valores_mismo_palo) == 3) {
      # Si las 3 cartas son del mismo palo, se toman las dos de mayor valor
      dos_mayores <- sort(valores_mismo_palo, decreasing = TRUE)[1:2]
      return(20 + sum(dos_mayores))
    } else {
      # Exactamente 2 cartas del mismo palo
      return(20 + sum(valores_mismo_palo))
    }
  } else {
    # Si no hay dos del mismo palo: el mayor valor individual
    return(max(v_envido))
  }
}

#' Calcular probabilidad exacta de que el oponente gane el envido (1 vs 1)
#' @param tu_mano data.frame con tus 3 cartas
#' @param mazo_restante data.frame con las 37 cartas restantes
#' @param tu_eres_mano Booleano indicando si tú eres mano
#' @return Probabilidad exacta (casos favorables / combinaciones totales)
prob_oponente_gana <- function(tu_mano, mazo_restante, tu_eres_mano = TRUE) {
  tus_tantos <- contar_tantos(tu_mano)
  
  # Todas las combinaciones de 3 cartas tomadas de las 37 restantes
  combs <- combn(nrow(mazo_restante), 3)
  n_combs <- ncol(combs)
  
  tantos_oponente <- apply(combs, 2, function(idx) {
    mano_rival <- mazo_restante[idx, ]
    contar_tantos(mano_rival)
  })
  
  if (tu_eres_mano) {
    # En caso de empate ganas tú; el oponente debe superar estrictamente
    favorables <- sum(tantos_oponente > tus_tantos)
  } else {
    # Si el oponente es mano, él gana en empate
    favorables <- sum(tantos_oponente >= tus_tantos)
  }
  
  return(favorables / n_combs)
}

#' Simulación Monte Carlo de Truco 3 vs 3 con señas (< 27 en compañeros)
#' @param tu_mano data.frame con tus 3 cartas
#' @param mazo data.frame con el mazo completo de 40 cartas
#' @param tu_equipo_es_mano Booleano indicando si tu equipo es mano
#' @param B Cantidad de simulaciones válidas a computar
#' @return Probabilidad estimada de victoria del equipo
simular_truco_6 <- function(tu_mano, mazo, tu_equipo_es_mano = TRUE, B = 10000) {
  # Remover tu mano del mazo disponible
  idx_tu_mano <- which(apply(mazo, 1, function(r) {
    any(tu_mano[["numero"]] == as.numeric(r["numero"]) & tu_mano[["palo"]] == r["palo"])
  }))
  mazo_disponible <- mazo[-idx_tu_mano, ]
  
  tus_tantos <- contar_tantos(tu_mano)
  victorias_equipo <- 0
  sims_validas <- 0
  
  while (sims_validas < B) {
    # Repartir 15 cartas (3 para cada uno de los 5 jugadores restantes)
    indices <- sample(nrow(mazo_disponible), size = 15, replace = FALSE)
    
    c1 <- mazo_disponible[indices[1:3], ]
    c2 <- mazo_disponible[indices[4:6], ]
    
    t_c1 <- contar_tantos(c1)
    t_c2 <- contar_tantos(c2)
    
    # Condición de señas: ambos compañeros tienen menos de 27 tantos
    if (t_c1 < 27 && t_c2 < 27) {
      sims_validas <- sims_validas + 1
      
      max_tu_equipo <- max(tus_tantos, t_c1, t_c2)
      
      # 3 rivales
      r1 <- mazo_disponible[indices[7:9], ]
      r2 <- mazo_disponible[indices[10:12], ]
      r3 <- mazo_disponible[indices[13:15], ]
      
      max_rival <- max(contar_tantos(r1), contar_tantos(r2), contar_tantos(r3))
      
      if (tu_equipo_es_mano) {
        if (max_tu_equipo >= max_rival) victorias_equipo <- victorias_equipo + 1
      } else {
        if (max_tu_equipo > max_rival) victorias_equipo <- victorias_equipo + 1
      }
    }
  }
  
  return(victorias_equipo / B)
}

#' Simular la distribución del puntaje máximo de los 3 rivales
#' @param mazo data.frame con el mazo de cartas
#' @param N Cantidad de repeticiones de la simulación
#' @return Vector numérico con los máximos registrados
simular_maximos_rivales <- function(mazo, N = 30000) {
  maximos_rivales <- numeric(N)
  
  for (i in 1:N) {
    idx <- sample(nrow(mazo), size = 9, replace = FALSE)
    r1 <- mazo[idx[1:3], ]
    r2 <- mazo[idx[4:6], ]
    r3 <- mazo[idx[7:9], ]
    
    maximos_rivales[i] <- max(contar_tantos(r1), contar_tantos(r2), contar_tantos(r3))
  }
  
  return(maximos_rivales)
}
