# ==============================================================================
# Ejercicio Simulación 3: Envido en el Truco
# ==============================================================================

# Semilla para garantizar reproducibilidad
set.seed(123)

# ------------------------------------------------------------------------------
# 1. Generar la variable mazo (40 cartas del Truco)
# ------------------------------------------------------------------------------
# Valores del 1 al 7 y del 10 al 12 en espadas, bastos, oros y copas
palos <- c("espadas", "bastos", "oros", "copas")
numeros <- c(1:7, 10:12)

mazo <- expand.grid(numero = numeros, palo = palos, stringsAsFactors = FALSE)


# ------------------------------------------------------------------------------
# 2. Función sacar_al_azar(mazo) e Histograma del valor medio
# ------------------------------------------------------------------------------
sacar_al_azar <- function(mazo) {
  idx <- sample(nrow(mazo), size = 3, replace = FALSE)
  mano <- mazo[idx, ]
  mazo_restante <- mazo[-idx, ]
  return(list(mano = mano, mazo_restante = mazo_restante))
}

# Simulación para el histograma del valor medio de las cartas de la mano
n_sim <- 10000
medias_manos <- replicate(n_sim, {
  res <- sacar_al_azar(mazo)
  mean(res[["mano"]][["numero"]])
})

# GRÁFICO 1: Distribución del valor medio
hist(medias_manos, 
     breaks = 30, 
     col = "steelblue", 
     border = "white",
     main = "Distribución del Valor Medio de las Manos Extraídas",
     xlab = "Valor medio de la mano", 
     ylab = "Frecuencia")


# ------------------------------------------------------------------------------
# 3. Función contar_tantos(mano)
# ------------------------------------------------------------------------------
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


# ------------------------------------------------------------------------------
# 4. Probabilidad exacta de que el oponente gane el envido (1 vs 1)
# ------------------------------------------------------------------------------
prob_oponente_gana <- function(tu_mano, mazo_restante, tu_eres_mano = TRUE) {
  tus_tantos <- contar_tantos(tu_mano)
  
  # Todas las combinaciones posibles de 3 cartas tomadas de las 37 restantes
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


# ------------------------------------------------------------------------------
# 5. Simulación Truco de 6 jugadores (3 vs 3 con señas < 27)
# ------------------------------------------------------------------------------
simular_truco_6 <- function(tu_mano, tu_equipo_es_mano = TRUE, B = 10000) {
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
    
    # Condición de señas: ambos compañeros deben tener menos de 27 tantos
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


# ------------------------------------------------------------------------------
# 6. (Opcional) Distribución del máximo puntaje del equipo rival
# ------------------------------------------------------------------------------
simular_rivales_opcional <- function(N = 30000) {
  maximos_rivales <- numeric(N)
  
  for (i in 1:N) {
    # Repartir 9 cartas a 3 rivales cualesquiera del mazo
    idx <- sample(nrow(mazo), size = 9, replace = FALSE)
    r1 <- mazo[idx[1:3], ]
    r2 <- mazo[idx[4:6], ]
    r3 <- mazo[idx[7:9], ]
    
    maximos_rivales[i] <- max(contar_tantos(r1), contar_tantos(r2), contar_tantos(r3))
  }
  
  # GRÁFICO 2: Histograma del máximo rival que llega hasta 33
  hist(maximos_rivales, 
       breaks = seq(-0.5, 33.5, by = 1),
       col = "darkorange", 
       border = "white",
       xlim = c(0, 33),
       xaxt = "n",  # Desactiva el eje automático de R
       main = "Distribución del Máximo Envido del Equipo Rival (3 jugadores)",
       xlab = "Máximo tanto de envido entre los 3 rivales", 
       ylab = "Frecuencia Relativa",
       freq = FALSE)
  
  # Eje X con marcas cada 5 unidades y explicitando el 33
  axis(side = 1, at = c(seq(0, 30, by = 5), 33))
  
  return(maximos_rivales)
}


# ==============================================================================
# Ejecución y visualización de resultados
# ==============================================================================

# Ejemplo de extracción para probar los puntos 4 y 5
ejemplo <- sacar_al_azar(mazo)
mi_mano <- ejemplo[["mano"]]
mazo_rest <- ejemplo[["mazo_restante"]]

cat("Tus cartas:\n")
print(mi_mano)
cat("Tus tantos de envido:", contar_tantos(mi_mano), "\n\n")

# Punto 4: Probabilidad exacta en 1 vs 1
p_rival_gana <- prob_oponente_gana(mi_mano, mazo_rest, tu_eres_mano = TRUE)
cat("Punto 4 - Probabilidad exacta de que gane el rival (siendo vos mano):", 
    round(p_rival_gana, 4), "\n\n")

# Punto 5: Probabilidad estimada en 3 vs 3 con señas
p_ganar_equipo <- simular_truco_6(mi_mano, tu_equipo_es_mano = TRUE, B = 5000)
cat("Punto 5 - Probabilidad estimada de que gane tu equipo:", 
    round(p_ganar_equipo, 4), "\n\n")

# Punto 6: Ejecutar y renderizar el SEGUNDO GRÁFICO
maximos_rivales <- simular_rivales_opcional(N = 30000)
