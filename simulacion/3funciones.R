# FUNCIONES AUXILIARES: SIMULACIÓN DE ENVIDO EN EL TRUCO

# 1. Asignar el valor de envido a cada número (figuras valen 0)
valor_envido <- function(numero) {
  ifelse(numero <= 7, numero, 0)
}

# 2. Extraer 3 cartas al azar y devolver la mano y el mazo restante
sacar_al_azar <- function(mazo) {
  posicion <- sample(nrow(mazo), 3)
  list(mano = mazo[posicion, ], mazo_restante = mazo[-posicion, ])
}

# 3. Contar tantos de envido según las reglas del Truco
contar_tantos <- function(mano) {
  v <- valor_envido(mano$numero)
  tantos <- -Inf
  for (p in unique(mano$palo)) {
    vals <- sort(v[mano$palo == p], decreasing = TRUE)
    if (length(vals) >= 2) tantos <- max(tantos, sum(vals[1:2]) + 20)
  }
  if (is.infinite(tantos)) tantos <- max(v)
  tantos
}

# 4. Calcular probabilidad exacta de que el oponente gane (1 vs 1)
prob_oponente_gana <- function(mi_mano, mazo_restante, soy_mano = FALSE) {
  mis_tantos <- contar_tantos(mi_mano)
  combos <- combn(nrow(mazo_restante), 3)
  tantos_op <- apply(combos, 2, function(posicion) contar_tantos(mazo_restante[posicion, ]))
  if (soy_mano) mean(tantos_op > mis_tantos) else mean(tantos_op >= mis_tantos)
}

# 5. Simular partida de 6 jugadores (3 vs 3) con señas (< 27 en compañeros)
simular_envido_equipos <- function(mi_mano, mazo_restante, n_sim = 10000,
                                   mano_mio = TRUE, tope_comp = 27) {
  mis_tantos <- contar_tantos(mi_mano)
  gana <- logical(n_sim)
  max_rival <- numeric(n_sim)
  
  for (i in seq_len(n_sim)) {
    repeat {
      cartas <- sample(nrow(mazo_restante), 15)
      comp1 <- contar_tantos(mazo_restante[cartas[1:3], ])
      comp2 <- contar_tantos(mazo_restante[cartas[4:6], ])
      if (comp1 < tope_comp && comp2 < tope_comp) break
    }
    riv <- c(contar_tantos(mazo_restante[cartas[7:9], ]),
             contar_tantos(mazo_restante[cartas[10:12], ]),
             contar_tantos(mazo_restante[cartas[13:15], ]))
    
    max_nuestro <- max(mis_tantos, comp1, comp2)
    max_rival[i] <- max(riv)
    gana[i] <- if (mano_mio) max_nuestro >= max_rival[i] else max_nuestro > max_rival[i]
  }
  list(prob = mean(gana), max_rival = max_rival)
}
