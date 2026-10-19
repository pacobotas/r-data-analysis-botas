# SIMULACION 3: envido en el Truco

set.seed(123)

# 1
mazo <- expand.grid(numero = c(1:7, 10:12),
                    palo = c("espadas", "bastos", "oros", "copas"),
                    stringsAsFactors = FALSE)
nrow(mazo)

# 2

sacar_al_azar <- function(mazo) {
  posicion <- sample(nrow(mazo), 3)
  list(mano = mazo[posicion, ], mazo_restante = mazo[-posicion, ])
}

extraccion <- sacar_al_azar(mazo)
extraccion$mano
nrow(extraccion$mazo_restante)  # Verifica que queden 37 cartas

# Histograma del valor medio 
n_sim <- 10000
valor_medio <- replicate(n_sim, mean(sacar_al_azar(mazo)$mano$numero))

hist(valor_medio, breaks = 30, col = "steelblue", border = "white",
     main = "Distribución del Valor Medio de las Manos Extraídas", 
     xlab = "Valor medio de la mano", ylab = "Frecuencia")

# 3
valor_envido <- function(numero) ifelse(numero <= 7, numero, 0)
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

contar_tantos(extraccion$mano)

# 4
prob_oponente_gana <- function(mi_mano, mazo_restante, soy_mano = FALSE) {
  mis_tantos <- contar_tantos(mi_mano)
  combos <- combn(nrow(mazo_restante), 3)
  tantos_op <- apply(combos, 2, function(posicion) contar_tantos(mazo_restante[posicion, ]))
  if (soy_mano) mean(tantos_op > mis_tantos) else mean(tantos_op >= mis_tantos)
}

mi_mano <- extraccion$mano
resto   <- extraccion$mazo_restante

cat("Mis tantos:", contar_tantos(mi_mano), "\n")
cat("P(oponente gana | soy mano):   ", round(prob_oponente_gana(mi_mano, resto, TRUE), 4), "\n")
cat("P(oponente gana | no soy mano):", round(prob_oponente_gana(mi_mano, resto, FALSE), 4), "\n")

# 5

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

sim_mano   <- simular_envido_equipos(mi_mano, resto, n_sim, mano_mio = TRUE)
sim_nomano <- simular_envido_equipos(mi_mano, resto, n_sim, mano_mio = FALSE)

cat("P(gana mi equipo | mano de mi equipo):", round(sim_mano$prob, 4), "\n")
cat("P(gana mi equipo | mano del rival):   ", round(sim_nomano$prob, 4), "\n")
