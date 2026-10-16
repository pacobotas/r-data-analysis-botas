# ==============================================================================
# Ejercicio 2: Análisis econométrico con datos de gapminder
# ==============================================================================

library(tidyverse)

# Semilla obligatoria por pauta general del examen
set.seed(123)

# 1. Cargar el dataset gapminder (si no está ya en el entorno)
if (!exists("gapminder")) {
  gapminder <- read_csv("gapminder.csv")
}

# Explorar las primeras filas del dataset
head(gapminder)


# ==============================================================================
# PARTE 1: INGRESO POR PERSONA
# ==============================================================================

# ------------------------------------------------------------------------------
# Inciso 1: Evolución temporal de income_per_person en Argentina
# ------------------------------------------------------------------------------
arg_datos <- gapminder %>% 
  filter(country == "Argentina") %>% 
  arrange(year)

ggplot(arg_datos, aes(x = year, y = income_per_person)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Evolución temporal del Ingreso por Persona en Argentina",
    x = "Año",
    y = "Ingreso por persona (USD)"
  ) +
  theme_minimal()


# ------------------------------------------------------------------------------
# Inciso 2: Train/Test Split, Regresión Polinómica y Overfitting
# ------------------------------------------------------------------------------

# 1) Separar los últimos 10 años para Test (2001-2010) y el resto para Train (1960-2000)
arg_train <- subset(arg_datos, year <= 2000)
arg_test  <- subset(arg_datos, year > 2000)

# 2) Estimación de los 3 modelos sobre el conjunto de entrenamiento (Train)
# (a) Modelo lineal: income = beta0 + beta1 * year
mod_lineal <- lm(income_per_person ~ year, data = arg_train)

# (b) Polinómico grado 2: income = beta0 + beta1 * year + beta2 * year^2
mod_poly2  <- lm(income_per_person ~ poly(year, 2, raw = TRUE), data = arg_train)

# (c) Polinómico grado 10: income = beta0 + ... + beta10 * year^10
mod_poly10 <- lm(income_per_person ~ poly(year, 10, raw = TRUE), data = arg_train)

# 3) Predicciones en una grilla para graficar los modelos ajustados en train
grid <- data.frame(year = seq(min(arg_datos$year), max(arg_datos$year), length.out = 400))
grid$y_lin    <- predict(mod_lineal, newdata = grid)
grid$y_poly2  <- predict(mod_poly2,  newdata = grid)
grid$y_poly10 <- predict(mod_poly10, newdata = grid)

# 4) Representación gráfica de los tres ajustes junto con los datos observados
g_overfit <- ggplot() +
  geom_point(data = arg_train, aes(x = year, y = income_per_person), color = "blue", alpha = 0.7) +
  geom_point(data = arg_test,  aes(x = year, y = income_per_person), color = "red",  alpha = 0.7) +
  geom_line(data = grid, aes(x = year, y = y_lin,    color = "Lineal"),   linewidth = 1) +
  geom_line(data = grid, aes(x = year, y = y_poly2,  color = "Grado 2"),  linewidth = 1) +
  geom_line(data = grid, aes(x = year, y = y_poly10, color = "Grado 10"), linewidth = 1) +
  scale_color_manual(values = c("Lineal" = "green", "Grado 2" = "purple", "Grado 10" = "orange")) +
  coord_cartesian(ylim = c(0, 30000)) +
  labs(
    title = "Ajuste de Modelos sobre Argentina (Lineal, Grado 2 y Grado 10)",
    subtitle = "Azul = train (1960-2000), Rojo = test (2001-2010)",
    x = "Año",
    y = "Ingreso por persona (USD)",
    color = "Modelo"
  ) +
  theme_minimal()

print(g_overfit)

# 5) Cuantificar los resultados sobre Train y Test (Cálculo de RMSE)
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))

rmse_train <- c(
  Lineal  = rmse(arg_train$income_per_person, fitted(mod_lineal)),
  Grado2  = rmse(arg_train$income_per_person, fitted(mod_poly2)),
  Grado10 = rmse(arg_train$income_per_person, fitted(mod_poly10))
)

rmse_test <- c(
  Lineal  = rmse(arg_test$income_per_person, predict(mod_lineal, newdata = arg_test)),
  Grado2  = rmse(arg_test$income_per_person, predict(mod_poly2,  newdata = arg_test)),
  Grado10 = rmse(arg_test$income_per_person, predict(mod_poly10, newdata = arg_test))
)

cat("\nRMSE en TRAIN:\n")
print(round(rmse_train, 2))

cat("\nRMSE en TEST:\n")
print(round(rmse_test, 2))


# ------------------------------------------------------------------------------
# Inciso 3: Matrices de correlación con 4 países sudamericanos
# ------------------------------------------------------------------------------

# 1) Extraer los ingresos de cada país ordenados cronológicamente
ing_arg <- (gapminder %>% filter(country == "Argentina") %>% arrange(year))$income_per_person
ing_bra <- (gapminder %>% filter(country == "Brazil")    %>% arrange(year))$income_per_person
ing_chl <- (gapminder %>% filter(country == "Chile")     %>% arrange(year))$income_per_person
ing_col <- (gapminder %>% filter(country == "Colombia")  %>% arrange(year))$income_per_person
ing_uru <- (gapminder %>% filter(country == "Uruguay")   %>% arrange(year))$income_per_person

# Armar la tabla de niveles con data.frame
tabla_ingresos <- data.frame(
  Argentina = ing_arg,
  Brasil    = ing_bra,
  Chile     = ing_chl,
  Colombia  = ing_col,
  Uruguay   = ing_uru
)

# (a) Matriz de correlaciones de niveles de ingreso (income_per_person)
matriz_niveles <- cor(tabla_ingresos)
cat("\n=== (a) MATRIZ DE CORRELACIÓN DE NIVELES DE INGRESO ===\n")
print(round(matriz_niveles, 3))

# (b) Matriz de correlaciones de variaciones porcentuales anuales (Delta Y / Y)
crec_arg <- diff(ing_arg) / ing_arg[-length(ing_arg)]
crec_bra <- diff(ing_bra) / ing_bra[-length(ing_bra)]
crec_chl <- diff(ing_chl) / ing_chl[-length(ing_chl)]
crec_col <- diff(ing_col) / ing_col[-length(ing_col)]
crec_uru <- diff(ing_uru) / ing_uru[-length(ing_uru)]

tabla_crecimiento <- data.frame(
  Argentina = crec_arg,
  Brasil    = crec_bra,
  Chile     = crec_chl,
  Colombia  = crec_col,
  Uruguay   = crec_uru
)

matriz_crecimiento <- cor(tabla_crecimiento)
cat("\n=== (b) MATRIZ DE CORRELACIÓN DE CRECIMIENTO INTERANUAL (Y/Y) ===\n")
print(round(matriz_crecimiento, 3))


# ==============================================================================
# PARTE 2: ESPERANZA DE VIDA Y GÉNERO
# ==============================================================================

# Filtrar un año en particular a elección (2010) y asegurar formato numérico sin NAs
df_p2 <- gapminder %>% 
  filter(year == 2010) %>% 
  mutate(
    life_expectancy_female = as.numeric(life_expectancy_female),
    life_expectancy        = as.numeric(life_expectancy),
    income_per_person      = as.numeric(income_per_person),
    child_mortality        = as.numeric(child_mortality),
    children_per_woman     = as.numeric(children_per_woman)
  ) %>% 
  filter(!is.na(life_expectancy) & !is.na(life_expectancy_female) & !is.na(income_per_person))


# ------------------------------------------------------------------------------
# Inciso 5: Esperanza de vida total vs. femenina
# ------------------------------------------------------------------------------
plot(df_p2$life_expectancy_female, df_p2$life_expectancy,
     main = "Esperanza de Vida Total vs. Femenina (Año 2010)",
     xlab = "Esperanza de vida femenina (años)",
     ylab = "Esperanza de vida total (años)",
     pch = 16, col = "blue")

# Diagonal de 45 grados (y = x)
abline(a = 0, b = 1, col = "darkgreen", lwd = 2, lty = 2)


# ------------------------------------------------------------------------------
# Inciso 6: Regresión lineal simple y cálculo de R^2
# ------------------------------------------------------------------------------
modelo_simple <- lm(life_expectancy ~ life_expectancy_female, data = df_p2)
sum_simple    <- summary(modelo_simple)

# Agregar la línea de regresión al gráfico
abline(modelo_simple, col = "red", lwd = 2)
legend("topleft", legend = c("Recta estimada", "Igualdad (y = x)"),
       col = c("red", "darkgreen"), lwd = 2, lty = c(1, 2), bty = "n")

# R^2 desde summary y manual
R2_sum <- sum_simple$r.squared
y_hat  <- fitted(modelo_simple)
Y_obs  <- df_p2$life_expectancy
R2_man <- 1 - sum((Y_obs - y_hat)^2) / sum((Y_obs - mean(Y_obs))^2)

cat("\n=== Inciso 6: Modelo Simple ===\n")
print(sum_simple)
cat(sprintf("\nR^2 (summary) = %.4f | R^2 (manual) = %.4f\n\n", R2_sum, R2_man))


# ------------------------------------------------------------------------------
# Inciso 7: Test de hipótesis H0: female = total vs H1: female > total
# ------------------------------------------------------------------------------
df_p2 <- df_p2 %>% 
  mutate(diferencia = life_expectancy_female - life_expectancy)

test_lm <- lm(diferencia ~ 1, data = df_p2)
summary(test_lm)


# ------------------------------------------------------------------------------
# Inciso 8: Regresión múltiple agregando income_per_person
# ------------------------------------------------------------------------------
mod_simple   <- lm(life_expectancy ~ life_expectancy_female, data = df_p2)
mod_multiple <- lm(life_expectancy ~ life_expectancy_female + income_per_person, data = df_p2)

summary(mod_simple)
summary(mod_multiple)


# ------------------------------------------------------------------------------
# Inciso 9: Modelo de hasta 3 covariables (excluyendo género)
# ------------------------------------------------------------------------------
df_modelo9 <- df_p2 %>% 
  filter(!is.na(child_mortality) & !is.na(children_per_woman)) %>% 
  mutate(log_income = log(income_per_person))

mod_final <- lm(life_expectancy ~ log_income + child_mortality + children_per_woman, data = df_modelo9)
summary(mod_final)
