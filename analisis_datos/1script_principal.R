# ==============================================================================
# EJERCICIO 1 Trabajo práctico análisis de datos
# ==============================================================================

library(learningtower)
library(tidyverse)
set.seed(123)

# Cargar el dataset
pisa_completo <- load_student(year = 2022)


# ==============================================================================
# PREGUNTA 1:  ¿En qué medida el estatus socioeconómico del hogar (ESCS)
#condiciona el rendimiento en Matemática en los países evaluados de 
#Latinoamérica en PISA 2022? 

# ==============================================================================

# Países elegidos
paises_latam <- c("ARG", "BRA", "CHL", "COL", "MEX", "PER", "URY")

# NAs
pisa_completo %>% 
  filter(country %in% paises_latam) %>% 
  summarise(
    total = n(),
    nas_escs = sum(is.na(escs)),
    nas_math = sum(is.na(math))
  )

# Filtrar
pisa_p1 <- pisa_completo %>% 
  filter(country %in% paises_latam) %>% 
  filter(!is.na(escs) & !is.na(math))

#Visualización previa
ggplot(pisa_p1, aes(x = country, y = escs)) +
  geom_boxplot(fill = "darkolivegreen3") +
  labs(
    title = "Distribución del Índice ESCS por País",
    x = "País",
    y = "Índice ESCS"
  ) +
  theme_minimal()

#Análisis:
rendimiento_escs <- pisa_p1 %>% 
  group_by(country) %>% 
  mutate(cuartil = ntile(escs, 4)) %>% 
  group_by(country, cuartil) %>% 
  summarise(
    nota_promedio = mean(math),
    cantidad = n()
  )
print(rendimiento_escs)

#Gráfico
ggplot(rendimiento_escs, aes(x = factor(cuartil), y = nota_promedio, color = country, group = country)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Puntaje Medio en Matemática según Cuartil Socioeconómico",
    x = "Cuartil ESCS (1 = Menor ingreso, 4 = Mayor ingreso)",
    y = "Puntaje Promedio",
    color = "País"
  ) +
  theme_minimal()


# ==============================================================================
# Pregunta 2: Qué proporción de estudiantes del cuartil socioeconómico más bajo
#(Q1) logra un rendimiento superior al promedio global de Matemática en Asia 
#Oriental frente a Latinoamérica? 
 
# ==============================================================================
# Países de Asia Oriental presentes en PISA 2022
paises_asia_oriental <- c("JPN", "KOR", "TWN", "HKG", "MAC", "MNG")
todos_paises_p2 <- c(paises_latam, paises_asia_oriental)

#NAs
pisa_p2 <- pisa_completo %>% 
  filter(country %in% todos_paises_p2) %>% 
  filter(!is.na(escs) & !is.na(math)) %>% 
  mutate(region = if_else(country %in% paises_asia_oriental, "Asia Oriental", "Latinoamérica"))

#Visualización previa
ggplot(pisa_p2, aes(x = country, y = math, fill = region)) +
  geom_boxplot() +
  facet_wrap(~region, scales = "free_x") +
  labs(
    title = "Distribución de Notas en Matemática por País y Región",
    x = "País",
    y = "Puntaje de Matemática"
  ) +
  theme_minimal()

#Análisis
media_muestra <- mean(pisa_p2$math)

resiliencia <- pisa_p2 %>% 
  group_by(country) %>% 
  mutate(cuartil = ntile(escs, 4)) %>% 
  filter(cuartil == 1) %>% 
  group_by(region, country) %>% 
  summarise(
    total_q1 = n(),
    resilientes = sum(math > media_muestra),
    tasa_resiliencia = (resilientes / total_q1) * 100
  ) %>% 
  arrange(desc(tasa_resiliencia))
print(resiliencia)

# Gráfico
ggplot(resiliencia, aes(x = reorder(country, tasa_resiliencia), y = tasa_resiliencia, fill = region)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Porcentaje de Alumnos Resilientes en el Cuartil Más Vulnerable (Q1)",
    subtitle = "Alumnos de Q1 con puntaje superior al promedio general de la muestra",
    x = "País",
    y = "Porcentaje de Alumnos (%)",
    fill = "Región"
  ) +
  theme_minimal()
# ==============================================================================
# PREGUNTA 3: ¿Cómo se relaciona el nivel de instrucción alcanzado por la madre
#con el puntaje de Lectura de los estudiantes en Latinoamérica? 

# ==============================================================================

# NA y filtro
pisa_completo %>% 
  filter(country %in% paises_latam) %>% 
  summarise(
    nas_madre = sum(is.na(mother_educ)),
    nas_read = sum(is.na(read))
  )

pisa_p3 <- pisa_completo %>% 
  filter(country %in% paises_latam) %>% 
  filter(!is.na(mother_educ) & !is.na(read))

#Visualización previa
ggplot(pisa_p3, aes(x = mother_educ)) +
  geom_bar(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Cantidad de Respuestas por Nivel Educativo de la Madre",
    x = "Nivel Educativo (ISCED)",
    y = "Cantidad de Estudiantes"
  ) +
  theme_minimal()

#Análisis
rendimiento_madre <- pisa_p3 %>% 
  group_by(mother_educ) %>% 
  summarise(
    promedio_lectura = mean(read),
    cantidad = n()
  ) %>% 
  arrange(promedio_lectura)

print(rendimiento_madre)

# Gráfico
ggplot(rendimiento_madre, aes(x = reorder(mother_educ, promedio_lectura), y = promedio_lectura)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Puntaje Promedio en Lectura según Educación Materna",
    x = "Nivel Educativo de la Madre",
    y = "Puntaje Medio en Lectura"
  ) +
  theme_minimal()
