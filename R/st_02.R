## ------------------------------------------------------------------------------------------------------------------
library(readxl)
library(tidyverse)
library(corrplot)
library(naniar)

# Consultar la carpeta de trabajo.
getwd()

# Nombre del archivo Excel.
archivo <- "../datos/base_borregos_espaciotemporal_Hidalgo.xlsx"

# Consultar las hojas disponibles.
excel_sheets(archivo)

# Importar la tabla longitudinal.
datos <- read_excel(archivo, sheet = "datos_long")



## ------------------------------------------------------------------------------------------------------------------
datos_b2 <- datos |>
  mutate(
    animal_id = as.factor(animal_id),
    granja_id = as.factor(granja_id),
    sexo = as.factor(sexo),
    raza = as.factor(raza),
    tratamiento = as.factor(tratamiento),
    sistema_produccion = as.factor(sistema_produccion),
    salud_score_0_4 = factor(
      salud_score_0_4,
      levels = 0:4,
      ordered = TRUE
    ),
    salud_alta = as.factor(salud_alta),
    periodo = cut(
      tiempo,
      breaks = c(-Inf, 20, 40, 60, 80, Inf),
      labels = c(
        "Semanas 1-20",
        "Semanas 21-40",
        "Semanas 41-60",
        "Semanas 61-80",
        "Semanas 81-100"
      )
    )
  )

# Comprobar las variables preparadas.
datos_b2 |>
  select(
    animal_id, granja_id, tiempo, periodo,
    sexo, raza, peso_kg, consumo_kg_dia,
    salud_score_0_4, salud_alta
  ) |>
  glimpse()


## ------------------------------------------------------------------------------------------------------------------
resumen_productivo <- datos_b2 |>
  summarise(
    n_peso = sum(!is.na(peso_kg)),
    peso_media = mean(peso_kg, na.rm = TRUE),
    peso_mediana = median(peso_kg, na.rm = TRUE),
    peso_de = sd(peso_kg, na.rm = TRUE),
    peso_q1 = quantile(peso_kg, 0.25, na.rm = TRUE),
    peso_q3 = quantile(peso_kg, 0.75, na.rm = TRUE),
    n_consumo = sum(!is.na(consumo_kg_dia)),
    consumo_media = mean(consumo_kg_dia, na.rm = TRUE),
    consumo_mediana = median(consumo_kg_dia, na.rm = TRUE),
    consumo_de = sd(consumo_kg_dia, na.rm = TRUE),
    consumo_q1 = quantile(consumo_kg_dia, 0.25, na.rm = TRUE),
    consumo_q3 = quantile(consumo_kg_dia, 0.75, na.rm = TRUE)
  )

resumen_productivo


## ------------------------------------------------------------------------------------------------------------------
ggplot(datos_b2, aes(x = peso_kg)) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white",
    na.rm = TRUE
  ) +
  geom_vline(
    aes(xintercept = median(peso_kg, na.rm = TRUE)),
    color = "firebrick",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  labs(
    title = "Distribución del peso",
    subtitle = "La línea discontinua representa la mediana",
    x = "Peso (kg)",
    y = "Número de mediciones"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
ggplot(datos_b2, aes(x = consumo_kg_dia)) +
  geom_histogram(
    bins = 30,
    fill = "darkseagreen4",
    color = "white",
    na.rm = TRUE
  ) +
  geom_vline(
    aes(xintercept = median(consumo_kg_dia, na.rm = TRUE)),
    color = "firebrick",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  labs(
    title = "Distribución del consumo diario de alimento",
    subtitle = "La línea discontinua representa la mediana",
    x = "Consumo de alimento (kg/día)",
    y = "Número de mediciones"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
tabla_salud <- datos_b2 |>
  count(salud_score_0_4, name = "n") |>
  mutate(
    porcentaje = 100 * n / sum(n)
  )

tabla_salud


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  tabla_salud,
  aes(x = salud_score_0_4, y = porcentaje)
) +
  geom_col(fill = "darkorange3", width = 0.75) +
  geom_text(
    aes(label = paste0(round(porcentaje, 1), "%")),
    vjust = -0.3,
    size = 3.5
  ) +
  labs(
    title = "Distribución de la puntuación de salud",
    x = "Puntuación de salud",
    y = "Porcentaje de mediciones"
  ) +
  expand_limits(y = max(tabla_salud$porcentaje) * 1.12) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
resumen_sexo <- datos_b2 |>
  group_by(sexo) |>
  summarise(
    n_animales = n_distinct(animal_id),
    n_mediciones = sum(!is.na(peso_kg)),
    peso_medio = mean(peso_kg, na.rm = TRUE),
    peso_mediano = median(peso_kg, na.rm = TRUE),
    consumo_medio = mean(consumo_kg_dia, na.rm = TRUE),
    .groups = "drop"
  )

resumen_sexo


## ------------------------------------------------------------------------------------------------------------------
ggplot(datos_b2, aes(x = sexo, y = peso_kg, fill = sexo)) +
  geom_boxplot(
    alpha = 0.75,
    outlier.alpha = 0.25,
    na.rm = TRUE
  ) +
  labs(
    title = "Distribución del peso según el sexo",
    x = "Sexo",
    y = "Peso (kg)"
  ) +
  guides(fill = "none") +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
resumen_raza <- datos_b2 |>
  group_by(raza) |>
  summarise(
    n_animales = n_distinct(animal_id),
    n_mediciones = sum(!is.na(peso_kg)),
    peso_medio = mean(peso_kg, na.rm = TRUE),
    peso_mediano = median(peso_kg, na.rm = TRUE),
    consumo_medio = mean(consumo_kg_dia, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(peso_medio))

resumen_raza


## ------------------------------------------------------------------------------------------------------------------
ggplot(datos_b2, aes(x = raza, y = peso_kg, fill = raza)) +
  geom_boxplot(
    alpha = 0.75,
    outlier.alpha = 0.20,
    na.rm = TRUE
  ) +
  labs(
    title = "Distribución del peso según la raza",
    x = "Raza",
    y = "Peso (kg)"
  ) +
  guides(fill = "none") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1)
  )


## ------------------------------------------------------------------------------------------------------------------
resumen_semana <- datos_b2 |>
  group_by(tiempo) |>
  summarise(
    n = sum(!is.na(peso_kg)),
    peso_medio = mean(peso_kg, na.rm = TRUE),
    peso_mediano = median(peso_kg, na.rm = TRUE),
    peso_de = sd(peso_kg, na.rm = TRUE),
    peso_error_estandar = peso_de / sqrt(n),
    limite_inferior = peso_medio - 1.96 * peso_error_estandar,
    limite_superior = peso_medio + 1.96 * peso_error_estandar,
    consumo_medio = mean(consumo_kg_dia, na.rm = TRUE),
    .groups = "drop"
  )

head(resumen_semana, 10)


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  resumen_semana,
  aes(x = tiempo, y = peso_medio)
) +
  geom_ribbon(
    aes(
      ymin = limite_inferior,
      ymax = limite_superior
    ),
    fill = "steelblue",
    alpha = 0.20
  ) +
  geom_line(color = "steelblue4", linewidth = 0.9) +
  geom_point(color = "steelblue4", size = 1.2) +
  labs(
    title = "Evolución semanal del peso medio",
    subtitle = "La banda representa media ± 1,96 errores estándar",
    x = "Semana de seguimiento",
    y = "Peso medio (kg)"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
consumo_periodo <- datos_b2 |>
  group_by(periodo) |>
  summarise(
    consumo_medio = mean(consumo_kg_dia, na.rm = TRUE),
    consumo_de = sd(consumo_kg_dia, na.rm = TRUE),
    n = sum(!is.na(consumo_kg_dia)),
    .groups = "drop"
  )

consumo_periodo


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  consumo_periodo,
  aes(x = periodo, y = consumo_medio)
) +
  geom_col(fill = "darkseagreen4", width = 0.75) +
  geom_text(
    aes(label = round(consumo_medio, 2)),
    vjust = -0.3,
    size = 3.3
  ) +
  labs(
    title = "Consumo medio de alimento por periodo",
    x = "Periodo de seguimiento",
    y = "Consumo medio (kg/día)"
  ) +
  expand_limits(y = max(consumo_periodo$consumo_medio) * 1.10) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1)
  )


## ------------------------------------------------------------------------------------------------------------------
granjas_mas_observadas <- datos_b2 |>
  count(granja_id, sort = TRUE) |>
  slice_head(n = 10) |>
  pull(granja_id)

resumen_granja <- datos_b2 |>
  filter(granja_id %in% granjas_mas_observadas) |>
  group_by(granja_id) |>
  summarise(
    n_animales = n_distinct(animal_id),
    n_mediciones = sum(!is.na(peso_kg)),
    peso_medio = mean(peso_kg, na.rm = TRUE),
    peso_mediano = median(peso_kg, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(peso_medio))

resumen_granja


## ------------------------------------------------------------------------------------------------------------------
datos_b2 |>
  filter(granja_id %in% granjas_mas_observadas) |>
  ggplot(
    aes(
      x = reorder(granja_id, peso_kg, FUN = median, na.rm = TRUE),
      y = peso_kg,
      fill = granja_id
    )
  ) +
  geom_boxplot(
    outlier.alpha = 0.15,
    na.rm = TRUE
  ) +
  coord_flip() +
  labs(
    title = "Distribución del peso en diez granjas",
    subtitle = "Granjas con mayor número de mediciones",
    x = "Granja",
    y = "Peso (kg)"
  ) +
  guides(fill = "none") +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
# Frecuencias absolutas.
tabla_sexo_salud <- table(
  datos_b2$sexo,
  datos_b2$salud_alta,
  useNA = "ifany"
)

tabla_sexo_salud

# Porcentajes dentro de cada sexo.
round(
  prop.table(tabla_sexo_salud, margin = 1) * 100,
  1
)


## ------------------------------------------------------------------------------------------------------------------
tabla_sexo_salud_larga <- datos_b2 |>
  filter(
    !is.na(sexo),
    !is.na(salud_alta)
  ) |>
  count(sexo, salud_alta, name = "n") |>
  group_by(sexo) |>
  mutate(
    porcentaje = 100 * n / sum(n)
  ) |>
  ungroup()

tabla_sexo_salud_larga


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  tabla_sexo_salud_larga,
  aes(x = sexo, y = porcentaje, fill = salud_alta)
) +
  geom_col(position = "dodge") +
  labs(
    title = "Salud alta según el sexo",
    x = "Sexo",
    y = "Porcentaje dentro de cada sexo",
    fill = "Salud alta"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
tabla_raza_salud <- table(
  datos_b2$raza,
  datos_b2$salud_score_0_4,
  useNA = "ifany"
)

tabla_raza_salud

# Porcentajes dentro de cada raza.
round(
  prop.table(tabla_raza_salud, margin = 1) * 100,
  1
)


## ------------------------------------------------------------------------------------------------------------------
tabla_raza_salud_larga <- datos_b2 |>
  filter(
    !is.na(raza),
    !is.na(salud_score_0_4)
  ) |>
  count(raza, salud_score_0_4, name = "n") |>
  group_by(raza) |>
  mutate(
    porcentaje = 100 * n / sum(n)
  ) |>
  ungroup()

# Barras apiladas al 100 %.
ggplot(
  tabla_raza_salud_larga,
  aes(x = raza, y = porcentaje, fill = salud_score_0_4)
) +
  geom_col() +
  labs(
    title = "Composición de la puntuación de salud por raza",
    x = "Raza",
    y = "Porcentaje dentro de cada raza",
    fill = "Puntuación de salud"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1)
  )


## ------------------------------------------------------------------------------------------------------------------
salud_periodo <- datos_b2 |>
  filter(
    !is.na(periodo),
    !is.na(salud_score_0_4)
  ) |>
  count(periodo, salud_score_0_4, name = "n") |>
  group_by(periodo) |>
  mutate(
    porcentaje = 100 * n / sum(n)
  ) |>
  ungroup()

salud_periodo


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  salud_periodo,
  aes(
    x = periodo,
    y = porcentaje,
    fill = salud_score_0_4
  )
) +
  geom_col() +
  labs(
    title = "Distribución de la salud a lo largo del seguimiento",
    x = "Periodo",
    y = "Porcentaje de mediciones",
    fill = "Puntuación de salud"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1)
  )


## ------------------------------------------------------------------------------------------------------------------
salud_alta_semana <- datos_b2 |>
  filter(!is.na(salud_alta)) |>
  count(tiempo, salud_alta, name = "n") |>
  group_by(tiempo) |>
  mutate(
    porcentaje = 100 * n / sum(n)
  ) |>
  ungroup()

salud_alta_semana


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  salud_alta_semana,
  aes(x = factor(tiempo), y = porcentaje, fill = salud_alta)
) +
  geom_col(width = 0.85) +
  labs(
    title = "Salud alta por semana de seguimiento",
    x = "Semana",
    y = "Porcentaje de mediciones",
    fill = "Salud alta"
  ) +
  scale_x_discrete(
    breaks = as.character(seq(
      min(datos_b2$tiempo, na.rm = TRUE),
      max(datos_b2$tiempo, na.rm = TRUE),
      by = 10
    ))
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  datos_b2,
  aes(x = consumo_kg_dia, y = peso_kg)
) +
  geom_point(
    alpha = 0.15,
    size = 1,
    na.rm = TRUE
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "firebrick",
    linewidth = 0.9,
    na.rm = TRUE
  ) +
  labs(
    title = "Relación entre consumo de alimento y peso",
    subtitle = "La línea representa una tendencia lineal descriptiva",
    x = "Consumo de alimento (kg/día)",
    y = "Peso (kg)"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  datos_b2,
  aes(
    x = consumo_kg_dia,
    y = peso_kg,
    color = sexo
  )
) +
  geom_point(
    alpha = 0.18,
    size = 1,
    na.rm = TRUE
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  labs(
    title = "Consumo y peso según el sexo",
    x = "Consumo de alimento (kg/día)",
    y = "Peso (kg)",
    color = "Sexo"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
variables_cor_b2 <- datos_b2 |>
  select(
    peso_kg,
    consumo_kg_dia,
    consumo_agua_l_dia,
    actividad_h_dia,
    temperatura_C,
    humedad_pct
  )

matriz_cor_b2 <- cor(
  variables_cor_b2,
  use = "pairwise.complete.obs",
  method = "pearson"
)

round(matriz_cor_b2, 2)


## ------------------------------------------------------------------------------------------------------------------
corrplot(
  matriz_cor_b2,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.70
)


## ------------------------------------------------------------------------------------------------------------------
animal_seleccionado <- datos_b2 |>
  filter(!is.na(peso_kg)) |>
  count(animal_id, sort = TRUE) |>
  slice_head(n = 1) |>
  pull(animal_id)

animal_seleccionado


## ------------------------------------------------------------------------------------------------------------------
datos_animal <- datos_b2 |>
  filter(animal_id == animal_seleccionado) |>
  arrange(tiempo)

 datos_animal |>
  select(
    animal_id, granja_id, tiempo,
    peso_kg, consumo_kg_dia,
    salud_score_0_4
  ) |>
  head(10)


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  datos_animal,
  aes(x = tiempo, y = peso_kg)
) +
  geom_line(color = "steelblue4", linewidth = 0.9, na.rm = TRUE) +
  geom_point(color = "steelblue4", size = 2, na.rm = TRUE) +
  labs(
    title = paste(
      "Evolución del peso del animal",
      animal_seleccionado
    ),
    x = "Semana de seguimiento",
    y = "Peso (kg)"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
peso_promedio_semana <- datos_b2 |>
  group_by(tiempo) |>
  summarise(
    peso_promedio = mean(peso_kg, na.rm = TRUE),
    .groups = "drop"
  )

grafico_individual_promedio <- datos_animal |>
  select(tiempo, peso_individual = peso_kg) |>
  full_join(
    peso_promedio_semana,
    by = "tiempo"
  ) |>
  arrange(tiempo)

head(grafico_individual_promedio, 10)


## ------------------------------------------------------------------------------------------------------------------
grafico_individual_promedio |>
  pivot_longer(
    cols = c(peso_individual, peso_promedio),
    names_to = "serie",
    values_to = "peso_kg"
  ) |>
  mutate(
    serie = recode(
      serie,
      peso_individual = paste("Animal", animal_seleccionado),
      peso_promedio = "Promedio semanal"
    )
  ) |>
  ggplot(
    aes(
      x = tiempo,
      y = peso_kg,
      color = serie,
      linetype = serie
    )
  ) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  labs(
    title = "Trayectoria individual y promedio semanal",
    x = "Semana de seguimiento",
    y = "Peso (kg)",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )


## ------------------------------------------------------------------------------------------------------------------
peso_semana_sexo <- datos_b2 |>
  group_by(tiempo, sexo) |>
  summarise(
    peso_medio = mean(peso_kg, na.rm = TRUE),
    n = sum(!is.na(peso_kg)),
    .groups = "drop"
  )

ggplot(
  peso_semana_sexo,
  aes(x = tiempo, y = peso_medio, color = sexo)
) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "Peso medio semanal según el sexo",
    x = "Semana de seguimiento",
    y = "Peso medio (kg)",
    color = "Sexo"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
peso_semana_raza <- datos_b2 |>
  group_by(tiempo, raza) |>
  summarise(
    peso_medio = mean(peso_kg, na.rm = TRUE),
    n = sum(!is.na(peso_kg)),
    .groups = "drop"
  )

ggplot(
  peso_semana_raza,
  aes(x = tiempo, y = peso_medio, color = raza)
) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Peso medio semanal según la raza",
    x = "Semana de seguimiento",
    y = "Peso medio (kg)",
    color = "Raza"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )


## ------------------------------------------------------------------------------------------------------------------
# Paquetes utilizados en este bloque.
library(tidyverse)
library(broom)

# Comprobar que la base preparada en el bloque anterior está disponible.
dim(datos_b2)
glimpse(datos_b2)


## ------------------------------------------------------------------------------------------------------------------
variables_modelo <- datos_b2 |>
  select(
    animal_id,
    granja_id,
    peso_kg,
    consumo_kg_dia,
    edad_inicio_meses,
    sexo,
    raza,
    tiempo
  )

# Revisar la estructura de las variables.
glimpse(variables_modelo)

# Contar valores ausentes.
variables_modelo |>
  summarise(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  )


## ------------------------------------------------------------------------------------------------------------------
datos_modelo <- variables_modelo |>
  drop_na(
    peso_kg,
    consumo_kg_dia,
    edad_inicio_meses,
    sexo,
    raza,
    tiempo
  ) |>
  mutate(
    sexo = droplevels(as.factor(sexo)),
    raza = droplevels(as.factor(raza)),
    tiempo_c = tiempo - mean(tiempo),
    edad_inicio_c = edad_inicio_meses -
      mean(edad_inicio_meses)
  )

# Número de filas y unidades representadas.
datos_modelo |>
  summarise(
    n_filas = n(),
    n_animales = n_distinct(animal_id),
    n_granjas = n_distinct(granja_id)
  )


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  datos_modelo,
  aes(x = consumo_kg_dia, y = peso_kg)
) +
  geom_point(
    alpha = 0.15,
    size = 1,
    color = "grey35"
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = "steelblue4",
    fill = "steelblue",
    linewidth = 0.9
  ) +
  labs(
    title = "Relación entre consumo de alimento y peso",
    subtitle = "Línea de regresión e intervalo de confianza del 95 %",
    x = "Consumo de alimento (kg/día)",
    y = "Peso (kg)"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
cor_consumo_peso <- cor(
  datos_modelo$consumo_kg_dia,
  datos_modelo$peso_kg,
  use = "complete.obs",
  method = "pearson"
)

cor_consumo_peso


## ------------------------------------------------------------------------------------------------------------------
modelo_simple <- lm(
  peso_kg ~ consumo_kg_dia,
  data = datos_modelo
)

summary(modelo_simple)


## ------------------------------------------------------------------------------------------------------------------
coeficientes_simple <- tidy(
  modelo_simple,
  conf.int = TRUE,
  conf.level = 0.95
)

coeficientes_simple


## ------------------------------------------------------------------------------------------------------------------
range(
  datos_modelo$consumo_kg_dia,
  na.rm = TRUE
)


## ------------------------------------------------------------------------------------------------------------------
nuevos_consumos <- tibble(
  consumo_kg_dia = quantile(
    datos_modelo$consumo_kg_dia,
    probs = c(0.25, 0.50, 0.75),
    na.rm = TRUE
  )
)

predicciones_media <- predict(
  modelo_simple,
  newdata = nuevos_consumos,
  interval = "confidence",
  level = 0.95
)

bind_cols(
  nuevos_consumos,
  as.data.frame(predicciones_media)
)


## ------------------------------------------------------------------------------------------------------------------
predicciones_individuales <- predict(
  modelo_simple,
  newdata = nuevos_consumos,
  interval = "prediction",
  level = 0.95
)

bind_cols(
  nuevos_consumos,
  as.data.frame(predicciones_individuales)
)


## ------------------------------------------------------------------------------------------------------------------
ajuste_simple <- glance(modelo_simple)

ajuste_simple |>
  select(
    nobs,
    r.squared,
    adj.r.squared,
    sigma,
    statistic,
    p.value
  )


## ------------------------------------------------------------------------------------------------------------------
c(
  R2_modelo = summary(modelo_simple)$r.squared,
  correlacion_al_cuadrado = cor_consumo_peso^2
)


## ------------------------------------------------------------------------------------------------------------------
modelo_consumo_edad <- lm(
  peso_kg ~ consumo_kg_dia + edad_inicio_c,
  data = datos_modelo
)

summary(modelo_consumo_edad)


## ------------------------------------------------------------------------------------------------------------------
tidy(
  modelo_consumo_edad,
  conf.int = TRUE,
  conf.level = 0.95
)


## ------------------------------------------------------------------------------------------------------------------
comparacion_consumo <- bind_rows(
  tidy(modelo_simple, conf.int = TRUE) |>
    filter(term == "consumo_kg_dia") |>
    mutate(modelo = "Consumo"),
  tidy(modelo_consumo_edad, conf.int = TRUE) |>
    filter(term == "consumo_kg_dia") |>
    mutate(modelo = "Consumo + edad")
) |>
  select(
    modelo,
    term,
    estimate,
    std.error,
    conf.low,
    conf.high,
    p.value
  )

comparacion_consumo


## ------------------------------------------------------------------------------------------------------------------
levels(datos_modelo$sexo)
levels(datos_modelo$raza)

contrasts(datos_modelo$sexo)
contrasts(datos_modelo$raza)


## ------------------------------------------------------------------------------------------------------------------
# Consultar primero los niveles reales antes de cambiar la referencia.
# Sustituir "categoria_referencia" por un nivel existente si se desea.

# datos_modelo <- datos_modelo |>
#   mutate(
#     sexo = relevel(sexo, ref = "categoria_referencia"),
#     raza = relevel(raza, ref = "categoria_referencia")
#   )


## ------------------------------------------------------------------------------------------------------------------
modelo_individual <- lm(
  peso_kg ~
    consumo_kg_dia +
    edad_inicio_c +
    sexo +
    raza,
  data = datos_modelo
)

summary(modelo_individual)


## ------------------------------------------------------------------------------------------------------------------
coeficientes_individual <- tidy(
  modelo_individual,
  conf.int = TRUE,
  conf.level = 0.95
)

coeficientes_individual


## ------------------------------------------------------------------------------------------------------------------
modelo_multiple <- lm(
  peso_kg ~
    consumo_kg_dia +
    edad_inicio_c +
    sexo +
    raza +
    tiempo_c,
  data = datos_modelo
)

summary(modelo_multiple)


## ------------------------------------------------------------------------------------------------------------------
coeficientes_multiple <- tidy(
  modelo_multiple,
  conf.int = TRUE,
  conf.level = 0.95
)

coeficientes_multiple


## ------------------------------------------------------------------------------------------------------------------
datos_modelo <- datos_modelo |>
  mutate(
    consumo_c = consumo_kg_dia -
      mean(consumo_kg_dia)
  )

modelo_multiple_centrado <- lm(
  peso_kg ~
    consumo_c +
    edad_inicio_c +
    sexo +
    raza +
    tiempo_c,
  data = datos_modelo
)

tidy(
  modelo_multiple_centrado,
  conf.int = TRUE
)


## ------------------------------------------------------------------------------------------------------------------
c(
  pendiente_original = coef(modelo_multiple)["consumo_kg_dia"],
  pendiente_centrada = coef(modelo_multiple_centrado)["consumo_c"],
  R2_original = summary(modelo_multiple)$r.squared,
  R2_centrado = summary(modelo_multiple_centrado)$r.squared
)


## ------------------------------------------------------------------------------------------------------------------
coeficientes_grafico <- tidy(
  modelo_multiple_centrado,
  conf.int = TRUE,
  conf.level = 0.95
) |>
  filter(term != "(Intercept)") |>
  mutate(
    term = forcats::fct_reorder(term, estimate)
  )

ggplot(
  coeficientes_grafico,
  aes(x = estimate, y = term)
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey45"
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.20,
    color = "steelblue4"
  ) +
  geom_point(
    size = 2.5,
    color = "steelblue4"
  ) +
  labs(
    title = "Coeficientes del modelo múltiple",
    subtitle = "Estimaciones e intervalos de confianza del 95 %",
    x = "Coeficiente estimado",
    y = "Variable"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
comparacion_modelos <- bind_rows(
  glance(modelo_simple) |>
    mutate(modelo = "Consumo"),
  glance(modelo_consumo_edad) |>
    mutate(modelo = "Consumo + edad"),
  glance(modelo_individual) |>
    mutate(modelo = "Consumo + edad + sexo + raza"),
  glance(modelo_multiple) |>
    mutate(modelo = "Modelo múltiple + semana")
) |>
  select(
    modelo,
    nobs,
    r.squared,
    adj.r.squared,
    sigma,
    AIC,
    BIC
  )

comparacion_modelos


## ------------------------------------------------------------------------------------------------------------------
comparacion_modelos |>
  pivot_longer(
    cols = c(r.squared, adj.r.squared),
    names_to = "medida",
    values_to = "valor"
  ) |>
  mutate(
    medida = recode(
      medida,
      r.squared = "R²",
      adj.r.squared = "R² ajustado"
    )
  ) |>
  ggplot(
    aes(x = modelo, y = valor, fill = medida)
  ) +
  geom_col(position = "dodge") +
  labs(
    title = "Capacidad descriptiva de los modelos",
    x = "Modelo",
    y = "Coeficiente de determinación",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = "bottom"
  )


## ------------------------------------------------------------------------------------------------------------------
diagnostico_multiple <- augment(modelo_multiple_centrado)

head(
  diagnostico_multiple |>
    select(
      peso_kg,
      .fitted,
      .resid,
      .std.resid,
      .hat,
      .cooksd
    ),
  10
)


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  diagnostico_multiple,
  aes(x = .fitted, y = peso_kg)
) +
  geom_point(
    alpha = 0.18,
    size = 1,
    color = "grey35"
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "firebrick",
    linewidth = 0.9,
    linetype = "dashed"
  ) +
  labs(
    title = "Peso observado y peso ajustado",
    subtitle = "La línea discontinua representa la igualdad perfecta",
    x = "Peso ajustado (kg)",
    y = "Peso observado (kg)"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  diagnostico_multiple,
  aes(x = .fitted, y = .resid)
) +
  geom_point(
    alpha = 0.18,
    size = 1,
    color = "grey35"
  ) +
  geom_hline(
    yintercept = 0,
    color = "firebrick",
    linetype = "dashed"
  ) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    color = "steelblue4",
    linewidth = 0.9
  ) +
  labs(
    title = "Residuos frente a valores ajustados",
    x = "Peso ajustado (kg)",
    y = "Residuo (kg)"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  diagnostico_multiple,
  aes(
    x = .fitted,
    y = sqrt(abs(.std.resid))
  )
) +
  geom_point(
    alpha = 0.18,
    size = 1,
    color = "grey35"
  ) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    color = "firebrick",
    linewidth = 0.9
  ) +
  labs(
    title = "Dispersión de los residuos",
    subtitle = "Gráfico de escala-localización",
    x = "Peso ajustado (kg)",
    y = "Raíz de |residuo estandarizado|"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  diagnostico_multiple,
  aes(x = .resid)
) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribución de los residuos",
    x = "Residuo (kg)",
    y = "Número de mediciones"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  diagnostico_multiple,
  aes(sample = .std.resid)
) +
  stat_qq(alpha = 0.25, size = 1) +
  stat_qq_line(
    color = "firebrick",
    linewidth = 0.9
  ) +
  labs(
    title = "Gráfico Q-Q de los residuos estandarizados",
    x = "Cuantiles teóricos",
    y = "Cuantiles observados"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
diagnostico_multiple |>
  mutate(
    fila_modelo = row_number()
  ) |>
  arrange(desc(.cooksd)) |>
  select(
    fila_modelo,
    peso_kg,
    consumo_c,
    edad_inicio_c,
    tiempo_c,
    .fitted,
    .std.resid,
    .hat,
    .cooksd
  ) |>
  slice_head(n = 10)


## ------------------------------------------------------------------------------------------------------------------
diagnostico_multiple |>
  mutate(
    fila_modelo = row_number()
  ) |>
  ggplot(
    aes(x = fila_modelo, y = .cooksd)
  ) +
  geom_col(fill = "steelblue4") +
  labs(
    title = "Distancia de Cook por observación",
    x = "Fila utilizada en el modelo",
    y = "Distancia de Cook"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  diagnostico_multiple,
  aes(x = sexo, y = .resid, fill = sexo)
) +
  geom_boxplot(
    outlier.alpha = 0.20
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "firebrick"
  ) +
  labs(
    title = "Residuos según el sexo",
    x = "Sexo",
    y = "Residuo (kg)"
  ) +
  guides(fill = "none") +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  diagnostico_multiple,
  aes(x = raza, y = .resid, fill = raza)
) +
  geom_boxplot(
    outlier.alpha = 0.15
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "firebrick"
  ) +
  labs(
    title = "Residuos según la raza",
    x = "Raza",
    y = "Residuo (kg)"
  ) +
  guides(fill = "none") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1)
  )


## ------------------------------------------------------------------------------------------------------------------
modelo_tiempo_cuadratico <- lm(
  peso_kg ~
    consumo_c +
    edad_inicio_c +
    sexo +
    raza +
    tiempo_c +
    I(tiempo_c^2),
  data = datos_modelo
)

summary(modelo_tiempo_cuadratico)


## ------------------------------------------------------------------------------------------------------------------
bind_rows(
  glance(modelo_multiple_centrado) |>
    mutate(modelo = "Tiempo lineal"),
  glance(modelo_tiempo_cuadratico) |>
    mutate(modelo = "Tiempo lineal y cuadrático")
) |>
  select(
    modelo,
    r.squared,
    adj.r.squared,
    sigma,
    AIC,
    BIC
  )


## ------------------------------------------------------------------------------------------------------------------
datos_modelo |>
  summarise(
    filas = n(),
    animales = n_distinct(animal_id),
    granjas = n_distinct(granja_id),
    mediciones_medias_por_animal = n() /
      n_distinct(animal_id),
    mediciones_medias_por_granja = n() /
      n_distinct(granja_id)
  )


## ------------------------------------------------------------------------------------------------------------------
animales_regresion <- datos_modelo |>
  distinct(animal_id) |>
  slice_head(n = 8)

datos_animales_regresion <- datos_modelo |>
  semi_join(
    animales_regresion,
    by = "animal_id"
  )

ggplot(
  datos_animales_regresion,
  aes(
    x = tiempo,
    y = peso_kg,
    group = animal_id,
    color = animal_id
  )
) +
  geom_line(alpha = 0.75) +
  geom_point(size = 1.2, alpha = 0.75) +
  geom_smooth(
    aes(group = 1),
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    color = "black",
    linewidth = 1.1,
    linetype = "dashed"
  ) +
  labs(
    title = "Trayectorias individuales y tendencia lineal general",
    x = "Semana de seguimiento",
    y = "Peso (kg)",
    color = "Animal"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )


## ------------------------------------------------------------------------------------------------------------------
tabla_modelo_final <- tidy(
  modelo_multiple_centrado,
  conf.int = TRUE,
  conf.level = 0.95
) |>
  mutate(
    across(
      c(estimate, std.error, conf.low, conf.high, p.value),
      ~ round(.x, 3)
    )
  )

tabla_modelo_final


## ------------------------------------------------------------------------------------------------------------------
resumen_modelo_final <- glance(
  modelo_multiple_centrado
) |>
  transmute(
    n_observaciones = nobs,
    R2 = round(r.squared, 3),
    R2_ajustado = round(adj.r.squared, 3),
    error_residual = round(sigma, 3),
    AIC = round(AIC, 1),
    BIC = round(BIC, 1)
  )

resumen_modelo_final

