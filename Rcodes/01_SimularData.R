
# ============================================================
# CURSO: ANALISIS DE DATOS ESPACIO-TEMPORALES EN R
# Ciencia animal: borregos en Hidalgo, Mexico
# TODOS LOS DATOS SON SINTETICOS
# ============================================================

# 0. CONFIGURACION --------------------------------------------

set.seed(20261015)

# Instalar una sola vez si es necesario
# install.packages(c("dplyr", "tidyr", "ggplot2",
#                    "openxlsx", "spdep"))

library(tidyverse)
library(ggplot2)
library(openxlsx)
library(spdep)

# Dimensiones
n_granjas <- 50
n_animales_granja <- 10
n_tiempos <- 100
n_razas <- 5

# Identificadores
ids_granjas <- sprintf("G%02d", 1:n_granjas)
ids_animales <- sprintf("A%03d", 1:(n_granjas *
                                      n_animales_granja))

razas <- c(
  "Raza_A", "Raza_B", "Raza_C", "Raza_D", "Raza_E"
)

# ============================================================
# 1. INFORMACION ESPACIAL DE LAS GRANJAS
# ============================================================

# Area aproximada alrededor de Hidalgo, Mexico.
# Son coordenadas simuladas, NO localizaciones reales.

granjas <- data.frame(
  granja_id = ids_granjas,
  longitud = runif(n_granjas, -99.20, -98.30),
  latitud = runif(n_granjas, 19.65, 20.55),
  tamano_granja = sample(
    c("Pequena", "Mediana", "Grande"),
    n_granjas, replace = TRUE,
    prob = c(0.35, 0.45, 0.20)
  ),
  sistema_produccion = sample(
    c("Extensivo", "Semi-intensivo", "Intensivo"),
    n_granjas, replace = TRUE,
    prob = c(0.40, 0.40, 0.20)
  )
)

granjas$zona <- cut(
  granjas$latitud,
  breaks = c(-Inf, 19.95, 20.25, Inf),
  labels = c("Sur", "Centro", "Norte")
)

# Efecto propio de cada granja
granjas$efecto_granja <- rnorm(n_granjas, 0, 2)

# Distancias geograficas aproximadas en km
coords <- as.matrix(granjas[, c("longitud", "latitud")])

distancias <- as.matrix(
  dist(coords)
)

# Correccion: transformar grados a distancias aproximadas en km
# En esta zona, 1 grado de latitud ~111 km.
# La longitud se multiplica por cos(latitud).
lat_media <- mean(granjas$latitud) * pi / 180

x_km <- granjas$longitud * 111.32 * cos(lat_media)
y_km <- granjas$latitud * 111.32

coords_km <- cbind(x_km, y_km)
distancias_km <- as.matrix(dist(coords_km))

# Matriz de vecindad: los 4 vecinos mas cercanos.
# Se usa union simetrica para que i sea vecino de j
# si i o j seleccionan al otro.

vecinos <- vector("list", n_granjas)

for (i in seq_len(n_granjas)) {
  orden <- order(distancias_km[i, ])
  vecinos[[i]] <- orden[2:5]
}

W_binaria <- matrix(
  0, nrow = n_granjas, ncol = n_granjas,
  dimnames = list(ids_granjas, ids_granjas)
)

for (i in seq_len(n_granjas)) {
  W_binaria[i, vecinos[[i]]] <- 1
}

W_binaria <- 1 * ((W_binaria + t(W_binaria)) > 0)
diag(W_binaria) <- 0

# Estandarizacion por filas
W <- W_binaria / rowSums(W_binaria)

rownames(W) <- ids_granjas
colnames(W) <- ids_granjas

# Comprobacion
stopifnot(all(diag(W) == 0))
stopifnot(all(abs(rowSums(W) - 1) < 1e-8))

# ============================================================
# 2. INFORMACION INDIVIDUAL DE LOS ANIMALES
# ============================================================

animales <- data.frame(
  animal_id = ids_animales,
  granja_id = rep(ids_granjas,
                  each = n_animales_granja),
  sexo = sample(
    c("Hembra", "Macho"),
    n_granjas * n_animales_granja,
    replace = TRUE
  ),
  edad_inicio_meses = sample(
    6:24,
    n_granjas * n_animales_granja,
    replace = TRUE
  ),
  raza = sample(
    razas,
    n_granjas * n_animales_granja,
    replace = TRUE
  )
)

animales <- animales %>%
  left_join(
    granjas %>%
      select(granja_id, longitud, latitud,
             zona, tamano_granja, sistema_produccion,
             efecto_granja),
    by = "granja_id"
  )

# Efectos aleatorios individuales
animales$efecto_animal <- rnorm(
  nrow(animales), 0, 2.5
)

# Efectos de raza para el peso inicial
efecto_raza_peso <- c(
  Raza_A = 0,
  Raza_B = 1.5,
  Raza_C = -1,
  Raza_D = 2.2,
  Raza_E = 0.8
)

animales$peso_inicial <- (
  18 +
    0.65 * animales$edad_inicio_meses +
    1.2 * (animales$sexo == "Macho") +
    efecto_raza_peso[animales$raza] +
    animales$efecto_granja +
    animales$efecto_animal +
    rnorm(nrow(animales), 0, 1)
)

# ============================================================
# 3. CREAR EL PANEL LONGITUDINAL
# ============================================================

datos <- expand_grid(
  animal_id = animales$animal_id,
  tiempo = seq_len(n_tiempos)
) %>%
  left_join(animales, by = "animal_id") %>%
  mutate(
    fecha = as.Date("2026-01-05") +
      7 * (tiempo - 1)
  ) %>%
  arrange(animal_id, tiempo)

# Variables ambientales compartidas por granja y tiempo.
# La estacionalidad es una señal artificial para docencia.

ambiente <- expand_grid(
  granja_id = ids_granjas,
  tiempo = seq_len(n_tiempos)
) %>%
  left_join(
    granjas %>%
      select(granja_id, latitud, longitud),
    by = "granja_id"
  ) %>%
  mutate(
    temperatura_C =
      18 +
      5 * sin(2 * pi * tiempo / 52) +
      0.8 * (latitud - 20) +
      rnorm(n(), 0, 1.5),
    
    humedad_pct = pmin(
      95, pmax(
        20,
        60 - 0.7 * (temperatura_C - 18) +
          rnorm(n(), 0, 7)
      )
    ),
    
    precipitacion_mm = pmax(
      0,
      4 + 3 * sin(2 * pi * (tiempo + 12) / 52) +
        rnorm(n(), 0, 4)
    )
  ) %>%
  select(
    granja_id, tiempo,
    temperatura_C, humedad_pct, precipitacion_mm
  )

datos <- datos %>%
  left_join(ambiente, by = c("granja_id", "tiempo"))

# ============================================================
# 4. VARIABLES PRODUCTIVAS Y DE SALUD
# ============================================================

# Diferencias sistematicas entre razas
efecto_raza_consumo <- c(
  Raza_A = 0.00,
  Raza_B = 0.08,
  Raza_C = -0.05,
  Raza_D = 0.12,
  Raza_E = 0.04
)

efecto_raza_ganancia <- c(
  Raza_A = 0.00,
  Raza_B = 0.025,
  Raza_C = -0.015,
  Raza_D = 0.035,
  Raza_E = 0.015
)

datos <- datos %>%
  mutate(
    # Consumo diario de alimento en kg
    consumo_kg_dia = pmax(
      0.2,
      0.65 +
        0.018 * peso_inicial +
        efecto_raza_consumo[raza] +
        0.025 * (temperatura_C - 18) +
        rnorm(n(), 0, 0.12)
    ),
    
    # Consumo diario de agua en litros
    consumo_agua_l_dia = pmax(
      1,
      2.5 + 1.5 * consumo_kg_dia +
        0.15 * (temperatura_C - 18) +
        rnorm(n(), 0, 0.45)
    ),
    
    # Salud: puntuacion sintetica ordinal 0-4
    salud_latente =
      1.4 +
      0.12 * (temperatura_C - 18) -
      0.25 * (consumo_kg_dia - 1) +
      rnorm(n(), 0, 0.9),
    
    salud_score_0_4 = pmin(
      4, pmax(0, round(salud_latente))
    ),
    
    salud_alta = as.integer(salud_score_0_4 >= 3),
    
    # Actividad estimada en horas al dia
    actividad_h_dia = pmax(
      0,
      6 - 0.18 * (temperatura_C - 18) -
        0.3 * salud_score_0_4 +
        rnorm(n(), 0, 0.7)
    ),
    
    # Ganancia semanal media en kg
    ganancia_semanal_kg = pmax(
      -0.3,
      0.12 +
        0.10 * consumo_kg_dia +
        efecto_raza_ganancia[raza] -
        0.025 * salud_score_0_4 +
        rnorm(n(), 0, 0.09)
    )
  ) %>%
  group_by(animal_id) %>%
  arrange(tiempo, .by_group = TRUE) %>%
  mutate(
    # Peso longitudinal: acumula la ganancia en el tiempo.
    # Se incluye ruido temporal para crear variacion individual.
    peso_kg = first(peso_inicial) +
      cumsum(ganancia_semanal_kg) +
      cumsum(rnorm(n(), 0, 0.03))
  ) %>%
  ungroup() %>%
  mutate(
    produccion_lana_kg = pmax(
      0,
      0.04 + 0.002 * peso_kg +
        rnorm(n(), 0, 0.015)
    ),
    
    n_visitas_veterinarias = rpois(
      n(),
      lambda = pmax(
        0.03,
        0.08 + 0.08 * salud_score_0_4
      )
    ),
    
    tratamiento = ifelse(
      salud_score_0_4 >= 3 & runif(n()) < 0.35,
      "Si", "No"
    )
  ) %>%
  select(-salud_latente)

# Faltantes artificiales en algunas mediciones.
# Los identificadores, tiempos y coordenadas se conservan.
for (v in c(
  "peso_kg", "consumo_kg_dia",
  "consumo_agua_l_dia", "actividad_h_dia"
)) {
  datos[[v]][runif(nrow(datos)) < 0.015] <- NA
}

# ============================================================
# 5. TABLA AGREGADA POR GRANJA Y TIEMPO
# ============================================================

granja_tiempo <- datos %>%
  group_by(granja_id, tiempo, fecha) %>%
  summarise(
    peso_medio_kg = mean(peso_kg, na.rm = TRUE),
    ganancia_media_kg = mean(
      ganancia_semanal_kg, na.rm = TRUE
    ),
    consumo_medio_kg_dia = mean(
      consumo_kg_dia, na.rm = TRUE
    ),
    agua_media_l_dia = mean(
      consumo_agua_l_dia, na.rm = TRUE
    ),
    temperatura_media_C = mean(
      temperatura_C, na.rm = TRUE
    ),
    humedad_media_pct = mean(
      humedad_pct, na.rm = TRUE
    ),
    precipitacion_mm = mean(
      precipitacion_mm, na.rm = TRUE
    ),
    proporcion_salud_alta = mean(
      salud_alta, na.rm = TRUE
    ),
    produccion_lana_media_kg = mean(
      produccion_lana_kg, na.rm = TRUE
    ),
    n_animales_observados = sum(!is.na(peso_kg)),
    .groups = "drop"
  ) %>%
  left_join(
    granjas %>%
      select(granja_id, latitud, longitud, zona,
             tamano_granja, sistema_produccion),
    by = "granja_id"
  )

# ============================================================
# 6. DICCIONARIO DE VARIABLES
# ============================================================

diccionario <- data.frame(
  variable = c(
    "animal_id", "granja_id", "tiempo", "fecha",
    "latitud", "longitud", "raza", "sexo",
    "edad_inicio_meses", "zona", "tamano_granja",
    "sistema_produccion", "temperatura_C", "humedad_pct",
    "precipitacion_mm", "peso_kg", "peso_inicial",
    "ganancia_semanal_kg", "consumo_kg_dia",
    "consumo_agua_l_dia", "actividad_h_dia",
    "salud_score_0_4", "salud_alta",
    "produccion_lana_kg", "n_visitas_veterinarias",
    "tratamiento", "efecto_granja", "efecto_animal",
    "W_vecindad"
  ),
  descripcion = c(
    "Identificador unico sintetico del animal",
    "Identificador unico sintetico de la granja",
    "Periodo semanal de seguimiento, 1 a 100",
    "Fecha semanal simulada",
    "Latitud decimal sintetica en Hidalgo, Mexico",
    "Longitud decimal sintetica en Hidalgo, Mexico",
    "Una de cinco razas sinteticas",
    "Sexo simulado del animal",
    "Edad al inicio en meses",
    "Zona definida a partir de la latitud",
    "Categoria sintetica de tamano de granja",
    "Sistema de produccion sintetico",
    "Temperatura ambiental simulada en grados C",
    "Humedad relativa simulada en porcentaje",
    "Precipitacion simulada en mm por periodo",
    "Peso longitudinal simulado en kg",
    "Peso inicial simulado en kg",
    "Ganancia semanal simulada en kg",
    "Consumo diario de alimento en kg",
    "Consumo diario de agua en litros",
    "Actividad diaria simulada en horas",
    "Puntuacion ordinal sintetica de salud, 0-4",
    "Indicador binario derivado de salud_score_0_4",
    "Produccion sintetica de lana en kg por periodo",
    "Numero simulado de visitas veterinarias",
    "Indicador sintetico de tratamiento",
    "Efecto aleatorio usado para generar datos",
    "Efecto individual usado para generar datos",
    "Matriz espacial de pesos estandarizada por filas"
  ),
  tipo = c(
    "Nominal", "Nominal", "Entera", "Fecha",
    "Continua", "Continua", "Nominal", "Nominal",
    "Entera", "Nominal", "Nominal", "Nominal",
    "Continua", "Continua", "Continua", "Continua",
    "Continua", "Continua", "Continua", "Continua",
    "Continua", "Ordinal", "Binaria", "Continua",
    "Conteo", "Binaria", "Continua latente",
    "Continua latente", "Matriz"
  ),
  uso_docente = c(
    "Unidad individual", "Agrupacion espacial",
    "Eje temporal", "Eje temporal",
    "Mapa y analisis espacial", "Mapa y analisis espacial",
    "Comparacion entre razas", "Covariable",
    "Covariable basal", "Comparacion territorial",
    "Covariable de granja", "Covariable de granja",
    "Covariable ambiental", "Covariable ambiental",
    "Covariable ambiental", "Regresion y modelos mixtos",
    "Respuesta basal", "Respuesta alternativa",
    "Covariable productiva", "Covariable productiva",
    "Covariable conductual", "Descriptiva/modelos ordinales",
    "GLMM logistico", "Respuesta alternativa",
    "GLMM Poisson", "Covariable binaria",
    "No usar como predictor observado real",
    "No usar como predictor observado real",
    "Modelos SAR"
  )
)

# ============================================================
# 7. EXPORTAR TODO A EXCEL
# ============================================================

# Matriz de distancias
distancias_df <- data.frame(
  granja_id = rownames(distancias_km),
  as.data.frame(distancias_km),
  check.names = FALSE
)

W_df <- data.frame(
  granja_id = rownames(W),
  as.data.frame(W),
  check.names = FALSE
)

# Tabla de lectura
leeme <- data.frame(
  informacion = c(
    "Todos los datos son sinteticos y se han generado en R.",
    "Las coordenadas son aproximadas a una zona de Hidalgo, Mexico.",
    "Las coordenadas no identifican granjas reales.",
    "50 granjas, 10 animales por granja y 100 tiempos semanales.",
    "La tabla datos_long contiene una fila por animal y tiempo.",
    "La tabla granja_tiempo contiene medias por granja y tiempo.",
    "La tabla W_vecindad contiene pesos espaciales estandarizados.",
    "Las variables de efecto_granja y efecto_animal se incluyen para explicar la simulacion; no son observaciones reales.",
    "Los datos no deben utilizarse para conclusiones veterinarias reales."
  )
)

archivo <- "base_borregos_espaciotemporal_Hidalgo.xlsx"

write.xlsx(
  list(
    datos_long = datos,
    granja_tiempo = granja_tiempo,
    animales = animales,
    granjas = granjas,
    W_vecindad = W_df,
    distancias_km = distancias_df,
    diccionario = diccionario,
    LEEME = leeme
  ),
  file = archivo,
  overwrite = TRUE
)

cat("Archivo creado:", archivo, "\n")
cat("Granjas:", n_distinct(datos$granja_id), "\n")
cat("Animales:", n_distinct(datos$animal_id), "\n")
cat("Tiempos:", n_distinct(datos$tiempo), "\n")
cat("Filas longitudinales:", nrow(datos), "\n")
cat("Razas:", n_distinct(datos$raza), "\n")



library(sf)
library(geodata)
library(terra)


head(granjas[, c("granja_id", "longitud", "latitud")])

granjas_sf <- st_as_sf(
  granjas,
  coords = c("longitud", "latitud"),
  crs = 4326,
  remove = FALSE
)


# Descargar los límites geográficos
mexico <- geodata::gadm(
  country = "MEX",
  level = 0,
  path = "mapas"
)

estados <- geodata::gadm(
  country = "MEX",
  level = 1,
  path = "mapas"
)

# Convertir a objetos sf
mexico_sf <- st_as_sf(mexico)
estados_sf <- st_as_sf(estados)

# Comprobar los nombres de los estados
estados_sf$NAME_1

# Seleccionar Hidalgo -- México -- "Tlaxcala" -- "Veracruz"
hidalgo_sf <- estados_sf %>%
  filter(NAME_1 %in%c("Hidalgo", "México","Tlaxcala","Veracruz"))



ggplot() +
  geom_sf(data = mexico_sf,fill = "grey95",color = "grey55",linewidth = 0.3) +
  geom_sf(data = hidalgo_sf,fill="lightblue",color="steelblue4",linewidth = 0.7) +
  geom_sf(data = granjas_sf,aes(color = tamano_granja),size = 2.5,alpha = 0.9) +
  scale_color_brewer( palette = "Dark2",name = "Tamaño de granja") +
  labs( title = "Granjas ovinas simuladas en Hidalgo, México",
    subtitle = "Ubicación sintética de 50 granjas", x="Longitud",y="Latitud",
    caption = "Datos simulados; no representan granjas reales.") +
  theme_minimal()


delta = 0.4
ggplot() +
  geom_sf(data = hidalgo_sf,fill = "grey95",color = "grey45") +
  geom_sf(data = granjas_sf,aes(color=zona,shape=sistema_produccion),size=3) +
  coord_sf(xlim = range(granjas$longitud) + c(-delta, delta),
    ylim = range(granjas$latitud) + c(-delta, delta),expand = FALSE) +
  labs(title = "Distribución espacial de las granjas",
    color = "Zona", shape = "Sistema productivo",x = "Longitud",
    y = "Latitud") +
  theme_minimal()
