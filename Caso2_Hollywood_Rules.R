# CASO HOLLYWOOD RULES
# Analítica de Negocios

library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)

Hollywood <- read_excel(
  "Hollywood.xls",
  sheet = "Exhibit 1"
)
# Verificación de la base
dim(Hollywood)
names(Hollywood)
# Crear carpeta de resultados si no existe
dir.create(
  "outputs",
  showWarnings = FALSE
)
list.files()
dim(Hollywood)

# ============================================================
# PUNTO 1
# ============================================================

# Tabla de estadisticos descriptivos
tabla_descriptivos_p1 <- data.frame(
  
  Variable = c(
    "Opening Gross",
    "Total U.S. Gross",
    "Total Non-U.S. Gross",
    "Opening Theatres"
  ),
  
  Minimo = c(
    min(Hollywood$`Opening Gross`, na.rm = TRUE),
    min(Hollywood$`Total U.S. Gross`, na.rm = TRUE),
    min(Hollywood$`Total Non-U.S. Gross`, na.rm = TRUE),
    min(Hollywood$`Opening Theatres`, na.rm = TRUE)
  ),
  
  Promedio = c(
    mean(Hollywood$`Opening Gross`, na.rm = TRUE),
    mean(Hollywood$`Total U.S. Gross`, na.rm = TRUE),
    mean(Hollywood$`Total Non-U.S. Gross`, na.rm = TRUE),
    mean(Hollywood$`Opening Theatres`, na.rm = TRUE)
  ),
  
  Maximo = c(
    max(Hollywood$`Opening Gross`, na.rm = TRUE),
    max(Hollywood$`Total U.S. Gross`, na.rm = TRUE),
    max(Hollywood$`Total Non-U.S. Gross`, na.rm = TRUE),
    max(Hollywood$`Opening Theatres`, na.rm = TRUE)
  )
)

tabla_conteos_p1 <- data.frame(
  Categoria = c(
    "Peliculas de comedia",
    "Peliculas clasificacion R"
  ),
  Cantidad = c(
    sum(Hollywood$Genre == "Comedy", na.rm = TRUE),
    sum(Hollywood$MPAA == "R", na.rm = TRUE)
  )
)
tabla_conteos_p1
View(tabla_conteos_p1)

library(gridExtra)
tabla_grob_p1 <- gridExtra::tableGrob(
  tabla_descriptivos_p1,
  rows = NULL
)
exists("tabla_grob_p1")
ggplot2::ggsave(
  "tabla_descriptivos_punto1.png",
  plot = tabla_grob_p1,
  width = 9,
  height = 4,
  dpi = 300
)
tabla_grob_conteos_p1 <- gridExtra::tableGrob(
  tabla_conteos_p1,
  rows = NULL
)


ggplot2::ggsave(
  "tabla_conteos_punto1.png",
  plot = tabla_grob_conteos_p1,
  width = 6,
  height = 3,
  dpi = 300
)

# PUNTO 2
# Crear la variable ROI en Estados Unidos

Hollywood <- Hollywood |>
  mutate(
    ROI_US = (`Total U.S. Gross` - Budget) / Budget
  )
# Promedio del ROI
mean(Hollywood$ROI_US, na.rm = TRUE)
# Intervalo de confianza del 95%
ic_roi <- t.test(
  Hollywood$ROI_US,
  conf.level = 0.95
)
ic_roi
# Prueba de hipotesis:
# H0: ROI promedio = 12%
# H1: ROI promedio > 12%

prueba_roi <- t.test(
  Hollywood$ROI_US,
  mu = 0.12,
  alternative = "greater"
)
prueba_roi

# Tabla resumen del punto 2

tabla_resultados_p2 <- data.frame(
  Medida = c(
    "ROI promedio",
    "Limite inferior IC 95%",
    "Limite superior IC 95%",
    "p-value prueba ROI > 12%"
  ),
  Resultado = c(
    sprintf("%.2f%%", mean(Hollywood$ROI_US, na.rm = TRUE) * 100),
    sprintf("%.2f%%", ic_roi$conf.int[1] * 100),
    sprintf("%.2f%%", ic_roi$conf.int[2] * 100),
    sprintf("%.4f", prueba_roi$p.value)
  )
)

tabla_resultados_p2
View(tabla_resultados_p2)
tabla_grob_p2 <- gridExtra::tableGrob(
  tabla_resultados_p2,
  rows = NULL
)


ggplot2::ggsave(
  "tabla_resultados_punto2.png",
  plot = tabla_grob_p2,
  width = 8,
  height = 4,
  dpi = 300
)
file.exists("tabla_resultados_punto2.png")

# ============================================================
# PUNTO 3
# Comparacion entre peliculas de comedia y otros generos
# ============================================================

# PUNTO 3a
# Comparacion del Total U.S. Gross
# Crear grupos: Comedy y Other
Hollywood <- Hollywood |>
  mutate(
    Tipo_Genero = ifelse(Genre == "Comedy", "Comedy", "Other")
  )

# Verificar cuantas peliculas hay en cada grupo
table(Hollywood$Tipo_Genero)
Hollywood <- readxl::read_excel(
  "Hollywood.xls",
  sheet = "Exhibit 1"
)
dim(Hollywood)
names(Hollywood)
Hollywood <- Hollywood |>
  dplyr::mutate(
    Tipo_Genero = ifelse(Genre == "Comedy", "Comedy", "Other")
  )

table(Hollywood$Tipo_Genero)

# Tabla descriptiva del punto 3a
tabla_3a <- Hollywood |>
  group_by(Tipo_Genero) |>
  summarise(
    n = sum(!is.na(`Total U.S. Gross`)),
    Promedio_US_Gross = mean(`Total U.S. Gross`, na.rm = TRUE),
    Desviacion_US_Gross = sd(`Total U.S. Gross`, na.rm = TRUE),
    .groups = "drop"
  )

# Convertir los resultados a millones de dolares
tabla_3a_bonita <- tabla_3a |>
  mutate(
    Promedio_US_Gross = round(Promedio_US_Gross / 1000000, 2),
    Desviacion_US_Gross = round(Desviacion_US_Gross / 1000000, 2)
  )

tabla_3a_bonita

# Guardar tabla del punto 3a como imagen PNG

library(gridExtra)
library(grid)

tabla_imagen_3a <- tabla_3a_bonita

colnames(tabla_imagen_3a) <- c(
  "Género",
  "n",
  "Promedio US Gross",
  "Desviación estándar"
)

tema_3a <- ttheme_minimal(
  base_size = 13,
  core = list(
    fg_params = list(col = "black"),
    bg_params = list(
      fill = c("#FFFFFF", "#E6E6E6"),
      col = NA
    )
  ),
  colhead = list(
    fg_params = list(fontface = "bold", col = "black"),
    bg_params = list(fill = "#BFBFBF", col = NA)
  )
)

png(
  filename = "tabla_resultados_punto3a.png",
  width = 1200,
  height = 350,
  res = 150
)

grid.newpage()

grid.table(
  tabla_imagen_3a,
  rows = NULL,
  theme = tema_3a
)

dev.off()

file.exists("tabla_resultados_punto3a.png")

# Prueba t de Welch: comedias vs otros géneros

prueba_3a <- t.test(
  `Total U.S. Gross` ~ Tipo_Genero,
  data = Hollywood
)

prueba_3a

# ============================================================
# PUNTO 3b
# Comparacion del ROI en Estados Unidos por genero
# ============================================================

# Verificar que la variable ROI_US exista
names(Hollywood)

# Calcular estadisticos descriptivos
tabla_3b <- Hollywood |>
  group_by(Tipo_Genero) |>
  summarise(
    n = sum(!is.na(ROI_US)),
    Promedio_ROI = mean(ROI_US, na.rm = TRUE),
    Desviacion_ROI = sd(ROI_US, na.rm = TRUE),
    .groups = "drop"
  )

# Redondear los resultados
tabla_3b_bonita <- tabla_3b |>
  mutate(
    Promedio_ROI = round(Promedio_ROI, 2),
    Desviacion_ROI = round(Desviacion_ROI, 2)
  )

tabla_3b_bonita

# Prueba t de Welch: ROI de comedias vs otros generos

prueba_3b <- t.test(
  ROI_US ~ Tipo_Genero,
  data = Hollywood
)

prueba_3b
# Mostrar los resultados en porcentaje
tabla_3b_bonita <- tabla_3b |>
  mutate(
    Promedio_ROI = round(Promedio_ROI * 100, 2),
    Desviacion_ROI = round(Desviacion_ROI * 100, 2)
  )

tabla_3b_bonita
# Guardar tabla del punto 3b como imagen PNG

tabla_imagen_3b <- tabla_3b_bonita

colnames(tabla_imagen_3b) <- c(
  "Género",
  "n",
  "ROI promedio (%)",
  "Desviación estándar (%)"
)

tema_3b <- gridExtra::ttheme_minimal(
  base_size = 13,
  core = list(
    fg_params = list(col = "black"),
    bg_params = list(
      fill = c("#FFFFFF", "#E6E6E6"),
      col = NA
    )
  ),
  colhead = list(
    fg_params = list(fontface = "bold", col = "black"),
    bg_params = list(fill = "#BFBFBF", col = NA)
  )
)

png(
  filename = "tabla_resultados_punto3b.png",
  width = 1250,
  height = 350,
  res = 150
)

grid::grid.newpage()

gridExtra::grid.table(
  tabla_imagen_3b,
  rows = NULL,
  theme = tema_3b
)

dev.off()

file.exists("tabla_resultados_punto3b.png")
prueba_3b