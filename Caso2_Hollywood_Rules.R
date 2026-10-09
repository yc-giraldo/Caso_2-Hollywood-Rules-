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
