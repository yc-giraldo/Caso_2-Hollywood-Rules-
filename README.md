# Hollywood Rules

## Descripción del proyecto

Este repositorio contiene el desarrollo del caso Hollywood Rules para la asignatura Analítica de Negocios.

El caso analiza información de 75 películas con el objetivo de estudiar su desempeño comercial y evaluar algunos factores relacionados con la rentabilidad de las inversiones en la industria cinematográfica.

El análisis desarrollado comprende las preguntas 1, 2, 3 y 4 completas, además de los literales a, b y g de la pregunta 7.

Para resolver estas preguntas se utilizan herramientas trabajadas en clase como:

- Estadísticos descriptivos.
- Cálculo del retorno sobre la inversión (ROI).
- Intervalos de confianza.
- Pruebas de hipótesis.
- Comparación de medias.
- Análisis de la relación entre el ingreso del fin de semana de estreno y el ingreso total en Estados Unidos.

Las preguntas 5, 6, 8, 9 y 10, así como los literales c, d, e y f de la pregunta 7, no se incluyen porque requieren herramientas de regresión que todavía no se han trabajado en el curso.

## Archivos principales

- `Hollywood.xls`: base de datos original utilizada para desarrollar el caso.
- `Caso_Hollywood_Rules.R`: script principal con el desarrollo de los análisis estadísticos.
- `outputs/`: carpeta en la que se generan las tablas y resultados utilizados en el informe.
- `README.md`: descripción general del repositorio e instrucciones para ejecutar el análisis.

## Base de datos

La información se encuentra en la hoja `Exhibit 1` del archivo `Hollywood.xls`.

La base contiene **75 observaciones y 18 variables**, entre las que se encuentran:

- Ingreso del fin de semana de estreno.
- Ingreso total en Estados Unidos.
- Ingreso total fuera de Estados Unidos.
- Presupuesto.
- Número de salas durante el estreno.
- Género.
- Clasificación MPAA.
- Secuela.
- Historia conocida.
- Temporada de estreno.
- Opinión de críticos.
- Nominaciones y premios Oscar.

## Librerías necesarias

El análisis utiliza las siguientes librerías de R:

- `readxl`
- `dplyr`
- `ggplot2`
- `gridExtra`

Estas librerías deben estar instaladas previamente en el computador.

El código utiliza rutas relativas, por lo que no depende de una ubicación específica en el computador.


