# OscarGarcia_Trabajo2.R
# Trabajo final Bioinformática - Curso 25/26
# Análisis de parámetros biomédicos por tratamiento

# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)

if(!require(readr)) install.packages("readr")

# Cargamos la librería después de verificar o instalar
library(readr)

# Definimos la ruta del archivo para mac
ruta_archivo <- "~/Downloads/datos_biomed.csv"

# llemos, read_csv() detecta automáticamente encabezados, tipos de dato y separadores
datos <- read_csv(ruta_archivo)

# mostramos las primeras 6 filas para verificar que se ha cargado bien
head(datos)

# Mostramos un resumen estadístico de las columnas (mínimos, máximos, media...9
summary(datos)

# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)

head(datos) # head() muestra las primeras 6 filas del dataset para ver una vista rápida del contenido

summary(datos) # summary() resume estadísticamente cada variable: medias, medianas, mínimos, máximos...

dim(datos) # dim() devuelve las dimensiones del data frame: número de filas y número de columnas

str(datos) # str() muestra la estructura interna del objeto: tipos de datos, número de columnas y ejemplos de valores

num_variables <- ncol(datos)     # ncol() cuenta columnas
num_variables                     # Mostramos el resultado

num_tratamientos <- length(unique(datos$Tratamiento)) # length(unique()) cuenta cuántos valores distintos hay en una columna.
num_tratamientos

# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)

# cargamos las librerías necesarias
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

library(ggplot2)
library(tidyr)
library(dplyr)

# elegimos solo las variables numéricas
vars_numericas <- names(datos)[sapply(datos, is.numeric)]
vars_numericas <- vars_numericas[vars_numericas != "ID"]  # excluir ID

# pasamos datos a formato largo
datos_long <- datos %>%
  pivot_longer(
    cols = all_of(vars_numericas),  # las columnas numéricas sin ID
    names_to = "variable",          # nombre de la variable
    values_to = "valor"             # valor de la variable
  )

# hacemos el boxplot
ggplot(datos_long, aes(x = Tratamiento, y = valor, fill = Tratamiento)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +  # un panel por variable biométrica
  theme_bw() +
  labs(
    title = "Boxplots de variables biomédicas por tratamiento",
    x = "Tratamiento",
    y = "Valor"
  )
  
# 4. Realiza un violin plot (investiga qué es). (1 pt)

# instalar y cargar librerías necesarias
# ggplot2 se usa para crear el violin plot
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# tidyr se usa para transformar los datos a formato largo
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}
library(tidyr)

# seleccionar las variables numéricas
# sapply(datos, is.numeric) devuelve TRUE donde la columna es numérica
vars_numericas <- names(datos)[sapply(datos, is.numeric)]

# Excluir ID porque no es una variable biométrica
vars_numericas <- setdiff(vars_numericas, "ID")

# reorganizar los datos a formato largo
# pivot_longer convierte varias columnas en dos columnas:
#   "variable" = nombre de la variable
#   "valor" = valores numéricos de esa variable
datos_long <- datos %>%
  pivot_longer(
    cols = all_of(vars_numericas),
    names_to = "variable",
    values_to = "valor"
  )

# creamos el violin plot
ggplot(datos_long, aes(x = Tratamiento, y = valor, fill = Tratamiento)) +
  geom_violin(trim = FALSE, alpha = 0.7) +   
  # geom_violin dibuja la forma de violín que representa la distribución
  # trim = FALSE evita recortar la distribución
  # alpha = 0.7 da transparencia para que se vean mejor las formas

  facet_wrap(~ variable, scales = "free_y") + 
  # Crea un panel diferente para cada variable (Glucosa, Presion, Colesterol)

  theme_bw() +  

  labs(
    title = "Violin plots por tratamiento",
    x = "Tratamiento",
    y = "Valor"
  )

# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)

# crear gráfico de dispersión Glucosa vs presión
# usamos ggplot2 para crear el gráfico
ggplot(datos, aes(x = Glucosa, y = Presion, color = Tratamiento)) +
  geom_point(size = 3, alpha = 0.7) +  # Puntos con transparencia
  theme_bw() + 
  labs(
    title = "Gráfico de dispersión: Glucosa vs Presión",
    x = "Glucosa (mg/dL)",
    y = "Presión arterial (mmHg)"
  ) +
  theme(legend.position = "bottomright")  # Leyenda en la parte inferior derecha

# como en ggplot2, "bottomright" no existe directamente asi que usamos coordenadas especificas
ggplot(datos, aes(x = Glucosa, y = Presion, color = Tratamiento)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_bw() +
  labs(
    title = "Gráfico de dispersión: Glucosa vs Presión",
    x = "Glucosa (mg/dL)",
    y = "Presión arterial (mmHg)"
  ) +
  theme(
    legend.position = c(0.95, 0.05),  
    legend.justification = c("right", "bottom"),  
    legend.box.just = "right",
    legend.background = element_rect(fill = "white", color = "black")  
  )

# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)

# crear gráfico con facet_grid, facet_grid() crea una cuadrícula de paneles organizados por una o más variables categóricas, en este caso un panel separado para cada tratamiento.

ggplot(datos, aes(x = Colesterol, y = Presion)) +
  geom_point(aes(color = Tratamiento), size = 3, alpha = 0.7) +  # Puntos coloreados por tratamiento
  geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "dashed") +  # Línea de tendencia
  facet_grid(. ~ Tratamiento) +  # Crear un panel por tratamiento (horizontalmente)
  theme_bw() +
  labs(
    title = "Colesterol vs Presión por Tratamiento (facet_grid)",
    x = "Colesterol (mg/dL)",
    y = "Presión arterial (mmHg)"
  ) +
  theme(legend.position = "bottom")

# si no podemos hacer un facet_grid vertical (un panel sobre otro)
# ggplot(datos, aes(x = Colesterol, y = Presion)) +
#   geom_point(aes(color = Tratamiento), size = 3, alpha = 0.7) +
#   facet_grid(Tratamiento ~ .) +  # Paneles verticalmente
#   theme_bw()

# 7. Realiza un histogramas para cada variable. (0.5 pts)

# identificamos qué columnas del dataset son numéricas
variables_numericas <- sapply(datos, is.numeric)

# Creamos un objeto que contiene solo las columnas numéricas
datos_numericos <- datos[, variables_numericas]

# configuramos la ventana gráfica para mostrar varios histogramas a la vez, par(mfrow=c(filas, columnas)) divide el área en una cuadrícula
num_var <- ncol(datos_numericos)    # número de variables numéricas
par(mfrow = c(ceiling(num_var/2), 2))  # crea filas necesarias y 2 columnas

# Creamos un bucle for para generar un histograma por cada variable numérica
for(nombre in colnames(datos_numericos)) {  
  hist(datos_numericos[[nombre]],      # variable que queremos graficar
       main = paste("Histograma de", nombre),
       xlab = nombre,                  # eje X con nombre de variable
       col = "lightblue",              
       border = "black")          
}

# Restablecemos la configuración gráfica a la normal (una figura por ventana)
par(mfrow=c(1,1))

# 8. Crea un factor a partir del tratamiento. Investifa factor(). (1 pt)

# un factor es un tipo de dato en R usado para representar variables categóricas ya que internamente R almacena los factores como números enteros con etiquetas (niveles)

# ver el tipo actual de la columna tratamiento
cat("Tipo de datos original de Tratamiento:\n")
print(class(datos$Tratamiento))

# convertir tratamiento a factor
datos$Tratamiento <- factor(datos$Tratamiento)

# También podemos especificar el orden de los niveles (util para gráficos y analisis)
# datos$Tratamiento <- factor(datos$Tratamiento, levels = c("Placebo", "FarmacoA", "FarmacoB"))

# Ver información sobre el factor
cat("\nDespués de convertir a factor:\n")
print(class(datos$Tratamiento))
cat("\nNiveles del factor:\n")
print(levels(datos$Tratamiento))
cat("\nResumen del factor:\n")
print(summary(datos$Tratamiento))

# Verificar la estructura
cat("\nEstructura del factor:\n")
print(str(datos$Tratamiento))

# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)

# uso dplyr que es mas moderno y tiene una sintaxis mas clara
if (requireNamespace("dplyr", quietly = TRUE)) {
  library(dplyr)
  estadisticas_dplyr <- datos %>%                          # tomar el dataset
    group_by(Tratamiento) %>%                              # Agrupar por tratamiento
    summarise(                                             # Calcular estadísticas para cada grupo
      Media = mean(Glucosa, na.rm = TRUE),                 # media de Glucosa (ignorar NA)
      Desv_Std = sd(Glucosa, na.rm = TRUE),                # Desviación estándar (ignorar NA)
      n = n()                                              # numero de observaciones
    )
  cat("\n=== Estadísticas con dplyr (incluye n) ===\n")
  print(estadisticas_dplyr)  # Mostrar tabla con estadísticas
}

# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de Placebo en una variable llamada placebo. (1 pt)

# nos aseguramos de que la columna tratamiento es un factor 
datos$Tratamiento <- factor(datos$Tratamiento)

# Vemos qué tratamientos existen en el dataset
cat("\nNiveles (tipos) de Tratamiento presentes en los datos:\n")
print(levels(datos$Tratamiento))

# split() para dividir el data frame 'datos' en una lista, donde cada elemento contiene solo las filas de un tratamiento
lista_tratamientos <- split(datos, datos$Tratamiento)
# split(datos, datos$Tratamiento):
#   - coge el data frame completo 'datos'
#   - lo separa en varios data frames según el valor de 'Tratamiento'
#   - devuelve una lista donde cada elemento tiene el nombre de un tratamiento

# nombres de los elementos de la lista (deben coincidir con los niveles de Tratamiento)
cat("\nElementos de 'lista_tratamientos':\n")
print(names(lista_tratamientos))

if ("Placebo" %in% names(lista_tratamientos)) {
  placebo <- lista_tratamientos[["Placebo"]]
  cat("\nSe ha creado la variable 'placebo' con los datos del tratamiento Placebo.\n")
}

if ("FarmacoA" %in% names(lista_tratamientos)) {
  FarmacoA <- lista_tratamientos[["FarmacoA"]]
  cat("Se ha creado la variable 'FarmacoA' con los datos del tratamiento FarmacoA.\n")
}

if ("FarmacoB" %in% names(lista_tratamientos)) {
  FarmacoB <- lista_tratamientos[["FarmacoB"]]
  cat("Se ha creado la variable 'FarmacoB' con los datos del tratamiento FarmacoB.\n")
}

# - 'lista_tratamientos' es una lista con todos los tratamientos
# - si existen los tratamientos Placebo, FarmacoA y FarmacoB, también tienes las variables 'placebo', 'FarmacoA' y 'FarmacoB' como data frames separados

# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)

# comprobamos que Tratamiento sea un factor que define grupos
datos$Tratamiento <- factor(datos$Tratamiento)

# Creamos un vector vacío donde guardar los resultados del test de normalidad
normalidad <- list()

# usamos el test de normalidad Shapiro-Wilk dentro de cada grupo de tratamiento, split() separa la variable Glucosa en listas 
for (nivel in levels(datos$Tratamiento)) {
  
  # Extraemos valores de glucosa correspondientes al tratamiento actual del bucle
  glucosa_nivel <- datos$Glucosa[datos$Tratamiento == nivel]
  
  # ejecutamos el test Shapiro-Wilk para evaluar normalidad del grupo, el valor p indica si NO podemos rechazar la normalidad (>0.05 normalmente significa datos normales)
  normalidad[[nivel]] <- shapiro.test(glucosa_nivel)
}

# vemos resultados
normalidad

# si TODOS los grupos tienen normalidad (p > 0.05), usamos ANOVA o t-test
# Si ALGÚN grupo NO es normal (p < 0.05), usamos Kruskal-Wallis

# Miramos si todos los tratamientos cumplen normalidad
todos_normales <- all(sapply(normalidad, function(x) x$p.value > 0.05))

if (todos_normales) {
  
  # Si los datos son normales -> ANOVA
  resultado_test <- aov(Glucosa ~ Tratamiento, data = datos)
  print("Resultado: Se usa un ANOVA (datos normales).")
  
} else {
  
  # Si los datos NO son normales -> Kruskal-Wallis
  resultado_test <- kruskal.test(Glucosa ~ Tratamiento, data = datos)
  print("Resultado: Se usa Kruskal-Wallis (datos no normales).")
  
}

# mostramos el resultado del test seleccionado
resultado_test

# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)

# comprobamos que la columna "Tratamiento" sea de tipo factor, el factor representa categorías (placebo, farmacoA, farmacoB...), necesario para que el ANOVA trate correctamente los grupos
datos$Tratamiento <- factor(datos$Tratamiento)

# Comprobamos los de "Tratamiento" para ver cuántos tratamientos hay y cómo se llaman.
levels(datos$Tratamiento)

# ajustamos un modelo one-way ANOVA donde la glucosa es la variable dependiente y tratamiento es la variable independiente (factor, grupos)
anova_glucosa <- aov(Glucosa ~ Tratamiento, data = datos)

# vemos el estadístico F, los grados de libertad y el p-value para evaluar si hay diferencias significativas de glucosa entre tratamientos
summary(anova_glucosa)

# media de glucosa por tratamiento para ver como se dan las diferencias entre grupos
tapply(datos$Glucosa, datos$Tratamiento, mean)

# no es necesario pero he hecho una post-hoc de Tukey para comparar todos los pares de tratamientos y ver entre qué grupos concretos hay diferencias
TukeyHSD(anova_glucosa)

# Lo representamos gráficamente 
plot(TukeyHSD(anova_glucosa))



