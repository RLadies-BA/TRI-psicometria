# Psicometría con R: Introducción a la TRI
# Paquete principal: mirt


# Carga de paquetes ----------------------

library(mirt)   # Ajuste de modelos TRI
library(sirt)   # Dataset de ejemplo

# Cargar datos ---------------------------
# Cargar dataset incluido en sirt
data(data.read)

# Exploración inicial del dataset --------
# Ver primeras filas
head(data.read)

# Estructura del dataset
str(data.read)

# Dimensión del dataset
dim(data.read)

# Promedio de aciertos por ítem
colMeans(data.read)

# Interpretación desde la Teoría Clásica de los Tests:
# Valores cercanos a 1 -> ítems fáciles
# Valores cercanos a 0 -> ítems difíciles

# EJERCICIO 1
# Mirando los promedios:
# - ¿Qué ítem se podría interpretar como el más fácil?
# - ¿Cuál parece el más difícil?

# Distribución de puntajes totales --------
puntaje_total <- rowSums(data.read)

hist(puntaje_total,
     main = "Distribución de puntajes totales",
     xlab = "Puntaje",
     ylab = "Frecuencia")

# Modelo 1PL / Rasch ----

# El modelo Rasch asume que:
# - Todos los ítems discriminan igual
# - Los ítems solo difieren en dificultad

# Se lo llama modelo de "1 parámetro"
# porque estima únicamente el parámetro b (dificultad)

## Ajustar modelo de 1 parámetro ----------
modelo_1PL <- mirt(data = data.read, 
                  model = 1,
                  itemtype = "Rasch")

# Evaluar ajuste del modelo
M2(modelo_1PL)

# Interpretación:
# Algunos índices útiles:
#
# RMSEA:
# Valores más bajos indican mejor ajuste (< .06); 
#
# CFI / TLI:
# Valores más cercanos a 1 indican mejor ajuste (> .95)

## Parámetros de los ítems -----------------
coeficientes_1PL <- coef(modelo_1PL, 
                         IRTpars = TRUE, 
                         simplify = TRUE)
coeficientes_1PL

# Interpretación:
# b = dificultad del ítem - Valores más altos = ítems más difíciles 
# (se necesita más habilidad para responder correctamente)

# EJERCICIO 2
# Mirando los parámetros b:
# - ¿Qué ítem es el más difícil?
# - ¿Coincide con lo que habían observado
#   usando los promedios de aciertos?

## Curvas características de los ítems - CCI ----
plot(modelo_1PL, type = "trace")

# Interpretación:
# Eje X -> habilidad
# Eje Y -> probabilidad de responder correctamente


# EJERCICIO 3
# Mirando las curvas:
# - ¿Qué ítems parecen más difíciles?
# - ¿Qué pasa con la posición de la curva?
# - ¿Qué representa que una curva esté más a la derecha?

### Información del ítem ------------------------
itemplot(modelo_1PL, item = "A1", type = "info")

### Información del test ------------------------
plot(modelo_1PL, type = "info")

# Interpretación:
# En qué niveles de habilidad el test mide con mayor precisión.
# Más información = menor error de medición

# EJERCICIO 4
# Mirando la curva de información:
# - ¿El test mide mejor habilidades bajas,
#   medias o altas?

# Modelo 2PL ---------------------------------
# El modelo 2PL:
# a = discriminación
# b = dificultad
#
# Los ítems pueden diferenciarse por dificultad,
# pero tambien por qué tan bien discriminan
# entre personas con distintos niveles de habilidad.

## Ajustar modelo de 2 parámetros -----------
modelo_2PL <- mirt(data = data.read, 
                   model = 1, 
                   itemtype = "2PL")
M2(modelo_2PL)

# EJERCICIO 5
# Obtener los parámetros del modelo 2PL
# Mirando el parámetro a:
# - ¿Qué ítems discriminan mejor?
# - ¿Hay ítems con discriminación baja?

### Parámetros de los ítems -----------------
# Interpretación:
# a = discriminación del ítem - cuanto más alta, mayor pendiente
# el ítem discrimina mejor entre personas
# b = dificultad del ítem



# EJERCICIO 6
# Obtener las Curvas Características y las de Información de los Ítems 
# - ¿Qué cambia ahora en las curvas?

### Curvas CCI -----------------------------


### Información del ítem ------------------------



# EJERCICIO 7
# Obtener el gráfico de Información del Test.
# - ¿Cambió respecto al modelo de 1 parámetro?

### Información del test -------------------


# Modelo 3PL -------------------------------
# El modelo 3PL agrega:
# g = guessing / azar
# Representa la probabilidad de responder correctamente
# el ítem por azar.
# Este modelo suele utilizarse en tests de opción múltiple.

## Ajustar modelo de 3 parámetros ----------
modelo_3PL <- mirt(data = data.read, 
                   model = 1, 
                   itemtype = "3PL")
M2(modelo_3PL)

### Parámetros de los ítems -----------------
coeficientes_3PL <- coef(modelo_3PL, 
                         IRTpars = TRUE,
                         simplify = TRUE)
coeficientes_3PL

# Interpretación:
# a = discriminación
# b = dificultad
# g = probabilidad de acertar por azar


### Curvas CCI ------------------------------
plot(modelo_3PL, type = "trace")

### Información del ítem -----------------------
itemplot(modelo_3PL, item = "A1", type = "info")

### Información del test --------------------
# Función de información del test
plot(modelo_3PL, type = "info")


# Comparación entre modelos -----------------
anova(modelo_1PL, modelo_2PL)

# Interpretación:
# Evaluar si un modelo más complejo ajusta 
# significativamente mejor que uno más simple.
#
# Hipótesis nula:
# El modelo más simple (1PL) es suficiente.
#
# Si el valor p es pequeño (< .05),
# el modelo más complejo (2PL)
# mejora significativamente el ajuste.
