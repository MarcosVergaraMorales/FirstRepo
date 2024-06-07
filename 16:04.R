# ---- Limpieza del entorno ----
rm(list = ls())

# ---- Paquetes ----
library(haven)
library(tidyverse) 
library(dplyr)
# install.packages("psych") # Para media geométrica y armónica
library(psych) 
# install.packages("timeDate") #Para las fechas
library(timeDate) 

library(haven)
PISA22SP <- read_sav("Desktop/Data Science/PISA22SP.sav")
View(PISA22SP)


pisa <- PISA22SP


pisa <- read_sav("PISA22SP.sav")
factores <- c("region", "estrato", "genero", "meduc", 
              "peduc", "heduc")
pisa[, factores] <- as_factor(pisa[,factores]
) |> droplevels()
summary(pisa)


plot(pisa$mates,
     ylab="Nota prueba matemáticas")


plot(pisa$mates, pisa$lectu, 
     ylab="Nota prueba matemáticas",
     xlab="Nota prueba lectura")

slices <- c(10, 12,4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
pie(slices, labels = lbls, main="Gráfico de países")


class(pisa$heduc)

ggplot(data = pisa) +
  geom_histogram(mapping = aes(x = mates))


ggplot(data = pisa, 
       mapping = aes(x = mates, colour = genero)) +
  geom_freqpoly()


ggplot(data = pisa) +
  geom_count(mapping = aes(x = meduc, y = peduc))


datos.num <- pisa[, c('lectu', 'hisei', 'status')]
pairs(datos.num)


ggplot(data = pisa) +
  geom_point(mapping = aes(x = mates, y = lectu))


ggplot(data = pisa, 
       mapping = aes(x = mates, y = lectu)) + 
  geom_boxplot(mapping = aes(
    group = cut_number(mates, 20)))


ggplot(data = pisa, mapping = aes(x = mates)) + 
  geom_freqpoly(mapping = aes(colour = heduc))


# Verificación de los datos recodificados
table(pisa$heduc_recoded, useNA = "ifany")

ggplot(data = pisa, 
       mapping = aes(x = mates)) + 
  geom_freqpoly(mapping = aes(colour = heduc_recoded))

ggplot(data = pisa, mapping = aes(x = mates)) + 
  geom_freqpoly(mapping = aes(colour = heduc_recoded))

install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(pisa$mates, pisa$lectu, pisa$cienc, 
              main="Relación entre las tres pruebas",
              xlab="Var1", ylab="Var2", zlab="Var3")


#Ejercicio clase

# Library
library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)

ggplot(data = pisa, mapping = aes(x = mates)) + ggplotly (mapping = aes(colour = pocup))
                                                          



