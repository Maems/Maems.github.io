#------------------------------ Trabajo final ----------------------------------------------------------

# Cargar la base de datos inicial
concreto <- read.csv(file.choose(), sep = ";")
View(concreto)

# Cambiar ceros por NA

concreto$SLUMP_SALIDA_PLANTA[concreto$SLUMP_SALIDA_PLANTA==""] <- NA
concreto$SLUMP_EN_OBRA[concreto$SLUMP_EN_OBRA==0] <- NA
concreto$MINUTOS_SALIDA_PLANTA[concreto$MINUTOS_SALIDA_PLANTA==0] <- NA
concreto$MINUTOS_INICIO_FUNDIDA[concreto$MINUTOS_INICIO_FUNDIDA==0] <- NA

# Determinamos donde hay valores faltantes en cada una de las variables de interés

which(is.na(concreto$SLUMP))
planta <- which(is.na(concreto$MINUTOS_SALIDA_PLANTA))
which(is.na(concreto$MINUTOS_INICIO_FUNDIDA))

# Base de datos 1

concreto1 <- na.omit(concreto)
View(concreto1)

# Base de datos 2

diferencia <- concreto1$MINUTOS_INICIO_FUNDIDA - concreto1$MINUTOS_SALIDA_PLANTA
diferencia1 <- concreto1$SLUMP_SALIDA_PLANTA - concreto1$SLUMP_EN_OBRA

# Base de datos 3

concreto2 <- data.frame(slump = diferencia1, tiempo = diferencia)
View(concreto2)

# Diagrama de dispersión del slump vs tiempo

with(concreto2, plot(x = tiempo, y = slump, pch = 20,
                     xlab = "Tiempo (min)",
                     ylab = "Asentamiento del concreto (pulgadas)"))

# Histograma de la variable respuesta (slump)

with(concreto2, hist(concreto2$slump,
                     xlab = "Asentamiento del concreto (pulgadas)",
                     ylab = " Densidad",
                     main = "",
                     freq = FALSE))

#-------------------------- MODELAMIENTO ----------------------------------------------

# Distribución de la variable respuesta

library(gamlss)

distribucion <- fitDist(concreto2$slump, type = "realplus")

# Mejores distribuciones

distribucion$fits

# Densidad de las mejores distribuciones junto con el histograma

par(mfrow = c(1,2))
fitexGAUS <- histDist(y = concreto2$slump, family = exGAUS, main = "exGAUS", 
                      xlab = "Asentamiento del concreto (pulgadas)")
fitEXP <- histDist(y = concreto2$slump, family = EXP, main = "EXP",
                   xlab = "Asentamiento del concreto (pulgadas)")


#------------------------- Modelos ----------------------------------------------------

# Modelo 1

m1 <- lm(slump ~ tiempo,
         data = concreto2)
summary(m1)

abline(m1, col = "red")

mean((concreto2$slump-m1$fitted.values)^2)         # Error cuadratico medio
cor(concreto2$slump, m1$fitted.values)             # Coeficiente de correlación
plot(m1)

# Modelo 1

m2 <- gamlss(slump ~ tiempo, 
             data = concreto2,
             family = exGAUS())
summary(m2)

Rsq(m2)                                    # Coeficiente de determinación
mean((concreto2$slump-m2$mu.fv)^2)         # Error cuadratico medio
cor(concreto2$slump, m2$mu.fv)             # Coeficiente de correlación
AIC(m2)                                    # AIC
plot(m2)                                   # Graficos de residuales

# Modelo 3

m3 <- gamlss(slump ~ tiempo,
             sigma.formula = ~ tiempo,
             data = concreto2,
             family = exGAUS())
summary(m3)

Rsq(m3)                                    # Coeficiente de determinación
mean((concreto2$slump-m3$mu.fv)^2)         # Error cuadratico medio
cor(concreto2$slump, m3$mu.fv)             # Coeficiente de correlación
AIC(m3)                                    # AIC
plot(m3)                                   # Graficos de residuales

# Modelo 4

m4 <- gamlss(slump ~ cs(tiempo),
             data = concreto2,
             family = exGAUS())
summary(m4)

Rsq(m4)                                    # Coeficiente de determinación
mean((concreto2$slump-m4$mu.fv)^2)         # Error cuadratico medio
cor(concreto2$slump, m4$mu.fv)             # Coeficiente de correlación
AIC(m4)                                    # AIC

plot(m3)                                   # Graficos de residuales

# Modelo 5

m5 <- gamlss(slump ~ cs(tiempo),
             sigma.formula = ~ tiempo,
             data = concreto2,
             family = exGAUS())
summary(m5)

Rsq(m5)                                    # Coeficiente de determinación
mean((concreto2$slump-m5$mu.fv)^2)         # Error cuadratico medio
cor(concreto2$slump, m5$mu.fv)             # Coeficiente de correlación
AIC(m5)                                    # AIC
plot(m5)                                   # Graficos de residuales

# Modelo 6

m6 <- gamlss(slump ~ tiempo + cs(tiempo),
             sigma.formula = ~ tiempo,
             nu.formula = ~ tiempo,
             data = concreto2,
             family = exGAUS())
summary(m6)

# Estrategia A 

m66 <- stepGAICAll.A(m6)
m66$anova
summary(m66)

Rsq(m66)                                    # Coeficiente de determinación
mean((concreto2$slump-m66$mu.fv)^2)         # Error cuadratico medio
cor(concreto2$slump, m66$mu.fv)             # Coeficiente de correlación
AIC(m66)                                    # AIC
plot(m66)                                   # Graficos de residuales

# Modelo 7

m7 <- gamlss(slump ~ tiempo+I(tiempo^(1/2))+I(1/tiempo)+I(sin(tiempo)),
             sigma.formula = ~ tiempo,
             nu.formula = ~ tiempo,
             data = concreto2,
             family = exGAUS())
summary(m7)

# Estrategia A

m77 <- stepGAICAll.A(m7)
summary(m77)

Rsq(m77)                                    # Coeficiente de determinación
mean((concreto2$slump-m77$mu.fv)^2)         # Error cuadratico medio
cor(concreto2$slump, m77$mu.fv)             # Coeficiente de correlación
AIC(m77)                                    # AIC
plot(m77)                                   # Graficos de residuales

# Modelo 7

m7 <- gamlss(slump ~ tiempo,
             data = concreto2,
             family = PARETO2())
summary(m7)

Rsq(m7)                                    # Coeficiente de determinación
mean((concreto2$slump-m7$mu.fv)^2)         # Error cuadratico medio
cor(concreto2$slump, m7$mu.fv)             # Coeficiente de correlación
AIC(m7)                                    # AIC
plot(m7)                                   # Graficos de residuales

# Modelo 8

m8 <- gamlss(slump ~ tiempo,
             sigma.formula = ~ tiempo,
             data = concreto2,
             family = PARETO2())
summary(m8)

Rsq(m3)                                    # Coeficiente de determinación
mean((concreto2$slump-m3$mu.fv)^2)         # Error cuadratico medio
cor(concreto2$slump, m3$mu.fv)             # Coeficiente de correlación
AIC(m3)                                    # AIC
plot(m3)                                   # Graficos de residuales

write.csv(concreto2, "concreto1.csv")
str(concreto2)

