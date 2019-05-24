# Trabajo Practico NÂ°1
# Metodos Estadisticos Aplicados a Negocios
#
# Agustin Alba Chicar <ag.albachicar@gmail.com>
# Joaquin Gonzalez <joagonzalez@gmail.com>
#
#
# AÃ±o: 2019
# Profesora: Magdalena Cornejo
# Jefe de Trabajos Practicos: Mauricio Grotz
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Carga de paquetes
# -------------------------------------------------------------------
# install.packages("dplyr") # Ejecutar en caso de no tenerla instalada
library("dplyr")
# install.packages("ggplot2")  # Ejecutar en caso de no tenerla instalada
library("ggplot2")
# Para consigna 4 se aumenta el max.print
options(max.print=1000000)
# -------------------------------------------------------------------
# Limpieza del workspace
# -------------------------------------------------------------------
rm(list = ls())
cat("\f")

# Cargar el path donde este script se va a correer
path_to_workspace <- "C:/Users/jgonzalez/Google Drive/DiTella/Materias/Metodos Estadisticos Aplicados a Negocios/Trabajo Practico N1"
#path_to_workspace <- "/home/agustin/facultad/ditella/mean/tps/tp1/repo"
setwd(path_to_workspace)
# -------------------------------------------------------------------
# Carga de informacion
# -------------------------------------------------------------------
black_friday <- read.table("BlackFriday.csv", header = TRUE, sep = ",")

# -------------------------------------------------------------------
# Consigna 1
# -------------------------------------------------------------------
# Generalidades sobre los datos.
head(black_friday)
nrow(black_friday)
anyNA(black_friday)
summary(black_friday) # Generalidades de los datos

# Valores generales sobre las transacciones.
pur_mean <- mean(black_friday$Purchase, na.rm = TRUE)
pur_sd <- sd(black_friday$Purchase, na.rm = TRUE)

# Valores generales de los productos
# Product ID
p_id <- summary(as.factor(black_friday$Product_ID)) # Cuenta de transaciones por producto
length(p_id)
p_id[which.min(p_id)]
p_id[which.max(p_id)]
p_id[which.max(p_id[-which.max(p_id)])]
mean(p_id[-which.max(p_id)])
sd(p_id[-which.max(p_id)])

# Valores generales sobre los productos
p_cat_1 <- prop.table(summary(as.factor(black_friday$Product_Category_1))) # Cuenta de productos por tipo de categoria 1
p_cat_1 <- p_cat_1 * 100
length(p_cat_1)
p_cat_1[which.min(p_cat_1)]
p_cat_1[which.max(p_cat_1)]
p_cat_1 <- summary(as.factor(black_friday$Product_Category_1))
mean(p_cat_1, na.rm = TRUE)
sd(p_cat_1, na.rm = TRUE)

p_cat_2 <- prop.table(summary(as.factor(black_friday$Product_Category_2))) # Cuenta de productos por tipo de categoria 2
p_cat_2 <- p_cat_2 * 100
length(p_cat_2)
p_cat_2[which.min(p_cat_2)]
p_cat_2[which.max(p_cat_2)] # Max son los NA
p_cat_2[which.max(p_cat_2[-which.max(p_cat_2)])]
p_cat_2 <- summary(as.factor(black_friday$Product_Category_2))
mean(p_cat_2, na.rm = TRUE)
sd(p_cat_2, na.rm = TRUE)

p_cat_3 <- prop.table(summary(as.factor(black_friday$Product_Category_3))) # Cuenta de productos por tipo de categoria 3
p_cat_3 <- p_cat_3 * 100
length(p_cat_3)
p_cat_3[which.min(p_cat_3)]
p_cat_3[which.max(p_cat_3)] # Max son los NA
p_cat_3[which.max(p_cat_3[-which.max(p_cat_3)])]
p_cat_3 <- summary(as.factor(black_friday$Product_Category_3))
mean(p_cat_3, na.rm = TRUE)
sd(p_cat_3, na.rm = TRUE)

# Valores generales sobre los clientes
# Genero
gender <- prop.table(summary(black_friday$Gender))
gender_lbs <- c("Femenino", "Masculino")
gender_lbs <- paste(gender_lbs, round(gender * 100))
gender_lbs <- paste(gender_lbs, rep("%", 2))
pie(gender, main="Distribucion por genero",
    labels=gender_lbs, col=c("#6AA5C8", "#ED8B4E"))

# Segmento etario
age <- prop.table(summary(black_friday$Age)) * 100 
barplot(age, main="Distribucion etaria", horiz=TRUE,
        names.arg=labels(age), las=1, col=c("#6AA5C8"),
        xlab="Porcentaje [%]", ylab="Rangos etarios [aÃ±os]",
        xlim=c(0, max(age) + 5))

# Estado marital (1 es casado/a y 0 es otros)
mar_status <- prop.table(summary(as.factor(black_friday$Marital_Status)))
mar_status_lbl <- c("Casado/a", "Otro")
mar_status_lbl <- paste(mar_status_lbl, round(mar_status * 100))
mar_status_lbl <- paste(mar_status_lbl, rep("%", 2))
pie(mar_status, main="Estado civil",
    labels=mar_status_lbl, col=c("#6AA5C8", "#ED8B4E"))

# -------------------------------------------------------------------
# Consigna 2
# -------------------------------------------------------------------
gender_data <- black_friday$Gender
gender_props <- prop.table(summary(gender_data))

p_f <- gender_props["F"]
n_p_f <- length(gender_data)
s_p_f <- sqrt(p_f * (1 - p_f) / n_p_f)
alpha_p_f <- 0.05
t_p_f = qt(1 - alpha_p_f / 2, df = n_p_f - 1)
p_f_range <- c(p_f - t_p_f * s_p_f / sqrt(n_p_f), p_f + t_p_f * s_p_f / sqrt(n_p_f))

# -------------------------------------------------------------------
# Consigna 3
# -------------------------------------------------------------------

# Se filtran la base para quedarse con las columnas:
# Purchase, Age (1)
#
# Finalmente se realizan promedios, desvios e intervalos de confianza
# para cada uno de los segmentos etarios. (2)

# Paso 1
work_frame <- black_friday %>% select(Age, Purchase)
# Paso 2
work_frame <-
  work_frame %>%
  group_by(Age) %>%
  summarise(MeanPurchase = mean(Purchase),
            StdDevPurchase = sd(Purchase),
            NPurchase = length(Purchase))

# Computa el intervalo de confianza para cada uno de los consumos promedios
# Se considera stddev_x como parametro obtenido de la muestra
ic_compute <- function(mean_x, stddev_x, n_x, alpha) {
  t_x <- qt(1 - alpha / 2., df = n_x - 1)
  delta <- t_x * stddev_x / sqrt(n_x)
  return (c("min" = mean_x - delta, "max" = mean_x + delta))
}

# Asignacion de alpha para el intervalo de confianza
alpha = 0.05
# Creo las columnas para los maximos y minimos
work_frame$MinPurchase <- rep(0, nrow(work_frame))
work_frame$MaxPurchase <- rep(0, nrow(work_frame))
# Computo los intervalos de confianza para cada segmento etario
for(i in seq(from=1, to=length(work_frame)+1, by=1)) {
  ic <- ic_compute(as.numeric(work_frame[i, "MeanPurchase"]), 
                   as.numeric(work_frame[i, "StdDevPurchase"]),
                   as.numeric(work_frame[i, "NPurchase"]),
                   alpha)
  work_frame[i, "MinPurchase"] <- ic["min"]
  work_frame[i, "MaxPurchase"] <- ic["max"]
}

# Grafico.
ggplot(work_frame, aes(Age, MeanPurchase)) +
  geom_point(mapping = aes(x = Age, y = MeanPurchase)) +
  geom_errorbar(aes(ymin = MinPurchase, ymax = MaxPurchase), color="blue") +
  xlab("Rangos etarios [años]") +
  ylab("Montos de transacciones [U$D]") +
  ggtitle("Intervalos de compras promedio acumulada por segmento etario.")

# Obtengo el vector de edades, y genero los test de hipotesis combinatorios entre
# los rangos etarios.
ages <- levels(black_friday$Age)
conf_level <- 1 - alpha

n_cases <- ncol(combn(length(ages), 2))
res <- data.frame()
k <- 1

for (i in seq(from = 1, to = length(ages) - 1, by = 1)) {
  purchases_i <- black_friday %>% select(Age, Purchase) %>% filter(Age == ages[i]) %>% select(Purchase)
  
  for (j in seq(from = i + 1, to = length(ages), by = 1)) {
    purchases_j <- black_friday %>% select(Age, Purchase) %>% filter(Age == ages[j]) %>% select(Purchase)
    
    res[k, "age_x"] <- ages[i]
    res[k, "age_y"] <- ages[j]
    res[k, "p_value"] <- t.test(purchases_i, y = purchases_j, alternative = c("two.sided"), 
                         mu = 0, paired = FALSE, var.equal = FALSE, conf.level = conf_level)$p.value
    k <- k + 1
  }
}

# Evaluo los casos en los que se produce diferencia de compras significativa.
res$is_diff <- res$p_value < (alpha / 2)
# Como son mayoria los casos de diferencia, notifico los que son iguales
res[res$is_diff == FALSE, c("age_x", "age_y")]

# -------------------------------------------------------------------
# Consigna 4
# -------------------------------------------------------------------
# Se filtra la base para quedarse con las columnas:
# Product_ID, Gender, Purchase (1)
#
# Se calcula la proporcion de mujeres y se realiza test de hipotesis nula (2)
# - H_0 : proporcion de mujeres que compran el producto = 27%
# - H_1 : proporcion de mujeres que compran el producto != 27%
# Es un test bilateral a dos colas.
# Para ello definimos un valor de significancia alpha igual al 5%.
#
# Finalmente, se calcula la potencia del test, beta. (3)

# Paso 1
work_frame <- select(black_friday,Product_ID)

p <- work_frame %>% group_by(Product_ID) %>% summarise(Buy = length(Product_ID))
p_ID <- p$Product_ID[which.max(p$Buy)] # Buscamos el Product_ID con mayor cantidad de compras

p_ID # Imprimimos ID producto mas comrpado
p$Buy[which.max(p$Buy)] # Imprimimos cantidad de productos vendidos



# Paso 2
work_frame <- black_friday %>% select(Product_ID, Gender) %>% filter(Product_ID==p_ID)

n_m <-length(work_frame$Gender)
p_m <- prop.table(summary(work_frame$Gender)) # proporcion de la muestra
p_0 <- 0.27 # proporcion de hipotesis

p_m_f <- p_m["F"]
e_m <- sqrt(p_m_f*(1-p_m_f) / n_m)
e_m["F"] # Error muestral para mujeres

# Asignacion de alpha para el intervalo de confianza
alpha = 0.05

binom.test(p_m*n_m, n, p=0.27, alternative=("two.sided"), conf.level = 1 - alpha)

#  IC = [0.2516727 0.2926454] => p_m cae dentro del intervalo de confianza
# No existe suficiente evidencia para refutar la hipotesis nula

# Paso 3

p_m <- seq(from = 0.15, to=0.27, by=0.001)
z_crit <- (qnorm(0.05)*sqrt(p_m * (1 - p_m) / n_m)) + p_0

beta <- 1-pnorm((z_crit-p_m)/sqrt(p_m * (1 - p_m) / n_m))
potencia <- 1-beta
plot(p_m*n_m,potencia, ylim=c(0,1),type = "l", col="red", xlab= "p_muestra", ylab= "potencia", lwd=2)

# Es razonable que la potencia del test aumente a medida que nos alejamos del valor esperado. 1-Beta es la 
# capacidad de rechazar la hipotesis nula siendo esta falsa (evitar errores tipo II), y mientras mas alejados del valor real estemos en
# terminos de las proporciones obtenidas de la muestra, mas dificil es y mas potencia tiene el test.

# -------------------------------------------------------------------
# Consigna 5
# -------------------------------------------------------------------
# Se filtra la base para quedarse con las columnas:
# User_ID, Purchase (1)
#
# Si bien el enunciado pregunta por gasto medio por usuario, la mean(Purchase) = 9333 (sin acumular por user_ID).
# Se trabaja sobre esta media para la formulacion de la hipotesis nula (2)
# - H_0 : media de gasto por usuario <= u$9300
# - H_1 : media de gasto por usuario > u$9300
# Es un test unilateral a una cola.
# Para ello definimos un valor de significancia alpha igual al 5%.
#
# Finalmente, se vuelve a realizar el test de hipotesis nula para una cota de u$9400 (3)

# Paso 1

work_frame <- select(black_friday, User_ID, Purchase) %>%
  group_by(User_ID)

# Si fuese media por usuario, contemplando todas las compras que realiza cada usuario
# u <- ddply(work_frame,.(User_ID),summarize, Expense_By_User=sum(Purchase) , Purchase_By_User=length(User_ID))

# Paso 2

# Asignacion de alpha para el intervalo de confianza
alpha = 0.05

m_m <- mean(work_frame$Purchase, na.rm = TRUE)  # Media muestral
s_m <- sd(work_frame$Purchase, na.rm = TRUE)    # Desviacion estandar muestral
n_m <- length(work_frame$User_ID)               # Tamaño de la muestra

m_0 <- 9300                                   # Media supuesta por H0
tstat <- (m_m - m_0) / (s_m /sqrt(n_m))       # Se desconoce sigma. Se utiliza t-student
tcritico <- qt(1-alpha,df=n_m-1)              # surge de la distribucion t-student, se compara con observado
tstat < tcritico  # Como FALSE, existe evidencia suficiente para rechazar H0

t.test(work_frame$Purchase, alternative = "greater", mu = m_0) # Evaluamos con funcion t.test()

pvalor <- pt(tstat,df=n_m-1,lower.tail=FALSE) # p-valor para cola de la derecha.
pvalor<alpha # Como p-valor menor que alpha, rechazho H0

# Paso 3

m_0 <- 9400                                   # Media supuesta por H0
tstat <- (m_m - m_0) / (s_m /sqrt(n_m))       # Se desconoce sigma. Se utiliza t-student
tcritico <- qt(1-alpha,df=n_m-1)              #surge de la distribucion t-student, se compara con observado
tstat < tcritico  # Como TRUE, NO rechazo H0. Era esperable por la gran diferencia entre el m_0 supuesto y la media muestral.

t.test(work_frame$Purchase, alternative = "greater", mu = m_0) 

pvalor <- pt(tstat,df=n_m-1,lower.tail=FALSE) # p-valor para cola de la derecha. H_1 es de "mayor que"
pvalor<alpha # Como p-valor mayor que alpha, no hay evidencia suficiente para recharzar H0


# -------------------------------------------------------------------
# Consigna 6
# -------------------------------------------------------------------
# Se filtran la base para quedarse con las columnas:
# Purchase, Gender (1)
#
# Se promedian los consumos de cada usuario segun su genero. (2)
#
# Finalmente, se realiza un test de hipotesis de diferencia de medias con:
# - H_0 : la diferencia de medias de consumo entre hombres y
#         mujeres es menor o igual a cero.
# - H_1 : la diferencia de medias de consumo entre hombres y
#         mujeres es mayor a cero.
# Para ello definimos un valor de significancia alpha igual al 5%. (3)


# Paso 1

work_frame <- black_friday %>% select(Gender, Purchase)
hombres <- work_frame %>% filter(Gender == "M") %>% select(Purchase)
mujeres <- work_frame %>% filter(Gender == "F") %>% select(Purchase)

# Paso 2

# Asignacion de alpha para el intervalo de confianza
alpha = 0.05
t.test(hombres, y=mujeres, alternative=c("greater"), mu=0, paired=FALSE, var.equal=FALSE, conf.level = 1-alpha)

# as.logical(p_valor < alpha)=TRUE, por lo tanto hay evidencia suficiente para rechazar hipotesis nula. 

# Forma manual #
##########
# Paso 1#
########

#work_frame <- select(black_friday, Gender, Purchase) 
#expenses <- ddply(work_frame,.(Gender),summarize, Mean=mean(Purchase), Count=length(Gender), stdDev=sd(Purchase))

##########
# Paso 2#
#########

#n_h <- expenses[2,3]          # Cantidad de hombres
#m_h <- expenses[2,2]          # Media compras hombres
#s_d_h <- expenses[2,4]         # Desviacion estandar hombres
#n_m <- expenses[1,3]          # Cantidad de mujeres
#m_m <- expenses[1,2]          # Media compras mujeres
#s_d_m <- expenses[1,4]         # Desviacion estandar mujeres
# Asignacion de alpha para el intervalo de confianza
#alpha = 0.05
# Se asume que la correlacion entre consumidores hombres y mujeres es nula
#d = m_h - m_m          # Diferenia de medias muestrales 
#d_0 <- 0               # Diferencia de medias supuesta por H0
#s_d = s_d_h + s_d_m       # Desviacion de la diferencia de medias
# El estimador resulta:
#tstat = (d - d_0) / sqrt(s_d) # OJO ACA, NO VA EXPRESION DE SLIDE 36 PPT CLASE 2 ??
# Computamos los grados de libertad de la t-student
#df_z_t = (s_d_h^2 * (n_h - 1) + s_d_m^2 * (n_m - 1)) / s_d^2
#df_z_t = floor(df_z_t)
#tcritico = qt(1 - alpha, df = df_z_t - 1)
#p_valor <- pt(tstat, df_z_t, lower.tail = FALSE)  # FALSE pues H_1 es de "mayor
#as.logical(p_valor < alpha) # Como TRUE, hay evidencia suficiente para rechazar hipotesis nula. Por lo tanto, es mas probable que la media de gastos en hombres sea mayor

# -------------------------------------------------------------------
# Consigna 7
# -------------------------------------------------------------------

# Se filtran la base para quedarse con las columnas:
# Purchase, Marital_Status (1)
#
# Finalmente, se realiza un test de hipÃ³tesis con:
# - H_0 : la diferencia de medias de consumo entre la poblacion casada y
#         la poblacion con otro estado civil es menor o igual a cero.
# - H_1 : la diferencia de medias de consumo entre la poblacion casada y
#         la poblacion con otro estado civil es mayor a cero.
# Para ello definimos un valor de significancia alpha igual al 5%. (2)

# Paso 1
work_frame <- select(black_friday, Marital_Status, Purchase)
# Convierto en un factor el estado civil y lo modifico para la visualizacion
work_frame$Marital_Status <- as.factor(work_frame$Marital_Status)
levels(work_frame$Marital_Status)[
  levels(work_frame$Marital_Status)=="0"] <- "Otro"
levels(work_frame$Marital_Status)[
  levels(work_frame$Marital_Status)=="1"] <- "Casado/a"

# Paso 2  - test de hipotesis.
casados <- work_frame %>%
  filter(Marital_Status == "Casado/a")  %>%
  select(Purchase)
otros <-  work_frame %>%
  filter(Marital_Status == "Otro")  %>%
  select(Purchase)

alpha <- 0.05
conf_int <- 1 - alpha

t.test(casados, y = otros, alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = conf_int)
t.test(casados, y = otros, alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = TRUE,
       conf.level = conf_int)

# -------------------------------------------------------------------
# Consigna 8
# -------------------------------------------------------------------

# Se filtran la base para quedarse con las columnas:
# Purchase, City_Category (1)
#
# Finalmente, se realiza un test de hipÃ³tesis con:
# - H_0 : la diferencia de medias de consumo entre la ciudad X e Y es cero.
# - H_1 : la diferencia de medias de consumo entre la ciudad X e Y es distinta
#         de cero.
# Con X e Y la combinatoria de A, B y C tomado de pares (A-B, B-C, A-C).
# Para ello definimos un valor de significancia alpha igual al 5%. (2)

# Paso 1
work_frame <- select(black_friday, City_Category, Purchase)
ciudad_a <- work_frame %>%
  filter(City_Category == "A") %>%
  select(Purchase)
ciudad_b <- work_frame %>%
  filter(City_Category == "B") %>%
  select(Purchase)
ciudad_c <- work_frame %>%
  filter(City_Category == "C") %>%
  select(Purchase)


# Paso 2 - test de hipotesis
alpha <- 0.05
conf_int <- 1 - alpha

# Ciudad A vs Ciudad B 
t.test(ciudad_a, y = ciudad_b, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = conf_int)
t.test(ciudad_a, y = ciudad_b, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = TRUE,
       conf.level = conf_int)

# Ciudad B vs Ciudad C 
t.test(ciudad_b, y = ciudad_c, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = conf_int)
t.test(ciudad_b, y = ciudad_c, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = TRUE,
       conf.level = conf_int)

# Ciudad A vs Ciudad C 
t.test(ciudad_a, y = ciudad_c, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = conf_int)
t.test(ciudad_a, y = ciudad_c, alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = TRUE,
       conf.level = conf_int)

# -------------------------------------------------------------------
# Consigna 9
# -------------------------------------------------------------------

# Se filtran la base para quedarse con las columnas:
# Purchase, Gender, Age (26-35), Marital_Status(Casados) (1)
#
# Finalmente, se realiza un test de hipÃ³tesis con:
# - H_0 : la diferencia de medias de consumo entre las muejeres casadas entre
#         26-35 aÃ±os es menor o igual a la de los hombres casados entre 26-35
#         aÃ±os.
# - H_1 : la diferencia de medias de consumo entre las muejeres casadas entre
#         26-35 aÃ±os es mayor a la de los hombres casados entre 26-35 aÃ±os.
#
# Para ello definimos un valor de significancia alpha igual al 5%. (2)

# Paso 1
work_frame <- 
  black_friday %>%
  select(Gender, Age, Marital_Status, Purchase) %>%
  filter(Age == "26-35", Marital_Status == 1) %>%
  select(Gender, Purchase)
mujeres <- work_frame %>% filter(Gender == "F") %>% select(Purchase)
hombres <- work_frame %>% filter(Gender == "M") %>% select(Purchase)

# Paso 2 - Test de hipotesis
alpha <- 0.05
conf_int <- 1 - alpha

t.test(mujeres, y = hombres, alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = conf_int)
t.test(mujeres, y = hombres, alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = TRUE,
       conf.level = conf_int)

# -------------------------------------------------------------------
# Consigna 10
# -------------------------------------------------------------------

# Consistencia: cuando n --> inf, el estimador --> parametro
# Eficiencia: es comparativa de las varianzas de un estiamdor frente a otro.
# Sesgo: error sistematico de un estimador.

estimador_1 <- function(sample_, success_eq) {
  return (length(sample_[sample_$Gender == success_eq,]) / length(sample_$Gender))
}
estimador_2 <- function(sample_, success_eq, c) {
  return (length(sample_[sample_$Gender == success_eq,]) / (length(sample_$Gender) - c))
}

gender <- black_friday %>% select(Gender)

p_0 <- estimador_1(gender, "F")
sd_0 <- p_0*(1 - p_0) / length(gender$Gender)
  
n_samples <- c(10, 20, 50, 100, 200, 500, 1000, 2000, 5000)
n_samples_of_samples <- c(10, 20, 50, 100)

res <- data.frame()
k <- 1

for(i in seq(from = 1, to = length(n_samples), by = 1)) {
  for(j in seq(from = 1, to = length(n_samples_of_samples), by = 1)) {
    p_1 <- replicate(n=n_samples_of_samples[j],
                     expr = estimador_1(sample_n(gender, n_samples[i], replace = TRUE),
                                        "F"))
    
    c <- as.integer(n_samples[i] / 2)
    p_2 <- replicate(n=n_samples_of_samples[j],
                     expr = estimador_2(sample_n(gender, n_samples[i], replace = TRUE),
                                        "F", c))
    
    res[k, "n_samples"] <- n_samples[i]
    res[k, "n_samples_of_samples"] <- n_samples_of_samples[j]
    
    res[k, "mean_p1"] <- mean(p_1)
    res[k, "sd_p1"] <- sd(p_1)
    res[k, "bias_p1"] <- mean(p_1) - p_0
    # res[k, "rms_p1"] <- res[k, "sd_p1"] * res[k, "sd_p1"] + res[k, "bias_p1"] * res[k, "bias_p1"]
    
    res[k, "mean_p2"] <- mean(p_2)
    res[k, "sd_p2"] <- sd(p_2)
    res[k, "bias_p2"] <- mean(p_2) - p_0
    # res[k, "rms_p2"] <- res[k, "sd_p2"] *res[k, "sd_p2"]  + res[k, "bias_p2"] * res[k, "bias_p2"]
    res[k, "c"] <- c
    
    k <- k + 1
  }
}

res$rms_p1 <- res$sd_p1 * res$sd_p1 + res$bias_p1 * res$bias_p1

res$rms_p2 <- res$sd_p2 * res$sd_p2 + res$bias_p2 * res$bias_p2

# Plot comparativo de sesgos
ggplot(res[res$n_samples_of_samples == 100,], aes(x = n_samples)) +
  geom_smooth(aes(y = bias_p1, colour="P1"), se = FALSE) +
  geom_smooth(aes(y = bias_p2, colour="P2"), se = FALSE) +
  xlab("Numero de muestras") +
  ylab("Sesgo") +
  scale_colour_manual("Estimadores", breaks = c("P1", "P2"), values = c("blue", "red")) +
  ggtitle("Comparacion de sesgo promediando 100 computos del estimador")

# Plot comparativo de medias
ggplot(res[res$n_samples_of_samples == 100,], aes(x = n_samples)) +
  geom_smooth(aes(y = mean_p1, colour="P1"), se=FALSE) +
  geom_smooth(aes(y = mean_p2, colour="P2"), se=FALSE) +
  geom_hline(yintercept=p_0, linetype="dashed", color = "green") +
  scale_colour_manual("Estimadores", breaks = c("P1", "P2"), values = c("blue", "red")) +
  xlab("Numero de muestras") +
  ylab("Media") +
  ggtitle("Comparacion de medias promediando 100 computos del estimador")

# Plot comparativo de desvios
ggplot(res[res$n_samples_of_samples == 100,], aes(x = n_samples)) +
  geom_smooth(aes(y = sd_p1, colour="P1"), se=FALSE) +
  geom_smooth(aes(y = sd_p2, colour="P2"), se=FALSE) +
  scale_colour_manual("Estimadores", breaks = c("P1", "P2"), values = c("blue", "red")) +
  xlab("Numero de muestras") +
  ylab("Desvio") +
  ggtitle("Comparacion de desvios promediando 100 computos del estimador")


# Plot comparativo de RMS
ggplot(res[res$n_samples_of_samples == 100,], aes(x = n_samples)) +
  geom_smooth(aes(y = rms_p1, colour="P1"), se=FALSE) +
  geom_smooth(aes(y = rms_p2, colour="P2"), se=FALSE) +
  scale_colour_manual("Estimadores", breaks = c("P1", "P2"), values = c("blue", "red")) +
  xlab("Numero de muestras") +
  ylab("RMS") +
  ggtitle("Comparacion de RMS promediando 100 computos del estimador")
