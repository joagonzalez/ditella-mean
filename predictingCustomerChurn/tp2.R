# Trabajo Practico 2
# Metodos Estadisticos Aplicados a Negocios
#
# Agustin Alba Chicar <ag.albachicar@gmail.com>
# Joaquin Gonzalez <joagonzalez@gmail.com>
#
#
# Año: 2019
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
# install.packages("lmtest") # Ejecutar en caso de no tenerla instalada
library("lmtest")
# install.packages("mfx") # Ejecutar en caso de no tenerla instalada
library("mfx")
# install.packages("gmodels") # Ejecutar en caso de no tenerla instalada
library("gmodels")
# install.packages("corrplot") # Ejecutar en caso de no tenerla instalada
library("corrplot")
# install.packages("AER") # Ejecutar en caso de no tenerla instalada
library("AER") # Applied Econometrics
# install.packages("DescTools") # Ejecutar en caso de no tenerla instalada
library("DescTools")


# Para consigna 4 se aumenta el max.print
options(max.print=100)
# Se modifica formato de show decimals
options(scipen = 99999) 


# -------------------------------------------------------------------
# Limpieza del workspace
# -------------------------------------------------------------------
rm(list = ls())
cat("\f")

# Cargar el path donde este script se va a correer
path_to_workspace <- "C:/Users/jgonzalez/Desktop/Git/mean_tp2"
#path_to_workspace <- "/home/agustin/facultad/ditella/mean/tps/tp2/repo"
setwd(path_to_workspace)
# -------------------------------------------------------------------
# Carga de informacion
# -------------------------------------------------------------------
qwe_churn <- read.table("UV6696-XLS-ENG-Caso_Customer_Churn.csv", 
                        header = TRUE, sep = ",")

# -------------------------------------------------------------------
# Consigna 1
# -------------------------------------------------------------------
# Informaciones generales de la base de datos
labels(qwe_churn)[2]
nrow(qwe_churn)
summary(qwe_churn)
head(qwe_churn)

# Edad de los clientes
customer_age <- mean(qwe_churn$Customer_Age)             ; customer_age
sd_customer_age <- sd(qwe_churn$Customer_Age)            ; sd_customer_age
t.test(x = qwe_churn$Customer_Age,
       alternative = c("two.sided"),
       mu = customer_age,
       conf.level = 0.95)

# Retencion de los clientes
num_customers <- length(qwe_churn$ID)                   ; num_customers
retention <- prop.table(table(qwe_churn$Churn))["0"]    ; retention
sd_retention <- sqrt(retention * (1 - retention) / num_customers); sd_retention
prop.test(x = table(qwe_churn$Churn),
          n = num_customers,
          alternative = c("two.sided"),
          conf.level = 0.95,
          correct = FALSE)

# Initial - End CHI Score
chi_score <- data.frame("start" = qwe_churn$CHI_Score_Month,
                        "end" = qwe_churn$CHI_Score_Month + qwe_churn$CHI_Score)

# Computo las medias con y sin el cero para entender como se distribuye la informacion
# Existe un numero considerable, alrededor de 1200 muestras que no tienen la informacion
# del CHI.
chi_score_start <- mean(chi_score$start)                    ; chi_score_start
chi_score_start_nz <- mean(chi_score$start[chi_score$start != 0]); chi_score_start_nz

chi_score_end <- mean(chi_score$end)                    ; chi_score_end
chi_score_end_nz <- mean(chi_score$end[chi_score$end != 0]); chi_score_end_nz

ggplot(chi_score, aes(x=start)) +
  geom_histogram(binwidth=10, color="blue", fill="white") +
  geom_vline(aes(xintercept=chi_score_start),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=chi_score_start_nz),
             color="green", linetype="dashed", size=1) +
  labs(title="Distribucion inicial de CHI", 
       x="CHI inical",
       y = "Frecuencia de CHI")

ggplot(chi_score, aes(x=end)) +
  geom_histogram(binwidth=10, color="blue", fill="white") +
  geom_vline(aes(xintercept=chi_score_end),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=chi_score_end_nz),
             color="green", linetype="dashed", size=1) +
  labs(title="Distribución final de CHI", 
       x="CHI final",
       y = "Frecuencia de CHI")

# Initial - End Support cases
support_cases <- data.frame("start" = qwe_churn$Support_Cases_Month,
                            "end" = qwe_churn$Support_Cases_Month + qwe_churn$Support_Cases)

support_cases_start <- mean(support_cases$start)  ; support_cases_start
support_cases_end <- mean(support_cases$end)      ; support_cases_end

t.test(support_cases$start,
       alternative = c("two.sided"),
       mu = support_cases_start,
       conf.level = 0.95)
t.test(support_cases$end,
       alternative = c("two.sided"),
       mu = support_cases_end,
       conf.level = 0.95)

ggplot(support_cases) +
  geom_histogram(mapping = aes(x=start, colour="Inicial"), fill="white", binwidth=1, alpha=0.5) +
  geom_histogram(mapping = aes(x=end, colour="Final"), fill="white", binwidth=1, alpha=0.5) +
  geom_vline(aes(xintercept=support_cases_start, colour="Inicial"),
             linetype="dashed", size=1, alpha=0.5) +
  geom_vline(aes(xintercept=support_cases_end, colour="Final"),
             linetype="dashed", size=1, alpha=0.5) +
  labs(title="Distribucion de casos de soporte", 
       x="Casos de soporte",
       y = "Frecuencia de casos de soporte") +
  xlim(-20, 20) +
  scale_colour_manual("Muestras", breaks = c("Inicial", "Final"), values = c("blue", "red"))

# Support Priority
# TODO(agalbachicar)    Ver issue #2
sp <- data.frame("start" = qwe_churn$SP_Month, "end" = qwe_churn$SP)

sp_start_prop <- prop.table(table(sp$start)) ; sp_start_prop[1] ; sp_start_prop[2]
sp_start_end <- prop.table(table(sp$end)) ; sp_start_end[1] ; sp_start_end[2]

# User usage data
logins <- mean(qwe_churn$Logins)
t.test(qwe_churn$Logins,
       alternative = c("two.sided"),
       mu = logins,
       conf.level = 0.95)

blogs <- mean(qwe_churn$Blog_Articles)
t.test(qwe_churn$Blog_Articles,
       alternative = c("two.sided"),
       mu = blogs,
       conf.level = 0.95)

views <- mean(qwe_churn$Views)
t.test(qwe_churn$Views,
       alternative = c("two.sided"),
       mu = views,
       conf.level = 0.95)

days_since_login <- mean(qwe_churn$Days_Since_Last_Login)
t.test(qwe_churn$Days_Since_Last_Login,
       alternative = c("two.sided"),
       mu = days_since_login,
       conf.level = 0.95)

# -------------------------------------------------------------------
# Consigna 2
# -------------------------------------------------------------------
# Se debe generar un training set y un testing set con la base general.
# Luego, comparar todas las medidas una contra la otra con el fin de 
# conocer que tan confiables son las estimaciones.
training_prop <- 0.8
indexes <- as.integer(labels(qwe_churn)[1][[1]])
training_indexes <- sample(indexes, 0.8 * length(indexes))
testing_indexes <- match(indexes, indexes, incomparables = training_indexes)
testing_indexes <- testing_indexes[complete.cases(testing_indexes)]

training_set <- qwe_churn[training_indexes,]
testing_set <- qwe_churn[testing_indexes,]

# Se recalculan los estadisticos representativos, pero comparando contra
# los valores de media previos. Los mismos seran considerados como los
# pobacionales con el fin de comparar las estimaciones.

# Edad de los clientes del training set
customer_age_t <- mean(training_set$Customer_Age)             ; customer_age_t
sd_customer_age_t <- sd(training_set$Customer_Age)            ; sd_customer_age_t
t.test(x = training_set$Customer_Age,
       alternative = c("two.sided"),
       mu = customer_age,
       conf.level = 0.95)

# Retencion de los clientes del training set
num_customers_t <- length(training_set$ID)                   ; num_customers_t
retention_t <- prop.table(table(training_set$Churn))["0"]    ; retention_t
sd_retention_t <- sqrt(retention_t * (1 - retention_t) / num_customers_t); sd_retention_t
binom.test(x = table(training_set$Churn),
           n = num_customers_t,
           p = retention,
           alternative = c("two.sided"),
          conf.level = 0.95)

# Initial - End CHI Score del training set
chi_score_t <- data.frame("start" = training_set$CHI_Score_Month,
                          "end" = training_set$CHI_Score_Month + training_set$CHI_Score)

t.test(chi_score_t$start, alternative = c("two.sided"), mu = chi_score_start,
       conf.level = 0.95)

t.test(chi_score_t$end, alternative = c("two.sided"), mu = chi_score_end,
       conf.level = 0.95)

# Support Priority del training set.

# User usage data del training set.
t.test(training_set$Logins,
       alternative = c("two.sided"),
       mu = logins,
       conf.level = 0.95)

t.test(training_set$Blog_Articles,
       alternative = c("two.sided"),
       mu = blogs,
       conf.level = 0.95)

t.test(training_set$Views,
       alternative = c("two.sided"),
       mu = views,
       conf.level = 0.95)

t.test(training_set$Days_Since_Last_Login,
       alternative = c("two.sided"),
       mu = days_since_login,
       conf.level = 0.95)


# -------------------------------------------------------------------
# Consigna 3
# -------------------------------------------------------------------
# Realizar un modelo lineal de probabilidad que estime la
# probabilidad que un cliente cancele el contrato utilizando el
# training_set y la edad del cliente

# Modelo complejo tomando todas las variables juntas
modelo <- lm(training_set$Churn~training_set$Customer_Age+
               training_set$CHI_Score_Month+
               training_set$CHI_Score+
               training_set$Support_Cases_Month+
               training_set$Support_Cases+
               training_set$SP_Month+
               training_set$SP+
               training_set$Logins+
               training_set$Blog_Articles+
               training_set$Views+
               training_set$Days_Since_Last_Login, 
             data=training_set)
coeftest(modelo, vcov = vcovHC(modelo, type="HC1")) #estimo usando SE robustos a heterocedasticidad, correccion necesaria para modelos lm

summary(modelo)

pred <- predict(modelo)
err <- training_set$Churn - pred 
pred_modelo <- data.frame(pred=pred, err=err, truth=training_set$Churn) 

ggplot(data=pred_modelo, aes(x=as.factor(truth), y=pred)) +
  geom_boxplot(outlier.size = 1) +
  labs(title="Modelo A", 
       x="Training set - Churn",
       y = "Distribucion de las predicciones para cada categoria")

# Modelo mas simple, dejando unicamente beta_i con un nivel de significacion
# de 95% o mas.
modelo <- lm(training_set$Churn~training_set$Customer_Age+
               training_set$CHI_Score_Month+
               training_set$CHI_Score+
               training_set$Views+
               training_set$Days_Since_Last_Login, 
             data=training_set)
coeftest(modelo, vcov = vcovHC(modelo, type="HC1")) #estimo usando SE robustos a heterocedasticidad, correccion necesaria para modelos lm

# Problemas con MLP (modelo lineal de probabilidad):
# 1) No se garantiza que la probabilidad este entre 0 y 1
# 2) El efecto de X en p(y=1|beta) es SIEMPRE LINEAL
# 3) El error del modelo es heteroced�stico, no es constante para distintos valores de X, entonces debe calcularse con errores est�ndares robustos

summary(modelo)

pred <- predict(modelo)
err <- training_set$Churn - pred 
pred_modelo <- data.frame(pred=pred, err=err, truth=training_set$Churn) 

ggplot(data=pred_modelo, aes(x=as.factor(truth), y=pred)) +
  geom_boxplot(outlier.size = 1) +
  labs(title="Modelo B", 
       x="Training set - Churn",
       y = "Distribucion de las predicciones para cada categoria")


# Vemos limitaciones del modelo lineal y ventajas de modelo logit para predicci�n de variables dicotomicas
training_set %>% ggplot(aes(x=CHI_Score_Month,y=Churn))+
  geom_point(aes(col=training_set$Churn))+
  geom_smooth(method = "lm", col="darkviolet")+
  stat_smooth(method="glm", method.args=list(family = binomial(link = "logit")), col="red", se=TRUE)

# Vemos limitaciones del modelo lineal y ventajas de modelo logit para predicci�n de variables dicotomicas
training_set %>% ggplot(aes(x=Days_Since_Last_Login,y=Churn))+
  geom_point(aes(col=training_set$Churn))+
  geom_smooth(method = "lm", col="darkgreen")+
  stat_smooth(method="glm", method.args=list(family = binomial(link = "logit")), col="red", se=TRUE)

# Se estima por maxmia verosimilitud


# -------------------------------------------------------------------
# Consigna 4
# -------------------------------------------------------------------
# Probit y Logit ofrecen otra forma funcional (no lineal) que se ajusta mejor a las predicciones con variables dicot�micas
# 1) No pueden ser estimados por MCO pues no lineales en los parametros
# 2) Se estima por maxima versimilitud (glm())

model_logit <- glm(Churn~Customer_Age+
               CHI_Score_Month+
               CHI_Score+
               Views+
               Days_Since_Last_Login, 
             data=training_set, 
             family = binomial(link = "logit"))
summary(model_logit)
predict(model_logit)

model_probit <- glm(Churn~Customer_Age+
                CHI_Score_Month+
                CHI_Score+
                Views+
                Days_Since_Last_Login, 
              data=training_set, 
             family = binomial(link = "probit"))
summary(model_probit)
predict(model_probit)

PseudoR2(model_logit, c("McFadden", "Nagel"))
PseudoR2(model_probit, c("McFadden", "Nagel"))

# An�lisis para modelo logit
# Estimate: coeficientes estimados en forma de logit (logaritmo de la razon de chances)
# Los valores de la columna "estimate" indican como cambia el logaritmo de la razon de probabilidades a favor de que un cliente cancele la subscripci�n (CHURN=1), ante cambios unitarios en una de las variables independientes, manteniendose constantes las restantes.

#En terminos de OR, podemos establecer una relaci�n cuantitativa en "veces" de los coeficientes!!!
exp(cbind(OR = coef(model_logit), confint(model_logit)))
logitor(training_set$Churn~training_set$Customer_Age+
          training_set$CHI_Score_Month+
          training_set$CHI_Score+
          training_set$Views+
          training_set$Days_Since_Last_Login, data=training_set)


# -------------------------------------------------------------------
# Consigna 5
# -------------------------------------------------------------------
# Funcion calculo de probabilidad

# Coeficientes
beta0 <- model_logit$coefficients[1]
betaCustomerAge <- model_logit$coefficients[2]
betaChiScoreMonth <- model_logit$coefficients[3]
betaChiScore <- model_logit$coefficients[4]
betaViews <- model_logit$coefficients[5]
betaDaysLogin <- model_logit$coefficients[6]

# Calculamos la probabilidad para distintos valores de customerAge para modelo logit a "mano"
customerAge <- c(1:1500)
p <- exp(beta0+
           betaCustomerAge*customerAge+
           betaChiScore*mean(training_set$CHI_Score)+
           betaChiScoreMonth*mean(training_set$CHI_Score_Month)+
           betaViews*mean(training_set$Views)+
           betaDaysLogin*mean(training_set$Days_Since_Last_Login)
         )/(1+exp(beta0+
                    betaCustomerAge*customerAge+
                    betaChiScore*mean(training_set$CHI_Score)+
                    betaChiScoreMonth*mean(training_set$CHI_Score_Month)+
                    betaViews*mean(training_set$Views)+
                    betaDaysLogin*mean(training_set$Days_Since_Last_Login)
         ))

plot(customerAge,p, col="darkblue")


# graficamos probabilidad vs CustomerAge para modelos probit y logit. Utilizamos rango de 1:1500 considerando que el maximo del dataframe real es 67
CustomerAgeProb <- seq(from = 1, to = 1500, by = 1) 



# Corremos predicciones sobre sobre testing_set
newdata_testing <- data.frame(Customer_Age = CustomerAgeProb,
                              CHI_Score = mean(testing_set$CHI_Score),
                              CHI_Score_Month = mean(testing_set$CHI_Score_Month),
                              Views = mean(testing_set$Views),
                              Days_Since_Last_Login = mean(testing_set$Days_Since_Last_Login))

p_logit_testing <- predict(model_logit, newdata = newdata_testing, type="response")
plot(CustomerAgeProb,p_logit, col="darkgreen", xlab='Customer Age', ylab='Probability', main="Model Logit - Testing")

p_probit_testing <- predict(model_probit, newdata = newdata_testing, type="response")
plot(CustomerAgeProb,p_probit, col="darkblue", xlab='Customer Age', ylab='Probability', main="Model Probit - Testing")


# Tambien graficamos sobre training test
newdata <- data.frame(Customer_Age = CustomerAgeProb,
  CHI_Score = mean(training_set$CHI_Score),
  CHI_Score_Month = mean(training_set$CHI_Score_Month),
  Views = mean(training_set$Views),
  Days_Since_Last_Login = mean(training_set$Days_Since_Last_Login))

p_logit <- predict(model_logit, newdata = newdata, type="response")
plot(CustomerAgeProb,p_logit, col="darkviolet", xlab='Customer Age', ylab='Probability', main="Model Logit - Training")

p_probit <- predict(model_probit, newdata = newdata, type="response")
plot(CustomerAgeProb,p_probit, col="darkred", xlab='Customer Age', ylab='Probability', main="Model Probit - Training")


# -------------------------------------------------------------------
# Consigna 6
# -------------------------------------------------------------------
# Corremos predicciones sobre sobre training_set
p_logit_training <- predict(model_logit, newdata = training_set, type="response")
p_probit_training <- predict(model_probit, newdata = training_set, type="response")

#training_set$coded_churn <- ifelse(training_set$Churn==1,"Yes","No")
names(training_set)
summary(training_set)


##########################
#     MODELO LOGIT      #
#########################

# Calculamos y Clasificamos las predicciones
predicted_value_logit <- p_logit_training
predicted_class_logit <- ifelse(predicted_value_logit>0.5,"Yes", "No")
# Usamos para verificar el valor que predice el modelo an base al cual toma la desici�n
predicted_class_logit_tbshoot <- ifelse(predicted_value_logit>0.5,sprintf("Yes %f",predicted_value_logit), sprintf("No %f",predicted_value_logit))

# Generamos dataframe de observado vs predicciones en el conjunto testing
performance_data_logit <- data.frame(observed=training_set$Churn, predicted=predicted_class_logit)
performance_data_logit_tbshoot <- data.frame(observed=training_set$Churn, predicted=predicted_class_logit_tbshoot)
error_pred_logit <- data.frame(observed=training_set$Churn, predicted=predicted_value_logit, error=training_set$Churn-predicted_value_logit)


total <- length(training_set$ID)
positive <- sum(performance_data_logit$observed=="1")
negative <-sum(performance_data_logit$observed=="0")
predicted_positive <- sum(performance_data_logit$predicted=="Yes")
predicted_negative <- sum(performance_data_logit$predicted=="No")

# Hallamos los valores de la matriz de confusi�n
VP <- sum(performance_data_logit$observed=="1" & performance_data_logit$predicted=="Yes")
VN <- sum(performance_data_logit$observed=="0" & performance_data_logit$predicted=="No")
FP <- sum(performance_data_logit$observed=="0" & performance_data_logit$predicted=="Yes")
FN <- sum(performance_data_logit$observed=="1" & performance_data_logit$predicted=="No")

PCR_logit <- VP + VN / total


##########################
#     MODELO PROBIT     #
########################

# Calculamos y Clasificamos las predicciones
predicted_value_probit <- p_probit_training
predicted_class_probit <- ifelse(predicted_value_probit>0.5,"Yes", "No")
# Usamos para verificar el valor que predice el modelo an base al cual toma la desici�n
predicted_class_probit_tbshoot <- ifelse(predicted_value_probit>0.5,sprintf("Yes %f",predicted_value_probit), sprintf("No %f",predicted_value_probit))

# Generamos dataframe de observado vs predicciones en el conjunto training
performance_data_probit <- data.frame(observed=training_set$Churn, predicted=predicted_class_probit)
performance_data_probit_tbshoot <- data.frame(observed=training_set$Churn, predicted=predicted_class_probit_tbshoot)
error_pred_probit <- data.frame(observed=training_set$Churn, predicted=predicted_value_probit, error=training_set$Churn-predicted_value_probit)


total <- length(training_set$ID)
positive <- sum(performance_data_probit$observed=="1")
negative <-sum(performance_data_probit$observed=="0")
predicted_positive <- sum(performance_data_probit$predicted=="Yes")
predicted_negative <- sum(performance_data_probit$predicted=="No")

# Hallamos los valores de la matriz de confusi�n
VP <- sum(performance_data_probit$observed=="1" & performance_data_probit$predicted=="Yes")
VN <- sum(performance_data_probit$observed=="0" & performance_data_probit$predicted=="No")
FP <- sum(performance_data_probit$observed=="0" & performance_data_probit$predicted=="Yes")
FN <- sum(performance_data_probit$observed=="1" & performance_data_probit$predicted=="No")

PCR_probit <- VP + VN / total

# -------------------------------------------------------------------
# Consigna 7
# -------------------------------------------------------------------


#######################################################
# Calculamos Error_pred_probit y logit para testing set
# Esto se vuelve a realizar en ejercicio 10
######################################################
# Corremos predicciones sobre sobre testing_set
p_logit_testing <- predict(model_logit, newdata = testing_set, type="response")
p_probit_testing <- predict(model_probit, newdata = testing_set, type="response")

# Calculamos y Clasificamos las predicciones
predicted_value_logit <- p_logit_testing
predicted_class_logit <- ifelse(predicted_value_logit>0.5,"Yes", "No")
# Usamos para verificar el valor que predice el modelo an base al cual toma la desici�n
predicted_class_logit_tbshoot <- ifelse(predicted_value_logit>0.5,sprintf("Yes %f",predicted_value_logit), sprintf("No %f",predicted_value_logit))

# Generamos dataframe de observado vs predicciones en el conjunto testing
performance_data_logit <- data.frame(observed=testing_set$Churn, predicted=predicted_class_logit)
performance_data_logit_tbshoot <- data.frame(observed=testing_set$Churn, predicted=predicted_class_logit_tbshoot)
error_pred_logit <- data.frame(observed=testing_set$Churn, predicted=predicted_value_logit, error=testing_set$Churn-predicted_value_logit)

# Calculamos y Clasificamos las predicciones
predicted_value_probit <- p_probit_testing
predicted_class_probit <- ifelse(predicted_value_probit>0.5,"Yes", "No")
# Usamos para verificar el valor que predice el modelo an base al cual toma la desici�n
predicted_class_probit_tbshoot <- ifelse(predicted_value_probit>0.5,sprintf("Yes %f",predicted_value_probit), sprintf("No %f",predicted_value_probit))

# Generamos dataframe de observado vs predicciones en el conjunto testing
performance_data_probit <- data.frame(observed=testing_set$Churn, predicted=predicted_class_probit)
performance_data_probit_tbshoot <- data.frame(observed=testing_set$Churn, predicted=predicted_class_probit_tbshoot)
error_pred_probit <- data.frame(observed=testing_set$Churn, predicted=predicted_value_probit, error=testing_set$Churn-predicted_value_probit)

######################################################

# Pred Modelo MLP
error_pred_mlp <- pred_modelo
error_pred_mlp
# Error Medio
me_mlp <- sum(abs(error_pred_mlp$err))/length(error_pred_mlp$pred)
me_mlp
# Varianza del error
var_mlp <- sum(abs(error_pred_mlp$err-me_mlp)^2)/length(error_pred_mlp$pred)
var_mlp

# Predicciones modelo probit
error_pred_probit
# Error Medio
me_probit <- sum(abs(error_pred_probit$error))/length(error_pred_probit$predicted)
me_probit
# Varianza del error
var_probit <- sum(abs(error_pred_probit$error-me_probit)^2)/length(error_pred_probit$predicted)
var_probit

#Predicciones modelo logit
error_pred_logit
# Error Medio
me_logit <- sum(abs(error_pred_logit$error))/length(error_pred_logit$predicted)
me_logit
# Varianza del error
var_logit <- sum(abs(error_pred_logit$error-me_logit)^2)/length(error_pred_logit$predicted)
var_logit


# La magnitud del error es altamente significativa para los falsos negativos. El sesgo se mantiene independientemente del modelo utilizado
# Errores medios bajos y varianzas grandes respecto de esa media (50%)

# PENDIENTE: PENSAR PLOT DE ERRORES
#Grafiquen los errores de pron�stico:
plot(error_pred_mlp$err, col="darkorange", xlab='Index', ylab='Error', main="Model MLP - Testing")
abline(h=0, col="darkblue")


plot(error_pred_logit$error, col="darkgreen", xlab='Index', ylab='Error', main="Model Logit - Testing")
abline(h=0, col="darkorange")

plot(error_pred_probit$error, col="blue", xlab='Index', ylab='Error', main="Model Probit - Testing")
abline(h=0, col="red")


# -------------------------------------------------------------------
# Consigna 8
# -------------------------------------------------------------------

# Definimos funcion de error porcentual
error_porcentual <- function(y, y_hat){
  ep <- (y-y_hat)/y * 100
  return(as.numeric(ep))
}

# Definimos funcion del error medio absoluto
mae <- function(actual,pred){
  mae <- mean(abs(actual - pred))
  return (mae)
}

# Definimos funcion del error medio cuadratico
rmse <- function(actual,pred){
  rmse <- sqrt(mean((actual - pred)^2))
  return (rmse)
}

# Definimos funcion de error medio absoluto porcentual (MAPE)
mape <- function(actual,pred){
  mape <- mean((abs(actual - pred)/actual) * 100)
  return (mape)
}

# Definimos funcion de la raiz del error medio cuadratico porcentual (RMSPE)
rmspe <- function(actual,pred){
  rmspe <- sqrt(mean((((actual - pred)/actual) * 100)^2))
  return (rmspe)
}

mape_mlp <- mape(error_pred_mlp$truth, error_pred_mlp$pred)
mape_logit <- mape(error_pred_logit$observed, error_pred_logit$predicted)
mape_probit <- mape(error_pred_probit$observed, error_pred_probit$predicted)

rmse_mlp <- rmse(error_pred_mlp$truth, error_pred_mlp$pred)
rmse_logit <- rmse(error_pred_logit$observed, error_pred_logit$predicted)
rmse_probit <- rmse(error_pred_probit$observed, error_pred_probit$predicted)


# -------------------------------------------------------------------
# Consigna 9
# -------------------------------------------------------------------





# -------------------------------------------------------------------
# Consigna 10
# -------------------------------------------------------------------

# Corremos predicciones sobre sobre testing_set
p_logit_testing <- predict(model_logit, newdata = testing_set, type="response")
p_probit_testing <- predict(model_probit, newdata = testing_set, type="response")

#training_set$coded_churn <- ifelse(training_set$Churn==1,"Yes","No")
names(testing_set)
summary(testing_set)


##########################
#     MODELO LOGIT      #
#########################

# Calculamos y Clasificamos las predicciones
predicted_value_logit <- p_logit_testing
predicted_class_logit <- ifelse(predicted_value_logit>0.5,"Yes", "No")
# Usamos para verificar el valor que predice el modelo an base al cual toma la desici�n
predicted_class_logit_tbshoot <- ifelse(predicted_value_logit>0.5,sprintf("Yes %f",predicted_value_logit), sprintf("No %f",predicted_value_logit))

# Generamos dataframe de observado vs predicciones en el conjunto testing
performance_data_logit <- data.frame(observed=testing_set$Churn, predicted=predicted_class_logit)
performance_data_logit_tbshoot <- data.frame(observed=testing_set$Churn, predicted=predicted_class_logit_tbshoot)
error_pred_logit <- data.frame(observed=testing_set$Churn, predicted=predicted_value_logit, error=testing_set$Churn-predicted_value_logit)


total <- length(testing_set$ID)
positive <- sum(performance_data_logit$observed=="1")
negative <-sum(performance_data_logit$observed=="0")
predicted_positive <- sum(performance_data_logit$predicted=="Yes")
predicted_negative <- sum(performance_data_logit$predicted=="No")

# Hallamos los valores de la matriz de confusi�n
VP <- sum(performance_data_logit$observed=="1" & performance_data_logit$predicted=="Yes")
VN <- sum(performance_data_logit$observed=="0" & performance_data_logit$predicted=="No")
FP <- sum(performance_data_logit$observed=="0" & performance_data_logit$predicted=="Yes")
FN <- sum(performance_data_logit$observed=="1" & performance_data_logit$predicted=="No")

PCR_logit <- VP + VN / total

##########################
#     MODELO PROBIT     #
########################

# Calculamos y Clasificamos las predicciones
predicted_value_probit <- p_probit_testing
predicted_class_probit <- ifelse(predicted_value_probit>0.5,"Yes", "No")
# Usamos para verificar el valor que predice el modelo an base al cual toma la desici�n
predicted_class_probit_tbshoot <- ifelse(predicted_value_probit>0.5,sprintf("Yes %f",predicted_value_probit), sprintf("No %f",predicted_value_probit))

# Generamos dataframe de observado vs predicciones en el conjunto testing
performance_data_probit <- data.frame(observed=testing_set$Churn, predicted=predicted_class_probit)
performance_data_probit_tbshoot <- data.frame(observed=testing_set$Churn, predicted=predicted_class_probit_tbshoot)
error_pred_probit <- data.frame(observed=testing_set$Churn, predicted=predicted_value_probit, error=testing_set$Churn-predicted_value_probit)


total <- length(testing_set$ID)
positive <- sum(performance_data_probit$observed=="1")
negative <-sum(performance_data_probit$observed=="0")
predicted_positive <- sum(performance_data_probit$predicted=="Yes")
predicted_negative <- sum(performance_data_probit$predicted=="No")

# Hallamos los valores de la matriz de confusi�n
VP <- sum(performance_data_probit$observed=="1" & performance_data_probit$predicted=="Yes")
VN <- sum(performance_data_probit$observed=="0" & performance_data_probit$predicted=="No")
FP <- sum(performance_data_probit$observed=="0" & performance_data_probit$predicted=="Yes")
FN <- sum(performance_data_probit$observed=="1" & performance_data_probit$predicted=="No")

PCR_probit <- VP + VN / total

