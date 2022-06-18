# Cargamos las librerias necesarias
library(readxl)
library(dplyr)
library(tidyverse)
library(corrplot)
library(lubridate)
library(plotly)

options(scipen=999)

# Fijamos el directorio de trabajo
setwd("~/Desktop/Máster/Segundo cuatrimestre/Obligatorias/Visualización de datos/R")

# Bibliografía
# https://www.kaggle.com/code/thasnihakeem/walmart-recruiting-store-sales-forecasting-predict/notebook
# https://www.datafied.world/walmart-store-sales-forecasting-106
# https://www.kaggle.com/code/fernandol/cracking-the-walmart-sales-forecasting-challenge/notebook
# https://medium.com/analytics-vidhya/how-to-use-historical-markdown-data-to-predict-store-sales-f670af542033

# Juntamos las distintas fuentes de datos ----
# Cargamos el conjunto de datos de entrenamiento
train <- read_csv("~/Documents/My Tableau Repository/Datasources/Archive/train.csv") %>% distinct() 
train <- train %>% mutate(conjunto = "train")
# Cargamos el conjunto de datos de test y le añadimos la información del outcome weekly_sales, que se encuentra en el archivo sample_submission
test <- read_csv("~/Documents/My Tableau Repository/Datasources/Archive/test.csv") %>% distinct() %>% mutate(id = paste(Store, Dept, Date, sep = "_"))
# Cargamos el conjunto de datos sample_submission
sample_submission <- read_csv("~/Documents/My Tableau Repository/Datasources/Archive/sample_submission.csv")
test <- inner_join(test, sample_submission) %>% select(-id) %>% mutate(conjunto = "test") %>% select(names(train))

# Combinamos el conjunto de entrenamiento y el de test
union <- rbind(train, test) %>% arrange(Date, Store, Dept)

# Cargamos el conjunto de datos complementario con la información bursatil
WMT <- read_csv("~/Documents/My Tableau Repository/Datasources/Archive/WMT.csv") %>% distinct()

# Vemos que fechas de cotización no están recogidas en WMT pero sí que tenemos información de union
fechas_no <- anti_join(union %>% select(Date), WMT) %>% distinct()

# Comprobamos que no tenga fechas duplicadas
prueba <- WMT[duplicated(WMT$Date)==TRUE,]

# Vemos cuanta información de WMT podemos usar en train
# Se puede utilizar la mayor parte de la información, train: 274534/282451 (el 97,2%)
# Train + test 409727/421570 (el 97,2%)
prueba <- inner_join(union, WMT)

# Conjunto de datos final, hacemos un left_join para no perder las observaciones del conjunto de entrenamiento
#union <- left_join(train, WMT) %>% distinct()
union <- left_join(union, WMT) %>% distinct()

# Guardamos el objeto union con el que poder seguir trabajando
saveRDS(union, file = "union.RDS")

# Liberamos memoria
rm(list=setdiff(ls(), "union"))

# Guardamos la unión
union <- write.table(union, file = "union.txt", row.names = F, sep = "\t")

# Hacemos pre-procesado básico ----
setwd("~/Desktop/Máster/Segundo cuatrimestre/Obligatorias/Visualización de datos/R")
union <- readRDS("~/Desktop/Máster/Segundo cuatrimestre/Obligatorias/Visualización de datos/R/union.RDS")

# Convertimos unidades
union$Temperature <- round((union$Temperature - 32) * 5/9, digits = 2)

# Intervalos temporales
# Creamos una columna de año, mes y semana del año
union$Week <- lubridate::week(ymd(union$Date))
union$Month <- lubridate::month(ymd(union$Date))
union$Quarter <- lubridate::quarter(ymd(union$Date))
union$Year <- lubridate::year(ymd(union$Date))

# Métricas ----
# Número de filas
nrow(union)

# ¿Reemplazar los missing de los markdown con 0?
# Describimos el conjunto de datos, en busca de outliers
Hmisc::describe(union)

# Borrar
#x <- union %>% select(Date, Weekly_Sales) %>% distinct()
#quantile(x$Weekly_Sales)

quantile(union$Weekly_Sales)
x <- union %>% select(Date, IsHoliday) %>% distinct()
prop.table(table(x$IsHoliday))*100
table(union$IsHoliday)

x <- union %>% select(Date, Temperature) %>% distinct()
quantile(x$Temperature)
quantile(union$Temperature)
x <- union %>% select(Date, Fuel_Price) %>% distinct()
quantile(union$Fuel_Price)
quantile(x$Fuel_Price)
quantile(union$CPI)
quantile(union$Unemployment)
prop.table(table(union$Type))*100
quantile(union$Size)
quantile(union$MarkDown1, na.rm = T)
quantile(union$MarkDown2, na.rm = T)
quantile(union$MarkDown3, na.rm = T)
quantile(union$MarkDown4, na.rm = T)
quantile(union$MarkDown5, na.rm = T)
quantile(union$Close, na.rm = T)
quantile(union$Volume, na.rm = T)
quantile(union$Open, na.rm = T)
quantile(union$High, na.rm = T)
quantile(union$Low, na.rm = T)
quantile(union$`Adj Close`, na.rm = T)

# Representamos los datos
# plot(union)

# Resumen estadístico de los datos
summary(union)

# Más resúmenes estadísticos de datos
skim_data <- function(df, vars=NULL) {
  df<-dplyr::as_tibble(df)
  if (is.null(vars) == TRUE) vars <- names(df)
  
  variable_type <- sapply(vars,
                          function(x) is(df[, x][[1]])[1])
  missing_count <- sapply(vars,
                          function(x) sum(!complete.cases(df[, x])))
  unique_count <- sapply(vars,
                         function(x) dplyr::n_distinct(df[, x]))
  data_count <- nrow(dplyr::as_tibble(df))
  Example <- sapply(vars,
                    function(x) (df[1, x]))
  
  dplyr::tibble(variables = vars, types = variable_type,
                Example = Example,
                missing_count = missing_count,
                missing_percent = (missing_count / data_count) * 100,
                unique_count = unique_count,
                Total_data = data_count)
}
knitr::kable(skim_data(union))

# Histograma de los datos
#hist.data.frame(data)

# Análisis de correlación entre valores continuos ----
# Calculamos la correlación de los datos
# https://stackoverflow.com/questions/7445639/dealing-with-missing-values-for-correlations-calculation
correlacion <- union %>%
  select_if(is.numeric) %>%
  cor(method = c("pearson"), use = "pairwise.complete.obs") %>%
  round(digits = 2)

# Correlación de Pearson
corrplot(correlacion, method = 'number', type = "upper")

# Relación entre variables ----
# Vamos a ir viendo la relación de variables dos a dos
# Relación entre tiendas y departamentos:
# Cuantas tiendas hay en cada departamento
table(train$Store, train$Dept)
table(train$Dept, train$Store)
 
relacion_store_dept <- union %>%
  select(Store, Dept) %>%
  distinct() %>%
  group_by(Dept) %>%
  summarise_all(max)
# En vista a los resultados entendemos que cada tienda tiene su propio departamento
# HACER ANÁLISIS TAMBIÉN POR DEPARTAMENTOS A VER CUAL VA MEJOR

# Relación entre tamaño y tipo de tienda
table(union$Size, union$Type)

# Vemos el número de semanas únicas y si falta alguna semana o son consecutivas
semanas <- union %>%
  select(Date) %>%
  distinct() %>%
  mutate(nueva = Date + 7, verificacion = ifelse(Date == lag(nueva), 1, 0))

table(semanas$verificacion)

# HACER VISUALIZACIÓN de:
# Vemos el número de departamentos que tienen/no tiene cada tienda
n_departamentos <- union %>% select(Store, Dept) %>% distinct()

# Relación fechas-tiendas: 143 fechas * 45 tiendas
fecha_tienda <- union %>% select(-Dept, -Weekly_Sales, -conjunto) %>% distinct()

# Calculamos las ventas por mes, cuatrimestre y ano
union <- union %>%
  #group_by(Store, Dept, ano, semana) %>% 
  #mutate(Weeky_Sales2 = sum(Weekly_Sales)) %>%
  group_by(Store, Dept, Year, Month) %>%
  mutate(Month_Sales = sum(Weekly_Sales)) %>%
  group_by(Store, Dept, Year, Quarter) %>%
  mutate(Quarter_Sales = sum(Weekly_Sales)) %>%
  group_by(Store, Dept, Year) %>%
  mutate(Year_Sales = sum(Weekly_Sales))

# Podríamos comprobar que los datos están bien calculados
#all.equal(union$Weekly_Sales, union$Weeky_Sales2)

# Agregación por tienda y agregación por departamento
union <- union %>%
  group_by(Store, Date) %>%
  mutate(ws_tienda = sum(Weekly_Sales)) %>%
  group_by(Dept, Date) %>%
  mutate(ws_departamento = sum(Weekly_Sales)) %>%
  group_by(Store, Year, Month) %>%
  mutate(ms_tienda = sum(Weekly_Sales)) %>%
  group_by(Dept, Year, Month) %>%
  mutate(ms_departamento = sum(Weekly_Sales)) %>%
  group_by(Store, Year, Quarter) %>%
  mutate(qs_tienda = sum(Weekly_Sales)) %>%
  group_by(Dept, Year, Quarter) %>%
  mutate(qs_departamento = sum(Weekly_Sales)) %>%
  group_by(Store, Year) %>%
  mutate(ys_tienda = sum(Weekly_Sales)) %>%
  group_by(Dept, Year) %>%
  mutate(ys_departamento = sum(Weekly_Sales))

# Si hacemos el análisis del precio de la cotización
cotizacion <- union %>%
  ungroup() %>%
  select(Date, Open:Volume, Week, Weekly_Sales, ws_tienda, ws_departamento,
         Month, Month_Sales, ms_tienda, ms_departamento,
         Quarter, Quarter_Sales, qs_tienda, qs_departamento,
         Year, Year_Sales, ys_tienda, ys_departamento) %>% distinct()

save.image(file = "imagen.RData")

# Vemos las tiendas que carecen de un departamento en concreto
prueba <- union %>%
  ungroup() %>%
  select(Store, Dept) %>%
  distinct() %>%
  select(-Store)  %>%
  group_by(Dept) %>%
  count() %>%
  filter(n != 45)

x <- 143-as.data.frame.matrix(table(union$Store, union$Dept))


# Relación entre tamaño tienda y tipo de tienda
x <- union %>% ungroup() %>% select(Type, Size, Dept) %>% distinct()
x <- union %>% ungroup() %>% select(Type, Size, Store) %>% distinct()


# Vemos relación entre temperatura y ventas
load("~/Desktop/Máster/Segundo cuatrimestre/Obligatorias/Visualización de datos/R/imagen.RData")

prueba <- union %>%
  ungroup() %>%
  select(Store, Dept, Date, Weekly_Sales, Temperature) %>%
  distinct()


# Ventas totales por año

prueba <- union %>%
  ungroup() %>%
  select(Date, Weekly_Sales) %>%
  mutate(ano = year(Date)) %>%
  group_by(ano) %>%
  select(-Date) %>%
  summarise_all(sum)

# Otros análisis de correlación
correlacion <- union %>% select(Weekly_Sales, Temperature, Fuel_Price, MarkDown1:MarkDown5, CPI, Unemployment) %>% cor(use = "na.or.complete")
write.table(correlacion, file = "correlacion.txt", sep = "\t", row.names = FALSE)
x <- round(correlacion, digits = 2)

fig <- plot_ly(x=colnames(correlacion), y=colnames(correlacion), z = as.matrix(correlacion), zauto = T, type = "heatmap")
fig

prueba <- round(cor(union, method = c("pearson")), digits = 2)
glimpse(union)

correlacion <- union %>% select(Weekly_Sales, MarkDown1:MarkDown5) %>% cor(use = "na.or.complete")
fig <- plot_ly(x=colnames(correlacion), y=colnames(correlacion), z = as.matrix(correlacion), zauto = T, type = "heatmap")
fig

write.table(correlacion, file = "correlacion.txt", sep = "\t", row.names = FALSE)


# Tiendas con mayor crecimiento
tiendas <- union %>%
  mutate(ano = year(Date)) %>%
  select(Store, ano, Weekly_Sales)%>%
  group_by(Store, ano) %>%
  mutate_all(sum) %>%
  distinct()

tiendas2 <- tiendas %>%
  group_by(Store) %>%
  arrange(Store) %>%
  mutate(variacion = Weekly_Sales - lag(Weekly_Sales),
         variacionTotal = sum(variacion, na.rm = T)) #%>%
 # filter(!is.na(variacion))
tiendas2 <- round(tiendas2, digits = 2)

write.table(tiendas2, file = "variacionTiendas.txt", row.names = F, sep = "\t")

# Departamentos con mayor crecimiento

departamentos <- union %>%
  mutate(ano = year(Date)) %>%
  select(Dept, ano, Weekly_Sales)%>%
  group_by(Dept, ano) %>%
  mutate_all(sum) %>%
  distinct()

departamentos2 <- departamentos %>%
  group_by(Dept) %>%
  arrange(Dept) %>%
  mutate(variacion = Weekly_Sales - lag(Weekly_Sales),
         variacionTotal = sum(variacion, na.rm = T)) #%>%
# filter(!is.na(variacion))
departamentos2 <- round(departamentos2, digits = 2)

write.table(departamentos2, file = "variacionDepartamentos.txt", row.names = F, sep = "\t")
