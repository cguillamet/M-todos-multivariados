library(readxl) #carga de libraria para importar datos desde un excel
library(tidyverse) #carga de libraria para dar formato wide a la tabla de datoss
library(ggplot2) #carga de libraria para realizar gráficos
library(reshape2) #carga de libraria para reorganizar la matriz de correlaciones y crear un mapa de calor

#Carga de los datos
df = read_excel('/Users/ceciliaguillametchargue/Library/Mobile Documents/com~apple~CloudDocs/Documents/Maestría_UBA/Métodos de Análisis Multivariado/Trabajo final/alimentacion.xlsx', skip = 1)

#Seleccion de las variables a utilizar
df = df[,c(1,3,4,8)]

#Renombro las columnas
colnames(df) = c('Pais', 'Poblacion', 'Grupo_alimento', 'Media')

#Elimino la notación cientifica y convierto a numérica la variable Media
options(scipen=999)
df['Media'] = as.numeric(unlist(df['Media']))

#Renombro las categorias de las variables
df$Pais[df$Pais == 'Belgium'] = 'Belgica'
df$Pais[df$Pais == 'Czech Republic'] = 'Republica Checa'
df$Pais[df$Pais == 'Denmark'] = 'Dinamarca'
df$Pais[df$Pais == 'Finland'] = 'Finlandia'
df$Pais[df$Pais == 'France'] = 'Francia'
df$Pais[df$Pais == 'Germany'] = 'Alemania'
df$Pais[df$Pais == 'Ireland'] = 'Irlanda'
df$Pais[df$Pais == 'Italy'] = 'Italia'
df$Pais[df$Pais == 'Netherlands'] = 'Holanda'
df$Pais[df$Pais == 'Romania'] = 'Rumania'
df$Pais[df$Pais == 'Spain'] = 'España'
df$Pais[df$Pais == 'Sweden'] = 'Suecia'
df$Pais[df$Pais == 'United Kingdom'] = 'Reino Unido'
df$Pais[df$Pais == 'Hungary'] = 'Hungria'
df$Pais[df$Pais == 'Latvia'] = 'Letonia'

df$Poblacion[df$Poblacion == 'Adolescents'] = 'Adolescentes'
df$Poblacion[df$Poblacion == 'Adults'] = 'Adultos'
df$Poblacion[df$Poblacion == 'Elderly'] = 'Ancianos'
df$Poblacion[df$Poblacion == 'Infants'] = 'Infantes'
df$Poblacion[df$Poblacion == 'Other children'] = 'Otros_niños'
df$Poblacion[df$Poblacion == 'Pregnant women'] = 'Mujeres_embarazadas'
df$Poblacion[df$Poblacion == 'Toddlers'] = 'Niños'
df$Poblacion[df$Poblacion == 'Very elderly'] = 'Muy_ancianos'

df$Grupo_alimento[df$Grupo_alimento == "Grains and grain-based products"] = 'granos_prod_granos'
df$Grupo_alimento[df$Grupo_alimento == "Starchy roots and tubers"] = 'almidon_tuberc_raices'
df$Grupo_alimento[df$Grupo_alimento == "Fruit and fruit products"] = 'frutas_prod_frutas'
df$Grupo_alimento[df$Grupo_alimento == "Fish and other seafood (including amphibians, rept"] = 'pescados_mariscos'
df$Grupo_alimento[df$Grupo_alimento == "Eggs and egg products"] = 'huevos_prod_huevos'
df$Grupo_alimento[df$Grupo_alimento == "Animal and vegetable fats and oils"] = 'grasa_aceite_ani_veg'
df$Grupo_alimento[df$Grupo_alimento == "Non-alcoholic beverages (excepting milk based beve"] = 'beb_no_alcoholicas'
df$Grupo_alimento[df$Grupo_alimento == "Drinking water (water without any additives except"] = 'agua'
df$Grupo_alimento[df$Grupo_alimento == "Food for infants and small children"] = 'alim_infantiles'
df$Grupo_alimento[df$Grupo_alimento == "Composite food (including frozen products)"] = 'alim_compuestos'
df$Grupo_alimento[df$Grupo_alimento == "Vegetables and vegetable products (including fungi"] = 'veg_prod_veg'
df$Grupo_alimento[df$Grupo_alimento == "Legumes, nuts and oilseeds"] = 'legum_frutos_secos'
df$Grupo_alimento[df$Grupo_alimento == "Meat and meat products (including edible offal)"] = 'carnes_prod_carnes'
df$Grupo_alimento[df$Grupo_alimento == "Milk and dairy products"] = 'prod_lacteos'
df$Grupo_alimento[df$Grupo_alimento == "Sugar and confectionary"] = 'azucar_conf'
df$Grupo_alimento[df$Grupo_alimento == "Fruit and vegetable juices"] = 'frutas_veg_jugos'
df$Grupo_alimento[df$Grupo_alimento == "Alcoholic beverages"] = 'beb_alcoholicas'
df$Grupo_alimento[df$Grupo_alimento == "Herbs, spices and condiments"] = 'hierbas_especias'
df$Grupo_alimento[df$Grupo_alimento == "Products for special nutritional use"] = 'prod_nutricionales'
df$Grupo_alimento[df$Grupo_alimento == "Snacks, desserts, and other foods"] = 'snacks_postres_otros'

#Pasamos al formato wide la base de datos
df_wide = pivot_wider(df, names_from = 'Grupo_alimento', values_from = 'Media')

#Verifico la existencia de valores na
sapply(df_wide, function(x) sum(is.na(x)))

#Tabla de doble entrada entre pais y grupo poblacional
table(df_wide$Pais, df_wide$Poblacion)
table(df$Pais, df$Poblacion)

#Elimino "Pregnant women" dado que solo está para Latvia
df = df[df$Poblacion != 'Mujeres_embarazadas',]
df_wide = df_wide[df_wide$Poblacion != 'Mujeres_embarazadas',]

#Obtención de medidas de dispersión y tendencia central de las variables
summary(df_wide)

#Boxplot de las cantidades medias consumidas según cada uno de los alimentos
ggplot(df, aes(x = Grupo_alimento, y = Media)) +
  geom_boxplot(fill = 'lightblue',
               outlier.colour = 'lightblue') +
  coord_flip() + 
  labs(y = 'Cantidad',
       x = 'Alimento') +
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

#Función para la imputación valores atípicos
replace_outliers <- function(x, removeNA = TRUE){
  qrts <- quantile(x, probs = c(0.25, 0.75), na.rm = removeNA)
  caps <- quantile(x, probs = c(.05, .95), na.rm = removeNA)
  iqr <- qrts[2]-qrts[1]
  h <- 1.5 * iqr
  x[x<qrts[1]-h] <- caps[1]
  x[x>qrts[2]+h] <- caps[2]
  x
}

#Imputación de los valores outliers
columns = colnames(df_wide[-c(1,2)])
df.cap = matrix(data = NA, nrow = 80)
for (col in columns){
  y = replace_outliers(df_wide[, col])
  df.cap = as.data.frame(cbind(df.cap, y))
}
df.cap = df.cap[, -c(1)]
df.cap['Pais'] = df_wide[,1]
df.cap['Poblacion'] = df_wide[,2]
df.cap = df.cap[, c(21, 22, 1:20)]
colnames(df.cap) = colnames(df_wide)

#Transformación a formato largo
df.cap.long = df.cap %>% pivot_longer(c(3:22), names_to = "Grupo_alimento", values_to = "Media")

#Estandarización de las variables
df.scaled <- as.data.frame(scale(df.cap[-c(1,2)], center = TRUE, scale = TRUE))
df.scaled['Pais'] = df.cap[,1]
df.scaled['Poblacion'] = df.cap[,2]
df.scaled = df.scaled[, c(21, 22, 1:20)]

#Transformación a formato largo de los datos estandarizados
df.scaled_long = df.scaled %>% pivot_longer(c(3:22), names_to = "Grupo_alimento", values_to = "Media")

#Boxplot de las cantidades medias consumidas según cada uno de los alimentos
ggplot(df.scaled_long, aes(x = Grupo_alimento, y = Media)) +
  geom_boxplot(fill = 'lightblue',
               outlier.colour = 'lightblue') +
  coord_flip() + 
  labs(y = 'Cantidad',
       x = 'Alimento') +
  theme(axis.text = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

#Detección de las observaciones outliers
var_outliers = c('prod_nutricionales', 'alim_infantiles', 'granos_prod_granos', 'frutas_veg_jugos')
outliers = as.data.frame(sapply(df_wide[, var_outliers], function(x) quantile(x, probs = seq(0, 1, 1/4))))
columns = colnames(outliers)
for (col in columns){
  iqr = outliers[4, col] - outliers[2, col]
  at = df_wide[, col] < (outliers[2, col] - 1.5*iqr) | df_wide[, col] > (outliers[4, col] + 1.5*iqr)
  print(paste('Las observaciones atípicas en', col, 'son:'))
  print(which(at))
}

#Gráfico de consumos medios según país, grupo alimenticio y grupo poblacional
ggplot(df.scaled_long, aes(x = factor(Poblacion), y = Media, color = Grupo_alimento)) +
  geom_point(aes(group = Pais), size = 3, alpha = 0.6) + 
  facet_wrap(~Pais, ncol = 4) + 
  scale_x_discrete(labels = abbreviate,
                   limits = c("Infantes", "Niños", "Otros_niños", "Adolescentes", "Adultos", "Ancianos", "Muy_ancianos")) +
  scale_color_manual(values=c("#D32601", "#C8D202", "#28D202", '#0244D2', '#D202CF', '#891E21', '#2B1E89', '#339DC8', '#76D7C4',
                              '#A260BE', '#FFA200', '#27FF00', '#ACA6F9', '#F5B6F8', '#72305E', '#2C3E50', '#138D75', '#5499C7', 
                              '#F7DC6F', '#909497')) +
  labs(color = 'Clasificación alimentos',
       x = 'Población',
       y = 'Consumo medio') +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        strip.text = element_text(size = 15))


#Matriz de correlaciones
cormat <- round(cor(df.scaled[, c(3:22)]),2)
melted_cormat <- melt(cormat)

#Gráfico de correlaciones
ggplot(melted_cormat, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient(low = 'white', high = 'steelblue') +
  labs(x = 'Clasificación alimentos',
       y = 'Clasificación alimentos') +
  guides(x = guide_axis(angle = 90)) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))


############################# ANALISIS MULTIVARIADO #############################

############# COMPONENTES PRINCIPALES #############
library(factoextra)
library(corrplot)

#Concateno pais con grupo de población
df.scaled['Pais_pobl'] = paste(df.scaled$'Pais','-', df.scaled$'Poblacion')
#y lo asigno a nombre de fila
df.scaled = column_to_rownames(df.scaled, 'Pais_pobl')

#realizo el pca
pca = princomp(x = df.scaled[,-c(1,2)], cor = TRUE, score = TRUE)
summary(pca)

#Gráfico del porcentaje de varianza explicada por cada CP
fviz_eig(pca, addlabels = TRUE, ylim = c(0,50), main = 'Porcentaje de varianza explicada por cada componente')
#Grafico de las contribuciones de cada variable a las CP
corrplot(pca$loadings, is.corr = TRUE)
#Biplot de la CP1 y CP2 con las observaciones y grupos de población
fviz_pca_biplot(pca, label = "var", invisible = "quali", habillage = df.scaled$Poblacion)


############# ANALISIS FACTORIAL #############
library(MVN)
library(psych)

#Pruebas de normalidad multivariada
result = mvn(data = df.scaled[,-c(1,2)], mvnTest = "mardia")
result$multivariateNormality

result <- mvn(data = df.scaled[,-c(1,2)], mvnTest = "hz")
result$multivariateNormality

result <- mvn(data = df.scaled[,-c(1,2)], mvnTest = "royston")
result$multivariateNormality

#Calculo de las correlaciones
r = round(cor(df.scaled[,-c(1,2)]),2)

#Gráfico para determinar el número de factores
fa.parallel(r, fm = "ml", fa = "fa", n.obs = 80, ylabel = "Eigenvalues")

#Análisis factorial
fa = principal(df.scaled[,-c(1,2)], nfactors = 6, rotate = "none")
fa

scores = fa$scores

#Gráfico de los individuos para los distintos factores
par(pty = "s")
plot(scores[,1], scores[,2],
     ylim = range(scores[,1]),
     xlab = "Factor 1", ylab = "Factor 2", type = "n", lwd = 2)
text(scores[,1], scores[,2],
     labels = row.names(df.scaled), cex = 0.6, lwd = 2)
 
par(pty="s")
plot(scores[,1], scores[,3],
     ylim = range(scores[,1]),
     xlab = "Factor 1", ylab = "Factor 3", type = "n", lwd = 2)
text(scores[,1], scores[,3],
     labels = row.names(df.scaled), cex = 0.6, lwd = 2)

par(pty="s")
plot(scores[,1], scores[,4],
     ylim = range(scores[,1]),
     xlab = "Factor 1", ylab = "Factor 4", type = "n", lwd = 2)
text(scores[,1], scores[,4],
     labels = row.names(df.scaled), cex = 0.6, lwd = 2)

par(pty="s")
plot(scores[,1], scores[,5],
     ylim = range(scores[,1]),
     xlab = "Factor 1", ylab = "Factor 5", type = "n", lwd = 2)
text(scores[,1], scores[,5],
     labels = row.names(df.scaled), cex = 0.6, lwd = 2)

par(pty="s")
plot(scores[,1], scores[,6],
     ylim = range(scores[,1]),
     xlab = "Factor 1", ylab = "Factor 6", type = "n", lwd = 2)
text(scores[,1], scores[,6],
     labels = row.names(df.scaled), cex = 0.6, lwd = 2)

par(pty="s")
plot(scores[,2], scores[,3],
     ylim = range(scores[,2]),
     xlab = "Factor 2", ylab = "Factor 3", type = "n", lwd = 2)
text(scores[,2], scores[,3],
     labels = row.names(df.scaled), cex = 0.6, lwd = 2)

par(pty="s")
plot(scores[,2], scores[,4],
     ylim = range(scores[,2]),
     xlab = "Factor 2", ylab = "Factor 4", type = "n", lwd = 2)
text(scores[,2], scores[,4],
     labels = row.names(df.scaled), cex = 0.6, lwd = 2)

par(pty="s")
plot(scores[,2], scores[,5],
     ylim = range(scores[,2]),
     xlab = "Factor 2", ylab = "Factor 5", type = "n", lwd = 2)
text(scores[,2], scores[,5],
     labels = row.names(df.scaled), cex = 0.6, lwd = 2)

par(pty="s")
plot(scores[,2], scores[,6],
     ylim = range(scores[,2]),
     xlab = "Factor 2", ylab = "Factor 6", type = "n", lwd = 2)
text(scores[,2], scores[,6],
     labels = row.names(df.scaled), cex = 0.6, lwd = 2)

par(pty="s")
plot(scores[,3], scores[,4],
     ylim = range(scores[,3]),
     xlab = "Factor 3", ylab = "Factor 4", type = "n", lwd = 2)
text(scores[,3], scores[,4],
     labels = row.names(df.scaled), cex = 0.6, lwd = 2)

par(pty="s")
plot(scores[,3], scores[,5],
     ylim = range(scores[,3]),
     xlab = "Factor 3", ylab = "Factor 5", type = "n", lwd = 2)
text(scores[,3], scores[,5],
     labels = row.names(df.scaled), cex = 0.6, lwd = 2)

par(pty="s")
plot(scores[,3], scores[,6],
     ylim = range(scores[,3]),
     xlab = "Factor 3", ylab = "Factor 6", type = "n", lwd = 2)
text(scores[,3], scores[,6],
     labels = row.names(df.scaled), cex = 0.6, lwd = 2)

par(pty="s")
plot(scores[,4], scores[,5],
     ylim = range(scores[,4]),
     xlab = "Factor 4", ylab = "Factor 5", type = "n", lwd = 2)
text(scores[,4], scores[,5],
     labels = row.names(df.scaled), cex = 0.6, lwd = 2)

par(pty="s")
plot(scores[,4], scores[,6],
     ylim = range(scores[,4]),
     xlab = "Factor 4", ylab = "Factor 6", type = "n", lwd = 2)
text(scores[,4], scores[,6],
     labels = row.names(df.scaled), cex = 0.6, lwd = 2)

par(pty="s")
plot(scores[,5], scores[,6],
     ylim = range(scores[,5]),
     xlab = "Factor 5", ylab = "Factor 6", type = "n", lwd = 2)
text(scores[,5], scores[,6],
     labels = row.names(df.scaled), cex = 0.6, lwd = 2)


############# ANALISIS CLUSTER #############
library(NbClust)
library(ggdendro)

#Metodo ward
#Dendograma Ward
dendrograma_ward = hclust(dist(df.scaled[,-c(1,2)], method = 'euclidean'), method = 'ward.D')
ggdendrogram(dendrograma_ward, rotate = FALSE, labels = F, theme_dendro = TRUE ) + 
  labs(title = "Dendrograma las variables (Método de Ward)")

#Plot Ward
hclust_ward = hclust(dist(df.scaled[,-c(1,2)], method = 'euclidean'), method = 'ward.D')
clust_ward = cutree(dendrograma_ward, k = 5)
fviz_cluster(list(data = df.scaled[,-c(1,2)], cluster = clust_ward), labelsize = 8, main = "Plot las variables (Método de Ward)"  )

#Metodo Centroide 
dendrograma_centroide = hclust(dist(df.scaled[,-c(1,2)], method = 'euclidean'), method = 'centroid')
ggdendrogram(dendrograma_centroide, rotate = FALSE, labels = F, theme_dendro = TRUE) + 
  labs(title = "Dendrograma las Variables (Método del Centroide)")

#Plot método del Centroide
hclust_centroide = hclust(dist(df.scaled[,-c(1,2)], method = 'euclidean'), method = 'centroid')
clust_centroide = cutree(hclust_centroide, k = 3)
fviz_cluster(list(data = df.scaled[,-c(1,2)], cluster = clust_centroide), labelsize = 8, main = "Plot todas las variables (Centroide)" )

#Determinacion de la cantidad optima de clusters - METODOS JERARQUICOS
cant_clusters = NbClust(df.scaled[,-c(1,2)], distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D2", index = "alllong")
cant_clusters$Best.nc


#K-means

#Determinacion de la cantidad optima de clusters - Kmeans
cant_clusters_km = NbClust(df.scaled[,-c(1,2)], distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", 
                           index = "alllong")
cant_clusters_km$Best.nc

#Método Kmeans
clusters = kmeans(df.scaled[,-c(1,2)], centers = 2, iter.max = 30,  nstart = 20)
clusters$centers
clusters$size
c = as.data.frame(clusters$cluster)

#Gráfico de los clusters por el método de Kmeans
fviz_cluster(clusters, data = df.scaled[,-c(1,2)], main = "2 clusters (Método de KMeans)")


############# ANALISIS DISCRIMINANTE #############
library(gmodels)
library(biotools)
library(MASS) # carga función lda
library(mvnormtest)

#Formacion de nuevos grupos
df.scaled$Grupo = df.scaled$Poblacion
df.scaled$Grupo[df.scaled$Grupo == 'Adolescentes'] = 'Adultos'
df.scaled$Grupo[df.scaled$Grupo == 'Ancianos'] = 'Adultos'
df.scaled$Grupo[df.scaled$Grupo == 'Muy_ancianos'] = 'Adultos'
df.scaled$Grupo[df.scaled$Grupo == 'Otros_niños'] = 'Niños'
df.scaled$Grupo[df.scaled$Grupo == 'Infantes'] = 'Niños'

#Supuestos del analisis discriminante
#Test de homocedasticidad de Bartlett-Box
boxM(data = df.scaled[, c(10, 19)], grouping = df.scaled[, c(23)])

#Separamos por grupos
Grupo1 = df.scaled[df.scaled$Grupo == 'Niños', c(10,19)]
Grupo2 = df.scaled[df.scaled$Grupo == 'Adultos', c(10,19)]

#Trasponemos las matrices
Grupo1<-t(Grupo1)
Grupo2<-t(Grupo2)

#Ejecutamos el test de analisis de normalidad multivariante
mshapiro.test(Grupo1) 
mshapiro.test(Grupo2)


#Modelo de analisis discriminante
lda = lda(data = df.scaled, df.scaled$Grupo ~ df.scaled$prod_lacteos +
            df.scaled$alim_infantiles)
lda

#Predicciones del modelo lda
predicciones = predict(object = lda, newdata = df.scaled[, c(10, 19)], method = "predictive")
#Tabla cruzada de aciertos y errores de clasificaci?n
CrossTable(df.scaled$Grupo, predicciones$class, digits = 2, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE,
           dnn=c("Grupo real","Grupo predicho"))

#Grafico de los individos de acuerdo a la clasificacion
par(mfrow = c(1, 1))
loading <- as.matrix(df.scaled[, c(10, 19)]) %*% lda$scaling
df.scaled$Grupo = as.factor(df.scaled$Grupo)
plot(loading, pch = 16, cex = 1.25, col = c(2,4)[df.scaled[, 23]])

#Prediccion de la clasificación
fit.p <- predict(lda)$class
predict(lda)$posterior






