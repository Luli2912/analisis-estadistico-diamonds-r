#title: "03GIAR Probabilidad y Estadística."
#subtitle: "Actividad 1. Evidencia 2"
#author: "2024@LUCIA, RODRIGUEZ, Y8678805E."
#date: " Fecha de entrega: dd/12/2024"
----
#Variable aleatoria discreta "Clarity"
install.packages("ggplot2",type = 
                     "source")
library(ggplot2)  #Instalo libreria necesaria para analizar variable
head(diamonds)
vc<-diamonds$clarity #La variable "Clarity" es una variables discreta, ya que 
                     #representa la claridad de un diamante. 
                    #Representa niveles categoricos.
v2<-table(vc)    #Creo una tabla de valores
v2     
# Defino vector con etiquetas para la variable "Clarity"
lv2<-c("I1","SI2","SI1","VS2","VS1","VVS2","VVS1","IF")
df1<-data.frame(lv2,v2)  #Creo un data frame

# Diagrama de barras, uso este grafico ya que es ideal para representar datos
# discretos. Permite visualizar frecuencias por categorias y comparar valores.
barplot(height=df1$Freq, names=df1$lv2,las=2,
        main="Limpieza del diamante",#titulo del grafico
        ylab="Valores",
        xlab="Categorias",
        ylim=c(0,16000),
        ##4 opciones para las barras y leyenda
        col=c("pink","red","lightblue","purple","violet","orange","yellow","green"),
        legend=rownames(df1),
        density=c(3,9,17,35,19),
        angle=c(10,25,60,41,80),)
#El siguiente diagrama de barras nos muestra como se distribuyen los datos por los niveles según el tipo indicado.
#Nos muestra que las categorias SI1 y VS2, que representan diamantes, de calidad moderada
#son los mas frecuentes, con valores entre 10000-15000, y por tanto podemos observar
#que para este conjunto de datos, las categorias I1, que representan a aquellos con calidad baja, 
#y a los incluidos en la categoria IF, que representan a los de mas alta calidad, 
#que son los menos frecuentes.

#Realizo calculo de estadisticos 
#En el caso de esta variable por su definicion discreta, no seria aplicable calcular, por ejemplo, medidas como la media aritmetica, para este conjunto de datos.
#Por esto evaluare su moda.
#Calculo de Moda
calcular_moda<-function(x) {
  frecuencia<-table(x)
  #Tabla de frecuencias
  moda<-
    names(frecuencia[frecuencia ==
                       max(frecuencia)]) #Categoria mas frecuente
  return(moda)}
#Moda de clarity
moda_clarity<-
  calcular_moda(diamonds$clarity)
cat("moda de clarity:",
    moda_clarity, "\n")
moda_clarity
#Como podemos ver definiendo la moda de esta variable, nos confirma lo visto en el grafico anterior, donde la mayor frecuencia esta en la categoria SI1.
#Podemos tambien verla graficamente con el Diagrama de Pareto, por ejemplo

moda<-names(which.max(table(diamonds$clarity)))
moda       #Aqui vemos el valor de la moda
install.packages("qcc")
library(qcc)
pareto.chart(table(diamonds$clarity),
             main="limpieza del diamante",        #Titulo del grafico
             ylab="Frecuencia",
             xlab="Niveles de pureza",
             col=c("purple","violet","pink")
             )
#Graficamente confirmamos, lo confirmado anteriormente, siendo los diamantes con caracteristicas SI1, los de mayor frecuencia.

#---------------------------------------------------------------------------------------------------------------
  
#Variable aleatoria continua "Carat"
v3<-diamonds$carat
v3
#Si realizamos un histograma los datos se muestran como una distribucion que refleja la frecuencia de los distintos pesos, valorados en Quilates, de la variable "Carat", a simple vista podemos observar que aquellos que presentan mayor frecuencia, seran los valorados, entre 0 y 1.


#Hacemos un histograma basico
hist(v3, main = "Peso de los diamantes en Quilates", 
     #xlab = "$", 
     #ylab="Frecuencia",
     #prob = TRUE, 
     col="lightpink",
     #ylim = c(0,0.44)
     )

#Si realizamos una transformacion a los datos con la raiz cuadrada, se expone una reduccion en la asimetria de la distribucion, lo que facilita el analisis y la interpretacion de los datos al acercarlos mas a una distribucion simetrica. Esto es util, ya que al aplicar la raiz cuadrada, los valores grandes se reducen proporcionalmente mas que los pequeños, obteniendo un sesgo disminuido. Y asi creando graficamente, una opcion visualmente mas clara.

#Hacemos un histograma pero aplicamos raiz cuadrada a "v3"
hist(sqrt(v3), breaks=85,main = "Peso", 
     #xlab = "$", 
     ylab="Frecuencia",
     prob = TRUE, 
     col="lightpink",
     xlim=c(0.2,2)
     #ylim = c(0,0.44)
)
#Defino la moda
mean(v3)
library(multimode)
modas <- locmodes(sqrt(v3), mod0 = 5)       # Localiza la moda
modas
plot(modas)

# Subjetivamente ¿ Dónde estarían los estadísticos clásicos?

hist(sqrt(v3), breaks=85,main = "Peso", 
     #xlab = "$", 
     ylab="Frecuencia",
     prob = TRUE, 
     col="lightpink",
     xlim=c(0.2,2)
     #ylim = c(0,0.44)
)

#Graficamos cada estadistico

abline(v = mean(v3), col = 3, lwd = 3)   # Línea para media aritmética.
abline(v = median(v3), col = 2, lwd = 3) # Línea para mediana.
abline(v =modas[[1]], col = 4, lwd = 3) # Línea para la Moda

#Apliquemos algunos métodos para los intervalos: 
#"Sturges", "Scott" y "freedman-diaconis"
#Metodo Sturges
par(mfrow = c(2, 2))    # Desplegamos 4 paneles
hist(v3, breaks="sturges", main = "Peso en Quilates", 
     xlab = "Quilates", 
     ylab="Frecuencia", 
     col="pink",
     #ylim = c(0,0.44)
)
#Freedman-Diaconis
hist(v3, breaks="fd", main = "Peso en Quilates", 
     xlab = "Quilates", 
     ylab="Frecuencia", 
     col="purple",
     #ylim = c(0,0.44)
     )
#Scott     
hist(v3, breaks="scott", main = "Peso en Quilates", 
     xlab = "Quilates", 
     ylab="Frecuencia",
     col="yellow",
     #ylim = c(0,0.44)
)
# ¿Con cuál nos quedamos?
#Teniendo en cuanta los datos de esta variable, podemos ver que tanto en metodo Scott o el Freedman-Diaconis, reflejan la informacion, graficamente de manera similar, por tanto cualquiera de estos, cumplirian, mejor con la muestra grafica, que la opcion de Sturges.
#Por ejemplo si optamos por el metodo Scott:
par(mfrow = c(1,1))

hist(v3, breaks="scott", main = "Peso en Quilates", 
     xlab = "Quilates", 
     ylab="Frecuencia",
     col="orange",
     xlim = c(min(v3),max(v3))
     #ylim = c(0,0.44)
)

abline(v = mean(v3), col = 2, lwd = 3)   # Línea para media aritmética.
abline(v = median(v3), col = 1, lwd = 3) # Línea para mediana.
modas
abline(v =modas[[1]], col = 4, lwd = 3) # Línea para la Moda.

#En este punto podemos ver que ni la media, ni la mediana ni 1 sola moda, representan correctamente, lo que necesitamos.
#Veamos si podemos exponer mas informacion, de estos datos, aplicando logaritmo.
par(mfrow=c(1,2))
hist(log(v3), breaks="sturges", main = "Pesos en Quilates", 
     xlab = "Peso en Quilates", 
     ylab="Frecuencia",
     col="pink",
     #ylim = c(0,0.44)
     )
hist(log(v3), breaks="fd", main = "Pesos en quilates", 
     xlab = "Peso en Quilates", 
     ylab="Frecuencia", 
     col="purple",
     #ylim = c(0,0.44)
)
hist(log(v3), breaks="scott", main = "Pesos en quilates", 
     xlab = "Peso en quilates", 
     ylab="Frecuencia", 
     col="lightblue",
     #ylim = c(0,0.44)
)
# Definitivamente, ni la media ni la mediana y una sola moda no 
# la representa. Sera multimoda, por ejemplo. Calculemos 
# las más significativas.
par(mfrow=c(1,1))
modas <- locmodes(log(v3), mod0 = 2)       # Localiza la moda
modas
plot(modas)
par(mfrow = c(1, 2))    # Desplegamos 2 paneles
hist(log(v3), breaks="scott", main = "Peso en quilates", 
     xlab = "peso en quilates", 
     ylab="Frecuencia", 
     col="violet",
     #ylim = c(0,0.44)
)
abline(v = mean(log(v3)), col = 3, lwd = 3)   # Línea para media aritmética.
abline(v = median(log(v3)), col = 2, lwd = 3) # Línea para mediana.
abline(v =modas[[1]][1], col = 5, lwd = 3) # Línea para la Moda 1.
abline(v =modas[[1]][3], col = 1, lwd = 3) # Línea para la Moda 2.
plot(modas)
# Pero para comparar, mejor en la misma escala los dos gráficos. 
hist(log(v3), breaks="scott", main = "Peso en quilates", 
     xlab = "peso en quilates", 
     ylab="Frecuencia",
     prob=TRUE,               #Añadimos esta función.
     col="lightblue",
     ylim = c(0,1)
)
abline(v = mean(log(v3)), col = 3, lwd = 5)   # Línea para media aritmética.
abline(v = median(log(v3)), col = 2, lwd = 2) # Línea para mediana.
abline(v =modas[[1]][1], col = 5, lwd = 3) # Línea para la Moda 1.
abline(v =modas[[1]][3], col = 6, lwd = 3) # Línea para la Moda 2.
plot(modas)
par(mfrow=c(1,1))
plot(modas)
# Como estadísticos de tendencia central tenemos dos valores: 
# según la moda:-1.09 0.029,0.60 0.65,-0.39 en Log.
modas
mean(log(v3))

# Calculemos otros estadísticos de dispersión.
rg=max(log(v3))-min(log(v3))  # Rango
rg
var(log(v3))       # Varianza 0.3420232
sd(log(v3))        # Desviación estándar 0.5848275
library(moments)   # Librería para el cálculo de los momentos.
skewness(log(v3))  # Asimetría 0.09610032
kurtosis(log(v3))  # Curtosis 1.935102

#Obtenidos estos valores, podemos decir
#1)Segun el valor de Varianza, indica que los datos estan relativamente dispersos, aunque no de forma excesiva. Estan alejados de la media, por tanto descartamos dicho estadistico.
#2)La desviacion estandar, es la raiz cuadrada de la varianza. En este caso sugiere una dispersion moderada.
#3)En el caso, de la Asimetria, podemos decir que en este caso, es casi simetrica, porque el valor es cercano a cero, con una leve inclinacion, a derecha (asimetria positiva).
#4)Por ultimo, la curtosis, en este caso, es inferior a 3, por tanto indica una curtosis leptocurtica, esto sugiere que hay menos datos extremos.

#Hasta aqui podemos decir, que esta variable parece tener una distribucion moderadamente dispersa,  con ligera asimetria positiva, lo que indica que los valores estan distribuidos de manera bastante equilibrada, hay menos valores extremos.

# Elijamos una muestra del total de valores disponibles, aplicando 
# un método de muestreo y repitamos los cálculos previos y verificamos si
# son consistentes o no. 

# Muestreo Simple.
length(v3) # Total de datos disponibles:  53940.
# Selección de la muestra
n<-length(v3)*0.4           # Tamaño de la muestra. 40%
n
# Elegimos una muestra de forma aleatoria de los 53940. Para ello, 
# hacemos uso de la función "sample" y considerando un 40% de la 
# muestra.
v4<- sample(log(v3),size=n,replace=FALSE) 
v4
# Gráfico de la muestra "v4" y comparamos con el original.
hist(v4, breaks="scott",main = "Distribución de la muestra ", 
     xlab = "Log(Peso en Quilates) [Q]", 
     ylab = "Frecuencia",
     ylim=c(0,1.5),
     prob=TRUE,               #Añadimos esta función.
     col="red")
# Nuestro gráfico original, con toda la muestra.
hist(log(v3), breaks="scott", main = "Distribución de todos los datos", 
     xlab = "Log(Peso en Quilates) [Q]", 
     ylab="Frecuencia",
     prob=TRUE,               #Añadimos esta función.
     col="blue",
     ylim = c(0,1.5)
)
#En esta caso, podemos quedarnos con la muestra, ya que al ver la comparacion, vemos que es representativa.
#Definimos los estadisticos, nuevamente con respecto a la muestra.
mean(v4)      # Media aritmética.
median(v4)    # Mediana.
library(multimode)
modas <- locmodes(sqrt(v4), mod0 = 2)       # Localiza la modas
modas
plot(modas)
var(v4)       # Varianza.
sd(v4)        # Desviación estándar. 
skewness(v4)  # Asimetría. 
kurtosis(v4)  # Curtosis.

#Conclusiones Ejercicio 1:
#Con respecto a la Variable discreta "Clarity"
#SI1 es el nivel de claridad más común en el conjunto de datos. Esto sugiere que una proporción significativa de los diamantes tiene pequeños defectos que son "ligeramente incluidas" (Slightly Included 1).
#SI1 suele encontrarse en una calidad intermedia entre diamantes de muy alta calidad (como IF, VVS1/VVS2) y aquellos con imperfecciones más visibles (como I1, I2). Esto podría indicar que los diamantes del dataset están orientados a un mercado de calidad media, donde SI1 ofrece una buena relación entre precio y apariencia.
#Con la herramiento del grafico con el metodo de Pareto, podemos confirmar,que la mayor concentracion de diamantes, son aquellos que estan catalogados, como SI1, es decir, para este grupo de datos analizados, podemos decir, que la mayor cantidad de diamantes son aquellos, de calidad media, o con defectos, poco visibles, seguido de aquellos agrupados en la categoria VS2, que como los anteriores, representan a aquellos, con ligeros defectos.
#Con respecto a la Variable continua "Carat"
#Modas (0.1627652, 0.4078262): Las modas representan los valores más frecuentes. En este caso, se encuentran en dos puntos diferentes, lo que sugiere que la distribución podría ser bimodal, es decir, hay dos valores en los que los datos se agrupan con mayor frecuencia.
#Antimoda (0.3192096):es el valor que es menos frecuente en la distribución.Una zona de menor densidad en los datos.
#Valor estimado de la densidad: Modas (1.129406, 1.071558): Indican la concentración de los datos alrededor de esos valores. En este caso, ambas son bastante cercanas, lo que sugiere que las modas son puntos con alta concentracion de datos.
#Antimoda (1.061815): La densidad en el antimoda es un poco más baja, indicando que es una zona menos concentrada.
#Análisis de la otros estadisticos: Varianza (0.3441411): En este caso, una varianza moderada indica que los valores de "Carat" no están excesivamente dispersos ni muy concentrados alrededor de la media.
#Desviación estándar (0.5866354): es relativamente baja en comparación con el rango. Esto indica que los datos no se desvían demasiado de la media.
#Asimetría (0.09669699): Es cercana a 0, lo que indica que la distribución es casi simétrica. No hay una tendencia clara hacia un sesgo positivo o negativo en los datos.
#Curtosis (1.921081): La curtosis es menor que 3, lo que indica que la distribución tiene colas más ligeras que una distribución normal. Esto puede sugerir que los valores extremos (outliers) son menos frecuentes en comparación con una distribución normal.
#Análisis de Region crítica(0.1091385): Podría indicar la suavidad de la estimación. Una region crítica relativamente pequeña sugiere que la estimación de la densidad está más ajustada a los puntos de datos y podría no ser tan suave.
#La distribución de "Carat" parece ser bimodal, con dos valores principales que tienen una alta frecuencia de ocurrencia (modas en 0.1627652 y 0.4078262). Es decir, podría haber dos grupos distintos o características con mayor frecuencia en los datos.
#Los datos están relativamente dispersos alrededor de un valor central.
#La curtosis sugiere que la distribución tiene colas ligeras, lo que significa que no hay una presencia significativa de valores extremos.
#El valor de la region crítica podría indicar que la estimación de la densidad se ajusta bien a los datos.
#En conclusion,la variable "Carat" muestra una distribución con dos grupos diferentes de datos mas frecuentes, no muestra una alta concentración en torno a un único valor, sino más bien una dispersión moderada con pocas colas extremas.

#Ejercicio 2
##Los L-Moments, son otros estadísticos alternativos que nos permiten estudiar ciertas distribuciones que se pueden considerar con colas muy pronunciadas, donde los momentos clásicos no logran adaptarse bien. Es decir, caracterizan mejor la distribución ya que son menos sencibles en los extremos. Para una variable estadística aleatoria $X$ están definidos en términos de los ordenes estadísticos ($p-$ésimos) tal que:

install.packages("evd")
install.packages("lmom")

#Cargo librerias necesarias
library(lmom)
library(Lmoments)
library(evd)

#Simulo un conjunto de datos, de ejemplo, para la distribucion GEV, que se encuentra en ambas librerias. 
set.seed(123)  #Reproducibilidad
n<-100
gev_data<- rgev(n,loc=0, scale=1, shape=0.2)

#Calculo los L-Moments con cada libreria
#Con la libreria lmom:
lmr_lmomco<-lmoms(gev_data)
lmr_lmom_values<-lmr_lmomco$lambdas[1:4]
print(lmr_lmom_values)


#Con la libreria L-moments
lmr_Lmoments<-Lmoments(gev_data)
print(lmr_Lmoments)

#Creo un dataframe para comparar los resultados
comparacion<-data.frame(L_moment= c("L1","L2","L3","L4"),
                        lmom=lmr_lmom_values,
                        Lmoments=lmr_Lmoments)
print(comparacion)


tinytex::install_tinytex()
