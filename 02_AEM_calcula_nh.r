#**************************************************************************************#
#**************************************************************************************#
#
#                       Taller de Análisis de Encuestas con R                        
#                        Sociedad Ecuatoriana de Estadística
#
#     Última actualización:   23/05/2023
#     Creado por:             Patricia Romero M.
#     Actualizado por:        Andrés Peña M.  y José Díaz M.             
#     Contacto:               Andrés Peña M. (agpena@colmex.mx)
#     Organización:           R Users Group - Ecuador
#                             
#
#**************************************************************************************#
#**************************************************************************************#

# programa calcula_nh.r

# este programa genera una base de datos
# con informacion del numero de casillas Nh
# en cada uno de los 300 distritos
# y calcula el tamaño de muestra nh en cada
# distrito con distribucion proporcional.

getwd() # verificar directorio de trabajo


# Paso 1. Instalación y carga de paquete necesario --------------------------------------

# Cargamos el paquete 'sampling' para usar funciones de diseño muestral.
# En este script, se usa especialmente la función 'cleanstrata()' para reindexar 
# los estratos de forma consecutiva.

install.packages("sampling") 
library(sampling) 


# Paso 2. Carga del dataset de trabajo ------------------------------------

datos <- read.csv("AEM_SEE_2nd-main/bases/presidencia_candidato_2018.csv", header = T)
names(datos) # ver las variables del dataset
names(datos)[1] <- "ID_ESTADO"

# Paso 3. Tamaño poblacional y tamaño de muestra --------------------------

N <- length(datos$ID_ESTADO) #tamaño de la poblacion
n <- 7500   # tamaño de muestra global (aplicando previamente la fórmula)

# primero genero la variable estrato
# que toma valores del 1 al 300


# Paso 4. Construcción de los estratos muestrales -------------------------
# a partir de las variables ID_ESTADO y ID_DISTRITO.

x <- datos$ID_ESTADO     #x tiene la id de estado
y <- datos$ID_DISTRITO   #y tiene la id de distrito
estr <- x*100+y       #estr tiene la combinacion de edo y dist
head(estr)            # primeros seis elementos
tail(estr)            # últimos seis elementos

# A continuación usamos la función cleanstrata del paquete sampling 
# cleanstrata Normaliza los identificadores de estratos para que estén numerados consecutivamente (1, 2, ..., L)
# Esto es necesario para aplicar correctamente técnicas de muestreo estratificado.
estrato <- cleanstrata(estr) # Renumera los códigos únicos de estratos 
                              # en una secuencia consecutiva (1, 2, ..., K) 
                              # necesaria para muestreo estratificado
                              # por ejemplo todos los estratos 101 los numera como 1  
                              # los estratos 102 los enumera como 2, etc.

head(estrato)
tail(estrato)
length(unique(estrato)) # para ver el total de estratos


# Se crea un nuevo data frame que resume las columnas clave para el análisis estratificado:
# incluye el ID del estado, el ID del distrito, la codificación original del estrato (estr)
# y el número correlativo de estrato limpio (estrato) generado por la función cleanstrata().
df_estratos <- data.frame(
  ID_ESTADO = datos$ID_ESTADO,
  ID_DISTRITO = datos$ID_DISTRITO,
  estr = estr,
  estrato = estrato
)

completo <- cbind(datos,estrato) # Añade la variable 'estrato' al conjunto de datos original 
                                  # para poder identificar fácilmente a qué estrato 
                                  # pertenece cada observación

#write.table(completo,file="completo.txt",sep=",",quote=FALSE,row.names=FALSE)

Nh <- tabulate(estrato)   # calcula la cantidad de casillas en cada estrato (tamaño poblacional del estrato)
Nh   # muestra el numero de casillas en cada distrito
sum(Nh) # suman el total de casillas, que es igual al total de la población (156436)



# Paso 5: Distribución proporcional del tamaño muestral por estrato

# En esta sección se calcula cuántas unidades (casillas) debe tomar la muestra en cada estrato,
# bajo el criterio de ASIGNACIÓN PROPORCIONAL. Es decir, la muestra se distribuye en proporción 
# al tamaño del estrato en la población (Nh), asegurando que los estratos grandes reciban más 
# unidades muestrales y los pequeños menos. Este enfoque es útil cuando no hay información sobre 
# varianzas ni costos diferenciales entre estratos.
#
# Se parte de un n total (por ejemplo, n = 7500) y se distribuye proporcionalmente entre los L estratos.
# El procedimiento asegura que el total muestral final siga siendo n, a través de una técnica de 
# redondeo que ajusta los sobrantes decimales de forma ordenada. total poblacional.

# Calcular la fracción que representa cada estrato en la población total
# primero la ponderacion de cada estrato
wh <- Nh/N        # wh representa el peso poblacional de cada estrato h en la población N
sum(wh) # debe ser igual a 1
data.frame(estrato = 1:length(wh), peso_relativo = wh) # muestra el peso relativo de cada estrato


# Calculamos los tamaños muestrales reales para cada estrato (en números decimales)
rh <-  wh * n     # rh es el tamaño muestral teórico (con decimales) que le tocaría a cada estrato 
                  # bajo asignación proporcional.
head(rh) # Visualizamos los primeros valores
# Tabla que muestra el peso relativo de cada estrato respecto a la población total
data.frame(estrato = 1:length(wh), Nh = Nh, peso_relativo = wh, rh = rh) 
sum(rh) # debe ser igual al tamaño de la muestra (7500)


# Tomamos solo la parte entera de rh para obtener un valor de muestra preliminar
nh <- floor(rh) # Redondea hacia abajo el tamaño muestral teórico (rh) para obtener nh como número 
                # entero de unidades muestrales por estrato
# Tabla que muestra el peso relativo de cada estrato respecto a la población total
data.frame(estrato = 1:length(wh), Nh = Nh, peso_relativo = wh, rh = rh, nh = nh)
sum(nh) # ya no nos dará el tamaño de muestra, sino menos

# Calculamos la parte decimal que no fue asignada a cada estrato
dh <- rh - nh       # dh: sobrante decimal de cada estrato
data.frame(estrato = 1:length(dh), dh = dh) # calcula y muestra la parte decimal del tamaño muestral 
                                            # teórico no asignado por estrato

# Sumamos todos los sobrantes decimales para saber cuántas unidades nos faltan distribuir
m <- sum(dh)      # m: total de unidades faltantes (por redondeo hacia abajo)
# Ejemplo típico: si n = 7500, y al redondear hacia abajo nos quedamos en 7361, entonces m = 139
# m es el número de unidades muestrales que faltan por asignar luego de redondear hacia abajo (floor)
# los tamaños muestrales por estrato (rh) en un muestreo estratificado proporcional.

# Ordenamos los sobrantes de mayor a menor para reasignar de forma eficiente
dhord <- sort(dh, decreasing=TRUE)  # ordena las partes decimales (dh) de mayor a menor para reasignar
                                    # los sobrantes de forma eficiente
data.frame(estrato = order(dh, decreasing = TRUE), dhord = dhord)


indhord <- order(dh, decreasing=TRUE)  # Indices de los estratos que ordenan dh de mayor a menor 
                                        # se usarán para reasignar unidades a los más cercanos al decimal completo
head(indhord)
tail(indhord)

# a los primeros nh (los mas grandes) les sumo 1 unidad
# y a los restantes nh los dejo igual

# Este bucle recorre los primeros m estratos con mayor parte decimal (es decir, donde el redondeo dejó
# más "resto") A esos estratos les suma 1 unidad a su nh.

# A esos estratos les suma 1 unidad a su nh.
for (i in 1:m){
  nh[indhord[i]] <- nh[indhord[i]] + 1
}
sum(nh) # verifica que la suma de las muestras asignadas a todos los estratos ahora sí da exactamente n.

hh <- 1:300 # Crea un vector hh que contiene los números del 1 al 300, es decir, los identificadores 
            # de los estratos.
result <- as.data.frame(cbind(hh,Nh,nh)) # Combina (cbind) los vectores hh, Nh (tamaño poblacional por estrato)
                                        # y nh (tamaño muestral por estrato) en una tabla (data.frame) llamada result.

sum(result$Nh) # total población
sum(result$nh) # total muestra
  

# Paso 6: Selección aleatoria simple dentro de cada estrato


#especificacion de la semilla
print("dame semilla")
sem <- scan(,n=1)

set.seed(sem)

# Usamos el paquete 'pps' para seleccionar aleatoriamente las unidades dentro de cada estrato 
# según nh (muestreo aleatorio simple sin reemplazo)
install.packages("pps")
library(pps)        # tiene una funcion para seleccionar m.a.s. en estratos

# Esta instrucción selecciona 'nh[h]' unidades aleatoriamente dentro de cada estrato 'h',
# de acuerdo con el esquema de muestreo estratificado con asignación proporcional.

# Recordemos:
  # El vector 'estrato' indica a qué estrato pertenece cada observación del dataset (valores del 1 al 300).
# Tiene 156,436 elementos, por tanto, coincide con el número total de filas de la base 'completo'.
# El vector 'nh' contiene el tamaño de muestra asignado a cada estrato (según asignación proporcional).
# Tiene 300 elementos, uno por cada estrato, y su suma total es igual al tamaño muestral global (n = 7500).

head(estrato, 20) # los 20 primeros elementos del vector estrato.
table(estrato) # una distribución de frecuencias del vector estrato.

# Ahora hacemos lo siguiente:
# Se selecciona de forma aleatoria 'nh[h]' unidades dentro de cada estrato 'h' del vector 'estrato'.
# Por ejemplo, si nh[1] = 23, entonces selecciona 23 unidades aleatoriamente entre todas las observaciones cuyo estrato es 1, y así sucesivamente para los 300 estratos.
mm <- stratsrs(estrato, nh) # Selecciona aleatoriamente 'nh' unidades por cada estrato usando muestreo aleatorio simple sin reemplazo
head(mm,20) # los 20 primeros elementos del vector mm.
length(mm)     # Cuántas observaciones fueron seleccionadas (debe ser igual al tamaño de muestra)
table(estrato[mm]) # la distribución de frecuencias de cada elemento del vector

# Se extraen las observaciones seleccionadas según los índices generados por 'stratsrs'.
# Por ejemplo, si el estrato 5 debe aportar 29 observaciones, aquí se recuperan esas 29 filas desde 'completo'.
muestra <- completo[mm, ]
nrow(muestra)  # Devuelve el número de observaciones seleccionadas en la muestra (debe ser igual al tamaño de muestra=7500)


# Añadir tamaño poblacional y muestral a cada unidad seleccionada
muestra$Nh <- Nh[muestra$estrato]
muestra$nh <- nh[muestra$estrato]

# A continuación se calcula el factor de expansión para cada observación, que indica cuántas unidades 
# representa en la población. Se obtiene como Nh (tamaño poblacional del estrato) 
# dividido para nh (tamaño muestral del estrato) correspondiente.
# Ejemplo: si en el estrato 1 hay Nh = 488 casillas y se seleccionaron nh = 23, 
# entonces cada observación representa 488/23 = 21.21739 unidades.
muestra$fexp <- muestra$Nh / muestra$nh

# La suma de los factores de expansión debe ser igual al tamaño poblacional total (N),
# ya que cada unidad muestral representa a un grupo de unidades de la población.
sum(muestra$fexp)  # Verifica que la suma sea igual a N (156436).

frec <- table(muestra$estrato)  # frecuencia de cada estrato en muestra
sum(frec==nh) # compara si los dos vectores son iguales elemento
# a elemento y suma 

# Paso 7: Especificación del diseño muestral y estimación de parámetros poblacionales
# En este paso se define formalmente el diseño muestral estratificado y se realizan estimaciones 
# estadísticas representativas para la población, como medias, proporciones y totales, 
# usando el paquete survey.

library(survey)

L <- length(unique(estrato))  # Número total de estratos

# Se define el diseño muestral estratificado, indicando los estratos, 
# el factor de expansión (cuántas unidades representa cada observación),
# y la corrección por población finita (tamaño total de cada estrato).
# Esto permite hacer estimaciones válidas (medias, proporciones, totales) 
# respetando la forma en que se construyó la muestra.

# Se usa la función svydesign() del paquete 'survey', que permite incorporar:
# - id = ~1: indica que no hay conglomerados (es muestreo sin agrupación, cada unidad es seleccionada individualmente).
# - strata = ~estrato: indica que el muestreo fue estratificado usando la variable 'estrato'.
# - data = muestra: es el data frame que contiene los datos muestrales seleccionados.
# - weights = ~fexp: incorpora el factor de expansión de cada observación (cuántas unidades reales representa).
# - fpc = ~Nh: es el tamaño poblacional real del estrato (finite population correction), mejora la precisión de estimadores.

disin <- svydesign(
  id = ~1,
  strata = ~estrato,
  data = muestra,
  weights = ~fexp,
  fpc = ~Nh
)

# Estimamos la proporción de votos del candidato RAC respecto al total de votos,
# utilizando el diseño muestral definido anteriormente (estratificado, con pesos y corrección por población finita).
svyratio(~RAC, ~TOTAL_VOTOS, design = disin)

# IC del 95% para la proporción estimada de RAC
r <- 0.2218429       # Proporción estimada de votos para RAC (salida de svyratio)
se <- 0.001084148    # Error estándar asociado a esa proporción

# Calcula el intervalo de confianza al 95% usando ±1.96 * error estándar
c(r - 1.96 * se, r + 1.96 * se)
# Salida esperada:
# [1] 0.2197180 0.2239678

# con un 95% de confianza, la proporción poblacional de votos para RAC
# está entre 21.97% y 22.40%. Es decir, el verdadero valor de la proporción se encuentra
# dentro de este rango si el diseño y muestreo son correctos.


summary(disin)


