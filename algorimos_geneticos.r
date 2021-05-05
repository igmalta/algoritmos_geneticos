rm(list=ls())
gc()

# Librerías
# ==============================================================================
if (!require("plotly"))  install.packages("plotly") ; library("plotly")
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")

# Funciones de conversión binario/decimal
# ==============================================================================
decimalAbinario <- function(numero){
  #' Convierte números decimales a binario.
  #' 
  #' numero: número decimal a convertir.
  
  # Se redondea el número
  numeroRedondeado <- floor(numero)
  # Se crea vector para guardar resultado
  binario <- c()
  # Se convierte a binario
  while (numeroRedondeado >= 2){
    binario <- c(binario, numeroRedondeado %% 2)
    numeroRedondeado <- numeroRedondeado %/% 2
  }
  binario <- c(binario, numeroRedondeado)
  binario[length(binario):1]
}

binarioAdecimal <- function(valor){
  #' Convierte valor binario en decimal
  #' 
  #' valor: valor binario a convertir

  cantidadBits  <- length(valor)
  multiplicador <- rep(2, times = cantidadBits)
  multiplicador <- multiplicador^(order(multiplicador) - 1)
  multiplicador <- multiplicador[order(multiplicador, decreasing = TRUE)]
  decimal <- sum(valor * multiplicador)
  return(decimal)
}

# Función para dividir cromosomas
# ==============================================================================
chunk <- function(x, n){
  #' Divide el cromosoma en n partes.
  #' 
  #' x: cromosoma.
  #' n: cantidad de partes a dividir.
  
  # Vector con letras de acuerdo a la cantidad de grupos
  grupos <- letters[1:n]
  # Se crea un vector para guardar resultados
  grSplit <- c()
  # Cada letra (representa un grupo) se repite "largo del cromosoma" / "n"
  for (i in grupos){
    grSplit <- c(grSplit, rep(i, length(x) / n))
  }
  # Se divide el cromosoma en grupos y se agregan a una lista
  listaGrupos <- split(x, grSplit)
  return(listaGrupos)
}

# Funciones para graficar
# ==============================================================================
# Gráfico 2D
graficarOptimo <- function(ag, funcionObjetivo, min, max, by){
  #' Se grafica la función en 2D con el punto óptimo calculado.
  #'
  #' ag: lista de resultados obtenida con el algoritmo.
  #' funcionObjetivo: función objetivo utilizada.
  #' min: valor del límite inferior establecido.
  #' max: valor del límite superior establecido.
  #' by: separación de los puntos de la función representados en el grafico.
  
  # Se crea una secuencia de puntos según función
  x <- seq(min, max, by = by)
  y <- funcionObjetivo(x)
  df <- data.frame(cbind(x, y))
  # Visualizar óptimo
  signo <- ag$mejor_individuo[1]
  valor <- ag$mejor_individuo[-1]
  valorEntero   <- valor[1:(length(valor) - 10)]
  valorFraccion <- tail(valor, n = 10)
  # Se convierte valores binarios en decimales
  decValorEntero <- binarioAdecimal(valorEntero)
  # Si el cromosoma comienza con 1 corresponde a un valor negativo
  if(signo == 1){
    decValorEntero <- decValorEntero * -1
  }
  # Se convierte valores binarios a decimal
  decValorFraccion <- binarioAdecimal(valorFraccion)
  # Se crea el valor decimal con la parte entera y la parte fraccionaria
  mejor_x <- as.numeric(paste0(decValorEntero, ".", decValorFraccion))
  mejor_y <- funcionObjetivo(mejor_x)
  # Se grafica
  gg <- ggplot(df, aes(x, y), color = "blue") +
    geom_point() + 
    geom_point(aes(x=mejor_x, y = mejor_y), color ="red", size = 4) +
    ggtitle(paste0("Coordenada Punto Óptimo: ","(", round(mejor_x, 3), ", ", 
                   round(mejor_y, 3), ")")) +
    theme_minimal()
  return(gg)
}

# Gráfico 3D
graficarOptimo3D <- function(ag, funcionObjetivoGrafico, cntVariables, min, max){
  #' Se grafica la función en 3D con el punto óptimo calculado.
  #'
  #' ag: lista de resultados obtenida con el algoritmo.
  #' funcionObjetivoGrafico: función objetivo utilizada.
  #' cntVariables: cantidad de variables en la función.
  #' min: valor del límite inferior establecido.
  #' max: valor del límite superior establecido.

  # Se busca el mejor individuo
  cromosoma <- ag$mejor_individuo
  # Se divide en n partes el cromosoma y se guardan en una lista
  listaCromosomas <- chunk(cromosoma, cntVariables)
  # El primer elemeto del cromosoma de la parte entera representa el signo del 
  # valor decimal
  signo <- lapply(listaCromosomas, function(x) (x[1]))
  # Se divide parte entrea de fracción
  valorEntero   <- lapply(listaCromosomas, function(x) (x[2:(length(x)-10)]))
  valorFraccion <- lapply(listaCromosomas, function(x) (tail(x, 10)))
  # Se convierte valores binarios en decimales
  decValorEntero <- lapply(valorEntero, function(x) (binarioAdecimal(x)))
  # Se convierte fracción a decimal
  decValorFraccion <- lapply(valorFraccion, function(x) (binarioAdecimal(x)))
  # Se crea una vector para guardar los valores
  valorDecimal <- c()
  # Si el cromosoma comienza con 1 corresponde a un valor negativo
  for(i in 1:length(signo)){
    if(signo[[i]] == 1){
      decValorEntero[[i]] <- decValorEntero[[i]] * -1
      # Se resta la fracción decimal a la entera
      valorDecimal[i] <- as.numeric(decValorEntero[[i]]) - 
        as.numeric(paste0(0, ".", decValorFraccion[[i]]))
    } else {
      # Se suma la fracción decimal a la entera
      valorDecimal[i] <- as.numeric(decValorEntero[[i]]) + 
        as.numeric(paste0(0, ".", decValorFraccion[[i]]))
    }
  }
  # Se calcula el resultado de la función
  valorFuncion <- funcionObjetivo(valorDecimal)
  # Coordenadas óptimas
  optimo <- c(valorDecimal, funcionObjetivo(valorDecimal))
  # Geometría de la superficie
  x <- y <- seq(-100, 100, length= 100)
  z <- outer(x, y, funcionObjetivoGrafico)
  # Se grafica la superficie en 3D
  plot_ly(x = x, y = y, z = z, type = "surface") %>% 
    add_trace(x = optimo[1], y = optimo[2], z = optimo[3], mode = "markers", 
              type = "scatter3d", marker = list(size = 5, color = "red", 
                                                symbol = 104))
}

# Funciones objetivo
# ==============================================================================
funcionObjetivoBin <- function(cromosoma, funcionObjetivo, min, max, objetivo){
  #' Calcula el fitness para funciones de una variable.
  #' Se penalizan los resultados de la función objetivo que excedan los límites 
  #' establecidos (min y max).
  #' 
  #' cromosoma: cromosoma binario.
  #' funcionObjetivo: función objetivo utilizada.
  #' min: valor del límite inferior establecido.
  #' max: valor del límite superior establecido.
  #' objetivo: si se "minimizar" o "maximazar" la función.
  
  # El primer elemeto del cromosoma representa el signo del valor decimal
  signo <- cromosoma[1]
  # Se divide la parte entrea de la fracción
  valorEntero   <- cromosoma[2:(length(cromosoma)-10)]
  # Los últimos 10 genes representan la fracción decimal
  valorFraccion <- tail(cromosoma, 10)
  # Se convierte valores binarios en decimales
  decValorEntero <- binarioAdecimal(valorEntero)
  # Se convierte fracción a decimal
  decValorFraccion <- binarioAdecimal(valorFraccion)
  # Si el cromosoma comienza con 1 corresponde a un valor negativo
  if(signo == 1){
    decValorEntero <- decValorEntero * -1
    valorDecimal <- as.numeric(decValorEntero) - 
      as.numeric(paste0(0, ".", decValorFraccion))
  } else {
    valorDecimal <- as.numeric(decValorEntero) + 
      as.numeric(paste0(0, ".", decValorFraccion))
  }
  # Se calcula el resultado de la función
  valorFuncion <- funcionObjetivo(valorDecimal)
  # Se penaliza el resultado si el valor del cromosoma en decimal excede los 
  # límites establecidos
  if (objetivo == "minimizar"){
    # Se penaliza llevando el valor al límite máximo (tener en cuenta que luego
    # se cambia de signo y se busca el máximo con which.max)
    valorFuncion <- ifelse(any(valorDecimal < min, valorDecimal > max), max, 
                           valorFuncion)
  } else {
    # Se penaliza con el valor mínimo posible
    valorFuncion <- ifelse(any(valorDecimal < min, valorDecimal > max), min, 
                           valorFuncion)
  }
  return(valorFuncion)
}

funcionObjetivoBinXY <- function(cromosoma, cntVariables, funcionObjetivo, min, 
                                 max, objetivo){
  #' Calcula el fitness para funciones con múltiples variables.
  #' Se penalizan los resultados de la función objetivo que excedan los límites 
  #' establecidos (min y max).
  #' 
  #' cromosoma: cromosoma binario.
  #' cntVariables: cantidad de variables en la función objetivo.
  #' funcionObjetivo: función objetivo utilizada.
  #' min: valor del límite inferior establecido.
  #' max: valor del límite superior establecido.
  #' objetivo: si se "minimizar" o "maximazar" la función.
  
  # Se divide en n partes el cromosoma y se guardan en una lista
  listaCromosomas <- chunk(cromosoma, cntVariables)
  # El primer elemeto del cromosoma de la parte entera representa el signo del 
  # valor decimal
  signo <- lapply(listaCromosomas, function(x) (x[1]))
  # Se divide parte entrea de fracción
  valorEntero   <- lapply(listaCromosomas, function(x) (x[2:(length(x)-10)]))
  valorFraccion <- lapply(listaCromosomas, function(x) (tail(x, 10)))
  # Se convierte valores binarios en decimales
  decValorEntero <- lapply(valorEntero, function(x) (binarioAdecimal(x)))
  # Se convierte fracción a decimal
  decValorFraccion <- lapply(valorFraccion, function(x) (binarioAdecimal(x)))
  # Se crea una vector para guardar los valores
  valorDecimal <- c()
  # Si el cromosoma comienza con 1 corresponde a un valor negativo
  for(i in 1:length(signo)){
    if(signo[[i]] == 1){
      decValorEntero[[i]] <- decValorEntero[[i]] * -1
      # Se resta la fracción decimal a la entera
      valorDecimal[i] <- as.numeric(decValorEntero[[i]]) - 
        as.numeric(paste0(0, ".", decValorFraccion[[i]]))
    } else {
      # Se suma la fracción decimal a la entera
      valorDecimal[i] <- as.numeric(decValorEntero[[i]]) + 
        as.numeric(paste0(0, ".", decValorFraccion[[i]]))
    }
  }
  # Se calcula el resultado de la función
  valorFuncion <- funcionObjetivo(valorDecimal)
  # Criterios de penalización si se exceden los límites establecidos
  if (objetivo == "minimizar"){
    # Se penaliza llevando el valor al límite máximo (tener en cuenta que luego
    # se cambia de signo y se busca el máximo con which.max)
    valorFuncion <- ifelse(any(valorDecimal < min, valorDecimal > max), 
                           max, valorFuncion)
  } else {
    # Se penaliza con el valor mínimo posible
    valorFuncion <- ifelse(any(valorDecimal < min, valorDecimal > max), 
                           min, valorFuncion)
  }
  return(valorFuncion)
}

# Funciones objetivo
# ==============================================================================
seleccionarIndividuo <- function(vectorAptitud, tamanoPoblacion, 
                                 metodoSeleccion){
  #' Devuelve un índice que indica la ubicación de un individuos en la población 
  #' calculado a partir de alguno de los criterios elegido.
  #' 
  #' vectorAptitud: vector con los fitness de cada individuo de la población.
  #' tamanoPoblacion: tamaño de la población.
  #' metodoSeleccion: "ruleta" o "torneo".
  
  # Se elige un método de selección
  if (metodoSeleccion == "ruleta") {
     # Probabilidades de selección
     probSeleccion <- abs((vectorAptitud) / sum(vectorAptitud))
     # Elije un individuo al azar de la población mediante un vector de 
     # ponderaciones probabilísticas 
     indSeleccionado <- sample(x = 1:tamanoPoblacion, size = 1, 
                               prob = probSeleccion)
     # Método de selección por "Torneo"
   } else if (metodoSeleccion == "torneo") {
     # Se seleccionan aleatoriamente dos parejas de individuos
     indCandidatosA <- sample(x = 1:length(vectorAptitud), size = 2)
     indCandidatosB <- sample(x = 1:length(vectorAptitud), size = 2)
     # De cada pareja se selecciona el de mayor fitness
     indGanadorA <- ifelse(vectorAptitud[indCandidatosA[1]] > 
                             vectorAptitud[indCandidatosA[2]], indCandidatosA[1], 
                           indCandidatosA[2])
     indGanadorB <- ifelse(vectorAptitud[indCandidatosB[1]] > 
                             vectorAptitud[indCandidatosB[2]], indCandidatosB[1], 
                           indCandidatosB[2])
     # Se comparan los dos ganadores de cada pareja
     indSeleccionado <- ifelse(vectorAptitud[indGanadorA] > 
                                 vectorAptitud[indGanadorB], indGanadorA, 
                               indGanadorB)
   } else {
     stop("El argumento metodoSeleccion debe ser: ruleta o torneo")
   }
   return(indSeleccionado)
 }

# Algoritmo Genético
# ==============================================================================
algoritmoGenetico <- function(tamanoCromosoma, tamanoPoblacion, nroGeneraciones, 
                              funcionObjetivo, min, max, elitismo, probMutacion, 
                              paradaTemprana, toleranciaParada, rondasParada, 
                              metodoSeleccion, cntVariables = 1, 
                              objetivo = "maximizar"){
  #' tamanoCromosoma: tamaño del cromosoma.
  #' tamanoPoblacion: tamaño de la población.
  #' nroGeneraciones: número de generaciones.
  #' funcionObjetivo: función objetivo elegida.
  #' min: valor límite inferior del fitness 
  #' max: valor límite superior del fitness.
  #' elitismo: probabilidad de mejores individuos que pasan a la siguiente 
  #' generación.
  #' probMutacion: probabilidad de mutación.
  #' paradaTemprana: si se desea para el algoritmo cuando no hay cambios 
  #' significativos
  #' o nulos entre los resultados de las iteraciones.
  #' toleranciaParada: rango o tolerancia de parada, 0 significa que el resultado 
  #' no cambia.
  #' rondasParada: cantidad de generaciones donde el resultado no cambia dentro
  #' de un rango establecido (toleranciaParada).
  #' metodoSeleccion: "ruleta" o "torneo".
  #' cntVariables: cantidad de variables de la función objetivo.
  #' objetivo: "minimizar" o "maximizar".
  
  # Por cada generación se almacena el mejor individuo, su fitness, y la 
  # diferencia absoluta respecto a la última generación.
  resultadosAptitud   <- vector(mode = "list", length = nroGeneraciones)
  resultadosIndividuo <- vector(mode = "list", length = nroGeneraciones)
  diferenciaAbsoluta  <- vector(mode = "list", length = nroGeneraciones)
  # GENERAR POBLACIÓN INICIAL
  # Fracción entera de los límites
  # Se convierten los valores límites a binario
  fraccionEnteraMin <- decimalAbinario(abs(min))
  fraccionEnteraMax <- decimalAbinario(abs(max))
  # Se calcula la longitud de los vectores
  longMin <- length(fraccionEnteraMin)
  longMax <- length(fraccionEnteraMax)
  # Se determina el cromosoma de mayor longitud binaria
  mayorLongitud <- ifelse(longMin > longMax, longMin, longMax)
  # El algortimo permite trabajar solo con tamaños de cromosomas mayores a la 
  # longitud
  # de los cromosomas binarios de los límites
  if (tamanoCromosoma < mayorLongitud){
    stop(paste0("Error: debe elegir un cromosoma de tamaño mayor a ", 
                length(fraccionEnteraMax)))
  }
  # Fracción decimal de límites (por defecto es 0)
  decimal <- (1:10) * 0
  # Signo de los valores mínimo y máximo
  signoMin <- ifelse(min < 0, 1, 0)
  signoMax <- ifelse(max < 0, 1, 0)
  # Si el tamaño del cromosoma elegido es mayor el cromosoma binario de los 
  # límites se agregan 0 para compensar la diferencia de longitud del vector
  ## Cromosoma límite mínimo
  if(tamanoCromosoma > longMin){
    diferencia <- tamanoCromosoma - longMin
    fraccionEnteraMin <- c((1:diferencia) * 0, fraccionEnteraMin)
  } 
  ## Cromosoma límite máximo
  if (tamanoCromosoma > longMax){
    diferencia <- tamanoCromosoma - longMax
    fraccionEnteraMax <- c((1:diferencia) * 0, fraccionEnteraMax)
  }
  # Se crean los cromosomas de los límites establecidos
  cromosomaMin <- c(signoMin, fraccionEnteraMin, decimal)
  cromosomaMax <- c(signoMax, fraccionEnteraMax, decimal) 
  # Se crea una matriz con cromosomas
  # Se suma 11 (1 + 10) donde 1 gen representa el signo del número decimal y 
  # 10 genes representan la fracción decimal con 3 unidades (milésimas)
  # Ejemplo: 0(signo)-1010(entero)-0010101101(decimal) = (-)-10-173 = -10.173
  poblacion <- matrix(round(runif((tamanoCromosoma + 11) * 
                                    cntVariables * tamanoPoblacion)),
                      nrow = tamanoPoblacion, ncol = (tamanoCromosoma + 11) 
                      * cntVariables, byrow = TRUE)
  # Se reemplazan los cromosomas superiores por los límites
  # De esta forma se asegura que el resultado final va estar dentro del rango 
  # definido
  # Si hay más de una variable de multiplica el cromosoma
  poblacion[1,] <- rep(cromosomaMin, cntVariables)
  poblacion[2,] <- rep(cromosomaMax, cntVariables)
  # Nueva población
  # Se crea una matriz de inicialización del tamaño de la población
  nuevaPoblacion <- poblacion * NA
  # APTITUD DE LOS INDIVIDUOS
  # Iteración de generaciones
  for (i in 1:nroGeneraciones) {
    cat(paste0(" Generación N°: ", i, "\n"))
    # Se crea vector para guardar resultados
    vectorAptitud <- c()
    # Se evalúa la aptitud de cada cromosoma
    if (cntVariables  == 1){
      for (j in 1:tamanoPoblacion){
        vectorAptitud <- c(vectorAptitud, 
                           funcionObjetivoBin(poblacion[j, ], funcionObjetivo, 
                                              min, max, objetivo))
      }
    } else {
      for (j in 1:tamanoPoblacion){
        vectorAptitud <- c(vectorAptitud, 
                           funcionObjetivoBinXY(poblacion[j, ], cntVariables, 
                                                funcionObjetivo, min, max, objetivo))
      }
    }
    # Se cambia de signo si el objetivo es minimizar
    if (objetivo == "minimizar"){
      vectorAptitud <- -1 * vectorAptitud
    }
    # Se almacena el mejor individuo de la población actual
    mejorAptitud             <- max(vectorAptitud)
    mejorIndividuo           <- poblacion[which.max(vectorAptitud), ]
    resultadosAptitud[[i]]   <- mejorAptitud
    resultadosIndividuo[[i]] <- mejorIndividuo
    # Se calcula la diferencia absoluta respecto a la generación anterior
    # La diferencia solo puede calcularse a partir de la segunda generación
    if (i > 1) {
      diferenciaAbsoluta[[i]] <- abs(resultadosAptitud[[i - 1]] - 
                                       resultadosAptitud[[i]])
    }
    # ELITISMO
    # El elitismo indica el porcentaje de mejores individuos de la población
    # actual que pasan directamente a la siguiente población. De esta forma, se
    # asegura que, la siguiente generación, no sea nunca inferior.
    if (elitismo > 0) {
      nElitismo        <- ceiling(tamanoPoblacion * elitismo)
      posicionNmejores <- order(vectorAptitud, decreasing = TRUE)
      posicionNmejores <- posicionNmejores[1:nElitismo]
      nuevaPoblacion[1:nElitismo, ] <- poblacion[posicionNmejores, ]
    } else {
      nElitismo <- 0
    }
    # NUEVA POBLACIÓN
    # Se recorre la nueva población desde nElitismo + 1
    for (j in (nElitismo + 1):nrow(nuevaPoblacion)) { 
      # SELECCIÓN
      # Se seleccionan dos individuos de la población con el criterio elegido 
      indiceParental1 <- seleccionarIndividuo(vectorAptitud, tamanoPoblacion, 
                                              metodoSeleccion)
      indiceParental2 <- seleccionarIndividuo(vectorAptitud, tamanoPoblacion, 
                                              metodoSeleccion)
      parental1 <- poblacion[indiceParental1, ]
      parental2 <- poblacion[indiceParental2, ]
      # CRUCE
      # Se crea el vector que representa el nuevo individuo
      descendencia <- rep(NA, times = tamanoCromosoma)
      # Se seleccionan aleatoriamente las posiciones que se heredan del parental1.
      herenciaParent1 <- sample(x = c(TRUE, FALSE), size = length(parental1), 
                                replace = TRUE)
      # El resto de posiciones se heredan del parental2.
      herenciaParent2 <- !(herenciaParent1)
      # Se heredan de parental1 y parental2 los valores de cada posición
      descendencia[herenciaParent1] <- parental1[herenciaParent1]
      descendencia[herenciaParent2] <- parental2[herenciaParent2]
      # MUTACIÓN DEL INDIVIDUO
      posicionesMutadas <- 
        runif(n = length(descendencia), min = 0, max = 1) < probMutacion
      # Se remplazan los genes seleccionados al azar por 0 (si es 1) o 1 (si es 0)
      descendencia[posicionesMutadas] <- ((descendencia[posicionesMutadas] + 1) %% 2)
      # Se agrega la descendencia a la matriz de la nueva población
      nuevaPoblacion[j,] <- descendencia
    }
    # La nueva población pasa a ser la población siguiente
    poblacion <- nuevaPoblacion
    # CRITERIO DE PARADA
    # Si durante las últimas n generaciones, la diferencia absoluta entre mejores
    # individuos no es superior al valor de tolerancia de parada, se detiene el
    # algoritmo y no se crean nuevas generaciones.
    if (any(i > nroGeneraciones, paradaTemprana)) {
      ultimos <- tail(unlist(diferenciaAbsoluta), n = rondasParada)
      if (length(ultimos) == rondasParada){
        if (all(ultimos <= toleranciaParada)) {
          break()
        }
      }
    }
  }
  # RESULTADOS
  mejorIndividuo <- resultadosIndividuo[[which.max(unlist(resultadosAptitud))]]
  # Para crear el dataframe se convierten las listas a vectores del mismo tamaño.
  aptitud <- unlist(resultadosAptitud)

  diferenciaAbsoluta <- c(NA, unlist(diferenciaAbsoluta))
  # dataset con iteraciones y fitness
  df_resultados <- data.frame(
    generacion  = seq_along(aptitud),
    aptitud     = aptitud,
    diferencia_abs = diferenciaAbsoluta
  )
  return(list(aptitud = resultadosAptitud, mejores_individuos = resultadosIndividuo,
              diferencia_abs = diferenciaAbsoluta, df_resultados = df_resultados,
              mejor_individuo = mejorIndividuo))
}

# EVALUAR FUNCIONES
# ==============================================================================
# ==============================================================================
#' El algoritmo desarrollado no usa semillas por lo que cada prueba puede variar.

# Función a)
# ==============================================================================
# Función objetivo
funcionObjetivo <- function(x){
  (-x * sin(sqrt(abs(x))))
}
# Corrida del programa
ag1 <- algoritmoGenetico(tamanoCromosoma  = 10, 
                         tamanoPoblacion  = 50, 
                         nroGeneraciones  = 5000, 
                         funcionObjetivo  = funcionObjetivo,
                         min              = -512,
                         max              = 512,
                         elitismo         = 0.1, 
                         probMutacion     = 0.1, 
                         paradaTemprana   = TRUE, 
                         toleranciaParada = 0, 
                         rondasParada     = 20, 
                         metodoSeleccion  = "torneo",
                         cntVariables     = 1,
                         objetivo         = "minimizar")
# Gráfico de la función con el punto óptimo calculado
graficarOptimo(ag1, funcionObjetivo, -512, 512)

# Función b)
# ==============================================================================
# Función objetivo
funcionObjetivo <- function(x){
  (x + 5*sin(3*x) + 8*cos(5*x))
}
# Corrida del programa
ag2 <- algoritmoGenetico (tamanoCromosoma  = 5, 
                          tamanoPoblacion  = 50, 
                          nroGeneraciones  = 5000, 
                          funcionObjetivo  = funcionObjetivo,
                          min              = 0,
                          max              = 20,
                          elitismo         = 0.1, 
                          probMutacion     = 0.1, 
                          paradaTemprana   = TRUE, 
                          toleranciaParada = 0, 
                          rondasParada     = 20, 
                          metodoSeleccion  = "torneo",
                          cntVariables     = 1,
                          objetivo         = "minimizar")

# Gráfico de la función con el punto óptimo calculado
graficarOptimo(ag2, funcionObjetivo, 0, 20, 0.01)

# Función c)
# ==============================================================================
# Función objetivo para optimizar
funcionObjetivo <- function(vectorVar){
  x = vectorVar[1]
  y = vectorVar[2]
  (x^2 + y^2)^0.25 * ((0.5 - 0.5 * cos(2 * (50*(x^2 + y^2)^0.1))) + 1)
}
# Función objetivo para graficar
funcionObjetivoGrafico <- function(x, y){
  (x^2 + y^2)^0.25 * ((0.5 - 0.5 * cos(2 * (50*(x^2 + y^2)^0.1))) + 1)
}
# Corrida del programa
ag3 <- algoritmoGenetico (tamanoCromosoma  = 7, 
                          tamanoPoblacion  = 50, 
                          nroGeneraciones  = 5000, 
                          funcionObjetivo  = funcionObjetivo,
                          min              = -100,
                          max              = 100,
                          elitismo         = 0.1, 
                          probMutacion     = 0.1, 
                          paradaTemprana   = TRUE, 
                          toleranciaParada = 0, 
                          rondasParada     = 20, 
                          metodoSeleccion  = "torneo",
                          cntVariables     = 2,
                          objetivo         = "minimizar")
# Gráfico de la superficie de la función con el punto óptimo calculado
graficarOptimo3D(ag3, funcionObjetivoGrafico, 2, -100, 100)

