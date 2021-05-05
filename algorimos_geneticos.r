rm(list=ls())
gc()

# Librer�as
# ==============================================================================
if (!require("plotly"))  install.packages("plotly") ; library("plotly")
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")

# Funciones de conversi�n binario/decimal
# ==============================================================================
decimalAbinario <- function(numero){
  #' Convierte n�meros decimales a binario.
  #' 
  #' numero: n�mero decimal a convertir.
  
  # Se redondea el n�mero
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

# Funci�n para dividir cromosomas
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
# Gr�fico 2D
graficarOptimo <- function(ag, funcionObjetivo, min, max, by){
  #' Se grafica la funci�n en 2D con el punto �ptimo calculado.
  #'
  #' ag: lista de resultados obtenida con el algoritmo.
  #' funcionObjetivo: funci�n objetivo utilizada.
  #' min: valor del l�mite inferior establecido.
  #' max: valor del l�mite superior establecido.
  #' by: separaci�n de los puntos de la funci�n representados en el grafico.
  
  # Se crea una secuencia de puntos seg�n funci�n
  x <- seq(min, max, by = by)
  y <- funcionObjetivo(x)
  df <- data.frame(cbind(x, y))
  # Visualizar �ptimo
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
    ggtitle(paste0("Coordenada Punto �ptimo: ","(", round(mejor_x, 3), ", ", 
                   round(mejor_y, 3), ")")) +
    theme_minimal()
  return(gg)
}

# Gr�fico 3D
graficarOptimo3D <- function(ag, funcionObjetivoGrafico, cntVariables, min, max){
  #' Se grafica la funci�n en 3D con el punto �ptimo calculado.
  #'
  #' ag: lista de resultados obtenida con el algoritmo.
  #' funcionObjetivoGrafico: funci�n objetivo utilizada.
  #' cntVariables: cantidad de variables en la funci�n.
  #' min: valor del l�mite inferior establecido.
  #' max: valor del l�mite superior establecido.

  # Se busca el mejor individuo
  cromosoma <- ag$mejor_individuo
  # Se divide en n partes el cromosoma y se guardan en una lista
  listaCromosomas <- chunk(cromosoma, cntVariables)
  # El primer elemeto del cromosoma de la parte entera representa el signo del 
  # valor decimal
  signo <- lapply(listaCromosomas, function(x) (x[1]))
  # Se divide parte entrea de fracci�n
  valorEntero   <- lapply(listaCromosomas, function(x) (x[2:(length(x)-10)]))
  valorFraccion <- lapply(listaCromosomas, function(x) (tail(x, 10)))
  # Se convierte valores binarios en decimales
  decValorEntero <- lapply(valorEntero, function(x) (binarioAdecimal(x)))
  # Se convierte fracci�n a decimal
  decValorFraccion <- lapply(valorFraccion, function(x) (binarioAdecimal(x)))
  # Se crea una vector para guardar los valores
  valorDecimal <- c()
  # Si el cromosoma comienza con 1 corresponde a un valor negativo
  for(i in 1:length(signo)){
    if(signo[[i]] == 1){
      decValorEntero[[i]] <- decValorEntero[[i]] * -1
      # Se resta la fracci�n decimal a la entera
      valorDecimal[i] <- as.numeric(decValorEntero[[i]]) - 
        as.numeric(paste0(0, ".", decValorFraccion[[i]]))
    } else {
      # Se suma la fracci�n decimal a la entera
      valorDecimal[i] <- as.numeric(decValorEntero[[i]]) + 
        as.numeric(paste0(0, ".", decValorFraccion[[i]]))
    }
  }
  # Se calcula el resultado de la funci�n
  valorFuncion <- funcionObjetivo(valorDecimal)
  # Coordenadas �ptimas
  optimo <- c(valorDecimal, funcionObjetivo(valorDecimal))
  # Geometr�a de la superficie
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
  #' Se penalizan los resultados de la funci�n objetivo que excedan los l�mites 
  #' establecidos (min y max).
  #' 
  #' cromosoma: cromosoma binario.
  #' funcionObjetivo: funci�n objetivo utilizada.
  #' min: valor del l�mite inferior establecido.
  #' max: valor del l�mite superior establecido.
  #' objetivo: si se "minimizar" o "maximazar" la funci�n.
  
  # El primer elemeto del cromosoma representa el signo del valor decimal
  signo <- cromosoma[1]
  # Se divide la parte entrea de la fracci�n
  valorEntero   <- cromosoma[2:(length(cromosoma)-10)]
  # Los �ltimos 10 genes representan la fracci�n decimal
  valorFraccion <- tail(cromosoma, 10)
  # Se convierte valores binarios en decimales
  decValorEntero <- binarioAdecimal(valorEntero)
  # Se convierte fracci�n a decimal
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
  # Se calcula el resultado de la funci�n
  valorFuncion <- funcionObjetivo(valorDecimal)
  # Se penaliza el resultado si el valor del cromosoma en decimal excede los 
  # l�mites establecidos
  if (objetivo == "minimizar"){
    # Se penaliza llevando el valor al l�mite m�ximo (tener en cuenta que luego
    # se cambia de signo y se busca el m�ximo con which.max)
    valorFuncion <- ifelse(any(valorDecimal < min, valorDecimal > max), max, 
                           valorFuncion)
  } else {
    # Se penaliza con el valor m�nimo posible
    valorFuncion <- ifelse(any(valorDecimal < min, valorDecimal > max), min, 
                           valorFuncion)
  }
  return(valorFuncion)
}

funcionObjetivoBinXY <- function(cromosoma, cntVariables, funcionObjetivo, min, 
                                 max, objetivo){
  #' Calcula el fitness para funciones con m�ltiples variables.
  #' Se penalizan los resultados de la funci�n objetivo que excedan los l�mites 
  #' establecidos (min y max).
  #' 
  #' cromosoma: cromosoma binario.
  #' cntVariables: cantidad de variables en la funci�n objetivo.
  #' funcionObjetivo: funci�n objetivo utilizada.
  #' min: valor del l�mite inferior establecido.
  #' max: valor del l�mite superior establecido.
  #' objetivo: si se "minimizar" o "maximazar" la funci�n.
  
  # Se divide en n partes el cromosoma y se guardan en una lista
  listaCromosomas <- chunk(cromosoma, cntVariables)
  # El primer elemeto del cromosoma de la parte entera representa el signo del 
  # valor decimal
  signo <- lapply(listaCromosomas, function(x) (x[1]))
  # Se divide parte entrea de fracci�n
  valorEntero   <- lapply(listaCromosomas, function(x) (x[2:(length(x)-10)]))
  valorFraccion <- lapply(listaCromosomas, function(x) (tail(x, 10)))
  # Se convierte valores binarios en decimales
  decValorEntero <- lapply(valorEntero, function(x) (binarioAdecimal(x)))
  # Se convierte fracci�n a decimal
  decValorFraccion <- lapply(valorFraccion, function(x) (binarioAdecimal(x)))
  # Se crea una vector para guardar los valores
  valorDecimal <- c()
  # Si el cromosoma comienza con 1 corresponde a un valor negativo
  for(i in 1:length(signo)){
    if(signo[[i]] == 1){
      decValorEntero[[i]] <- decValorEntero[[i]] * -1
      # Se resta la fracci�n decimal a la entera
      valorDecimal[i] <- as.numeric(decValorEntero[[i]]) - 
        as.numeric(paste0(0, ".", decValorFraccion[[i]]))
    } else {
      # Se suma la fracci�n decimal a la entera
      valorDecimal[i] <- as.numeric(decValorEntero[[i]]) + 
        as.numeric(paste0(0, ".", decValorFraccion[[i]]))
    }
  }
  # Se calcula el resultado de la funci�n
  valorFuncion <- funcionObjetivo(valorDecimal)
  # Criterios de penalizaci�n si se exceden los l�mites establecidos
  if (objetivo == "minimizar"){
    # Se penaliza llevando el valor al l�mite m�ximo (tener en cuenta que luego
    # se cambia de signo y se busca el m�ximo con which.max)
    valorFuncion <- ifelse(any(valorDecimal < min, valorDecimal > max), 
                           max, valorFuncion)
  } else {
    # Se penaliza con el valor m�nimo posible
    valorFuncion <- ifelse(any(valorDecimal < min, valorDecimal > max), 
                           min, valorFuncion)
  }
  return(valorFuncion)
}

# Funciones objetivo
# ==============================================================================
seleccionarIndividuo <- function(vectorAptitud, tamanoPoblacion, 
                                 metodoSeleccion){
  #' Devuelve un �ndice que indica la ubicaci�n de un individuos en la poblaci�n 
  #' calculado a partir de alguno de los criterios elegido.
  #' 
  #' vectorAptitud: vector con los fitness de cada individuo de la poblaci�n.
  #' tamanoPoblacion: tama�o de la poblaci�n.
  #' metodoSeleccion: "ruleta" o "torneo".
  
  # Se elige un m�todo de selecci�n
  if (metodoSeleccion == "ruleta") {
     # Probabilidades de selecci�n
     probSeleccion <- abs((vectorAptitud) / sum(vectorAptitud))
     # Elije un individuo al azar de la poblaci�n mediante un vector de 
     # ponderaciones probabil�sticas 
     indSeleccionado <- sample(x = 1:tamanoPoblacion, size = 1, 
                               prob = probSeleccion)
     # M�todo de selecci�n por "Torneo"
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

# Algoritmo Gen�tico
# ==============================================================================
algoritmoGenetico <- function(tamanoCromosoma, tamanoPoblacion, nroGeneraciones, 
                              funcionObjetivo, min, max, elitismo, probMutacion, 
                              paradaTemprana, toleranciaParada, rondasParada, 
                              metodoSeleccion, cntVariables = 1, 
                              objetivo = "maximizar"){
  #' tamanoCromosoma: tama�o del cromosoma.
  #' tamanoPoblacion: tama�o de la poblaci�n.
  #' nroGeneraciones: n�mero de generaciones.
  #' funcionObjetivo: funci�n objetivo elegida.
  #' min: valor l�mite inferior del fitness 
  #' max: valor l�mite superior del fitness.
  #' elitismo: probabilidad de mejores individuos que pasan a la siguiente 
  #' generaci�n.
  #' probMutacion: probabilidad de mutaci�n.
  #' paradaTemprana: si se desea para el algoritmo cuando no hay cambios 
  #' significativos
  #' o nulos entre los resultados de las iteraciones.
  #' toleranciaParada: rango o tolerancia de parada, 0 significa que el resultado 
  #' no cambia.
  #' rondasParada: cantidad de generaciones donde el resultado no cambia dentro
  #' de un rango establecido (toleranciaParada).
  #' metodoSeleccion: "ruleta" o "torneo".
  #' cntVariables: cantidad de variables de la funci�n objetivo.
  #' objetivo: "minimizar" o "maximizar".
  
  # Por cada generaci�n se almacena el mejor individuo, su fitness, y la 
  # diferencia absoluta respecto a la �ltima generaci�n.
  resultadosAptitud   <- vector(mode = "list", length = nroGeneraciones)
  resultadosIndividuo <- vector(mode = "list", length = nroGeneraciones)
  diferenciaAbsoluta  <- vector(mode = "list", length = nroGeneraciones)
  # GENERAR POBLACI�N INICIAL
  # Fracci�n entera de los l�mites
  # Se convierten los valores l�mites a binario
  fraccionEnteraMin <- decimalAbinario(abs(min))
  fraccionEnteraMax <- decimalAbinario(abs(max))
  # Se calcula la longitud de los vectores
  longMin <- length(fraccionEnteraMin)
  longMax <- length(fraccionEnteraMax)
  # Se determina el cromosoma de mayor longitud binaria
  mayorLongitud <- ifelse(longMin > longMax, longMin, longMax)
  # El algortimo permite trabajar solo con tama�os de cromosomas mayores a la 
  # longitud
  # de los cromosomas binarios de los l�mites
  if (tamanoCromosoma < mayorLongitud){
    stop(paste0("Error: debe elegir un cromosoma de tama�o mayor a ", 
                length(fraccionEnteraMax)))
  }
  # Fracci�n decimal de l�mites (por defecto es 0)
  decimal <- (1:10) * 0
  # Signo de los valores m�nimo y m�ximo
  signoMin <- ifelse(min < 0, 1, 0)
  signoMax <- ifelse(max < 0, 1, 0)
  # Si el tama�o del cromosoma elegido es mayor el cromosoma binario de los 
  # l�mites se agregan 0 para compensar la diferencia de longitud del vector
  ## Cromosoma l�mite m�nimo
  if(tamanoCromosoma > longMin){
    diferencia <- tamanoCromosoma - longMin
    fraccionEnteraMin <- c((1:diferencia) * 0, fraccionEnteraMin)
  } 
  ## Cromosoma l�mite m�ximo
  if (tamanoCromosoma > longMax){
    diferencia <- tamanoCromosoma - longMax
    fraccionEnteraMax <- c((1:diferencia) * 0, fraccionEnteraMax)
  }
  # Se crean los cromosomas de los l�mites establecidos
  cromosomaMin <- c(signoMin, fraccionEnteraMin, decimal)
  cromosomaMax <- c(signoMax, fraccionEnteraMax, decimal) 
  # Se crea una matriz con cromosomas
  # Se suma 11 (1 + 10) donde 1 gen representa el signo del n�mero decimal y 
  # 10 genes representan la fracci�n decimal con 3 unidades (mil�simas)
  # Ejemplo: 0(signo)-1010(entero)-0010101101(decimal) = (-)-10-173 = -10.173
  poblacion <- matrix(round(runif((tamanoCromosoma + 11) * 
                                    cntVariables * tamanoPoblacion)),
                      nrow = tamanoPoblacion, ncol = (tamanoCromosoma + 11) 
                      * cntVariables, byrow = TRUE)
  # Se reemplazan los cromosomas superiores por los l�mites
  # De esta forma se asegura que el resultado final va estar dentro del rango 
  # definido
  # Si hay m�s de una variable de multiplica el cromosoma
  poblacion[1,] <- rep(cromosomaMin, cntVariables)
  poblacion[2,] <- rep(cromosomaMax, cntVariables)
  # Nueva poblaci�n
  # Se crea una matriz de inicializaci�n del tama�o de la poblaci�n
  nuevaPoblacion <- poblacion * NA
  # APTITUD DE LOS INDIVIDUOS
  # Iteraci�n de generaciones
  for (i in 1:nroGeneraciones) {
    cat(paste0(" Generaci�n N�: ", i, "\n"))
    # Se crea vector para guardar resultados
    vectorAptitud <- c()
    # Se eval�a la aptitud de cada cromosoma
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
    # Se almacena el mejor individuo de la poblaci�n actual
    mejorAptitud             <- max(vectorAptitud)
    mejorIndividuo           <- poblacion[which.max(vectorAptitud), ]
    resultadosAptitud[[i]]   <- mejorAptitud
    resultadosIndividuo[[i]] <- mejorIndividuo
    # Se calcula la diferencia absoluta respecto a la generaci�n anterior
    # La diferencia solo puede calcularse a partir de la segunda generaci�n
    if (i > 1) {
      diferenciaAbsoluta[[i]] <- abs(resultadosAptitud[[i - 1]] - 
                                       resultadosAptitud[[i]])
    }
    # ELITISMO
    # El elitismo indica el porcentaje de mejores individuos de la poblaci�n
    # actual que pasan directamente a la siguiente poblaci�n. De esta forma, se
    # asegura que, la siguiente generaci�n, no sea nunca inferior.
    if (elitismo > 0) {
      nElitismo        <- ceiling(tamanoPoblacion * elitismo)
      posicionNmejores <- order(vectorAptitud, decreasing = TRUE)
      posicionNmejores <- posicionNmejores[1:nElitismo]
      nuevaPoblacion[1:nElitismo, ] <- poblacion[posicionNmejores, ]
    } else {
      nElitismo <- 0
    }
    # NUEVA POBLACI�N
    # Se recorre la nueva poblaci�n desde nElitismo + 1
    for (j in (nElitismo + 1):nrow(nuevaPoblacion)) { 
      # SELECCI�N
      # Se seleccionan dos individuos de la poblaci�n con el criterio elegido 
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
      # Se heredan de parental1 y parental2 los valores de cada posici�n
      descendencia[herenciaParent1] <- parental1[herenciaParent1]
      descendencia[herenciaParent2] <- parental2[herenciaParent2]
      # MUTACI�N DEL INDIVIDUO
      posicionesMutadas <- 
        runif(n = length(descendencia), min = 0, max = 1) < probMutacion
      # Se remplazan los genes seleccionados al azar por 0 (si es 1) o 1 (si es 0)
      descendencia[posicionesMutadas] <- ((descendencia[posicionesMutadas] + 1) %% 2)
      # Se agrega la descendencia a la matriz de la nueva poblaci�n
      nuevaPoblacion[j,] <- descendencia
    }
    # La nueva poblaci�n pasa a ser la poblaci�n siguiente
    poblacion <- nuevaPoblacion
    # CRITERIO DE PARADA
    # Si durante las �ltimas n generaciones, la diferencia absoluta entre mejores
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
  # Para crear el dataframe se convierten las listas a vectores del mismo tama�o.
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

# Funci�n a)
# ==============================================================================
# Funci�n objetivo
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
# Gr�fico de la funci�n con el punto �ptimo calculado
graficarOptimo(ag1, funcionObjetivo, -512, 512)

# Funci�n b)
# ==============================================================================
# Funci�n objetivo
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

# Gr�fico de la funci�n con el punto �ptimo calculado
graficarOptimo(ag2, funcionObjetivo, 0, 20, 0.01)

# Funci�n c)
# ==============================================================================
# Funci�n objetivo para optimizar
funcionObjetivo <- function(vectorVar){
  x = vectorVar[1]
  y = vectorVar[2]
  (x^2 + y^2)^0.25 * ((0.5 - 0.5 * cos(2 * (50*(x^2 + y^2)^0.1))) + 1)
}
# Funci�n objetivo para graficar
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
# Gr�fico de la superficie de la funci�n con el punto �ptimo calculado
graficarOptimo3D(ag3, funcionObjetivoGrafico, 2, -100, 100)

