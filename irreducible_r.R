
# Author: Jazielinho

# Dado un decimal, devuelve una fracción irreducible
# ejemplo:
# decimal = 0.25
# retorna = 1 / 4




get_irreducible <- function(numero){
  tryCatch({
  numero <- as.numeric(numero)
  
  if((0.0001 <= numero) & (numero <= 0.9999)){
    # Verificamos que el numero esté dentro de un rango válido
    
    # convertimos el decimal en string (coma flotante)
    decimal <- strsplit(as.character(numero), '[.]')[[1]][2]
    
    # Calculamos el numero de decimales y convertimos el decimal en fraccion
    num_decimal <- nchar(decimal)
    denominador <- 10 ^ num_decimal
    numerador <- as.integer(decimal)

    # Calculando el máximo común divisor
    valor_1 <- numerador
    valor_2 <- denominador
    
    while(valor_2 != 0){
      aux_1 <- valor_1
      aux_2 <- valor_2
      valor_1 <- aux_2
      valor_2 <- aux_1 %% aux_2
    }
    
    mcd <- valor_1
    
    print(paste("numero ingresado:", numero))
    print(paste("fracción irreducible:", as.integer(numerador/mcd), "/", 
                as.integer(denominador/mcd)))
  }else{
    print(paste("El numero", numero, "no es valido"))
  }
  },
  error = function(e){
    print(paste("Error en get_irreducible, numero:", numero, "error:", e))
  },
  warning = function(e){
    print(paste("Error en get_irreducible, numero:", numero, "error:", e))
  }
  )
}



main <- function () {
  numero <- readline(prompt = "Ingrese numero:\t")
  get_irreducible(numero = numero)
}

if (!interactive()){
  main()
}

