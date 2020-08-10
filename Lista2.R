  #######################  LISTA 2 ##############################3
  #1. JACKKNIFE
  sample_lista2_1 = rnorm(20)
  N <- length(sample_lista2_1)
  
  #Zad2
  zad2_JK <- function(N, proba){
    mean_value_JK <- numeric(N)
    for (i in 1:N){
      x = proba[-i]
      mean_value_JK[i] = mean(x)
    }
    return (mean_value_JK)
  }
  
  zad2_BT <- function(N, proba){
    mean_value_BT <- numeric(N)
    for (i in 1:N) {
      x = sample(proba, N, replace = TRUE)
      mean_value_BT[i] = mean(x) }
    return (mean_value_BT)
  }
  value_BT = zad2_BT(20, sample_lista2_1)
  value_JK = zad2_JK(20, sample_lista2_1)
  mean(zad2_JK(20, sample_lista2_1)) #  --> średnia ze średnich z wartości z jackknife
  mean(zad2_BT(20, sample_lista2_1)) #--> średnia ze średnich w bootstrapowych próbach
  
  #3. Dla bootstrapa: 
  BT_error = sqrt(1/(N-1) * sum((value_BT-mean(value_BT))^2))  #0.1513944- Błąd standardowy dla Bootstrapa
  BT_bias = abs(mean(value_BT)-mean(sample_lista2_1)) #0.02752436 (albo na odwrót: średnia z estymatorów bootstrap-średnia na podst próby)
  
  #Dla Jackkknife
  JK_error = sqrt((N-1)/N * sum((value_JK-mean(value_JK))^2))  #0.1939156 
  JK_obc = abs((N-1) * (mean(value_JK)-mean(sample_lista2_1))) #0 
  
  #4 Powtorzyć 100x
  N=100
  value_BT = zad2_BT(100, sample_lista2_1)
  value_JK = zad2_JK(100, sample_lista2_1)
  
  BT_error = sqrt(1/(N-1) * sum((value_BT-mean(value_BT))^2))  # 0.09576123
  BT_bias = abs(mean(value_BT)-mean(sample_lista2_1)) #  0.01665157
  
  JK_error = sqrt((N-1)/N * sum((value_JK-mean(value_JK))^2))  #0.2136318
  JK_bias = abs((N-1) * (mean(value_JK)-mean(sample_lista2_1))) #0 
  
  result <- c()
  result$BT <- c(BT_error, BT_bias)
  result$JK <- c(JK_error, JK_bias)
  #Błąd przy JK jest wyższy, ale obciążenie mniejsze
  
  #5 Rozkład dwumianowy oraz wykładniczy
  N=100
  sample_2 = rbinom(100, size=2, prob=0.5)
  sample_3 = rexp(100)
  
  BT = zad2_BT(100, sample_2) #Wszystkie wartości średnich z wylosowanych prób
  JK = zad2_JK(100, sample_2)
  
  zad5_func <- function(N, proba){
    value_BT = zad2_BT(N, proba)
    value_JK = zad2_JK(N, proba)
    
    BT_error = sqrt(1/(N-1) * sum((value_BT-mean(value_BT))^2))  
    BT_bias = abs(mean(value_BT)-mean(proba)) 
    
    JK_error = sqrt((N-1)/N * sum((value_JK-mean(value_JK))^2))  
    JK_bias = abs((N-1) * (mean(value_JK)-mean(proba))) 
    result <- c()
    result$BT <- c(BT_error, BT_bias)
    result$JK <- c(JK_error, JK_bias)
    print(result)
  } 
  zad5_func(100, sample_2)
  zad5_func(100, sample_3)
