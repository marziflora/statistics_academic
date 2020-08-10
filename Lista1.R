#Zad 4, lista1
class = c(138.5, 138.5, 140.0, 141.0, 141.0, 143.5, 145.0, 147.0, 148.5, 150.0, 153.0,
154.0, 155.0, 156.5, 157.0, 158.5, 159.0, 159.0, 159.0, 160.5, 161.0, 162)
#a)
plot(class)
hist(class)
#b) Nie wiemy, ale możemy przeprowadzić test na sprawdzenie normalności
shapiro.test(class)
#p value =  p-value = 0.02798 --> małe --> nie pochodzą z rozkładu norm wg H1

#c) 
n <- length(class)
N <- 5 #ile prób bootstrapowych
stat <- numeric(N)
macierz <-   matrix(N, n)
x <- numeric(N*n)

for (i in 1:N) {
  classB = sample (class, n, replace = TRUE)
  stat[i] = mean(classB)
  for (j in 1:n){
    x[j+n*i] = classB[j] #wszystkie spisane wartości classB
  }
}


boxplot(stat)
stripchart(stat)
range(stat) #57.37879 67.21861 --> zakres wariancji w próbach bootstrapowych
var(class) #66.77976 --> wariancja z próby orginalnej
mean(class) #średnia orginalna
mean(stat) #62.3118 -> średnia z wariancji z prób = Estymator
#Możemy powiedzieć, że wariancja z 5 prób bootrapowych mieści się w przedziale^,
#a wariancja z próby=66.77976 i średnia wariancja z prób boot=62,3118

#d) Oszacuj średni wzrost
N <- 100 #ile prób bootstrapowych
stat <- numeric(N)
x <- numeric(N*n)
wzrostsredni <- numeric(N)

for (i in 1:N) {
  classB = sample (class, n, replace = TRUE)
  stat[i] = var(classB)
  wzrostsredni[i] = mean(classB)
  for (j in 1:n){
    x[j+n*i] = classB[j] #wszystkie spisane wartości classB
  }
}

mean(stat) #63.50594 - średnia variancja wzrostu
mean(x) #149.5126 - oszacowany średni wzrost  na podstawie 100 prób bootstap.
mean(class) #151.25 - średni wzrost na podstawie orginalnej próby
mean(wzrostsredni) #!!!! 150.9436 - średni wzrost na podst 100 prób bootstrap (średnia ze średnich)
#e) błąd standardowy = różnica między średnią ze średnich - każda średnia
sqrt(1/(N-1) * sum((wzrostsredni-mean(wzrostsredni))^2))   # 1.884176
sd(wzrostsredni) #to samo co wyżej^ #błęd standardowy powinniśmy jeszcze uśrednić o pierw z rozmiaru

#obciążenia dla estymatora średniej próby:
bias_var_estymatora = mean(var(class))-mean(stat) #3.577089 wariancja z próby-średnia z wariancji z bootstrap
obc_boot = abs(mean(class)-mean(wzrostsredni))
#0.15022727 (albo na odwrót: średnia z estymatorów bootstrap-średnia na podst próby)

#f) 
zad4_f <- function(N){
  stat <- numeric(N)
  x <- numeric(N*n)
  wzrostsredni <- numeric(N)
  
  for (i in 1:N) {
    classB = sample (class, n, replace = TRUE)
    wzrostsredni[i] = mean(classB) }
  return (wzrostsredni)
}
mean(class) # 151.25
wzrostsredni = (zad4_f(100))
mean(wzrostsredni)
abs(mean(class)-mean(wzrostsredni)) 

wzrostsredni = (zad4_f(200))
mean(wzrostsredni)
abs(mean(class)-mean(wzrostsredni)) 

wzrostsredni = (zad4_f(500))
mean(wzrostsredni)
abs(mean(class)-mean(wzrostsredni)) 

wzrostsredni = (zad4_f(1000))
mean(wzrostsredni)
abs(mean(class)-mean(wzrostsredni)) 

wzrostsredni = (zad4_f(10000))
mean(wzrostsredni) #przybliżenie estymatora do wartości z oryginalnej próby
abs(mean(class)-mean(wzrostsredni)) #Poprawa estymacji (zmniejszenie obciążenia)

srednie = cbind(c(mean(wzrost), sd(wzrost), mean(wzrost)-mean()))

abs(mean(class)-mean(wzrostsredni)) #0.1165909  e)Obciążenie Estymatora: średnia z estymatorów bootstrap-średnia na podst próby)

#mean(var(class))-mean(stat) # 3.269922 - wzrost wariancji

#g)
N <- 100 #ile prób bootstrapowych
stat <- numeric(N)
wzrostsredni <- numeric(N)

for (i in 1:N) {
  classB = sample (class, n, replace = TRUE)
  stat[i] = var(classB)
  wzrostsredni[i] = median(classB)
  }
median(var(class))-median(stat) #2.935065 dla 10000, 1.839827 dla 100. Dla wariancji 
abs(median(class)-median(wzrostsredni)) # 0.5 dla 100, 0 dla 1000. Czyli maleje wraz ze wzrostem ilości prób boots.

#Precyzja rośnie wraz z ilością losowań, obciążenie estymatora maleje
