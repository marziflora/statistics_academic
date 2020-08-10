#Zad 1
klasyczne <- c(65, 79, 90, 75, 61, 85, 98, 80, 97, 75)
nowe <-      c(90, 98, 73, 79, 84, 81, 98, 90, 83, 88)
#Czy na poziomie istotno±ci 0.05 możemy twierdzi¢, że próby są podobne?
#Sprawdzenie rozkładu norm:
shapiro.test(klasyczne) #H0
shapiro.test(nowe) #H0
#Sprawdzenie równości wariancji:
#library("car")
y <- c(klasyczne, nowe)
group <- as.factor(c(rep(1, length(klasyczne)), rep(2, length(nowe))))
library(sur)
levenes.test(y, group) #0.2384 H0
#FUNKCJA Z WYKŁADU:
diff = function ( samp ){
  s= sort ( samp )
  l= length ( samp )
  d =1:( l -1)
  for (k in 2: l ){ d[k -1]= s [k]- s[k -1]}
  return ( d)}
aly = function ( samp ){
  stat =0
  l= length ( samp )
  for (k in 1: l ) stat = stat +k*(l +1 - k)* samp [k ]
  return ( stat )}
vartest = function ( samp1 , samp2 , NMonte ){
  d1 = diff ( samp1 )
  d2 = diff ( samp2 )
  l= length ( d1 )
  stat0 = aly ( d1 )
  pd = d1
  cnt =0
  for (j in 1: NMonte ){  r= rbinom (l ,1 ,.5)
    for (k in 1: l ){ pd [k ]= ifelse (r[ k], d1 [k], d2 [k ]) }
    if( aly ( pd ) >= stat0 ) cnt = cnt +1  }
  return ( cnt / NMonte )  } ##one - sided p- value
vartest (klasyczne, nowe, 3000) # 0.144 .> H0

#T studenta dla sparowanych
diff = klasyczne - nowe
#N = 1000 #albo 2**N
n = length (diff)
N = 2**n
stat = numeric(n)
orgttest = t.test(diff)$statistic
cnt = 0 

for (i in 1: N ){
  for (j in 1: n ){
    stat [j]= ifelse (runif (1) < 0.5 , diff [j] , - diff [ j ]) } 
  if(t.test(stat)$statistic >= orgttest ){ cnt = cnt +1}}
cnt /N #
2*cnt/N #p-value = 0.234 - brak różnic.

#Test t-studenta
t.test(klasyczne, nowe) #p-value = 0.2239 = H0
#ODP: Zastosowane metody są porównywalne, brak różnic.

#ZAD2.
Old <- c(141.85, 134.36, 131.87, 137.28, 122.72, 136.44, 128.96, 136.86)
New <- c(125.10, 122.00, 123.10, 119.92, 124.11, 121.91, 122.00, 135.95, 127.10, 125.10)
shapiro.test(Old) #H0
shapiro.test(New) #H1, p=0.01277
Permutation = choose(length(Old)+length(New), length(Old)) #43758
options(warn = -1) 

wilcoxontest <- function(sample1, sample2, Permutation){
  orgtest = wilcox.test(sample1, sample2)$statistic
  combin = combn((length(sample1)+length(sample2)), length(sample1))
  cnt=0
  z = c(sample1, sample2)
  for (i in 1:Permutation){
  x1 <-z[combin[,i]]
  y1 <-z[-combin[,i]]
  wilcoxon_value = wilcox.test(x1,y1)$statistic 
  if (wilcoxon_value >= orgtest){ cnt=cnt+1 }}#dwustronny test, dlatego mnożę 2*
  return (cnt/Permutation)}
wilcoxontest(Old, New, 1000) #p wartość
0.036
#Istnieją różnice w 2 próbach

#Zad3. New = Old*0.9
#value = 0.9*mean(Old) #120.4133
#t.test(New, mu=value)
wilcoxontest_lower <- function(sample1, sample2, Permutation){
  orgtest = wilcox.test(sample1, sample2)$statistic
  combin = combn((length(sample1)+length(sample2)), length(sample1))
  cnt=0
  z = c(sample1, sample2)
  for (i in 1:Permutation){
    x1 <-z[combin[,i]]
    y1 <-z[-combin[,i]]
    wilcoxon_value = wilcox.test(x1,y1)$statistic 
  if (wilcoxon_value >= orgtest) {cnt=cnt+1}} #dwustronny, czy brak różnic w pomniejszonej próbce Old
  return (cnt/Permutation)}
wilcoxontest_lower((Old*0.9), New, 4000)
#0.94225 --> H0 o braku różnic między 2017*0.9, a więc jest różnica o 10%.

#Zad4.
library("mvtnorm")
my_sample = rmvnorm(n=100, mean=c(0,0), sigma=diag(2))

#Współczynnik korelacji:
bootstrap <- function(N, my_sample){
  n <- length(my_sample)
  coeff <- numeric(N)
  for (i in 1:N) {
    x = sample(my_sample, n, replace = TRUE)
    y = sample(my_sample, n, replace = TRUE)
    coeff[i] = cor(x,y) }
  return (coeff)
}
boot = bootstrap(1000,my_sample)
coeff_mean = mean(boot) #średnia ze współczynników korelacji z losowanych danych
#0.001523495
sd(boot)
library(psychometric) 
CIr(r=coeff_mean, n=1000, level=.95) #-0.06047530  0.06351058

N = 1000
CI_lower = coeff_mean - 1.96*(1/sqrt(N-3))
CI_lower
CI_higher = coeff_mean + 1.96*(1/sqrt(N-3))
CI_higher
#wychodzi to samo ręcznie.

