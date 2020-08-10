###############KONSULTACJE: 1.
klasyczne <- c(65, 79, 90, 75, 61, 85, 98, 80, 97, 75)
nowe <-      c(90, 98, 73, 79, 84, 81, 98, 90, 83, 88)

wiloxtest = wilcox.test(klasyczne, nowe, paired=T)$statistic #0.26#Bo dane nie są ciągłe więc nie ma rozkładu normalnego.
klasyczne_s <- numeric(n)
nowe_s <- numeric(n)
n= length(klasyczne)
N=500
cnt = 0

for (i in 1:N){
  for (j in 1:n){
    if (runif(1) < 0.5){
      klasyczne_s[j] = klasyczne[j]
      nowe_s[j] =nowe[j]}
    else{
      klasyczne_s[j] = nowe[j]
      nowe_s[j] = klasyczne[j] } }
  tt_s = wilcox.test(klasyczne_s, nowe_s, paired=T)$statistic 
  if (tt_s >= wiloxtest) {cnt=cnt+1}}
c = cnt/N #wartość c
pvalue = 2-2*c #p value = 0.32

#Zad.2 Niezależne (przy zależnym by były w tym samym czasie)
Old <- c(141.85, 134.36, 131.87, 137.28, 122.72, 136.44, 128.96, 136.86)
New <- c(125.10, 122.00, 123.10, 119.92, 124.11, 121.91, 122.00, 135.95, 127.10, 125.10)
shapiro.test(Old) #H0
shapiro.test(New) #H1, p=0.01277

wilcox_org = wilcox.test(Old, New)$statistic #71
#Jak wygląda statystyka Wilcoxona? Sortujemy dane, dodajemy rangi dla grup.Suma rang dla grupy 2.
#Permutation = choose(length(Old)+length(New), length(Old)) #43758
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
    if (wilcoxon_value >= wilcox_org){ cnt=cnt+1 }}#dwustronny test, dlatego mnożę 2*.
#jeżeli by była taka sama to = Suma rang^ będzie większa niż dla jakiejkolwiek kombinacji
  return (cnt/Permutation)}
wilcoxontest(Old, New, 1000) #p wartość 0.036 <0.05 więc odrzucamy H0. Są różne.

#Zad4
library("mvtnorm")
my_sample = rmvnorm(n=100, mean=c(0,0), diag=2)

#Współczynnik korelacji:
bootstrap <- function(N, my_sample){
  n <- length(my_sample)
  coeff <- numeric(N)
  for (i in 1:N) {
    x = sample(my_sample[,1], n, replace = TRUE) #bootstrap parametryczny
    y = sample(my_sample[,2], n, replace = TRUE)
    coeff[i] = cor(x,y) }
  return (coeff)
}
cor_sequence = bootstrap(1000,my_sample)
coeff_mean = mean(cor_sequence) #średnia ze współczynników korelacji z losowanych danych
#0.001523495

quantile(cor_sequence, c(0.05,0.95))
alfa = 0.05
przedzial = quantile(bootstrap,c(alfa/2,1-(alfa/2))) 
przedzial





x - pierwsza próba 
y - druga próba

1. H1: y jest większe od x
2. H1: y jest mniejsze od x
3. H1: y różni się od x (w którąkolwiek stronę).

1. sprawdzamy czy y>x
if( t1 <= t0 )  {cnt ++}
p.val = cnt/N

2. sprawdzamy czy y<x
if( t1 >= t0)  {cnt ++}
p.val = cnt/N

3. sprawdzamy czy y i x w ogóle się różnią
if( t1>= t0)  {cnt ++}
c = cnt/N

if (c <= 0.5) p.val = 2*c
if (c > 0.5) p.val = 2-2*c
p.val



