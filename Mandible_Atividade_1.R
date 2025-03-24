
m <- c(120, 107, 110, 116, 114, 111, 113, 117, 114, 112)
f <- c(110, 111, 107, 108, 110, 105, 107, 106, 111, 111)


mean(f)
mean(m)

f <- sort(f)
m <- sort(m)

d0 <- m-f

med_d0 <- mean(d0)


dtest <- c()

dsort <- c()

f1 <- c()
m1 <- c()

for (i in 1:10000) {
  
  dsort <- sample(c(f,m))
  f1  <- dsort[1:10]
  m1  <- dsort[11:20]
  mean_dtest <- mean(m1-f1)
  dtest <- c(dtest,mean_dtest )
  
  
  
}



hist(dtest)


rej_zon <- max(quantile(dtest, 0.9))


if (mean(d0) > rej_zon) {
    
        print("Há evidência significativa para a diferença entre as medidas, os machos possuem medidas maiores que as fêmeas.")
  
} else{
  
  print("Não há evidência significativa para a diferencia entre as medidas, não rejeita H0.")
}






