N <- 10000

um_dado <- matrix(0,N)
dois_dados <- matrix(0,N,2)
tres_dados <- matrix(0,N,3)

dado <- c(1,2,3,4,5,6)


for (i in 1:N) {
  
  dado_1 = sample(dado,size = 1)
  dado_2 = sample(dado,size = 1)
  dado_3 = sample(dado,size = 1)
  
  
  um_dado[i,1] <- dado_1
  
  dois_dados[i,1:2] <- c(dado_1, dado_2)
  
  tres_dados[i,1:3] <- c(dado_1, dado_2, dado_3)
  
  
}


# Lançamento de um dado

#1.1 Sair a face 5

mean(um_dado == 5)

# Sair a face 2 ou 5

mean(um_dado == 5 | um_dado ==  2)


# No lançamento de dois dados



# 2.1 A soma dos valores ser 11.

mean(apply(dois_dados, 1 , function (x) sum(x) == 11))


mean(dois_dados[,1] + dois_dados[,2] == 11)




# A soma dos valores ser 7.

mean(apply(dois_dados, 1,function (x) sum(x) == 7))

# Alguma face ser 5

mean(apply(dois_dados, 1, function (x) any(x == 5)))




# No lançamento de três dados:



# A soma dos valores ser 18.


mean(apply(tres_dados, 1, function(x) sum(x) == 18))

# A soma dos valores ser 15.

mean(apply(tres_dados, 1, function(x) sum(x) == 15))

# Alguma face ser 5.

mean(apply(tres_dados, 1,  function(x) any(x == 5)))


# sair ao menos uma face 6.

mean(apply(tres_dados, 1, function(x) any(x == 6)))


#-------------------------------------------------------------------------------------------------------------------#


#1.2 Uma oficina realiza dois serviços de rotina em seus atendimentos (S1 e S2).
#O tempo para realizar o primeiro serviço tem distribuição uniforme (contínua) entre 15 e 25 minutos.
#O segundo serviço leva de 30 a 50 minutos. O tempo de um serviço

Resultado_s1 <- c()

Resultado_s2 <- c()

Resultado_s3 <- c()


 
 for (i in 1:10000) {
   
   s1 = runif(1, 15, 25)
   
   s2 = runif(1, 30, 50)
   

   
   Resultado_s1 <- c(Resultado_s1, s1)
   
   Resultado_s2 <- c(Resultado_s2, s2)
   
   Resultado_s3 <- c(Resultado_s3, s1 + s2)
 }



# Qual a probabilidade do primeiro serviço:

# 1.1 Ser concluído com menos de 22 minutos?

mean(Resultado_s1 < 22)

# 1.2 Ser concluído entre 18 a 20 minutos?

mean(Resultado_s1 <= 20 & Resultado_s1 >= 18)

# Qual a probabilidade do segundo serviço:

#2.1 ser concluído com menos de 35 minutos?

mean(Resultado_s2 < 35)


#2.2 Ser concluído com mais de 42 minutos?

mean(Resultado_s2 > 42)

# Qual a probabilidade de ambos serviços serem concluídos com mais de uma hora?

mean(Resultado_s3 > 60) 





#-------------------------------------------------------------------------------------------------------------------#
#1.3 Considere o contexto do problema anterior porém com a seguinte condição:
#se S1 demora até 18 minutos, S2 tem tempo uniforme entre 30 e 50 minutos.
#Porém, se S1 demora mais que 18 minutos, S2 tem distribuição uniforme entre 40 a 60 minutos.




#Qual a probabilidade do primeiro serviço:


Resultado_s1 <- c()

Resultado_s2 <- c()

Resultado_s3 <- c()

for (i in 1:10000) {
  
  
  
  s1 = runif(1, 15, 25)
  
  
  if ( s1 <= 18) {
    s2 = runif(1, 30, 50)
  } else {
    s2 = runif(1, 40, 60)
  }
  
  Resultado_s1 <- c(Resultado_s1, s1)
  
  Resultado_s2 <- c(Resultado_s2, s2)
  
  Resultado_s3 <- c(Resultado_s3, s1 + s2)
  
  
}


#1.1 ser concluído com menos de 22 minutos?
mean(Resultado_s1 < 22) 


#1.2 ser concluído entre 18 a 20 minutos?
mean(Resultado_s1 < 20 & Resultado_s1 > 18 )


# Qual a probabilidade do segundo serviço:

#2.1 ser concluído com menos de 35 minutos? 

mean(Resultado_s2 < 35)

#2.2 se concluído com mais de 42 minutos?

mean(Resultado_s2 > 42) 


# Qual a probabilidade de ambos serviços serem concluídos com mais de uma hora?

mean(Resultado_s3 > 60)



#-------------------------------------------------------------------------------------------------------------------
# 1.4 Seja Y uma v.a. com distribuição normal Y∼N(μ=100,σ2=102). 



y <- rnorm(10000, 100, 10)


#Calcule:

#P(Y<108)

mean(y < 108)

#P(Y>112)

mean(y > 112)

#P(90<Y<120)

mean(y>90 & y< 120)

#o valor de a tal que P(Y<a)=0.80

quantile(y, 0.8)

#o valor de a tal que P(Y>a)=0.60

quantile(y, 0.4)

#P(Y<102|Y<112)

mean(y < 102) / mean(y < 112) 

#P(Y>115|Y>105)

mean(y > 115) / mean(y > 105)

#P(Y<105|Y>92)

mean (y > 92 & y < 105) / mean(y > 92)

#P(100<Y<120|Y>105)

mean(y>105 & y <120) / mean(y > 105)


