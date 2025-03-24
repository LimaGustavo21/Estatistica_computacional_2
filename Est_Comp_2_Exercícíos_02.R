um_dado <- c()
dois_dados <- list()
tres_dados <- list()

dado <- c(1,2,3,4,5,6)


for (i in 1:10000) {
  
  dado_1 = sample(dado,size = 1)
  dado_2 = sample(dado,size = 1)
  dado_3 = sample(dado,size = 1)
  
  
  um_dado <- c(um_dado, dado_1 )
  
  dois_dados[[i]] <- c(dado_1, dado_2)
  
  tres_dados[[i]] <- c(dado_1, dado_2, dado_3)
  
  
}


# Lançamento de um dado

# Sair a face 5



# Sair a face 2 ou 5




# No lançamento de dois dados



# A soma dos valores ser 11.


# A soma dos valores ser 7.


# Alguma face ser 5





# No lançamento de três dados:



# A soma dos valores ser 18.




# A soma dos valores ser 15.



# Alguma face ser 5.




# sair ao menos uma face 6.





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

(resultado_11 <- (mean(Resultado_s1 < 22) ))

# 1.2 Ser concluído entre 18 a 20 minutos?

(resultado_12 <- (mean(Resultado_s1 <= 20 & Resultado_s1 >= 18))) 

# Qual a probabilidade do segundo serviço:

#2.1 ser concluído com menos de 35 minutos?

(resultado_21 <- (mean(Resultado_s2 < 35))) 


#2.2 Ser concluído com mais de 42 minutos?

(resultado_22 <- (mean(Resultado_s2 > 42))) 

# Qual a probabilidade de ambos serviços serem concluídos com mais de uma hora?

(resultado_3 <- (mean(Resultado_s3 > 60))) 




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
(resultado_11 <- (mean(Resultado_s1 < 22) ))


#1.2 ser concluído entre 18 a 20 minutos?
(resultado_12 <- (mean(Resultado_s1 < 20 & Resultado_s1 > 18 ) ))


# Qual a probabilidade do segundo serviço:

#2.1 ser concluído com menos de 35 minutos? 

(resultado_21 <- (mean(Resultado_s2 < 35) ))

#2.2 se concluído com mais de 42 minutos?

(resultado_22 <- (mean(Resultado_s2 > 42) ))


# Qual a probabilidade de ambos serviços serem concluídos com mais de uma hora?

(resultado_3 <- (mean(Resultado_s3 > 60) ))

