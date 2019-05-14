##############     Universidade Federal de Pernambuco   #########################
##############    Ruam Emanuel Rodrigues Chaves Pastor  #########################
##############        Analise de Dados - Lista 5        #########################


#### LISTA 1 ####

install.packages("tidyverse")
library(tidyverse)
install.packages("GGally")
library(GGally)
library(ggplot2)
library(readxl)
library(readr)

# Questao 5 ####

setwd("C:/GitHub/Lista_5/Lista_5/Dados")

bancodados_lista1 <- read_excel("bancodados_lista1_5.xlsx")

bancodados_lista1$Setor <- factor(bancodados_lista1$Setor, labels = c("Comercio", "Industria"), lev = c("C","I"))

bancodados_lista1$Tamanho <- factor(bancodados_lista1$Tamanho, labels = c("Pequeno", "Medio", "Grande"), lev = c("P", "M", "G"))

view(bancodados_lista1)


## a) 


names(bancodados_lista1)

# Meses: Discreta
# Setor: Nominal
# Tamanho: Ordinal


## b) 

# Comercio 

bancodados_lista1_comercio <- bancodados_lista1 %>% filter(Setor == "Comercio")

head(bancodados_lista1_comercio)

# media: 

mean(bancodados_lista1_comercio$Meses)

# mediana:

median(bancodados_lista1_comercio$Meses)

# Industria # 

bancodados_lista1_industria <- bancodados_lista1 %>% filter(Setor == "Industria")

head(bancodados_lista1_industria)

# media: 

mean(bancodados_lista1_industria$Meses)

# mediana: 

median(bancodados_lista1_industria$Meses)

## c) 

# Desvio padrao do Comercio

sd(bancodados_lista1_comercio$Meses)

# Desvio padrao da industria

sd(bancodados_lista1_industria$Meses)

## d) 

summary(bancodados_lista1$Meses)

0.25*(15+1) -> x #

x -> #quantidade de empresas que receberia incentivo fiscal

# e) 

# Pequeno

bancodados_lista1_Pequeno <- bancodados_lista1 %>% filter(Tamanho == "Pequeno")

summary(bancodados_lista1_5_pequeno)

sd(bancodados_lista1_5_pequeno$Meses)

# Medio

bancodados_lista1_medio <- bancodados_lista1 %>% filter(Tamanho == "Medio")

summary(bancodados_lista1_medio)

sd(bancodados_lista1_medio$Meses)

# Grande

bancodados_lista1_grande <- bancodados_lista1 %>% filter(Tamanho == "Grande")

summary(bancodados_lista1_grande)

sd(bancodados_lista1_grande$Meses)


# Questao 6 ####

# abrindo o banco de dados:

bancodados2_lista1 <- read_excel("bancodados_lista1_6.xlsx")

# observando o banco: 

head(bancodados2_lista1)

# a)

mean(bancodados2_lista1$Investimento)

ggplot(bancodados2_lista1, aes(Investimento))+geom_bar()

# b) 

sd(bancodados2_lista1$Investimento)

mean(bancodados2_lista1$Investimento) - 2*sd(bancodados2_lista1$Investimento)

mean(bancodados2_lista1$Investimento) - 2*sd(bancodados2_lista1$Investimento) == bancodados2_lista1

# Nenhuma cidade ira receber investimento. 


# c) 

mean(bancodados2_lista1$Investimento) - 2*sd(bancodados2_lista1$Investimento)

mean(bancodados2_lista1$Investimento) + 2*sd(bancodados2_lista1$Investimento) 

bancodados2_lista1_nova <- bancodados2_lista1 %>% filter(Investimento < 25.56)

summary(bancodados2_lista1_nova)

mean(bancodados2_lista1_nova$Investimento)

# Questao 7 ####

bancodados3_lista1 <- read_excel("bancodados_lista1_7.xlsx")

# Obtenha media, mediana, variancia e o desvio padr?o para cada estimulo:

## Estimulo A

mean(bancodados3_lista1$A)
median(bancodados3_lista1$A)
var(bancodados3_lista1$A)
sd(bancodados3_lista1$A)

View(bancodados_lista1)

## Est?mulo B

bancodados3_lista1$B

is.na(bancodados3_lista1$B)
na.exclude(bancodados3_lista1$B)


mean((na.exclude(bancodados3_lista1$B)))

median((na.exclude(bancodados3_lista1$B)))

var((na.exclude(bancodados3_lista1$B)))

sd((na.exclude(bancodados3_lista1$B)))


# b) 

boxplot(bancodados3_lista1$A, bancodados3_lista1$B)

# Questao 8 ####

bancodados4_lista1 <- read_excel("bancodados_lista1_8.xlsx")

# a) 

# Desvio padrao das variaveis: 

sd(bancodados4_lista1$renda_sm)

sd(bancodados4_lista1$saude_perc)

# Grafico do Boxplot: 

boxplot(bancodados4_lista1$renda_sm, bancodados4_lista1$saude_perc)

# Covariancia: 

cov(bancodados4_lista1$renda_sm, bancodados4_lista1$saude_perc)

# Correlacao e Teste de Correlacao: 

cor(bancodados4_lista1$renda_sm, bancodados4_lista1$saude_perc)

cor.test(bancodados4_lista1$renda_sm, bancodados4_lista1$saude_perc)

ggplot(bancodados4_lista1, aes(renda_sm, saude_perc))+geom_point()

ggcorr(bancodados4_lista1)

# b)

# Questao 9 ####


bancodados5_lista1 <- read_excel("bancodados_lista1_9.xlsx")

# a) 

# Coeficiente de correlacao 

cor(bancodados5_lista1$P1, bancodados5_lista1$P2)

ggplot(bancodados5_lista1, aes(P1, P2))+geom_point()

# b)  

cor.test(bancodados5_lista1$P1, bancodados5_lista1$P2)

#### LISTA 2 ####

## 5 ####

ID <- c(1:1000)
Ideologia <- c(rep(1, 620), rep(0, 380))
Base_Q5 <- data.frame(ID, Ideologia) 
summary(Base_Q5) #Gerando os dados necessarios para a questao

#A 

mean(Base_Q5$Ideologia) #Encontrando a media amostral

#B

sd(Base_Q5$Ideologia) #Encontrando o Desvio padrao amostral 

#C

mean(Base_Q5$Ideologia) - (1.96 * (sd(Base_Q5$Ideologia)/(sqrt(1000))))

mean(Base_Q5$Ideologia) + (1.96 *(sd(Base_Q5$Ideologia)/(sqrt(1000))))

# Logo o intervalo de confian?a ? entre[0,58; 0,65]


## 6 ####

#A

n_a <- (1.96^2 * 0.5 * (1 - 0.5) / 0.05^2)
n_a

#B

n_b <- (1.96^2 * 0.5 * (1 - 0.5) / 0.02^2)
n_b

#C

n_c <- (1.96^2 * 0.25 * (1 - 0.25) / 0.02^2)
n_c

#d 

564 / 2401

# Logo iremos atribuir o valor obtido, e iremos aplicar ao novo calculo da amostragem:

X <- c (1:2401)

Y <- as.numeric(X <= 564)

# calculo do novo intervalo de confian?a:

0.2349021 + ( 1.96 * ( sd (Y) / sqrt ( 2401 )))

0.2349021 - ( 1.96 * ( sd (Y) / sqrt ( 2401 )))


# O novo IC ? entre [0.21; 0.25]

## 11 ####

#A

#A ideologia dos candidatos eleitos a camara federal influencia sua decisao referente a descrimina-#
#lizacao das drogas?

#Hipotese Nula: A ideologia do candidato nao exerce influencia sobre sua decisao

#Hipotese Alternativa: Candidatos de esquerda tendem a ser favoraveis de descriminaliza?ao, enquanto-
#candidatos de direita tendem a ser contrarios 

#B

#O erro de tipo 1 rejeita a hipotese nula quando e verdadeira; o erro de tipo 2 nao rejeita a hipote-
#-se nula quando a hipotese alternativa e verdadeira. Erro TI: Rejeitar a ideia de que a ideologia
#do candidato nao exerce influencia sobre sua decisao. Erro T2: Encontra-se o resultado positivo de
#rela?ao entre ideologia e decisao, mas nao descarta-se a hipotese nula.

#C

ID <- c(1:1000)

Ideologia <- c(rep(1, 450), rep(0, 150), rep(1, 100),rep(0,300))

Opiniao <- c(rep(1, 600), rep(0,400))

Base_Q11 <- data.frame(ID, Ideologia, Opiniao)

table(Base_Q11$Ideologia,Base_Q11$Opiniao)

chisq.test(Base_Q11$Ideologia, Base_Q11$Opiniao)

## 12 ####

Ano <- c("1964", "1966", "1968", "1970", "1972", "1974", "1976", "1978",
         "1980", "1982", "1984", "1986", "1988", "1990", "1992", "1994",
         "1996", "1998", "2000", "2002", "2004", "2006")

Camara <- c("87", "88", "97", "85", "94", "88", "96", "94", "91", "90", "95",
            "98", "98", "96", "88", "90", "94", "98", "98", "96", "98", "94")

Senado <- c("85", "88", "71", "77", "74", "85", "64", "60", "55", "93", "90",
            "75", "85", "96", "83", "92", "91", "90", "79", "86", "96", "79")

taxa_reeleicao_incumbentes <- data.frame(Ano, Camara, Senado)

taxa_reeleicao_incumbentes1 <- transform(taxa_reeleicao_incumbentes, Camara = as.numeric(as.character(Camara)), 
                           Senado = as.numeric(as.character(Senado)), Ano = as.numeric(as.character(Ano)))


View(taxa_reeleicao_incumbentes1)
summary(taxa_reeleicao_incumbentes1)


t.test(taxa_reeleicao_incumbentes1$Camara[1:5])
t.test(taxa_reeleicao_incumbentes1$Camara[6:22])

t.test(taxa_reeleicao_incumbentes1$Camara[1:5], taxa_reeleicao_incumbentes1$Camara[6:22], var.equal = FALSE)

t.test(taxa_reeleicao_incumbentes1$Senado[1:5], taxa_reeleicao_incumbentes1$Senado[6:22], var.equal = FALSE )

## 13 ####
#a

#a partir da tabela t, podemos rejeitar a hipotese nula com o p-valor de 0,05

#b

getwd()
setwd("C:/GitHub/Lista_5/Lista_5/Dados")

load("vote_growth_usa.Rdata")

summary(bd)

bd2 <- bd[1:15, c('Year', 'Growth', 'Vote')]

cor.test(bd2$Growth,bd2$Vote)

ggplot(bd2, aes(Growth, Vote, color = Vote))+geom_point()

#c

cor.test(bd$Growth,bd$Vote)

#A partir do resultado do teste de correla?ao podemos verificar que a hipotese nula pode ser rejeitada


