#***********************************************************
# Delineamento completamente aleatorizado
# Esquema: fatorial 
# Objetivo: 
# - verificar os pressupostos
# - fazer uma análise de variância
# - teste de comparação múltipla
#***********************************************************
# 
# Pacotes 
library(car)
library(MASS)
library(ExpDes)
#
# Importando os dados 
dat<-read.csv("dadosR1.csv", dec=',',sep=';')
dat
#
# Declarando as variáveis
y<-with(dat,comprimento)
fator1<-as.factor(with(dat,operador))
fator2<-as.factor(with(dat,instrumento))
#
# Parte 1 - Estistica descritiva
boxplot(y~fator1*fator2,xlab='Combinação dos Níveis',ylab='Comprimento')
interaction.plot(fator2,fator1,y,type='b',pch=c(1,19),
                 xlab='Instrumento',ylab='Comprimento',lwd=c(2,2))
tapply(y,list(fator1,fator2),mean)
tapply(y,list(fator1,fator2),median)
tapply(y,list(fator1,fator2),var)
tapply(y,list(fator1,fator2),sd)
#
# IC para operador
sd_f1<-tapply(y,fator1,sd)
ttab<-qt(1-((1-0.95)/2),24-1);ttab
me11<-ttab*(sd_f1[[1]][[1]]/sqrt(24));me11
#
me12<-ttab*(sd_f1[[2]][[1]]/sqrt(24));me12
#
sd_f2<-tapply(y,fator2,sd)
ttab2<-qt(1-((1-0.95)/2),12-1);ttab2
me2<-ttab2*(sd_f2/sqrt(12));me2
#

# Parte 2 - Verificando os pressupostos
# Normalidade: Teste de Shapiro-Wilk
modelo1<-aov(y~fator1*fator2,data=dat)
res<-rstudent(modelo1)  
shapiro.test(res)
#
qqnorm(res, main='',xlab='Quantis da distribuição normal padrão',
       ylab='Quantis da amostra')
qqline(res)
#
# Homogeneidade de variâncias: Teste de levene
leveneTest(y~fator1*fator2)
#
# Parte 3 - Se pressupostos atendidos:
#           ANOVA e teste de comparacao multipla
fat2.crd(fator1,fator2,y,quali=c(TRUE,TRUE),mcomp="tukey",sigT=0.05,sigF=0.05)
#

