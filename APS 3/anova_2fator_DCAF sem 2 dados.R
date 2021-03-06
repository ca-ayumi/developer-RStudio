#***********************************************************
# Delineamento completamente aleatorizado
# Esquema: fatorial 
# Objetivo: 
# - verificar os pressupostos
# - fazer uma an�lise de vari�ncia
# - teste de compara��o m�ltipla
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
# Declarando as vari�veis
y<-with(dat,comprimento)
fator1<-as.factor(with(dat,operador))
fator2<-as.factor(with(dat,instrumento))
#
# Parte 1 - Estistica descritiva
boxplot(y~fator1*fator2,xlab='Combina��o dos N�veis',ylab='Comprimento')
interaction.plot(fator2,fator1,y,type='b',pch=c(1,19),
                 xlab='Instrumento',ylab='Comprimento',lwd=c(2,2))
tapply(y,list(fator1,fator2),mean)
tapply(y,list(fator1,fator2),sd)
#
# Parte 2 - Verificando os pressupostos
# Normalidade: Teste de Shapiro-Wilk
modelo1<-aov(y~fator1*fator2,data=dat)
res<-rstudent(modelo1)  
shapiro.test(res)
#
qqnorm(res, main='',xlab='Quantis da distribui��o normal padr�o',
       ylab='Quantis da amostra')
qqline(res)
#
# Homogeneidade de vari�ncias: Teste de levene
leveneTest(y~fator1*fator2)
#
# Parte 3 - Se pressupostos atendidos:
#           ANOVA e teste de comparacao multipla
fat2.crd(fator1,fator2,y,quali=c(TRUE,TRUE),mcomp="tukey",sigT=0.05,sigF=0.05)
#

