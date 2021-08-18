#*****************************************************************
#             Estatística Descritiva
# Nome Aluno
# dd/mm/aaaa
# Objetivo: construcao de tabelas, graficos e calculo
#           de medidas de posicao e dispersao 
#****************************************************************
# Gerando dados
dat<-read.csv("dadosR1.csv", dec=',',sep=';')
dat

# Definindo as variáveis
# with = escolher a variavel do conjunto de dados
y<-with(dat,comprimento)
fator1<-as.factor(with(dat,operador))
fator2<-as.factor(with(dat,instrumento))
#
#
# Parte 2 - Representacao tabular
# Comprimento
#  Variaveis quantitativas
nma<-length(y)
lim<-range(y)
ki<-round(1+(3.3*log10(nma)));ki
l<-(lim[2]-lim[1])/ki;l
quebra<-seq(lim[1],lim[2]+l,l)
ad.inter<-factor(cut(y,quebra,right=F))
out<-as.data.frame(table(ad.inter))
transform(out,relativa=prop.table(Freq),porcentagem=prop.table(Freq)*100)

# Parte 3- Representacao grafica
# 
#  Variaveis quantitativas
# Comprimento
#   Histograma
hist(y, main='', xlab='Comprimento', ylab='Frequência')
#   Box-plot
#    Individual
boxplot(y, xlab='Comprimento',horizontal=T)
# Parte 4 - Medidas de posicao e dispersao
#
#  Por grupo
#  tapply = calcular uma medida de uma variável quantitativa agrupada 
#           por uma variável qualitativa.
#   Media
xbg<-tapply(y,fator1,mean);xbg  
ybg<-tapply(y,fator2,mean);ybg 
#Mediana
mdx<-tapply (y,fator1,median);mdx
mdy<-tapply (y,fator2,median);mdy
#   variancia
s2gx<-tapply(y,fator1,var);s2gx
s2gy<-tapply(y,fator2,var);s2gy
#   Desvio padrao
sgx<-sqrt(s2gx);sgx 
sgy<-sqrt(s2gy);sgy
#   Coeficiente de variacao
cvg<-(sd(y)/mean(y))*100;cvg
#

