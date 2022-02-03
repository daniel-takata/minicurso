library(evd)

#ARTILHARIA LA LIGA

la_liga=read.table("C:\\Users\\danie\\Documents\\sports statistics\\aula 2\\arquivos\\la_liga.txt",header=T)

la_liga

hist(la_liga$gols)

modelo=fgev(la_liga$gols)

loc=modelo$estimate[1]
scale=modelo$estimate[2]
shape=modelo$estimate[3]



y=dgev(19:50,loc=loc,scale=scale,shape=0)

hist(la_liga$gols,freq=FALSE,xlim=c(15,50),ylim=c(0,0.07))
par(new=T)
plot(19:50,y,type="l",
     xlim=c(15,50),ylim=c(0,0.07))

library(ADGofTest)
library(goftest)
ad.test(la_liga$gols,pgev,loc=loc,scale=scale,shape=0)
ks.test(la_liga$gols,pgev,loc=loc,scale=scale,shape=0)
cvm.test(la_liga$gols,pgev,loc=loc,scale=scale,shape=0)


p=1-pgev(50,loc=loc,scale=scale,shape=0)
1/p


#PONTOS BRASILEIRAO

pontos_brasileirao_final=read.table("C:\\Users\\danie\\Documents\\sports statistics\\aula 2\\arquivos\\pontos_brasileirao_final.txt",header=T)

modelo=fgev(pontos_brasileirao_final$pontos,shape=0)
loc=modelo$estimate[1]
scale=modelo$estimate[2]
shape=modelo$estimate[3]

y=dgev(55:80,loc=loc,scale=scale,shape=shape)

hist(pontos_brasileirao_final$pontos,xlim=c(55,80),
     ylim=c(0,0.08),freq=FALSE)
par(new=T)
plot(55:80,y,type="l",
     xlim=c(55,80),ylim=c(0,0.08))

library(ADGofTest)
ad.test(pontos_brasileirao_final$pontos,pgev,loc=loc,scale=scale,shape=shape)
ks.test(pontos_brasileirao_final$pontos,pgev,loc=loc,scale=scale,shape=shape)
cvm.test(pontos_brasileirao_final$pontos,pgev,loc=loc,scale=scale,shape=shape)


p=1-pgev(91,loc=loc,scale=scale,shape=0)
p
1/p


#ANÁLISES ATLETISMO


library(POT)

dados <- read.csv("C:\\Users\\danie\\Documents\\sports statistics\\aula 2\\arquivos\\rasos100e200.csv",head=T)

head(dados)

#comparacao provas

dados=dados[order(dados$gender,dados$event,dados$time),]

prova="rasos100"
sexo="M"
distancia=100
wr=9.58
D=1000 #número de observações até threshold

novo=dados[!(dados$event!=paste(prova)),]

novo=novo[!(novo$gender!=paste(sexo)),]

novo=novo[!duplicated(novo$athlete),]


novo$count=1:length(novo[,1])

novo$count1=1

for (i in 2:length(novo[,1])){
  if (novo$time[i] == novo$time[i-1])
    novo$count1[i]=novo$count1[i-1]+1
}

contagens <- novo[order(novo$time, -novo$count1),]
contagens$time1<-NULL
contagens$athlete<-NULL
contagens$year<-NULL
contagens$count<-NULL

contagens=contagens[!duplicated(contagens$time),]

novo1=merge(novo,contagens,by="time")
novo1$timeok=0

for (i in 1:length(novo1[,1])){
  if (novo1$count1.y[i]==1) {novo1$timeok[i]=novo1$time[i]}
  else {novo1$timeok[i]=novo1$time[i]-0.005+0.01*(2*novo1$count1.x[i]-1)/(2*novo1$count1.y[i])}
}

novo1$timeok1=distancia/novo1$timeok

hist(novo1$timeok1,ylab="Frequência relativa",
     xlab="Velocidade (m/s)",main="100 m rasos masculino",freq=FALSE)

thresh=novo1$timeok1[D]-0.00005

scale=fitgpd(novo1$timeok1,threshold=thresh,"mle")$fitted.values[1]
scale_sd=fitgpd(novo1$timeok1,threshold=thresh,"mle")$std.err[1]
shape=fitgpd(novo1$timeok1,threshold=thresh,"mle")$fitted.values[2]
shape_sd=fitgpd(novo1$timeok1,threshold=thresh,"mle")$std.err[2]

scale
scale_sd
shape
shape_sd

distancia/(thresh-scale/shape)


1-pgpd(distancia/wr,loc=thresh,scale=scale,shape=shape)


x=1:10000
x=thresh+x/10000
y=dgpd(x,loc=thresh,scale=scale,shape=shape)

hist(novo1$timeok1[1:D], freq=FALSE, ylim=c(0,7),xlim=c(thresh,10.7),ylab="Frequência",xlab="Tempo")
par(new=T)
plot(x,y,type="l",ylim=c(0,7),xlim=c(thresh,10.7))

