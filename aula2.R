#MODELO INICIAL MAHER
#parametros alpha e beta

dados <- read.csv('C:\\Users\\danie\\Documents\\sports statistics\\aula 3\\BRA2021.csv')



#gols marcados por mandantes
aggregate(gols_mandante ~ mandante, data=dados, sum)
#gols sofridos por visitantes
aggregate(gols_mandante ~ visitante, data=dados, sum)

Sx=sum(dados$gols_mandante)

alphas=aggregate(gols_mandante ~ mandante, data=dados, sum)
alphas$gols_mandante=alphas$gols_mandante/sqrt(Sx)
betas=aggregate(gols_mandante ~ visitante, data=dados, sum)
betas$gols_mandante=betas$gols_mandante/sqrt(Sx)
colnames(alphas)[2]="alpha"
colnames(betas)[2]="beta"

alpha_emv=alphas
beta_emv=betas

for (j in 1:10){
for (i in 1:20){
  aux1=dados[dados$mandante==alpha_emv[i,1],]
  aux2=beta_emv[beta_emv$visitante!=alpha_emv[i,1],]
  alpha_emv[i,2]=sum(aux1$gols_mandante)/sum(aux2$beta)
}}

for (j in 1:10){
for (i in 1:20){
  aux1=dados[dados$visitante==beta_emv[i,1],]
  aux2=alpha_emv[alpha_emv$mandante!=beta_emv[i,1],]
  beta_emv[i,2]=sum(aux1$gols_mandante)/sum(aux2$alpha)
}}

#parametros gamma e delta

#gols tomados por mandantes
aggregate(gols_visitante ~ mandante, data=dados, sum)
#gols marcados por visitantes
aggregate(gols_visitante ~ visitante, data=dados, sum)

Sy=sum(dados$gols_visitante)

gammas=aggregate(gols_visitante ~ mandante, data=dados, sum)
gammas$gols_visitante=gammas$gols_visitante/sqrt(Sy)
deltas=aggregate(gols_visitante ~ visitante, data=dados, sum)
deltas$gols_visitante=deltas$gols_visitante/sqrt(Sy)
colnames(gammas)[2]="gamma"
colnames(deltas)[2]="delta"

gamma_emv=gammas
delta_emv=deltas

for (j in 1:10){
for (i in 1:20){
  aux1=dados[dados$mandante==gamma_emv[i,1],]
  aux2=delta_emv[delta_emv$visitante!=gamma_emv[i,1],]
  gamma_emv[i,2]=sum(aux1$gols_visitante)/sum(aux2$delta)
}
for (i in 1:20){
  aux1=dados[dados$visitante==delta_emv[i,1],]
  aux2=gamma_emv[gamma_emv$mandante!=delta_emv[i,1],]
  delta_emv[i,2]=sum(aux1$gols_visitante)/sum(aux2$gamma)
}
} 

posicao=c(8,14,9,1,18,11,20,5,15,2,7,4,17,12,16,3,6,13,10,19)
final=cbind(alpha_emv,beta_emv$beta,gamma_emv$gamma,delta_emv$delta,posicao)
final[order(final$posicao),]

#atl-mg x sp
#xij gols do atl x sp, poisson com média alpha_i x beta_j
#yij gols do sp x atl fora de casa, poisson com média gamma_i x delta_j


lambda1=final[4,2]*final[19,3]
lambda2=final[4,3]*final[19,4]


lambda1=final[16,2]*final[8,3]
lambda2=final[16,4]*final[8,5]
#calculando as probabilidades

max_gols = 10 
probabilidades <- dpois(0:max_gols, lambda1) %*% t(dpois(0:max_gols, lambda2))
#matriz de probabilidades, soma 1 (ou quase um, pq vai até 10 gols).
#prob. número de gols da equipe mandante na vertical, e da visitante na horizontal
prob_mandante <- sum(probabilidades[lower.tri(probabilidades)])
prob_empate <- sum(diag(probabilidades))
prob_visitante <- sum(probabilidades[upper.tri(probabilidades)])

prob_mandante
prob_empate
prob_visitante

#MODELO 2 MAHER

eta2=sum(dados$gols_visitante)/sum(dados$gols_mandante)

S=sum(dados$gols_mandante)+sum(dados$gols_visitante)

gols_mandante=aggregate(gols_mandante ~ mandante, data=dados, sum)
gols_visitante=aggregate(gols_visitante ~ visitante, data=dados, sum)

alphas1=(gols_mandante$gols_mandante+gols_visitante$gols_visitante)/((1+eta2)*sqrt(S))
alphas=gols_mandante
alphas$gols_mandante=alphas1
colnames(alphas)[2]="alpha"

gols_mandante_sofridos=aggregate(gols_visitante ~ mandante, data=dados, sum)
gols_visitante_sofridos=aggregate(gols_mandante ~ visitante, data=dados, sum)

betas1=(gols_mandante_sofridos$gols_visitante+gols_visitante_sofridos$gols_mandante)/((1+eta2)*sqrt(S))
betas=gols_mandante_sofridos
betas$gols_visitante=betas1
colnames(betas)[2]="beta"

alpha_emv=alphas
beta_emv=betas

for (j in 1:10){
for (i in 1:20){
  aux1=dados[dados$mandante==alpha_emv[i,1],]
  aux2=dados[dados$visitante==alpha_emv[i,1],]
  aux3=beta_emv[beta_emv$mandante!=alpha_emv[i,1],]
  alpha_emv[i,2]=(sum(aux1$gols_mandante)+sum(aux2$gols_visitante))/((1+eta2)*sum(aux3$beta))
}


for (i in 1:20){
  aux1=dados[dados$visitante==beta_emv[i,1],]
  aux2=dados[dados$mandante==beta_emv[i,1],]
  aux3=alpha_emv[alpha_emv$mandante!=beta_emv[i,1],]
  beta_emv[i,2]=(sum(aux1$gols_mandante)+sum(aux2$gols_visitante))/((1+eta2)*sum(aux3$alpha))
} 
}

posicao=c(8,14,9,1,18,11,20,5,15,2,7,4,17,12,16,3,6,13,10,19)
final=cbind(alpha_emv,beta_emv$beta,posicao)
final[order(final$posicao),]

#atl-mg x sp
#xij gols do atl x sp, poisson com média alpha_i x beta_j
#yij gols do sp x atl fora de casa, poisson com média gamma_i x delta_j

lambda1=final[4,2]*final[19,3]
lambda2=final[19,2]*final[4,3]*eta2

lambda1
lambda2
#calculando as probabilidades

max_gols = 10 
probabilidades <- dpois(0:max_gols, lambda1) %*% t(dpois(0:max_gols, lambda2))
#matriz de probabilidades, soma 1 (ou quase um, pq vai até 6 gols).
#prob. número de gols da equipe mandante na vertical, e da visitante na horizontal
prob_mandante <- sum(probabilidades[lower.tri(probabilidades)])
prob_empate <- sum(diag(probabilidades))
prob_visitante <- sum(probabilidades[upper.tri(probabilidades)])


#bondade de ajuste


parametros_mandante=final
colnames(parametros_mandante)[2]="alpha_mandante"
colnames(parametros_mandante)[3]="beta_mandante"
parametros_visitante=final
colnames(parametros_visitante)[1]="visitante"
colnames(parametros_visitante)[2]="alpha_visitante"
colnames(parametros_visitante)[3]="beta_visitante"

compl1=merge(dados,parametros_visitante,by="visitante")
compl2=merge(compl1,parametros_mandante,by="mandante")
compl2$prob0_mand=dpois(0,compl2$alpha_mandante*compl2$beta_visitante)
compl2$prob1_mand=dpois(1,compl2$alpha_mandante*compl2$beta_visitante)
compl2$prob2_mand=dpois(2,compl2$alpha_mandante*compl2$beta_visitante)
compl2$prob3_mand=dpois(3,compl2$alpha_mandante*compl2$beta_visitante)
compl2$prob4_mand=(1-ppois(3,compl2$alpha_mandante*compl2$beta_visitante))
compl2$prob0_visit=dpois(0,eta2*compl2$alpha_visitante*compl2$beta_mandante)
compl2$prob1_visit=dpois(1,eta2*compl2$alpha_visitante*compl2$beta_mandante)
compl2$prob2_visit=dpois(2,eta2*compl2$alpha_visitante*compl2$beta_mandante)
compl2$prob3_visit=dpois(3,eta2*compl2$alpha_visitante*compl2$beta_mandante)
compl2$prob4_visit=(1-ppois(3,eta2*compl2$alpha_visitante*compl2$beta_mandante))

gols_mand=table(compl2$gols_mandante)

infos1=matrix(0,5,4)
infos1[1:5,1]=0:4
infos1[1:4,2]=gols_mand[1:4]
infos1[5,2]=sum(gols_mand[5:length(gols_mand)])
infos1[1,3]=mean(compl2$prob0_mand)
infos1[2,3]=mean(compl2$prob1_mand)
infos1[3,3]=mean(compl2$prob2_mand)
infos1[4,3]=mean(compl2$prob3_mand)
infos1[5,3]=mean(compl2$prob4_mand)
infos1[,4]=infos1[,3]*380


sum((infos1[,2]-infos1[,4])^2/infos1[,4])
#de acordo com o artigo, 3 graus de liberdade, nesse caso não rejeita H0
qchisq(0.95,3)


gols_visit=table(compl2$gols_visitante)
infos2=matrix(0,5,4)
infos2[1:5,1]=0:4
infos2[1:4,2]=gols_visit[1:4]
infos2[5,2]=sum(gols_visit[5:length(gols_visit)])
infos2[1,3]=mean(compl2$prob0_visit)
infos2[2,3]=mean(compl2$prob1_visit)
infos2[3,3]=mean(compl2$prob2_visit)
infos2[4,3]=mean(compl2$prob3_visit)
infos2[5,3]=mean(compl2$prob4_visit)
infos2[,4]=infos2[,3]*380
sum((infos2[,2]-infos2[,4])^2/infos2[,4])
#de acordo com o artigo, 3 graus de liberdade, nesse caso não rejeita H0
qchisq(0.95,3)





#dixon-coles 1
  
  tau <- Vectorize(function(x, y, lambda, mu, rho){
    if (x == 0 & y == 0){return(1 - (lambda*mu*rho))
    } else if (x == 0 & y == 1){return(1 + (lambda*rho))
    } else if (x == 1 & y == 0){return(1 + (mu*rho))
    } else if (x == 1 & y == 1){return(1 - rho)
    } else {return(1)}
  })
  
  
  #formatar dados para incluir restricao de soma zero dos parametros
  
  dados_formatados <- function(df){
    times.n <- unique(c(levels(factor(df$mandante)), levels(factor(df$visitante))))
    # ataque, com a restricao de soma zero
    ## mandantes (cria matriz com 1 qdo o time for mandante,
    #e coloca -1 quando o confronto for contra o último time)
    mandante.a <- model.matrix(~ mandante - 1, data=df)
    mandante.a[df$mandante == times.n[length(factor(times.n))], ] <- -1
    mandante.a <- mandante.a[,1:(length(factor(times.n))-1)]
    # visitantes (mesma coisa)
    visitante.a <- model.matrix(~ visitante -1, data=df)
    visitante.a[df$visitante.a == times.n[length(factor(times.n))], ] <- -1
    visitante.a <- visitante.a[,1:(length(factor(times.n))-1)]
    # defesa 
    mandante.d <- model.matrix(~ mandante - 1, data=df)
    visitante.d <- model.matrix(~ visitante -1, data=df)
    return(list(mandante_ataque=mandante.a, mandante_defesa=mandante.d,
                visitante_ataque=visitante.a, visitante_defesa=visitante.d,
                gols_mandante=df$gols_mandante, gols_visitante=df$gols_visitante,
                times=times.n)) 
  }
  
  
  dcm <- dados_formatados(dados)
  
  log_veross <- function(y1, y2, lambda, mu, rho=0){
    sum(log(tau(y1, y2, lambda, mu, rho)) + log(dpois(y1, lambda)) + log(dpois(y2, mu)))
  }
  

  
  otimizador <- function(params, DCm){
    casa.p <- params[1]
    rho.p <- params[2]
    ntimes <- length(factor(DCm$times))
    ataque.p <- matrix(params[3:(ntimes+1)], ncol=1) #uma coluna a menos
    defesa.p <- matrix(params[(ntimes+2):length(params)], ncol=1) 
    lambda <- exp(DCm$mandante_ataque %*% ataque.p + DCm$visitante_defesa %*% defesa.p + casa.p)
    mu <- exp(DCm$visitante_ataque %*% ataque.p + DCm$mandante_defesa %*% defesa.p)
    return(
      log_veross(y1=DCm$gols_mandante, y2=DCm$gols_visitante, lambda, mu, rho.p) * -1
    )
  }
  
  
  ntimes <- length(factor(dcm$times))
  ataque.params <- rep(.1, times=ntimes-1)
  defesa.params <- rep(-0.8, times=ntimes)
  casa.param <- 0.06
  rho.0 <- 0.03
  par.0 <- c(casa.param, rho.0, ataque.params, defesa.params)
  names(par.0) <- c('CASA', 'RHO', 
                        paste('Ataque', dcm$times[1:(ntimes-1)], sep='.'),
                        paste('Defesa', dcm$times, sep='.'))
  res <- optim(par=par.0, fn=otimizador, DCm=dcm, method='BFGS')
res$par  

#reparametrizando:
parametros <- res$par
#calculando o ultimo parametro de ataque
missing.ataque <- sum(parametros[3:(ntimes+1)]) * -1
parametros <- c(parametros[1:(ntimes+1)], missing.ataque, parametros[(ntimes+2):length(parametros)])
names(parametros)[ntimes+2] <- paste('Ataque.', dcm$times[ntimes], sep='')
#aumentando os parametros de ataque por uma unidade
parametros[3:(ntimes+2)] <- parametros[3:(ntimes+2)] + 1  
#diminundo os parametros de defesa por uma unidade
parametros[(ntimes+3):length(parametros)] <- parametros[(ntimes+3):length(parametros)] - 1 


#calculo de probabilidades
lambda=exp(parametros['CASA']+parametros['Ataque.Santos']+parametros['Defesa.Palmeiras'])
mu=exp(parametros['Ataque.Palmeiras']+parametros['Defesa.Santos'])

max_gols = 10 
probabilidades <- dpois(0:max_gols, lambda) %*% t(dpois(0:max_gols, mu))
#matriz de probabilidades, soma 1 (ou quase um, pq vai até 6 gols).
#prob. número de gols da equipe mandante na vertical, e da visitante na horizontal
matriz_dependencia <- matrix(tau(c(0,1,0,1), c(0,0,1,1), lambda, mu, parametros['RHO']), nrow=2)
probabilidades[1:2, 1:2] <- probabilidades[1:2, 1:2] * matriz_dependencia
#recalculando usando as dependencias
prob_mandante <- sum(probabilidades[lower.tri(probabilidades)])
prob_empate <- sum(diag(probabilidades))
prob_visitante <- sum(probabilidades[upper.tri(probabilidades)])



#agora versão com pesos, mais peso às partidas mais recentes


log_veross_pesos <- function(y1, y2, lambda, mu, rho=0, pesos=NULL){
  #rho=0, independencia
  #y1 gols do mandante
  #y2 gols do visitante
  logvero <- log(tau(y1, y2, lambda, mu, rho)) + log(dpois(y1, lambda)) + log(dpois(y2, mu))
  if (is.null(pesos)){
    return(sum(logvero))
  } else {
    return(sum(logvero*pesos))
  }
}

datas=as.Date(dados$data)


data_referencia=as.Date("2021-12-10")

otimizador_pesos<- function(params, DCm, pesos){
  
  casa.p <- params[1]
  rho.p <- params[2]
  
  ntimes <- length(factor(DCm$times))
  ataque.p <- matrix(params[3:(ntimes+1)], ncol=1) #one column less
  defesa.p <- matrix(params[(ntimes+2):length(params)], ncol=1) 
  
  lambda <- exp(DCm$mandante_ataque %*% ataque.p + DCm$visitante_defesa %*% defesa.p + casa.p)
  mu <- exp(DCm$visitante_ataque %*% ataque.p + DCm$mandante_defesa %*% defesa.p)
  
  return(
    log_veross_pesos(y1=DCm$gols_mandante, y2=DCm$gols_visitante, lambda, mu, rho.p,pesos) * -1
  )
}

#DCweights
funcao_pesos <- function(dates, data_atual, xi=0){
  data_diffs <- dates - as.Date(data_atual)
  data_diffs <- as.numeric(data_diffs *-1)
  w <- exp(-1*xi*data_diffs)
  w[data_diffs <= 0] <- 0
  return(w)
}

weights=funcao_pesos(datas,data_referencia,xi=0.0018)
ntimes <- length(factor(dcm$times))
ataque.params <- rep(.1, times=ntimes-1)
defesa.params <- rep(-0.8, times=ntimes)
casa.param <- 0.06
rho.0 <- 0.03
par.0 <- c(casa.param, rho.0, ataque.params, defesa.params)

names(par.0) <- c('CASA', 'RHO', 
                      paste('Ataque', dcm$times[1:(ntimes-1)], sep='.'),
                      paste('Defesa', dcm$times, sep='.'))
res <- optim(par=par.0, pesos=weights, fn=otimizador_pesos, DCm=dcm, method='BFGS')
#reparametrizando:
parametros <- res$par
#calculando o ultimo parametro de ataque
missing.ataque <- sum(parametros[3:(ntimes+1)]) * -1
parametros <- c(parametros[1:(ntimes+1)], missing.ataque, parametros[(ntimes+2):length(parametros)])
names(parametros)[ntimes+2] <- paste('Ataque.', dcm$times[ntimes], sep='')
#aumentando os parametros de ataque por uma unidade
parametros[3:(ntimes+2)] <- parametros[3:(ntimes+2)] + 1  
#diminundo os parametros de defesa por uma unidade
parametros[(ntimes+3):length(parametros)] <- parametros[(ntimes+3):length(parametros)] - 1 

t(t(parametros))

#modelo dinâmico

dados$datas=as.Date(dados$data)
dados$datas_num=as.numeric(dados$datas)
head(dados)



forca_tempo=matrix(0,75,22)
colnames(forca_tempo)=c('CASA','RHO',dcm$times)

for (t in 1:75){
  print(t)
  ref=18895+t
  dados1=dados[ref-dados$datas_num<120,]
  dados1=dados1[ref-dados1$datas_num>-1,]
  
  datas=as.Date(dados1$data)
  
  data_referencia=as.Date(ref,origin="1970-01-01")

  
  dcm <- dados_formatados(dados1)
  ntimes <- length(factor(dcm$times))
  ataque.params <- rep(.1, times=ntimes-1)
  defesa.params <- rep(-0.8, times=ntimes)
  casa.param <- 0.06
  rho.0 <- 0.03
  par.0 <- c(casa.param, rho.0, ataque.params, defesa.params)
  
  names(par.0) <- c('CASA', 'RHO', 
                    paste('Ataque', dcm$times[1:(ntimes-1)], sep='.'),
                    paste('Defesa', dcm$times, sep='.'))
  weights=funcao_pesos(datas,data_referencia,xi=0.0018)
  res <- optim(par=par.0, pesos=weights, fn=otimizador_pesos, DCm=dcm, method='BFGS')
  #reparametrizando:
  
  parametros <- res$par
  
  #calculando o ultimo parametro de ataque
  missing.ataque <- sum(parametros[3:(ntimes+1)]) * -1
  
  parametros <- c(parametros[1:(ntimes+1)], missing.ataque, parametros[(ntimes+2):length(parametros)])
  names(parametros)[ntimes+2] <- paste('Ataque.', dcm$times[ntimes], sep='')
  
  #aumentando os parametros de ataque por uma unidade
  parametros[3:(ntimes+2)] <- parametros[3:(ntimes+2)] + 1  
  
  #diminundo os parametros de defesa por uma unidade
  parametros[(ntimes+3):length(parametros)] <- parametros[(ntimes+3):length(parametros)] - 1 
  
  forca_tempo[t,1]=parametros[1]
  forca_tempo[t,2]=parametros[2]
  for (k in 3:22){
    forca_tempo[t,k]=parametros[k]-parametros[k+20]}
  
}


t(t(parametros))
head(dados1)

head(forca_tempo)

ts.plot(forca_tempo[,6],ylim=c(1,3.5),ylab="")
par(new=T)
ts.plot(forca_tempo[,12],ylim=c(1,3.5),col=2,ylab="")
par(new=T)
ts.plot(forca_tempo[,18],ylim=c(1,3.5),col=3,ylab="")
par(new=T)
ts.plot(forca_tempo[,14],ylim=c(1,3.5),col=4,ylab="Força")
legend(1,2,legend=c("Atl-MG","Fla","Pal","For"),
       col=c(1,2,3,4),lty=1,cex=0.8)


ts.plot(forca_tempo[,1],ylab="Fator do time mandante")
