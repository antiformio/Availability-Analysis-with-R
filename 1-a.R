#-------------------------------------------------------------------------------
#
#                 PRIMEIRO GRUPO
#
#-------------------------------------------------------------------------------

#------------
#
# Alínea a 
#
#------------

#-------------------------------------------------------------------------------
#
#
#   Disponibilidade do servidor VSRV24 
#
#--------------------------------------------------------------------------------

DadosFiabilidade <- read.delim("vsrv24-ssh.txt", sep = ";", col.names = c("servername","state","limits","number","ssh"))
attach(DadosFiabilidade)

dados<-data.frame(DadosFiabilidade)


dados$time<-substr(dados$servername,2,11) #Segundos extraídos da coluna

dados$data = as.POSIXct(as.numeric(dados$time), origin = "1970-01-01",tz = "GMT") #Dados da data trabalhados

View(dados)



down_list<- grep("DOWN", dados$state)
View(down_list)

sum_down <- 0
arrayTempo_1=c()
#Calculo do tempo de indisponibilidade do servidor
for(i in down_list){
  data_ini <- dados[i,7]
  for(j in i:dim(dados)[1]){
    if(dados[j,2]=="UP"){
      data_fim <-dados[j,7]
      sum_down <- sum_down + (data_fim-data_ini)
      arrayTempo_1[j]=(data_fim-data_ini)
      break
    }
  }
  
}
View(arrayTempo_1)
View(sum_down)
#tempo total de atividade
tempo_total<-as.numeric(tail(dados, 1)$time)-as.numeric(head(dados,1)$time)
View(tempo_total)
#tempo total de disponibilidade
tempo_disp<-tempo_total-sum_down

View(tempo_disp)

dias<-tempo_disp/(3600*24)
View(dias)

percentagem<-(tempo_disp/tempo_total)*100
View(percentagem)

#decimal<-dias-trunc(dias)
#horas<- decimal*7
#View(horas)

#decimal<-horas-trunc(horas)
#minutos<- decimal*60
#View(minutos)

#decimal<-minutos-trunc(minutos)
#segundos<- decimal*60

#-------------------------------------------------------------------------------
#
#
#   Disponibilidade do servidor VSRV29 
#
#--------------------------------------------------------------------------------

dados29 <- read.delim("vsrv29-ssh.txt", sep = ";", col.names = c("servername","state","limits","number","ssh"))
attach(dados29)

dados2<-data.frame(dados29)

dados2$time<-substr(dados2$servername,2,11) #Segundos extraídos da coluna

dados2$data = as.POSIXct(as.numeric(dados2$time), origin = "1970-01-01",tz = "GMT") #Dados da data trabalhados

down_list2<- grep("DOWN", dados2$state)
View(down_list2)
View(dados2)


sum_down2 <- 0 #Inicializar o contador do tempo total em que esteve down o servidor
array_tempo_2=c()
#Calculo do tempo de indisponibilidade do servidor
for(i in down_list2){
  data_ini2 <- dados2[i,7]
  for(j in i:dim(dados2)[1]){
    if(dados2[j,2]=="UP"){
      data_fim2 <-dados2[j,7]
      sum_down2 <- sum_down2 + (data_fim2-data_ini2)
      array_tempo_2[j]=(data_fim2-data_ini2)
      break
    }
  }
  
}
View(sum_down2)
#tempo total de atividade (tempo total de actividade do sistema )
tempo_total2<-as.numeric(tail(dados2, 1)$time)-as.numeric(head(dados2,1)$time)

#tempo total de disponibilidade (em segundos)
tempo_disp2<-tempo_total2-sum_down2

View(tempo_disp2)

#calculo de dias em que o servidor esteve DISPONIVEL durante o periodo de actividade
dias2<-tempo_disp2/(3600*24)
View(dias2)

percentagem<-(tempo_disp2/tempo_total2)*100
View(percentagem)

#-------------------------------------------------------------------------------
#
#
#   Disponibilidade do servidor VSRV25 
#
#--------------------------------------------------------------------------------
dados25 <- read.delim("vsrv25-ssh.txt", sep = ";", col.names = c("servername","state","limits","number","ssh"))
attach(dados25)

dados3<-data.frame(dados25)

dados3$time<-substr(dados3$servername,2,11) #Segundos extraídos da coluna

dados3$data = as.POSIXct(as.numeric(dados3$time), origin = "1970-01-01",tz = "GMT") #Dados da data trabalhados

down_list3<- grep("DOWN", dados3$state)
View(down_list3)



sum_down3 <- 0 #Inicializar o contador do tempo total em que esteve down o servidor
array_tempo_3=c()
#Calculo do tempo de indisponibilidade do servidor
for(i in down_list3){
  data_ini3 <- dados3[i,7]
  for(j in i:dim(dados3)[1]){
    if(dados3[j,2]=="UP"){
      data_fim3 <-dados3[j,7]
      sum_down3 <- sum_down3 + (data_fim3-data_ini3)
      array_tempo_3[j]=(data_fim3-data_ini3)
      break
    }
  }
  
}
View(sum_down3)
#tempo total de atividade (tempo total de actividade do sistema )
tempo_total3<-as.numeric(tail(dados3, 1)$time)-as.numeric(head(dados3,1)$time)

#tempo total de disponibilidade (em segundos)
tempo_disp3<-tempo_total3-sum_down3

View(tempo_disp3)

#calculo de dias em que o servidor esteve DISPONIVEL durante o periodo de actividade
dias3<-tempo_disp3/(3600*24)
View(dias3)

percentagem<-(tempo_disp3/tempo_total3)*100
View(percentagem)

#-------------------------------------------------------------------------------
#
#
#   Disponibilidade do servidor VSRV27 
#
#--------------------------------------------------------------------------------
dados27 <- read.delim("vsrv27-ssh.txt", sep = ";", col.names = c("servername","state","limits","number","ssh"))
attach(dados27)

dados4<-data.frame(dados27)

dados4$time<-substr(dados4$servername,2,11) #Segundos extraídos da coluna

dados4$data = as.POSIXct(as.numeric(dados4$time), origin = "1970-01-01",tz = "GMT") #Dados da data trabalhados

down_list4<- grep("DOWN", dados4$state)




sum_down4 <- 0 #Inicializar o contador do tempo total em que esteve down o servidor
array_tempo_4=c()
#Calculo do tempo de indisponibilidade do servidor
for(i in down_list4){
  data_ini4 <- dados4[i,7]
  for(j in i:dim(dados4)[1]){
    if(dados4[j,2]=="UP"){ #quando o servidor voltar a estar up fazemos as contas da indisponibilidade
      data_fim4 <-dados4[j,7]
      sum_down4 <- sum_down4 + (data_fim4-data_ini4)
      array_tempo_4[j]=(data_fim4-data_ini4)
      break
    }
  }
  
}
View(sum_down4)
#tempo total de atividade (tempo total de actividade do sistema )
tempo_total4<-as.numeric(tail(dados4, 1)$time)-as.numeric(head(dados4,1)$time)

#tempo total de disponibilidade (em segundos)
tempo_disp4<-tempo_total4-sum_down4

View(tempo_disp4)

#calculo de dias em que o servidor esteve DISPONIVEL durante o periodo de actividade
dias4<-tempo_disp4/(3600*24)
View(dias4)

percentagem<-(tempo_disp4/tempo_total4)*100
View(percentagem)






#-------------------------------------------------------------------------------
#
#
#   NO ANO DE 2016 DO SERVIDOR VSRV24
#
#--------------------------------------------------------------------------------


ini_2016<-as.numeric(as.POSIXct("2016-01-01 00:00:00 GMT"))
fim_2016<-as.numeric(as.POSIXct("2016-12-31 23:59:59 GMT"))

sum_down_2016 <- 0

#Calculo do tempo de indisponibilidade do servidor (no ano de 2016)
for(i in down_list){ #Down list contém o GREP calculado em cima (as ocurrencias de todos os DOWNS)
  if(dados[i,7]<fim_2016){ #Se a data que estamos a ler for ainda dentro do ano de 2016 
    data_ini <- dados[i,7] 
    for(j in i:dim(dados)[1]){
      if(dados[j,2]=="UP"){
        data_fim <-dados[j,7]
        sum_down_2016 <- sum_down_2016 + (data_fim-data_ini)
        
        break
      }
    }
  }
}

#Procurar a ultima ocurrencia de 2016 do servidor 24
for(i in 1:dim(dados)[1]){
  if(dados[i,7]>fim_2016){
    fim<-dados[i-1,7]
    break;
  }
}

#tempo total de atividade(fim calculado em cima, menos o inicio do ficheiro-que é já em 2016)
tempo_total_2016<-as.numeric(fim)-as.numeric(head(dados,1)$time)

#tempo total de disponibilidade
tempo_disp_2016<-tempo_total_2016-sum_down_2016

View(tempo_disp_2016)

#Disponíbilidade total do servidor em dias
dias<-tempo_disp_2016/(3600*24)
View(dias)


percentagem<-(tempo_disp_2016/tempo_total_2016)*100
View(percentagem)






#-------------------------------------------------------------------------------
#
#
#   NO MÊS DE FEVEREIRO 2017 DO SERVIDOR VSRV27
#
#--------------------------------------------------------------------------------

ini_fev_2017<-as.numeric(as.POSIXct("2017-02-01 00:00:00 GMT"))
fim_fev_2017<-as.numeric(as.POSIXct("2017-02-28 23:59:59 GMT"))

sum_down_fev_2017 <- 0

#Calculo do tempo de indisponibilidade do servidor 27 (no ano de 2017 e mes fevereiro)
for(i in down_list4){ #Down list contém o GREP calculado em cima (as ocurrencias de todos os DOWNS)
  if(dados4[i,7]<fim_fev_2017 && dados4[i,7]>ini_fev_2017){  
    data_ini <- dados4[i,7] 
    for(j in i:dim(dados4)[1]){
      if(dados4[j,2]=="UP"){
        data_fim <-dados4[j,7]
        sum_down_fev_2017 <- sum_down_fev_2017 + (data_fim-data_ini)
        
        break
      }
    }
  }
}

#Primeira ocorrencia de fev 2017 do servidor 27
for(i in 1:dim(dados4)[1]){
  if(dados4[i,7]>=ini_fev_2017){
    ini<-dados4[i,7]
    break;
  }
}

#Procurar a ultima ocurrencia de fev 2017 do servidor 27
for(i in 1:dim(dados4)[1]){
  if(dados4[i,7]>fim_fev_2017){
    fim<-dados4[i-1,7]
    break;
  }
}

#tempo total de atividade
tempo_total_fev_2017<-as.numeric(fim)-as.numeric(ini)

#tempo total de disponibilidade
tempo_disp_fev_2017<-tempo_total_fev_2017-sum_down_fev_2017

View(tempo_disp_fev_2017)

#Disponíbilidade total do servidor em dias
dias<-tempo_disp_fev_2017/(3600*24)
View(dias)


percentagem<-(tempo_disp_fev_2017/tempo_total_fev_2017)*100
View(percentagem)


#-------------------------------------------------------------------------------
#
#
#   NO DIA 28 DE FEVEREIRO NO SERVIDOR VSRV29
#
#--------------------------------------------------------------------------------


ini_28_fev<-as.numeric(as.POSIXct("2017-02-28 00:00:00 GMT"))
fim_28_fev<-as.numeric(as.POSIXct("2017-02-28 23:59:59 GMT"))

sum_down_28_fev <- 0

#Calculo do tempo de indisponibilidade do servidor 29 (no ano de 2017 mes fevereiro dia 28)
for(i in down_list2){ #Down list contém o GREP calculado em cima (as ocurrencias de todos os DOWNS)
  if(dados2[i,7]<fim_28_fev && dados2[i,7]>ini_28_fev){  
    data_ini <- dados2[i,7] 
    for(j in i:dim(dados2)[1]){
      if(dados2[j,2]=="UP"){
        data_fim <-dados2[j,7]
        sum_down_28_fev <- sum_down_28_fev + (data_fim-data_ini)
        
        break
      }
    }
  }
}

View(sum_down_28_fev)



#tempo total de atividade
tempo_total_28_fev<-86400 # é 24 horas * 3600 segundos

#tempo total de disponibilidade
tempo_disp_28_fev<-tempo_total_28_fev-sum_down_28_fev #tempo disponivel 86400 menos o tempo em que esteve down

View(tempo_disp_28_fev) 

#Disponíbilidade total do servidor em dias
dias<-tempo_disp_28_fev/(3600*24) 
View(dias)

percentagem<-(tempo_disp_28_fev/tempo_total_28_fev)*100
View(percentagem)



