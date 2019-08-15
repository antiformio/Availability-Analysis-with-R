#-------------------------------------------------------------------------------
#
#
#   MES DE JANEIRO E FEVEREIRO DE 2017 NO SERVIDOR VSRV25
#
#--------------------------------------------------------------------------------

#Ler Dados do servidor
dados25 <- read.delim("vsrv25-ssh.txt", sep = ";", col.names = c("servername","state","limits","number","ssh"))
attach(dados25)

dados3<-data.frame(dados25)

dados3$time<-substr(dados3$servername,2,11) #Segundos extraídos da coluna

dados3$data = as.POSIXct(as.numeric(dados3$time), origin = "1970-01-01",tz = "GMT") #Dados da data trabalhados

down_list3<- grep("DOWN", dados3$state)

#-----------------------------------
#calcular taxa de disponibilidade
#-----------------------------------

ini_jan_fev<-as.numeric(as.POSIXct("2017-01-01 00:00:00 GMT"))
fim_jan_fev<-as.numeric(as.POSIXct("2017-02-28 23:59:59 GMT"))



sum_down_jan_fev <- 0
conta<- 0 #contador de numero de falhas

#Calculo do tempo de indisponibilidade do servidor 29 (no ano de 2017 mes janeiro e fevereiro)
for(i in down_list3){ #Down list contém o GREP calculado em cima (as ocurrencias de todos os DOWNS)
  if(dados3[i,7]<fim_jan_fev && dados3[i,7]>ini_jan_fev){  
    data_ini <- dados3[i,7] 
    for(j in i:dim(dados3)[1]){
      if(dados3[j,2]=="UP"){
        data_fim <-dados3[j,7]
        sum_down_jan_fev <- sum_down_jan_fev + (data_fim-data_ini)
        conta<-conta+1
        break
      }
    }
  }
}

View(sum_down_jan_fev)
View(conta) 

#Primeira ocorrencia de janeiro de 2017
for(i in 1:dim(dados3)[1]){
  if(dados3[i,7]>=ini_jan_fev){
    ini<-dados3[i,7]
    break;
  }
}

#Procurar a ultima ocurrencia de fevereiro 2017
for(i in 1:dim(dados3)[1]){
  if(dados3[i,7]>fim_jan_fev){
    fim<-dados3[i-1,7]
    break;
  }
}
#tempo de indisponibilidade do servidor
View(sum_down_jan_fev)

#calculo do tempo em que esteve operacional (5097600 são 59 dias em segundos)
periodoOperacional <- 5097600 - sum_down_jan_fev
View(periodoOperacional)

View(conta)

#calculo da taxa de falhas
taxaDeFalhasJanFev<-conta/as.double(periodoOperacional) #numero de falhas pelo numero de minutos que esteve ON
View(taxaDeFalhasJanFev)


#Função disponibilidade do servidor para os meses janeiro e fevereiro
func_jan_fev<-function(tempo)
{
  exp(-taxaDeFalhasJanFev*tempo)
}

timeInMinuts<-5097600/60 #dois meses (janeiro e fevereiro) em minutos

# desenho do gráfico da função para o periodo em minutos dos meses janeiro e fevereiro
plot(func_jan_fev,1,timeInMinuts)


#Resposta para o relatório: Pode-se verificar que o servidor reduz a sua fiabilidade ao longo do tempo
