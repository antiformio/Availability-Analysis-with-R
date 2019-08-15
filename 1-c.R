#-------------------------------------------------------------------------------
#
#
#   MES DE DEZEMBRO DE 2016 DO SERVIDOR VSR7V24
#
#--------------------------------------------------------------------------------

DadosFiabilidade <- read.delim("vsrv24-ssh.txt", sep = ";", col.names = c("servername","state","limits","number","ssh"))
attach(DadosFiabilidade)

dados<-data.frame(DadosFiabilidade)

dados$time<-substr(dados$servername,2,11) #Segundos extraídos da coluna

dados$data = as.POSIXct(as.numeric(dados$time), origin = "1970-01-01",tz = "GMT") #Dados da data trabalhados

View(dados)

down_list<- grep("DOWN", dados$state)

#----------------------------------------------
#
#   Verificar a taxa média real de falhas 
#
#----------------------------------------------

periodoDia <-1440 # Número de minutos num dia


View(listaFalhasDezembro)

#-------------------------------------------------------------------------------------
# Preenchimento de um vector com as taxas de falha de cada dia do mês de Dezembro 2016
#-------------------------------------------------------------------------------------

subLista <- subset(dados,dados$data>="2016-12-01 00:00:00" & dados$data<="2016-12-31 23:59:59") #todas as ocurrencias de dezembro 2016
View(subLista)

listaTaxaFalhas <- rnorm(31)

subLista$dataCompr<-substr(subLista$data,1,10) # Data em formato ano-mes-dia
View(subLista)
subLista_Length <- nrow(subLista) # Tamanho da sublista

for(i in 1:31){
  dia<-sprintf("2016-12-%d",i)
  falhas_dia<-0
  tempo_down<-0
  
  
  for(j in 1:subLista_Length){
    if(subLista[j,8]==dia && subLista[j,2]=="DOWN" && !is.na(subLista[j,])){
      falhas_dia<-falhas_dia+1
      tempo_falha_ini<-subLista[j,6]
      for(k in subLista[j,2])
        if(subLista[k,2]=="UP" && !is.na(subLista[k,])){
          tempo_falha_fim<-subLista[k,6]
          tempo_down <- tempo_down + (tempo_falha_fim-tempo_falha_ini)
          break
        }
      
      
    }
    
  }
  listaTaxaFalhas[i]<-falhas_dia/(periodoDia-tempo_down) #falhas por minuto
  
}



View(listaTaxaFalhas)

#verificar normalidade usando shapiro, qqnorm e qqline
shapiro.test(listaTaxaFalhas)
qqnorm(listaTaxaFalhas)
qqline(listaTaxaFalhas)

#os dados não são normais, no entanto como a amostra é superior a 30, vamos realizar o teste
# realização do teste com o valor de alternative a less uma vez que queremos estudar se foi inferior a 0.01(H1), logo e unilateral à esquerda

t.test(x = listaTaxaFalhas, alternative = "less", mu = 0.01, conf.level=0.95)
