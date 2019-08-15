#-------------------------------------------------------------------------------
#
#
#   VERIFICAÇÃO DE DIFERENÇAS ENTRE SERVIDOR 25 E 29
#
#--------------------------------------------------------------------------------

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


#-------------------------------------------------------------------------------
#
#
#   Comparação dos servidores
#
#--------------------------------------------------------------------------------

var.test(down_list2,down_list3)
#Como p-value = 0.6556 > 0.05 , não se rejeita H0, logo pode-se concluir que a variância é igual

#Verificar normalidade dos dados recorrendo ao shapiro, qqnorm e qqline
shapiro.test(down_list2)
qqnorm(down_list2)
qqline(down_list2)

shapiro.test(down_list3)
qqnorm(down_list3)
qqline(down_list3)

# realização do teste com o valor de alternative a two.sided uma vez que queremos estudar se forem diferentes de forma significativa, logo e bilateral

t.test(down_list2,down_list3,alternative = "two.sided",conf.level = 0.95, mu= 0, var.equal = TRUE)
