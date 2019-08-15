#Soma dos sumdowns de todos os servidores num dataframe (serve para a alínea e)
maior=0
tamanho <- c(as.numeric(length(arrayTempo_1)),as.numeric(length(array_tempo_2)),as.numeric(length(array_tempo_3)),as.numeric(length(array_tempo_4)))

#Ver o tamanho máximo de tamanho dos arrays
maior<-max(tamanho,na.rm=FALSE)
arrayTempo_1[maior]=NA
array_tempo_2[maior]=NA
array_tempo_3[maior]=NA
array_tempo_4[maior]=NA

total_sum_downs<-data.frame(SRV24=arrayTempo_1,SRV29=array_tempo_2,SRV25=array_tempo_3,SRV27=array_tempo_4)

library(reshape2)

Mdados<-melt(total_sum_downs, variable.name = "servidor", value.name = "tempoDown", na.rm=TRUE)
View(Mdados)

library(car)
#Teste de Levene para verificar as variâncias em todos os servidores
leveneTest(tempoDown~servidor,Mdados,center=mean)
#Como p-value = 0.003799 < 0.05, logo rejeita-se que as variancias sejam iguais.
#uma vez que a amostra é superior a 30 consideramos os dados normais e realiza-se o teste anova

anova(lm(tempoDown~servidor,Mdados))
#como pvalue (0.1323) > alfa (0.05) não se rejeita h0, como tal e a um grau de confiança de 95% pode-se concluir que nao existe diferença
# significativa entre as duraçoes médias de falhas de todos os servidores durante o periodo total de verificaçao



