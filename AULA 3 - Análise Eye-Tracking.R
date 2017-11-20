### AULA 3 - SINAIS BRUTOS DE EYE-TRACKING

#importando dados do eyetracking
#trocar tab por ;
#checar separador decimal (, ou .)
#checar linha de header/cabeçalho
#checar se a última linha de dados é de fato de dados e não rodapé

setwd <- "" #setando diretório
dataset <- read.table("dadosEYE.txt",header=TRUE,sep=";") #importando os dados no dataframe dataset

colnames(dataset)

# 1) descobrir taxa de amostragem
# intervalo médio de tempo (em segundos) entre observações
diff(dataset[,1]) #diferença do dado anterior
intervalo <- mean(diff(dataset[,1])) / 1000

tx <- (1 / intervalo) #taxa de amostragem
txnyquist <- tx / 2 #taxa de nyquist (metade da taxa de amostragem)

# diâmetro da pupila
# [13] pupil diameter right
pdireita <- dataset[,13]

ts.plot(pdireita)

# trabalhando com missing data
# missing data neste caso = valor zero
# tirar os missing data repetindo o último valor
for (i in 2:length(pdireita)){
        if (pdireita[i] == 0){
                pdireita[i] <- pdireita[i - 1]
        }
}

ts.plot(pdireita)

# remover picos do gráfico
library(signal)
#ordem do filtro = velocidade de decaimento, ordem mto grande = problema de artefatos de borda
#passa-baixa em 1Hz
filtro <- butter(5, 1/(tx/2), "low")
freqz(filtro)

pdireitaf <- filter(filtro, pdireita) #pdireita filtrada


#passa-baixa em 0.1Hz
filtro <- butter(5, 0.1/(tx/2), "low")

#passa-baixa em 0.5Hz
filtro <- butter(5, 0.5/(tx/2), "low")

#janela de 30 segundos
janela <- round(tx*30,0)

#ciclos
#trigger 1
# 61
# 121
# 181
# 241

# 30 caselas representam 1 segundo
# a cada segundo ele colheu 30 observações

pdireitaf[1:janela] #repouso
ciclo1 <- pdireitaf[1:(1+2*janela)] # repouso + tarefa
ciclo2 <- pdireitaf[(61*tx):(61*tx+2*janela)]
ciclo3 <- pdireitaf[(121*tx):(121*tx+2*janela)]
ciclo4 <- pdireitaf[(181*tx):(181*tx+2*janela)]
ciclo5 <- pdireitaf[(241*tx):(241*tx+2*janela)]

media <- (sum(ciclo1, ciclo2, ciclo3, ciclo4, ciclo5)/5)

ts.plot(media)
boxplot(media[1:902], media[903:1805])