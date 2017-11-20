# LEITURA DE DADOS EEG
sinais <- read.table("oddball250hz.txt")
nomes <- scan('nomecanais.txt', what='string')

HZ <- 250 #taxa de amostragem

dim(sinais)
# 45461/250 = 181.844 segundos de aquisição - 3 min de aquisição
# pra cada canal temos que filtrar esse sinal
Ncanais <- 32
tmax <- dim(sinais[1])
trigger <- sinais[,33]
# PROCESSAMENTO DE DADOS
ts.plot(sinais[,5]) # plot do sinal do canal 5, Fz

#filtro passa-banda 1-30Hz
library(signal)
filtro <- butter(5, c(1,30)/(HZ/2), 'pass')
freqz(filtro)

#matriz dos sinais filtrados
fsinais <- matrix(0,tmax, Ncanais)

for (canal in 1:Ncanais){
        fsinais[,canal] <- filter(filtro,sinais[,canal])
}
ts.plot(fsinais[,5])

# ANÁLISE
#pra identificar os tempos onde aconteceram os estímulos raros
raro <- 10
testim <- which(trigger == raro)

#pegar um trecho 100ms antes e 1000ms para cada estímulo raro

canal <- 25
media <- 0

# 100ms de baseline (HZ/10) e 1000ms pós estímulo (HZ)
for (i in 1:length(testim)){
        janela <- (testim[i]-HZ/10):(testim[i]+HZ)
        baseline <- (testim[i]-HZ/10:testim[i])
        mediabaseline <- mean(fsinais[baseline, canal])
        #corrigir pelo baseline
        media <- media+(fsinais[janela, canal]-mediabaseline)/length(testim)
}

plot((1:length(media))/HZ-0.1,media, type='l', xlab='tempo(s)', ylab='sinal(mV)')
abline(v=0, lty=2) #linha vertical pontilhada

erpraro <- media

###

frequente <- 11
testim <- which(trigger == frequente)

#pegar um trecho 100ms antes e 1000ms para cada estímulo frequente

canal <- 25 #canal Pz
media <- 0

# 100ms de baseline (HZ/10) e 1000ms pós estímulo (HZ)
for (i in 1:length(testim)){
        janela <- (testim[i]-HZ/10):(testim[i]+HZ)
        baseline <- (testim[i]-HZ/10:testim[i])
        mediabaseline <- mean(fsinais[baseline, canal])
        #corrigir pelo baseline
        media <- media+(fsinais[janela, canal]-mediabaseline)/length(testim)
}

plot((1:length(media))/HZ-0.1, media, type='l', xlab='tempo(s)', ylab='sinal(mV)', ylim=c(-8,6))
abline(v=0, lty=2) #linha vertical pontilhada

erpfrequente <- media

# Adicionar o ERP do estímulo raro no gráfico

lines((1:length(erpraro))/HZ-0.1, erpraro, col=2)