# LEITURA DE DADOS
sinais <- read.table('OlhosFechados.txt')
nomescanais <- scan('NOMEScanais.txt', what='string')

tmax <- nrow(sinais)
ncanais <- ncol(sinais)
hz <- 250

#Sinal no O1
ts.plot(sinais[,29])

#Espectro Original O1
espectro <- spectrum(sinais[,29], plot=F)
plot(espectro$freq*hz, log(espectro$spec), type='l') # Espectro bruto, sinal sem processamento

#Filtro Passa-Banda de 1-30Hz
filtro <- signal::butter(5, c(1,30)/(hz/2), 'pass')
fsinal <- signal::filter(filtro, sinais[,29])

# Definir tamanho da época (depende da situação que pode gerar artefatos)
# Criar épocas
tamEpoca <- 10 #tam. da epoca em segundos

Nepocas <- floor(length(fsinal)/(hz*tamEpoca))

media <- 0

# Controle de qualidade rigoroso
amplitude <- array(0, Nepocas)
cutoff <- 60 # Para descartar épocas com alta amplitude
NepocasBoas <- 0

#Percorre todas as épocas
for (epoca in 1:Nepocas){
        inicio <- hz*tamEpoca*(epoca-1)+1
        fim <- hz*tamEpoca*epoca
        janela <- inicio:fim
        amplitude[epoca] <- max(fsinal[janela]) - min(fsinal[janela]) #Variação do sinal naquela época
        
        #Periodograma do sinal nesta EPOCA
        espectro <- spectrum(fsinal[janela], plot=F)
        freq <- espectro$freq
        
        #Tira média do espectro
        #Só das épocas que passaram no controle de qualidade
        if (amplitude[epoca] < cutoff){
        media <- media + espectro$spec
        NepocasBoas <- NepocasBoas + 1
        
        }
}
media <- media/NepocasBoas #Computa a média só das épocas boas
#Plot do Espectro Médio
plot(freq, media, type='l',
     xlab = 'Freq(Hz)',
     ylab = 'Espectro Médio')

#Histograma das amplitudes
hist(amplitude[which(amplitude < 1000)]) # Retirando os valores muito altos
boxplot(amplitude[which(amplitude < 1000)])