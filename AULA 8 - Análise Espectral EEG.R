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
plot(espectro$freq*hz, log(espectro$spec), type='l')

#Filtro Passa-Banda de 1-30Hz
filtro <- signal::butter(5, c(1,30)/(hz/2), 'pass')
fsinal <- signal::filter(filtro, sinais[,29])

#Espectro O1 filtrado
espectro <- spectrum(fsinal, plot=F)
plot(espectro$freq*hz, espectro$spec, type='l',
     xlab='Freq(Hz)',
     ylab='Spec')

#Espectro O1 suavizado
espectro <- spectrum(fsinal, spans=50, plot=F)
plot(espectro$freq*hz, espectro$spec, type='l',
     xlab='Freq',
     ylab='Espectro')