# ANÁLISE ESPECTRAL

# SIMULAÇÃO 1 - Ruído Branco Gaussiano
t <-  100
x <-  rnorm(t)

# Análise Espectral - Espectro Estimado
espectro <- spectrum(x, plot=F)

plot(espectro$freq, espectro$spec, typ='l',
     xlab='Freq', ylab='|c|^2', main='Periodograma')

# Espectro estimado suavizado
espectro <- spectrum(x, spans=10, plot=F)
plot(espectro$freq, espectro$spec, type='l',
     xlab='Freq', ylab='|c|^2', main='Periodograma')

# SIMULAÇÃO 2 - Onda + Ruído Branco Gaussiano

t <- 100
x <- sin(2*pi*15*(1:t)/t) + rnorm(t) # Cria o seno com 15 ciclos
ts.plot(x) # Difícil visualizar periodicidade

# Análise Espectral - Espectro Estimado
espectro <- spectrum(x, plot=F)

plot(espectro$freq, espectro$spec, type = 'l',
     xlab = 'Freq', ylab = '|c|^2')

# Espectro Suavizado
espectro <- spectrum(x, spans=10, plot=F)

plot(espectro$freq, espectro$spec, type='l',
     xlab='Freq', ylab='|c|^2')

# SIMULAÇÃO 2.1 - Onda + Ruído Branco Gaussiano

t <- 100
x <- sin(2*pi*15*(1:t)/t)+0.5*sin(2*pi*30*(1:t)/t)+rnorm(t)
ts.plot(x) # Difícil visualizar periodicidade

# Análise Espectral - Espectro Estimado
espectro <- spectrum(x, plot=F)
plot(espectro$freq, espectro$spec, type='l',
     xlab = 'Freq', ylab = '|c|^2')

# Análise Espectral - Espectro Suavizado

espectro <- spectrum(x, spans=5, plot=F) # Diminuir o span faz ficar mais fácil visualizar o pico
                                        # Span grande pode distorcer o gráfico        
plot(espectro$freq, espectro$spec, type='l',
     xlab='Freq', ylab='|c|^2')