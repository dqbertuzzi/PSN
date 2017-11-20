### AULA 2 FILTRAGEM DE SINAIS

#Considere que um sinal foi adquirido sob uma taxa de amostragem de 100Hz.

t <- 1000
x <- numeric(t) # criação de um vetor vazio de t elementos

# input de dados aleatórios no vetor vazio
for (i in 2:t){
        x[i] <- x[i-1] + rnorm(1)
}

ts.plot(x)

#Taxa de amostragem
hz <- 100

#Aplicar filtro
library(signal) #selecionando pacote signal
filtro <- butter(5, 2/(hz/2), "low")
filtro2 <- butter(5, 2/(hz/2), "high")
filtro3 <- butter(5, c(1,10)/(hz/2), "pass") #passa-banda em 1-10hz
 
#Plotar desenho de filtro
freqz(filtro)

#Aplicar filtro no sinal x
xf <- filter(filtro, x) #passa-baixa
xf2 <- filter(filtro2, x) #passa-alta
xf3 <- filter(filtro3, x) #passa-banda

ts.plot(x, xf, col=c("green", "blue")) #plotagem do sinal x (bruto) em verde + sinal xf passa-baixa em azul
ts.plot(x, xf2, col=c("green", "red")) #plotagem do sinal x (bruto) em verde + sinal xf2 passa-alta em vermelho
library(ggplot2)
ggplot() +
        geom_line(aes(1:t,x, col="x"), size=0.8) +
        geom_line(aes(1:1000, xf, col="xf"), size=0.8) +
        geom_line(aes(1:1000, xf2, col="xf2")) +
        geom_line(aes(1:1000, xf3, col="xf3"))
