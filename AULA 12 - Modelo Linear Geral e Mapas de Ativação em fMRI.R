library(AnalyzeFMRI)

vol <- f.read.volume("Stroop.nii")

dim(vol) #45 x, 54 y, 45 z, 180 pontos no tempo

vol[30,30,30,] # Este número representa o sinal BOLD neste voxel que representa uma localização no cérebro

ts.plot(vol[30,30,30,])

##-- Como obter mapas de ativação (heatmaps)

# Ler o desenho experimental (as condições CONGRUENTE / INCONGRUENTE)
# Congruente
cong <- scan("congruent.txt")

# Incongruente
incong <- scan("incongruent.txt")

ts.plot(cbind(cong, incong), col=c(1,2))

# HRF - Hemodynamic Response Function (modelo)
glover <- function(HZ){
        a1=6
        a2=12
        b1=0.9
        b2=0.9
        d1=5.4
        d2=10.8
        c=0.35
        x=seq(0, 30, 1/HZ) # HZ is the Sampling Rate (Heartz)
        glover1=((x/d1)^a1)*exp((-x+d1)/b1)
        glover2=((x/d2)^a2)*exp((-x+d2)/b2)
        G=glover1-c*glover2
        return(G)
}

# TR = 2
HZ <- 1/2
hrf <- glover(HZ)
ts.plot(hrf)


# Convolução da condição incongruente pela HRF
incongc <- array(0, length(incong))

for(ti in (length(hrf) + 1):length(incong)){
        for (i in 1:length(hrf)){
                incongc[ti] <- incongc[ti] + incong[ti - i] * hrf[i]
        }
}

ts.plot(incongc)

# Convolução da condição congruente pela HRF
congc <- array(0, length(cong))

for(ti in (length(hrf) + 1):length(cong)){
        for (i in 1:length(hrf)){
                congc[ti] <- congc[ti] + cong[ti - i] * hrf[i]
        }
}

ts.plot(congc)

ts.plot(cbind(congc, incongc), col=c(1,2))

# Ajuste de escalas (normalização 0-1)

congc <- congc/max(congc)
incongc <- incongc/max(incongc)

ts.plot(cbind(congc, incongc), col=c(1,2))

# Modelo Linear Geral (Regressão Múltipla)
# Resposta = BOLD Observado
# Preditoras = Condições experimentais convoluídas
# Para cada voxel da imagem
# Voxel-wise/mass-univariate voxel analysis

# Modelo
# BOLD[t] <- a + B1 * congc[t] + b2 * icongc[t] + e[t]


beta1 <- array(0, c(dim(vol)[1], dim(vol)[2], dim(vol)[3], 1))
beta2 <- array(0, c(dim(vol)[1], dim(vol)[2], dim(vol)[3], 1))

for (xi in 1:(dim(vol)[1])){
        for (yi in 1:(dim(vol)[2])){
                for (zi in 1:(dim(vol)[3])){
                        # Sinal BOLD no voxel
                        bold <- vol[xi, yi, zi,]
                        
                        # Ajuste do modelo
                        glm <- lm(bold ~ congc + incongc)
                        
                        # Cria os mapas dos coeficientes
                        beta1[xi, yi, zi,1] <- glm$coef[2]
                        beta2[xi, yi, zi,1] <- glm$coef[3]
                }
        }
}
