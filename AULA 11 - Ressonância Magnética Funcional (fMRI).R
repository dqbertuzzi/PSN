# AULA 11 - fMRI

#install.packages('AnalyzeFMRI')
#library(AnalyzeFMRI)

vol <- f.read.volume('Stroop.nii') # Estrutura de dados que vai guardar estes dados
dim(vol) #45 Voxeis no x, 45 y, 45 z (fatia), 180 no tempo

# Imagem axial/horizontal no slice 20 e t = 1
image(vol[,,20,1], col=gray((1:50)/50))

# Imagem sagital no t = 1, escolha a fatia
image(vol[28,,,1], col=gray((1:50)/50))

# Imagem coronal no t = 1, escolha a fatia
image(vol[,20,,1], col=gray((1:50)/50))

# Selecionando um voxel x=35 y=43 z=23 t=1
# Ver localização
# vol[35,43,23,1] <- 0
image(vol[,,23,1], col=gray((1:50)/50))

# Sinal BOLD deste voxel ao longo do tempo
ts.plot(vol[35,43,23,]) # A gente vê oxigenação







