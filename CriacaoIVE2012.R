#IVE####

library(memisc)
options(scipen = 999, digits = 4)

#Criação do Índice de Valores Emancipatórios #
#2012####

#Variáveis a serem utilizadas:
#Igualdade (VB50, VB52)
# VB50 - Valência positiva, escala de 1 a 4 
# VB52- Valência positiva, escala de 1 a 3

#Escolhas individuais (D6, w14a)
# D6 - Valência positiva, escala de 1 a 10
# w14a - Valência negativa, de 1 a 2, teremos que recodificar 

#Voz(D4)
#D4 - Valência positiva, escala de 1 a 10

#Países
# 1.Argentina 2012
# 2.Bolívia 2012
# 3.Brasil 2012
# 4.Chile 2012
# 5.Costa Rica 2012
# 6.Ecuador 2012
# 7.El Salvador 2012
# 8.Guatemala 2012
# 9.Haiti 2012
# 10.Honduras 2012
# 11.México 2012
# 12.Nicarágua 2012
# 13.Panamá 2012
# 14.Paraguai 2012
# 15.Rep. Dom. 2012
# 16.Perú 2012
# 17.Uruguay 2012
# 18.Venezuela 2012



#Criação do Índice###
# Banco$NovoNome <- (Banco$V1 + Banco$V2 + Banco$V3 + Banco$V4)
# Banco$NovoNome <- Banco$NovoNome/max(Banco$NovoNome, na.rm=T)
# Banco$NovoNome <- Banco$NovoNome*10


# 1.Argentina 2012####

#recod da w14a
table(Arg2012$w14a)
#  1   2 
#  450 233 

Arg2012$Aborto <- as.factor(Arg2012$w14a)


Arg2012$Aborto <- recode(Arg2012$Aborto, 2 <- 1, 1 <- 2)

table(Arg2012$Aborto)
# 1    2 
# 233 450

Arg2012$IVE <- (Arg2012$vb50 + Arg2012$vb52 + Arg2012$d6 +
                  Arg2012$Aborto + Arg2012$d4)

Arg2012$IVE <- Arg2012$IVE/max(Arg2012$IVE, na.rm = T)

Arg2012$IVE <- Arg2012$IVE*10


summary(Arg2012$IVE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#2.1     5.5     6.6     6.7     8.3    10.0     935 


save(Arg2019, file = "Arg2019.Rdata")

#2.Bolívia 2012####

#recod da w14a
table(Bol2012$w14a)
# 1      2 
# 820 573 

Bol2012$w14a <- as.factor(Bol2012$w14a)
Bol2012$Aborto <- recode(Bol2012$w14a, 2 <- 1, 1 <- 2)

table(Bol2012$Aborto)
# 1    2 
# 573 820

Bol2012$IVE <- (Bol2012$vb50 + Bol2012$vb52 + Bol2012$d6 +
                  Bol2012$Aborto + Bol2012$d4)

Bol2012$IVE <- Bol2012$IVE/max(Bol2012$IVE, na.rm = T)

Bol2012$IVE <- Bol2012$IVE*10

summary(Bol2012$IVE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.8     4.3     5.4     5.4     6.4    10.0    1921


save(Bol2012, file = "Bol2012.Rdata")


#3.Brasil 2012####

#recod da w14a
table(Bra2012$w14a)
# 1      2 
# 963 462 

Bra2012$w14a <- as.factor(Bra2012$w14a)
Bra2012$Aborto <- recode(Bra2012$w14a, 2 <- 1, 1 <- 2)

table(Bra2012$Aborto)
# 1    2 
# 462 963 

Bra2012$IVE <- (Bra2012$vb50 + Bra2012$vb52 + Bra2012$d6 +
                  Bra2012$Aborto + Bra2012$d4)
Bra2012$IVE <- Bra2012$IVE/max(Bra2012$IVE, na.rm = T)
Bra2012$IVE <- Bra2012$IVE*10

summary(Bra2012$IVE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 2.4     5.2     6.6     6.5     7.9    10.0     847 

save(Bra2012, file = "Bra2012.Rdata")

# 4.Chile 2012####

#recod da w14a
table(Chi2012$w14a)
# 1      2 
# 371 334  


Chi2012$w14a <- as.factor(Chi2012$w14a)
Chi2012$Aborto <- recode(Chi2012$w14a, 2 <- 1, 1 <- 2)

table(Chi2012$Aborto)
# 1    2 
# 334 371

Chi2012$IVE <- (Chi2012$vb50 + Chi2012$vb52 + Chi2012$d6 +
                  Chi2012$Aborto + Chi2012$d4)
Chi2012$IVE <- Chi2012$IVE/max(Chi2012$IVE, na.rm = T)
Chi2012$IVE <- Chi2012$IVE*10

summary(Chi2012$IVE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 2.1     4.8     5.9     6.0     6.9    10.0     985

save(Chi2012, file = "Chi2012.Rdata")

# 5.Costa Rica 2012####

#recod da w14a
table(Cos2012$w14a)
# 1      2 
# 381 302 

Cos2012$w14a <- as.factor(Cos2012$w14a)
Cos2012$Aborto <- recode(Cos2012$w14a, 2 <- 1, 1 <- 2)

table(Cos2012$Aborto)
# 1    2 
# 302 381 

Cos2012$IVE <- (Cos2012$vb50 + Cos2012$vb52 + Cos2012$d6 +
                  Cos2012$Aborto + Cos2012$d4)
Cos2012$IVE <- Cos2012$IVE/max(Cos2012$IVE, na.rm = T)
Cos2012$IVE <- Cos2012$IVE*10

summary(Cos2012$IVE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 2.1     3.8     5.2     5.4     6.6    10.0     895

save(Cos2012, file = "Cos2012.Rdata")

# 6.Ecuador 2012####

#recod da w14a
table(Equ2012$w14a)
# 1      2 
# 341 347 


Equ2012$w14a <- as.factor(Equ2012$w14a)
Equ2012$Aborto <- recode(Equ2012$w14a, 2 <- 1, 1 <- 2)

table(Equ2012$Aborto)
# 1    2 
# 347 341  

Equ2012$IVE <- (Equ2012$vb50 + Equ2012$vb52 + Equ2012$d6 +
                  Equ2012$Aborto + Equ2012$d4)
Equ2012$IVE <- Equ2012$IVE/max(Equ2012$IVE, na.rm = T)
Equ2012$IVE <- Equ2012$IVE*10

summary(Equ2012$IVE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  2.1     3.8     4.8     5.1     6.2    10.0     949 

save(Equ2018, file = "Equ2018.Rdata")

# 7.El Salvador 2012####

#recod da w14a
table(Els2012$w14a)
# 1      2 
# 359 333

Els2012$w14a <- as.factor(Els2012$w14a)
Els2012$Aborto <- recode(Els2012$w14a, 2 <- 1, 1 <- 2)

table(Els2012$Aborto)
# 1    2 
# 333 359   

Els2012$IVE <- (Els2012$vb50 + Els2012$vb52 + Els2012$d6 +
                  Els2012$Aborto + Els2012$d4)
Els2012$IVE <- Els2012$IVE/max(Els2012$IVE, na.rm = T)
Els2012$IVE <- Els2012$IVE*10

summary(Els2012$IVE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   2.1     3.8     4.5     4.7     5.5    10.0     909  

save(Els2018, file = "Els2018.Rdata")

# 8.Guatemala 2012####

#recod da w14a
table(Gua2012$w14a)
# 1      2 
# 316 377 

Gua2012$w14a <- as.factor(Gua2012$w14a)
Gua2012$Aborto <- recode(Gua2012$w14a, 2 <- 1, 1 <- 2)

table(Gua2012$Aborto)
# 1    2 
#377 316

Gua2012$IVE <- (Gua2012$vb50 + Gua2012$vb52 + Gua2012$d6 +
                  Gua2012$Aborto + Gua2012$d4)
Gua2012$IVE <- Gua2012$IVE/max(Gua2012$IVE, na.rm = T)
Gua2012$IVE <- Gua2012$IVE*10

summary(Gua2012$IVE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.9     4.6     5.8     5.6     6.5    10.0     955

save(Gua2018, file = "Gua2018.Rdata")

# 9.Haiti 2012####

#recod da w14a
table(Hai2012$w14a)
# 1      2 
# 497 362  

Hai2012$w14a <- as.factor(Hai2012$w14a)
Hai2012$Aborto <- recode(Hai2012$w14a, 2 <- 1, 1 <- 2)

table(Hai2012$Aborto)
# 1    2 
# 362 497 

Hai2012$Aborto <- as.numeric(Hai2012$Aborto)
Hai2012$IVE <- (Hai2012$vb50 + Hai2012$vb52 + Hai2012$d6 +
                  Hai2012$Aborto + Hai2012$d4)
Hai2012$IVE <- Hai2012$IVE/max(Hai2012$IVE, na.rm = T)
Hai2012$IVE <- Hai2012$IVE*10

summary(Hai2012$IVE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 2.5     4.6     5.4     5.3     5.8    10.0    1080 

save(Hai2012, file = "Hai2012.Rdata")

# 10.Honduras 2012####

#recod da w14a
table(Hon2012$w14a)
# 1      2 
# 261 538 

Hon2012$w14a <- as.factor(Hon2012$w14a)
Hon2012$Aborto <- recode(Hon2012$w14a, 2 <- 1, 1 <- 2)

table(Hon2012$Aborto)
# 1    2 
# 538 261

Hon2012$IVE <- (Hon2012$vb50 + Hon2012$vb52 + Hon2012$d6 +
                  Hon2012$Aborto + Hon2012$d4)
Hon2012$IVE <- Hon2012$IVE/max(Hon2012$IVE, na.rm = T)
Hon2012$IVE <- Hon2012$IVE*10

summary(Hon2012$IVE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  1.8     3.6     4.3     4.7     5.4    10.0    1072 

save(Hon2018, file = "Hon2018.Rdata")

# 11.México 2012####
#recod da w14a
table(Mex2012$w14a)
# 1      2 
# 373 354 

Mex2012$w14a <- as.factor(Mex2012$w14a)
Mex2012$Aborto <- recode(Mex2012$w14a, 2 <- 1, 1 <- 2)

table(Mex2012$Aborto)
# 1    2 
# 354 373  

Mex2012$IVE <- (Mex2012$vb50 + Mex2012$vb52 + Mex2012$d6 +
                  Mex2012$Aborto + Mex2012$d4)
Mex2012$IVE <- Mex2012$IVE/max(Mex2012$IVE, na.rm = T)
Mex2012$IVE <- Mex2012$IVE*10

summary(Mex2012$IVE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  2.1     4.8     6.2     5.9     7.2    10.0     932 

save(Mex2018, file = "Mex2018.Rdata")

# 12.Nicarágua 2012####

#recod da w14a
table(Nic2012$w14a)
# 1      2 
# 365 445 

Nic2012$w14a <- as.factor(Nic2012$w14a)
Nic2012$Aborto <- recode(Nic2012$w14a, 2 <- 1, 1 <- 2)

table(Nic2012$Aborto)
# 1    2 
# 445 365 

Nic2012$Aborto <- as.numeric(Nic2012$Aborto)
Nic2012$IVE <- (Nic2012$vb50 + Nic2012$vb52 + Nic2012$d6 +
                  Nic2012$Aborto + Nic2012$d4)
Nic2012$IVE <- Nic2012$IVE/max(Nic2012$IVE, na.rm = T)
Nic2012$IVE <- Nic2012$IVE*10

summary(Nic2012$IVE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.7     4.1     5.5     5.3     6.2    10.0     918 

save(Nic2012, file = "Nic2012.Rdata")

# 13.Panamá 2012####

#recod da w14a
table(Pan2012$w14a)
# 1      2 
# 294 460 

Pan2012$w14a <- as.factor(Pan2012$w14a)
Pan2012$Aborto <- recode(Pan2012$w14a, 2 <- 1, 1 <- 2)

table(Pan2012$Aborto)
# 1    2 
#460 294 

Pan2012$IVE <- (Pan2012$vb50 + Pan2012$vb52 + Pan2012$d6 +
                  Pan2012$Aborto + Pan2012$d4)
Pan2012$IVE <- Pan2012$IVE/max(Pan2012$IVE, na.rm = T)
Pan2012$IVE <- Pan2012$IVE*10

summary(Pan2012$IVE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 2.1     4.3     5.7     5.6     6.8    10.0    1008 

save(Pan2012, file = "Pan2012.Rdata")

# 14.Paraguai 2012####

#recod da w14a
table(Par2012$w14a)
# 1      2 
# 340 349 

Par2012$w14a <- as.factor(Par2012$w14a)

Par2012$Aborto <- recode(Par2012$w14a, 2 <- 1, 1 <- 2)

table(Par2012$Aborto)
# 1    2 
# 349 340  

Par2012$Aborto <- as.numeric(Par2012$Aborto)

Par2012$IVE <- (Par2012$vb50 + Par2012$vb52 + Par2012$d6 +
                  Par2012$Aborto + Par2012$d4)
Par2012$IVE <- Par2012$IVE/max(Par2012$IVE, na.rm = T)
Par2012$IVE <- Par2012$IVE*10

summary(Par2012$IVE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.7     3.8     5.2     5.2     6.2    10.0     922 

save(Par2018, file = "Par2018.Rdata")

# 15.Rep. Dom. 2012####

#recod da w14a
table(Rep2012$w14a)
# 1      2 
# 903 572 

Rep2012$w14a <- as.factor(Rep2012$w14a)
Rep2012$Aborto <- recode(Rep2012$w14a, 2 <- 1, 1 <- 2)

table(Rep2012$Aborto)
# 1    2 
# 572 903

Rep2012$IVE <- (Rep2012$vb50 + Rep2012$vb52 + Rep2012$d6 +
                  Rep2012$Aborto + Rep2012$d4)
Rep2012$IVE <- Rep2012$IVE/max(Rep2012$IVE, na.rm = T)
Rep2012$IVE <- Rep2012$IVE*10

summary(Rep2012$IVE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  1.7     3.4     4.8     5.0     6.2    10.0     847  

save(Rep2012, file = "Rep2012.Rdata")


# 16.Perú 2012####
#recod da w14a
table(Per2012$w14a)
# 1      2 
# 462 238 

Per2012$w14a <- as.factor(Per2012$w14a)
Per2012$Aborto <- recode(Per2012$w14a, 2 <- 1, 1 <- 2)

table(Per2012$Aborto)
# 1    2 
# 238 462   

Per2012$IVE <- (Per2012$vb50 + Per2012$vb52 + Per2012$d6 +
                  Per2012$Aborto + Per2012$d4)
Per2012$IVE <- Per2012$IVE/max(Per2012$IVE, na.rm = T)
Per2012$IVE <- Per2012$IVE*10

summary(Per2012$IVE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.8     4.3     5.0     5.3     6.1    10.0     923 

save(Per2018, file = "Per2018.Rdata")

# 17.Uruguay 2012####

#recod da w14a
table(Uru2012$w14a)
# 1      2 
# 590 130

Uru2012$w14a <- as.factor(Uru2012$w14a)
Uru2012$Aborto <- recode(Uru2012$w14a, 2 <- 1, 1 <- 2)

table(Uru2012$Aborto)
# 1    2 
# 130 590   

Uru2012$IVE <- (Uru2012$vb50 + Uru2012$vb52 + Uru2012$d6 +
                  Uru2012$Aborto + Uru2012$d4)
Uru2012$IVE <- Uru2012$IVE/max(Uru2012$IVE, na.rm = T)
Uru2012$IVE <- Uru2012$IVE*10

summary(Uru2012$IVE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  2.4     5.9     7.2     7.2     9.3    10.0     865 

save(Uru2012, file = "Uru2012.Rdata")


# 18.Venezuela 2012####

#recod da w14a
table(Ven2012$w14a)
# 1      2 
# 353 321   

Ven2012$w14a <- as.factor(Ven2012$w14a)
Ven2012$Aborto <- recode(Ven2012$w14a, 2 <- 1, 1 <- 2)

table(Ven2012$Aborto)
# 1    2 
# 321 353  

Ven2012$Aborto <- as.numeric(Ven2012$Aborto)
Ven2012$IVE <- (Ven2012$vb50 + Ven2012$vb52 + Ven2012$d6 +
                  Ven2012$Aborto + Ven2012$d4)
Ven2012$IVE <- Ven2012$IVE/max(Ven2012$IVE, na.rm = T)
Ven2012$IVE <- Ven2012$IVE*10

summary(Ven2012$IVE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.7     4.5     5.2     5.4     6.2    10.0    1008 

save(Ven2012, file = "Ven2012.Rdata")


H