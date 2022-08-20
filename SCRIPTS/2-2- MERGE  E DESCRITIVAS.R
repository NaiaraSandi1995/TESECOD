
##Descritiva####

table(BancoLAPOPCompleto$pais)
table(BancoLAPOPCompleto$Denom)

#1     2     3     4 
#3642 15381  5041  3260 

BancoLAPOPCompleto$Denom <- as.factor(BancoLAPOPCompleto$Denom)

BancoLAPOPCompleto$Denom4 <- recode(BancoLAPOPCompleto$Denom, "Católico" <- 1, 
                                    "Protestante" <-2, "Outras" <- 3, "Ateu" <-4)
table(BancoLAPOPCompleto$Denom4)

##Ver religião em cada país
table(Arg2019$Denom)
#1   2   3   4 
#843 239  47 354 

table(Bol2019$Denom)
#1    2    3    4 
#1084  363   11  144 

table(Bra2019$Denom)
#1   2   3   4 
#746 463  25 178 

table(Chi2018$Denom)
#1   2   3   4 
#766 332   6 491 

table(Cos2018$Denom)
#1   2   3   4 
#864 409   2 145 

table(Els2018$Denom)
#1   2   3   4 
#679 293 311 201 

table(Equ2018$Denom)
#   1    2    3    4 
#1020   72  205  170 

table(Equ2018$Denom)
#1    2    3    4 
#1020   72  205  170 

table(Gua2018$Denom)
#  1   2   3   4 
#772  81 555  92 

table(Hai2016$Denom)
#1   2   3   4 
#910 580 365 169 

table(Ven2016$Denom)
#1   2   3   4 
#982 227 106  98 

table(Uru2018$Denom)
#1   2   3   4 
#477 176 349 550 

table(Rep2018$Denom)
#1   2   3   4 
#733 434  20 274 

table(Per2018$Denom)
#1    2    3    4 
#1066  198  135   90 

table(Par2018$Denom)
#    1    2    3    4 
#1218  175   40   63 

table(Pan2018$Denom)
#  1   2   3   4 
#861 495  45  84 

table(Nic2018$Denom)
#1   2   3   4 
#689 325 290 196 

table(Mex2018$Denom)
#   1    2    3    4 
#1145   79   67  162 

table(Hon2018$Denom)
#1   2   3   4 
#526 100 681 181 


#merge de novo

Banco1 <- merge(Arg2019, Bra2019, all= T)
Banco2 <- merge(Bol2019, Chi2018, all= T)
Banco3 <- merge(Cos2018, Els2018, all=T)
Banco4 <- merge(Gua2018, Hai2016, all=T)
Banco5 <- merge(Hon2018, Mex2018, all= T)
Banco6 <- merge(Nic2018, Pan2018, all = T)
Banco7 <- merge(Par2018, Equ2018, all= T)
Banco8 <- merge(Per2018, Rep2018, all= T)
Banco9 <- merge(Uru2018, Ven2016, all= T)


Banco10 <- merge(Banco1, Banco2, all = T)
Banco11 <- merge(Banco3, Banco4, all= T)
Banco12 <- merge(Banco5, Banco6, all=T)
Banco13 <- merge(Banco7, Banco8, all = T)

Banco14 <- merge(Banco10, Banco9, all = T)
Banco15 <- merge(Banco11, Banco12, all=T)
Banco16 <- merge(Banco13, Banco14, all=T)

BancoFinal <- merge(Banco15, Banco16, all=T)
BancoFinal <- merge(BancoFinal, DadosMacro, all = T )
table(BancoFinal$pais)

table(BancoFinal$pais)

save(BancoFinal, file= "BancoFinal.RData")
save(BancoFinal1, file = "BancoFinal1.RData")
table(BancoFinal$Denom)
#    1     2     3     4 
#15381  5041  3260  3642 


BancoFinal$DDenom <- recode(BancoFinal$Denom, "Católico" <- 1, 
                            "Protestante" <- 2, "Outras" <- 3, 
                            "Ateu" <- 4)
table(BancoFinal$DDenom)
#Católico Protestante      Outras        Ateu 
#  15381        5041        3260        3642 

table(BancoLAPOPCompleto$pais)
table(BancoFinal$IntRelig)
#0     1     2     3 
#2116  2194  4774 19270

table(BancoFinal$AtRelig)
#0    1    2    3    4 
#6177 4424 5046 6001 3599 

table(BancoFinal$TolerHomo)
BancoFinal$DTolerHomo <- recode(BancoFinal$TolerHomo, 
                                "Intolerante" <- c(1,2,3,4), 
                                "Neutro" <- c(5,6), 
                                "Tolerante" <- c(7,8,9,10))
table(BancoFinal$DTolerHomo) 
#Intolerante      Neutro   Tolerante 
#     12862        4481       10584


Obj1<- table(BancoFinal$DTolerHomo, BancoFinal$DDenom)

#            Católico Protestante Outras Ateu
#Intolerante     6385        2955   1837 1111
#Neutro          2707         668    398  531
#Tolerante       5955        1279    919 1939

Obj1 <- prop.table (Obj1, margin = 1)
Obj1 * 100

##Gráfico####
 
Religiões <- c(rep(c("Católico", "Protestante", "Outras", "Ateu")))  
TolHomo <- c(rep(c("Intolerante", "Neutro", "Tolerante")))
Frequência <-c( 6385,2955,1837,1111,2707, 668,398, 531,  5955, 1279, 919, 1939)
Data <- data.frame(Religiões, TolHomo, Frequência)

Gráfico <- ggplot(Data, aes(x = Religiões, y = Frequência,
                                  fill = TolHomo, label = Frequência)) +
  geom_bar(stat = "identity") +
  geom_text(size = 2, position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette = "RdGy", type = "seq")

Gráfico

Gráfico + theme_blank() + labs(x = "Denominações religiosas") +
  theme(legend.position = "bottom")

##Gráfico1 de barras simples####
Religiões <- c("Católico", "Protestante", "Outras", "Ateu")  
Frequência <- c(15381, 5041, 3260, 3642)  
Data2 <- data.frame(Religiões, Frequência)

Gráfico2 <-  ggplot(Data2, aes(y=Frequência, x= Religiões, 
                               labe = Frequência)) +
  geom_bar(stat = "identity", width = .75) + 
  scale_y_continuous(limits = c(0, 16000))+
  geom_text(aes(label = Frequência), vjust=1.6, color="white",size=3.5)+
  xlab("Denominações religiosas")
Gráfico2 + theme_blank()

##Gráfico2####
table(BancoFinal$DTolerHomo, BancoFinal$IntRelig)
#              0    1    2    3
#Intolerante  491  640 1683 9914
#Neutro       239  387  896 2932
#Tolerante   1357 1136 2115 5887


Intesidade <- c(rep(c(0, 1, 2, 3)))  
TolHomo <- c(rep(c("Intolerante", "Neutro", "Tolerante")))
Frequência <-c(491,640,1683,9914,239,387,896,2932,1357,1136,2115,5887)
Data3 <- data.frame(Intesidade, TolHomo, Frequência)

Gráfico3 <- ggplot(Data3, aes(x = Intesidade, y = Frequência,
                              fill = TolHomo, label = Frequência)) +
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "RdGy", type = "seq")

Gráfico3

#Para colocar a legenda
#geom_text(size = 2, position = position_stack(vjust = 0.5))
#Para colocar cor
library(RColorBrewer) # Tem um site 
#scale_fill_brewer(palette = 2, type = "seq")
#Para dados divergentes: type = "div"; 
#para dados em sequência: type = "seq"; para dados 
#categóricos em geral: type = "qual".

Gráfico3 + theme_blank() + labs(x = "Nível de Intensidade religiosa") +
  theme(legend.position = "bottom")

##Gráfico4
table(BancoFinal$DTolerHomo, BancoFinal$AtRelig)
#             0    1    2    3    4
#Intolerante 2277 1825 2309 3194 2188
#Neutro       994  805  852  893  465
#Tolerante   2780 1711 1788 1728  803


Ativismo <- c(rep(c(0, 1, 2, 3, 4)))  
TolHomo <- c(rep(c("Intolerante", "Neutro", "Tolerante")))
Frequência <-c(2277,1825,2309,3194,2188, 994, 805,852,893, 
               465,2780, 1711, 1788, 1728,  803)
Data4 <- data.frame(Ativismo, TolHomo, Frequência)

Gráfico4 <- ggplot(Data4, aes(x = Ativismo, y = Frequência,
                              fill = TolHomo, label = Frequência)) +
  geom_bar(stat = "identity") + 
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette = "RdGy", type = "seq")

Gráfico4

Gráfico4 + theme_blank() + labs(x = "Nível de Ativismo Religioso") +
  theme(legend.position = "bottom")



# Gráfico exemplo  ####
library(tidyr)
library(ggplot2)

Objc <- data.frame (Anos = c(2014,2016,	2018, "Médiaf."), 
                    Brasil=	c(6.37,	6.67,	6.89,	6.39),
                    Uruguai	=	c(8.03,8.1,7.65,7.52),
                    Guatemala =	c(2.65,	3.88,	4.07, 3.59),
                    El_Salvador =	c(3.85,	4.08,	4.4, 3.12)) 

gath

Objc.1 <- Objc %>% gather(Países, Médias_tolerância, -Anos)

Gra3 <- ggplot(data = Objc.1, aes(x= Anos, y= Médias_tolerância, 
                                  group = Países))+
  geom_line(aes(colour = Países), size = 1) + geom_point(aes(shape = Países))

Gra3

Gra3 + theme_bw()+ theme(legend.position = "bottom")


#Local: "bottom", "top", "left", ou "right" 
