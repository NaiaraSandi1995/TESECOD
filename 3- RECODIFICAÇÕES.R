################Pacotes##############
library(haven)
library(memisc)
library(sjlabelled)
library(labelled)
library(plyr)
library(tigerstats)

###Argentina
##2008
Arg2008 <- read_dta("Arg2008.dta")
Arg2008 <- remove_all_labels(Arg2008)
#Tolerância
Arg2008$TolerHomo <- Arg2008$d5
#Denominação Religiosa (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Arg2008$Denom <-  recode(Arg2008$q3, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Arg2008$Denom <- as.factor(Arg2008$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por 
#semana e 4=mais de 1 vez por semana)
Arg2008$AtRelig <- recode(Arg2008$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa ()
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Arg2008, mean)
aggregate(TolerHomo ~ AtRelig, Arg2008, mean)
summary(Arg2008$TolerHomo)

##2010
Arg2010 <- read_dta("Arg2010.dta")
Arg2010 <- remove_all_labels(Arg2010)
#Tolerância
Arg2010$TolerHomo <- Arg2010$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Arg2010$Denom <-  recode(Arg2010$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,12), 4 <- c(4,11))
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Arg2010$AtRelig <- recode(Arg2010$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Arg2010$IntRelig <- recode(Arg2010$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Arg2010, mean)
aggregate(TolerHomo ~ AtRelig, Arg2010, mean)
summary(Arg2010$TolerHomo)

Arg2019$p
##2012
Arg2012 <- read_dta("Arg2012.dta")
Arg2012 <- remove_all_labels(Arg2012)
#Tolerância
Arg2012$TolerHomo <- Arg2012$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Arg2012$Denom <-  recode(Arg2012$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,12), 4 <- c(4,11))
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Arg2012$AtRelig <- recode(Arg2012$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Arg2012$IntRelig <- recode(Arg2012$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Arg2012, mean)
aggregate(TolerHomo ~ AtRelig, Arg2012, mean)
summary(Arg2012$TolerHomo)


##2014
Arg2014 <- read_dta("Arg2014.dta")
Arg2014 <- remove_all_labels(Arg2014)
#Tolerância
Arg2014$TolerHomo <- Arg2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Arg2014$Denom <-  recode(Arg2014$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,12), 4 <- c(4,11))
#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Arg2014$IntRelig <- recode(Arg2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Arg2014, mean)
aggregate(TolerHomo ~ AtRelig, Arg2014, mean)
summary(Arg2014$TolerHomo)


##2017
Arg2017 <- read_dta("Arg2017.dta")
Arg2017 <- remove_all_labels(Arg2017)
#Tolerância
Arg2017$TolerHomo <- Arg2017$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Arg2017$Denom <-  recode(Arg2017$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,12,77), 4 <- c(4,11))
#Ativismo Religioso (NA)
Arg2017$AtRelig <- recode(Arg2017$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Arg2017$IntRelig <- recode(Arg2017$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Arg2017, mean)
aggregate(TolerHomo ~ AtRelig, Arg2017, mean)
summary(Arg2017$TolerHomo)

##2019
Arg2019 <- read_dta("Arg2019.dta")
Arg2019 <- remove_all_labels(Arg2019)
#Tolerância
Arg2019$TolerHomo <- Arg2019$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Arg2019$Denom <-  recode(Arg2019$q3cn, 1 <- 1, 2 <- c(2,5), 3 <- c(3,7,77), 4 <- c(4,11))
#Ativismo Religioso 
Arg2019$AtRelig <- recode(Arg2019$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Arg2019$IntRelig <- recode(Arg2019$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Arg2019, mean)
aggregate(TolerHomo ~ AtRelig, Arg2019, mean)
summary(Arg2019$TolerHomo)


##########Condicionantes

#Adesão a democracia (churchiliana)
val_labels(Arg2019$ing4) <- NULL#retira rótulos dos valores para uso do recode
Arg2019$Dem <- Arg2019$ing4


#Confiança
Arg2019$conf <- as.numeric(Arg2019$it1)
val_labels(Arg2019$conf) <- NULL#retira rótulos dos valores para uso do recode
Arg2019$ConfInt <- recode(Arg2019$conf, 0 <- c(4), 1 <- c(3), 2 <- c(2), 3 <- c(1))


#Escolaridade
Arg2019$Ed_sup <- cut(Arg2019$ed, c(0,15,19))
Arg2019$Ed_sup <- as.numeric(Arg2019$Ed_sup)


#Idade
val_labels(Arg2019$q2) <- NULL#retira rótulos dos valores para uso do recode
Arg2019$Idade <- Arg2019$q2

#Sexo
val_labels(Arg2019$q1) <- NULL#retira rótulos dos valores para uso do recode
Arg2019$Sexo <- recode(Arg2019$q1, 0 <- c(1), 1 <- c(2))

val_labels(Arg2019$it1) <- NULL
Arg2019$it1 <- as.numeric(Arg2019$it1)
Arg2019$Conf_int <- recode(Arg2019$it1, 0 <- 4, 1 <- 3, 2 <-2, 3 <- 1)

save(Arg2019, file = "Arg2019.RData")


###Bolívia
##2004
Bol2004 <- read_dta("Bol2004.dta")
Bol2004 <- remove_all_labels(Bol2004)
#Tolerância
Bol2004$TolerHomo <- Bol2004$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Bol2004$Denom <-  recode(Bol2004$q3,  1 <- c(1,2), 2 <- c(3,5), 3 <- c(4,7,9,10,11,12,13), 4 <- 6)
Bol2004$Denom <- as.factor(Bol2004$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Bol2004$AtRelig <- recode(Bol2004$q4, 0 <- c(1,2), 1 <- c(3,4) , 2 <- c(5,6), 3 <- c(7,8), 4 <- 9)
#Intensidade Religiosa ()
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Bol2004, mean)
aggregate(TolerHomo ~ AtRelig, Bol2004, mean)
summary(Bol2004$TolerHomo)

###Bolívia
##2006
Bol2006 <- read_dta("Bol2006.dta")
Bol2006 <- remove_all_labels(Bol2006)
#Tolerância
Bol2006$TolerHomo <- Bol2006$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Bol2006$Denom <-  recode(Bol2006$q3, 1 <- c(1,6), 2 <- c(3,5,9,17), 3 <- c(2,7,8,10,11,12,13,14,15,16,18,19,21,22), 4 <- 4)
Bol2006$Denom <- as.factor(Bol2006$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Bol2006$AtRelig <- recode(Bol2006$q4, 0 <- c(1,2,3,4,5,6,7), 1 <- c(8,9,10,11,12,13,14) , 2 <- c(15,16,17,18,19,20,21), 3 <- c(22,23,24,25,26,27), 4 <- c(28,29,30,31))
#Intensidade Religiosa ()
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Bol2006, mean)
aggregate(TolerHomo ~ AtRelig, Bol2006, mean)
summary(Bol2006$TolerHomo)


###Bolívia
##2008
Bol2008 <- read_dta("Bol2008.dta")
Bol2008 <- remove_all_labels(Bol2008)
#Tolerância
Bol2008$TolerHomo <- Bol2008$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Bol2008$Denom <-  recode(Bol2008$q3, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Bol2008$Denom <- as.factor(Bol2008$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Bol2008$AtRelig <- recode(Bol2008$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa ()
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Bol2008, mean)
aggregate(TolerHomo ~ AtRelig, Bol2008, mean) 
summary(Bol2008$TolerHomo)

###Bolívia
##2010
Bol2010 <- read_dta("Bol2010.dta")
Bol2010 <- remove_all_labels(Bol2010)
#Tolerância
Bol2010$TolerHomo <- Bol2010$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Bol2010$Denom <-  recode(Bol2010$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Bol2010$Denom <- as.factor(Bol2010$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Bol2010$AtRelig <- recode(Bol2010$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Bol2010$IntRelig <- recode(Bol2010$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Bol2010, mean)
aggregate(TolerHomo ~ AtRelig, Bol2010, mean)
summary(Bol2010$TolerHomo)

###Bolívia
##2012
Bol2012 <- read_dta("Bol2012.dta")
Bol2012 <- remove_all_labels(Bol2012)
#Tolerância
Bol2012$TolerHomo <- Bol2012$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Bol2012$Denom <-  recode(Bol2012$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Bol2012$Denom <- as.factor(Bol2012$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Bol2012$AtRelig <- recode(Bol2012$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Bol2012$IntRelig <- recode(Bol2012$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Bol2012, mean)
aggregate(TolerHomo ~ AtRelig, Bol2012, mean)
summary(Bol2012$TolerHomo)

###Bolívia
##2014
Bol2014 <- read_dta("Bol2014.dta")
Bol2014 <- remove_all_labels(Bol2014)
#Tolerância
Bol2014$TolerHomo <- Bol2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Bol2014$Denom <-  recode(Bol2014$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Bol2014$Denom <- as.factor(Bol2014$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Bol2014$IntRelig <- recode(Bol2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Bol2014, mean)
aggregate(TolerHomo ~ AtRelig, Bol2014, mean)
summary(Bol2014$TolerHomo)

###Bolívia
##2017
Bol2017 <- read_dta("Bol2017.dta")
Bol2017 <- remove_all_labels(Bol2017)
#Tolerância
Bol2017$TolerHomo <- Bol2017$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Bol2017$Denom <-  recode(Bol2017$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Bol2017$Denom <- as.factor(Bol2017$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Bol2017$AtRelig <- recode(Bol2017$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Bol2017$IntRelig <- recode(Bol2017$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Bol2017, mean)
aggregate(TolerHomo ~ AtRelig, Bol2017, mean)
summary(Bol2017$TolerHomo)

###Bolívia
##2019
Bol2019 <- read_dta("Bol2019.dta")
Bol2019 <- remove_all_labels(Bol2019)
#Tolerância
Bol2019$TolerHomo <- Bol2019$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Bol2019$Denom <-  recode(Bol2019$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Bol2019$Denom <- as.factor(Bol2019$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Bol2019$AtRelig <- recode(Bol2019$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Bol2019$IntRelig <- recode(Bol2019$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Bol2019, mean)
aggregate(TolerHomo ~ AtRelig, Bol2019, mean)
summary(Bol2019$TolerHomo)

##########Condicionantes

#Adesão a democracia (churchiliana)
val_labels(Bol2019$ing4) <- NULL#retira rótulos dos valores para uso do recode
Bol2019$Dem <- Bol2019$ing4


#Confiança
Bol2019$conf <- as.numeric(Bol2019$it1)
val_labels(Bol2019$conf) <- NULL#retira rótulos dos valores para uso do recode
Bol2019$ConfInt <- recode(Bol2019$conf, 0 <- c(4), 1 <- c(3), 2 <- c(2), 3 <- c(1))


#Escolaridade
Bol2019$Ed_sup <- cut(Bol2019$ed, c(0,15,19))
Bol2019$Ed_sup <- as.numeric(Bol2019$Ed_sup)

#Idade
val_labels(Bol2019$q2) <- NULL#retira rótulos dos valores para uso do recode
Bol2019$Idade <- Bol2019$q2

#Sexo
val_labels(Bol2019$q1) <- NULL#retira rótulos dos valores para uso do recode
Bol2019$Sexo <- recode(Bol2019$q1, 0 <- c(1), 1 <- c(2))

val_labels(Bol2019$it1) <- NULL
Bol2019$it1 <- as.numeric(Bol2019$it1)
Bol2019$Conf_int <- recode(Bol2019$it1, 0 <- 4, 1 <- 3, 2 <-2, 3 <- 1)

save(Bol2019, file = "Bol2019.RData")

###Brasil
##2007
Bra2007 <- read_dta("Bra2007.dta")
Bra2007 <- remove_all_labels(Bra2007)
#Tolerância
Bra2007$TolerHomo <- Bra2007$D5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Bra2007$Denom <-  recode(Bra2007$VS18, 1 <- 8, 2 <- c(2,3), 3 <- c(1,4,5,6,7,9,10,11), 4 <- c(12,13))
Bra2007$Denom <- as.factor(Bra2007$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Bra2007$AtRelig <- recode(Bra2007$VS19, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Bra2007, mean)
aggregate(TolerHomo ~ AtRelig, Bra2007, mean)
summary(Bra2007$TolerHomo)

###Brasil
##2008
Bra2008 <- read_dta("Bra2008.dta")
Bra2008 <- remove_all_labels(Bra2008)
#Tolerância
Bra2008$TolerHomo <- Bra2008$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Bra2008$Denom <-  recode(Bra2008$q3, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11), 4 <- c(4,13))
Bra2008$Denom <- as.factor(Bra2008$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Bra2008$AtRelig <- recode(Bra2008$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Bra2008, mean)
aggregate(TolerHomo ~ AtRelig, Bra2008, mean)
summary(Bra2008$TolerHomo)

###Brasil
##2010
Bra2010 <- read_dta("Bra2010.dta")
Bra2010 <- remove_all_labels(Bra2010)
#Tolerância
Bra2010$TolerHomo <- Bra2010$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Bra2010$Denom <-  recode(Bra2010$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,8,10,12), 4 <- c(4,11))
Bra2010$Denom <- as.factor(Bra2010$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Bra2010$AtRelig <- recode(Bra2010$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Bra2010$IntRelig <- recode(Bra2010$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Bra2010, mean)
aggregate(TolerHomo ~ AtRelig, Bra2010, mean)
summary(Bra2010$TolerHomo)

###Brasil
##2012
Bra2012 <- read_dta("Bra2010.dta")
Bra2012 <- remove_all_labels(Bra2010)
#Tolerância
Bra2012$TolerHomo <- Bra2012$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Bra2012$Denom <-  recode(Bra2012$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,8,10,12), 4 <- c(4,11))
Bra2012$Denom <- as.factor(Bra2012$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Bra2012$AtRelig <- recode(Bra2012$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Bra2012$IntRelig <- recode(Bra2012$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Bra2012, mean)
aggregate(TolerHomo ~ AtRelig, Bra2012, mean)
summary(Bra2012$TolerHomo)

###Brasil
##2014
Bra2014 <- read_dta("Bra2014.dta")
Bra2014 <- remove_all_labels(Bra2014)
#Tolerância
Bra2014$TolerHomo <- Bra2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Bra2014$Denom <-  recode(Bra2014$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,8,10,12), 4 <- c(4,11))
Bra2014$Denom <- as.factor(Bra2014$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Bra2014$IntRelig <- recode(Bra2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Bra2014, mean)
aggregate(TolerHomo ~ AtRelig, Bra2014, mean)
summary(Bra2014$TolerHomo)

###Brasil
##2017
Bra2017 <- read_dta("Bra2017.dta")
Bra2017 <- remove_all_labels(Bra2017)
#Tolerância
Bra2017$TolerHomo <- Bra2017$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Bra2017$Denom <-  recode(Bra2017$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,8,10,12), 4 <- c(4,11))
Bra2017$Denom <- as.factor(Bra2017$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Bra2017$AtRelig <- recode(Bra2017$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Bra2017$IntRelig <- recode(Bra2017$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Bra2017, mean)
aggregate(TolerHomo ~ AtRelig, Bra2017, mean)
summary(Bra2017$TolerHomo)

###Brasil
##2019
Bra2019 <- read_dta("Bra2019.dta")
Bra2019 <- remove_all_labels(Bra2019)
#Tolerância
Bra2019$TolerHomo <- Bra2019$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Bra2019$Denom <-  recode(Bra2019$q3cn, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,8,10,12), 4 <- c(4,11))
Bra2019$Denom <- as.factor(Bra2019$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Bra2019$AtRelig <- recode(Bra2019$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Bra2019$IntRelig <- recode(Bra2019$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Bra2019, mean)
aggregate(TolerHomo ~ AtRelig, Bra2019, mean)
summary(Bra2019$TolerHomo)

##########Condicionantes

#Adesão a democracia (churchiliana)
val_labels(Bra2019$ing4) <- NULL#retira rótulos dos valores para uso do recode
Bra2019$Dem <- Bra2019$ing4


#Confiança
Bra2019$conf <- as.numeric(Bra2019$it1)
val_labels(Bra2019$conf) <- NULL#retira rótulos dos valores para uso do recode
Bra2019$ConfInt <- recode(Bra2019$conf, 0 <- c(4), 1 <- c(3), 2 <- c(2), 3 <- c(1))


#Escolaridade
Bra2019$Ed_sup <- cut(Bra2019$ed, c(0,15,19))
Bra2019$Ed_sup <- as.numeric(Bra2019$Ed_sup)


#Idade
val_labels(Bra2019$q2) <- NULL#retira rótulos dos valores para uso do recode
Bra2019$Idade <- Bra2019$q2

#Sexo
val_labels(Bra2019$q1) <- NULL#retira rótulos dos valores para uso do recode
Bra2019$Sexo <- recode(Bra2019$q1, 0 <- c(1), 1 <- c(2))

val_labels(Bra2019$it1) <- NULL
Bra2019$it1 <- as.numeric(Bra2019$it1)
Bra2019$Conf_int <- recode(Bra2019$it1, 0 <- 4, 1 <- 3, 2 <-2, 3 <- 1)

save(Bra2019, file = "Bra2019.RData")

###Chile
##2006
Chi2006 <- read_dta("Chi2006.dta")
Chi2006 <- remove_all_labels(Chi2006)
#Tolerância
Chi2006$TolerHomo <- Chi2006$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Chi2006$Denom <-  recode(Chi2006$q3, 1 <- 1, 2 <- 5, 3 <- c(2,3,6), 4 <- 4)
Chi2006$Denom <- as.factor(Chi2006$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (NA)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Chi2006, mean)
aggregate(TolerHomo ~ AtRelig, Chi2006, mean)#(NA)
summary(Chi2006$TolerHomo)

###Chile
##2008
Chi2008 <- read_dta("Chi2008.dta")
Chi2008 <- remove_all_labels(Chi2008)
#Tolerância
Chi2008$TolerHomo <- Chi2008$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Chi2008$Denom <- recode(Chi2008$q3, 1<- 1, 2<- c(2,5), 3<- c(3,6,7), 4<- 4)
Chi2008$Denom <- as.factor(Chi2008$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Chi2008$AtRelig <- recode(Chi2008$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Chi2008, mean)
aggregate(TolerHomo ~ AtRelig, Chi2008, mean)
summary(Chi2008$TolerHomo)

###Chile
##2010
Chi2010 <- read_dta("Chi2010.dta")
Chi2010 <- remove_all_labels(Chi2010)
#Tolerância
Chi2010$TolerHomo <- Chi2010$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Chi2010$Denom <-  recode(Chi2010$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,8,10,12), 4 <- c(4,11))
Chi2010$Denom <- as.factor(Chi2010$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Chi2010$AtRelig <- recode(Chi2010$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Chi2010, mean)
aggregate(TolerHomo ~ AtRelig, Chi2010, mean)
summary(Chi2010$TolerHomo)

###Chile
##2012
Chi2012 <- read_dta("Chi2012.dta")
Chi2012 <- remove_all_labels(Chi2012)
#Tolerância
Chi2012$TolerHomo <- Chi2012$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Chi2012$Denom <-  recode(Chi2012$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,8,10,12), 4 <- c(4,11))
Chi2012$Denom <- as.factor(Chi2012$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Chi2012$AtRelig <- recode(Chi2012$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Chi2012$IntRelig <- recode(Chi2012$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Chi2012, mean)
aggregate(TolerHomo ~ AtRelig, Chi2012, mean)
summary(Chi2012$TolerHomo)

###Chile
##2014
Chi2014 <- read_dta("Chi2014.dta")
Chi2014 <- remove_all_labels(Chi2014)
#Tolerância
Chi2014$TolerHomo <- Chi2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Chi2014$Denom <-  recode(Chi2014$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,8,10,12), 4 <- c(4,11))
Chi2014$Denom <- as.factor(Chi2014$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Chi2014$IntRelig <- recode(Chi2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Chi2014, mean)
aggregate(TolerHomo ~ AtRelig, Chi2014, mean)
summary(Chi2014$TolerHomo)

###Chile
##2016
Chi2016 <- read_dta("Chi2016.dta")
Chi2016 <- remove_all_labels(Chi2016)
#Tolerância
Chi2016$TolerHomo <- Chi2016$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Chi2016$Denom <-  recode(Chi2016$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,8,10,12), 4 <- c(4,11))
Chi2016$Denom <- as.factor(Chi2016$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Chi2016$AtRelig <- recode(Chi2016$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Chi2016$IntRelig <- recode(Chi2016$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Chi2016, mean)
aggregate(TolerHomo ~ AtRelig, Chi2016, mean)
summary(Chi2016$TolerHomo)

###Chile
##2018
Chi2018 <- read_dta("Chi2018.dta")
Chi2018 <- remove_all_labels(Chi2018)
#Tolerância
Chi2018$TolerHomo <- Chi2018$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Chi2018$Denom <-  recode(Chi2018$q3cn, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,8,10,12), 4 <- c(4,11))
Chi2018$Denom <- as.factor(Chi2018$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Chi2018$AtRelig <- recode(Chi2018$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Chi2018$IntRelig <- recode(Chi2018$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Chi2018, mean)
aggregate(TolerHomo ~ AtRelig, Chi2018, mean)
summary(Chi2018$TolerHomo)

##########Condicionantes

#Adesão a democracia (churchiliana)
val_labels(Chi2018$ing4) <- NULL#retira rótulos dos valores para uso do recode
Chi2018$Dem <- Chi2018$ing4

#Confiança
Chi2018$conf <- as.numeric(Chi2018$it1)
val_labels(Chi2018$conf) <- NULL#retira rótulos dos valores para uso do recode
Chi2018$ConfInt <- recode(Chi2018$conf, 0 <- c(4), 1 <- c(3), 2 <- c(2), 3 <- c(1))


#Escolaridade
Chi2018$Ed_sup <- cut(Chi2018$ed, c(0,15,19))
Chi2018$Ed_sup <- as.numeric(Chi2018$Ed_sup)

#Idade
val_labels(Chi2018$q2) <- NULL#retira rótulos dos valores para uso do recode
Chi2018$Idade <- Chi2018$q2

#Sexo
val_labels(Chi2018$q1) <- NULL#retira rótulos dos valores para uso do recode
Chi2018$Sexo <- recode(Chi2018$q1, 0 <- c(1), 1 <- c(2))

val_labels(Chi2018$it1) <- NULL
Chi2018$it1 <- as.numeric(Chi2018$it1)
Chi2018$Conf_int <- recode(Chi2018$it1, 0 <- 4, 1 <- 3, 2 <-2, 3 <- 1)

save(Chi2018, file = "Chi2018.RData")

###Colômbia
##2005
Col2005 <- read_dta("Col2005.dta")
Col2005 <- remove_all_labels(Col2005)
#Tolerância
Col2005$TolerHomo <- Col2005$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Col2005$Denom <-  recode(Col2005$q3, 1 <- 1, 2 <- 2, 3 <- 3, 4 <- 4)
Col2005$Denom <- as.factor(Col2005$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Col2005$AtRelig <- recode(Col2005$q4, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Col2005, mean)
aggregate(TolerHomo ~ AtRelig, Col2005, mean)
summary(Col2005$TolerHomo)

###Colômbia
##2006
Col2006 <- read_dta("Col2006.dta")
Col2006 <- remove_all_labels(Col2006)
#Tolerância
Col2006$TolerHomo <- Col2006$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Col2006$Denom <-  recode(Col2006$q3, 1 <- 1, 2 <- 2, 3 <- 3, 4 <- 4)
Col2006$Denom <- as.factor(Col2006$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (NA)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Col2006, mean)
aggregate(TolerHomo ~ AtRelig, Col2006, mean)
summary(Col2006$TolerHomo)

###Colômbia
##2007
Col2007 <- read_dta("Col2007.dta")
Col2007 <- remove_all_labels(Col2007)
#Tolerância
Col2007$TolerHomo <- Col2007$D5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Col2007$Denom <-  recode(Col2007$Q3, 1 <- 1, 2 <- 5, 3 <- c(2,3), 4 <- 4)
Col2007$Denom <- as.factor(Col2007$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (NA)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Col2007, mean)
aggregate(TolerHomo ~ AtRelig, Col2007, mean)
summary(Col2007$TolerHomo)

###Colômbia
##2008
Col2008 <- read_dta("Col2008.dta")
Col2008 <- remove_all_labels(Col2008)
#Tolerância
Col2008$TolerHomo <- Col2008$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Col2008$Denom <-  recode(Col2008$q3, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Col2008$Denom <- as.factor(Col2008$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Col2008$AtRelig <- recode(Col2008$q5,0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Col2008, mean)
aggregate(TolerHomo ~ AtRelig, Col2008, mean)
summary(Col2008$TolerHomo)

###Colômbia
##2009
Col2009 <- read_dta("Col2009.dta")
Col2009 <- remove_all_labels(Col2009)
#Tolerância
Col2009$TolerHomo <- Col2009$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Col2009$Denom <-  recode(Col2009$q3, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Col2009$Denom <- as.factor(Col2009$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (NA)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Col2009, mean)
aggregate(TolerHomo ~ AtRelig, Col2009, mean)
summary(Col2009$TolerHomo)

###Colômbia
##2010
Col2010 <- read_dta("Col2010.dta")
Col2010 <- remove_all_labels(Col2010)
#Tolerância
Col2010$TolerHomo <- Col2010$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Col2010$Denom <-  recode(Col2010$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,8,10,12), 4 <- c(4,11))
Col2010$Denom <- as.factor(Col2010$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Col2010$AtRelig <- recode(Col2010$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Col2010$IntRelig <- recode(Col2010$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Col2010, mean)
aggregate(TolerHomo ~ AtRelig, Col2010, mean)
summary(Col2010$TolerHomo)

###Colômbia
##2011
Col2011 <- read_dta("Col2011.dta")
Col2011 <- remove_all_labels(Col2011)
#Tolerância
Col2011$TolerHomo <- Col2011$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Col2011$Denom <-  recode(Col2011$q3c, 1 <- c(1,2), 2 <- c(3,6), 3 <- c(4,7,8,10,11), 4 <- 5)
Col2011$Denom <- as.factor(Col2011$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Col2011$AtRelig <- recode(Col2011$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Col2011$IntRelig <- recode(Col2011$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Col2011, mean)
aggregate(TolerHomo ~ AtRelig, Col2011, mean)
summary(Col2011$TolerHomo)

###Colômbia
##2012
Col2012 <- read_dta("Col2012.dta")
Col2012 <- remove_all_labels(Col2012)
#Tolerância
Col2012$TolerHomo <- Col2012$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Col2012$Denom <-  recode(Col2012$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <- 4)
Col2012$Denom <- as.factor(Col2012$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Col2012$AtRelig <- recode(Col2012$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Col2012$IntRelig <- recode(Col2012$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Col2012, mean)
aggregate(TolerHomo ~ AtRelig, Col2012, mean)
summary(Col2012$TolerHomo)

###Colômbia
##2013
Col2013 <- read_dta("Col2013.dta")
Col2013 <- remove_all_labels(Col2013)
#Tolerância
Col2013$TolerHomo <- Col2013$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Col2013$Denom <-  recode(Col2013$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <- 4)
Col2013$Denom <- as.factor(Col2013$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Col2013$AtRelig <- recode(Col2013$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Col2013, mean)
aggregate(TolerHomo ~ AtRelig, Col2013, mean)
summary(Col2013$TolerHomo)

###Colômbia
##2014
Col2014 <- read_dta("Col2014.dta")
Col2014 <- remove_all_labels(Col2014)
#Tolerância
Col2014$TolerHomo <- Col2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Col2014$Denom <-  recode(Col2014$q3, 1 <- 1, 2 <- c(2,5), 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Col2014$Denom <- as.factor(Col2014$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Col2014$AtRelig <- recode(Col2014$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Col2014$IntRelig <- recode(Col2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Col2014, mean)
aggregate(TolerHomo ~ AtRelig, Col2014, mean)
summary(Col2014$TolerHomo)

###Colômbia
##2016
Col2016 <- read_dta("Col2016.dta")
Col2016 <- remove_all_labels(Col2016)
#Tolerância
Col2016$TolerHomo <- Col2016$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Col2016$Denom <-  recode(Col2016$q3, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Col2016$Denom <- as.factor(Col2016$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Col2016$AtRelig <- recode(Col2016$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Col2016$IntRelig <- recode(Col2016$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Col2016, mean)
aggregate(TolerHomo ~ AtRelig, Col2016, mean)
summary(Col2016$TolerHomo)

###Colômbia
##2018
Col2018 <- read_dta("Col2018.dta")
Col2018 <- remove_all_labels(Col2018)
#Tolerância
Col2018$TolerHomo <- Col2018$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Col2018$Denom <-  recode(Col2018$q3cn, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Col2018$Denom <- as.factor(Col2018$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Col2018$IntRelig <- recode(Col2018$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo
aggregate(TolerHomo ~ Denom, Col2018, mean)
aggregate(TolerHomo ~ AtRelig, Col2018, mean)
summary(Col2018$TolerHomo)


##########Condicionantes

#Adesão a democracia (churchiliana)
val_labels(Col2018$ing4) <- NULL#retira rótulos dos valores para uso do recode
Col2018$Dem <- Col2018$ing4


#Confiança
Col2018$conf <- as.numeric(Col2018$it1)
val_labels(Col2018$conf) <- NULL#retira rótulos dos valores para uso do recode
Col2018$ConfInt <- recode(Col2018$conf, 0 <- c(4), 1 <- c(3), 2 <- c(2), 3 <- c(1))


#Escolaridade
Col2018$Ed_sup <- cut(Col2018$ed, c(0,15,19))
Col2018$Ed_sup <- as.numeric(Col2018$Ed_sup)


#Idade
val_labels(Col2018$q2) <- NULL#retira rótulos dos valores para uso do recode
Col2018$Idade <- Col2018$q2

#Sexo
val_labels(Col2018$q1) <- NULL#retira rótulos dos valores para uso do recode
Col2018$Sexo <- recode(Col2018$q1, 0 <- c(1), 1 <- c(2))

val_labels(Col2018$it1) <- NULL
Col2018$it1 <- as.numeric(Col2018$it1)
Col2018$Conf_int <- recode(Col2018$it1, 0 <- 4, 1 <- 3, 2 <-2, 3 <- 1)

save(Col2018, file = "Col2018.RData")


###Costa Rica
##2004
Cos2004 <- read_dta("Cos2004.dta")
Cos2004 <- remove_all_labels(Cos2004)
#Tolerância
Cos2004$TolerHomo <- Cos2004$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Cos2004$Denom <-  recode(Cos2004$q3, 1 <- 1, 2 <- 2, 3 <- 3, 4 <- 4)
Cos2004$Denom <- as.factor(Cos2004$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Cos2004$AtRelig <- recode(Cos2004$q4, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo (NA)
aggregate(TolerHomo ~ Denom, Cos2004, mean)
aggregate(TolerHomo ~ AtRelig, Cos2004, mean)
summary(Cos2004$TolerHomo)

###Costa Rica
##2006
Cos2006 <- read_dta("Cos2006.dta")
Cos2006 <- remove_all_labels(Cos2006)
#Tolerância
Cos2006$TolerHomo <- Cos2006$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Cos2006$Denom <-  recode(Cos2006$q3, 1 <- 1, 2 <- 5, 3 <- c(2,3), 4 <- 4)
Cos2006$Denom <- as.factor(Cos2006$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (NA)
#Médias por Grupo (NA)
aggregate(TolerHomo ~ Denom, Cos2006, mean)
aggregate(TolerHomo ~ AtRelig, Cos2006, mean)
summary(Cos2006$TolerHomo)

###Costa Rica
##2008
Cos2008 <- read_dta("Cos2008.dta")
Cos2008 <- remove_all_labels(Cos2008)
#Tolerância
Cos2008$TolerHomo <- Cos2008$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Cos2008$Denom <-  recode(Cos2008$q3, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Cos2008$Denom <- as.factor(Cos2008$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Cos2008$AtRelig <- recode(Cos2008$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo (NA)
aggregate(TolerHomo ~ Denom, Cos2008, mean)
aggregate(TolerHomo ~ AtRelig, Cos2008, mean)
summary(Cos2008$TolerHomo)

###Costa Rica
##2010
Cos2010 <- read_dta("Cos2010.dta")
Cos2010 <- remove_all_labels(Cos2010)
#Tolerância
Cos2010$TolerHomo <- Cos2010$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Cos2010$Denom <-  recode(Cos2010$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Cos2010$Denom <- as.factor(Cos2010$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Cos2010$AtRelig <- recode(Cos2010$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Cos2010$IntRelig <- recode(Cos2010$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo (NA)
aggregate(TolerHomo ~ Denom, Cos2010, mean)
aggregate(TolerHomo ~ AtRelig, Cos2010, mean)
summary(Cos2010$TolerHomo)

###Costa Rica
##2012
Cos2012 <- read_dta("Cos2012.dta")
Cos2012 <- remove_all_labels(Cos2012)
#Tolerância
Cos2012$TolerHomo <- Cos2012$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Cos2012$Denom <-  recode(Cos2012$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Cos2012$Denom <- as.factor(Cos2012$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Cos2012$AtRelig <- recode(Cos2012$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Cos2010$IntRelig <- recode(Cos2010$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo (NA)
aggregate(TolerHomo ~ Denom, Cos2012, mean)
aggregate(TolerHomo ~ AtRelig, Cos2012, mean)
summary(Cos2012$TolerHomo)


###Costa Rica
##2014
Cos2014 <- read_dta("Cos2014.dta")
Cos2014 <- remove_all_labels(Cos2014)
#Tolerância
Cos2014$TolerHomo <- Cos2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Cos2014$Denom <-  recode(Cos2014$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Cos2014$Denom <- as.factor(Cos2014$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Cos2014$IntRelig <- recode(Cos2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Cos2014, mean)
aggregate(TolerHomo ~ AtRelig, Cos2014, mean)
summary(Cos2014$TolerHomo)

###Costa Rica
##2016
Cos2016 <- read_dta("Cos2016.dta")
Cos2016 <- remove_all_labels(Cos2016)
#Tolerância
Cos2016$TolerHomo <- Cos2016$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Cos2016$Denom <-  recode(Cos2016$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Cos2016$Denom <- as.factor(Cos2016$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Cos2016$AtRelig <- recode(Cos2016$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Cos2016$IntRelig <- recode(Cos2016$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo (NA)
aggregate(TolerHomo ~ Denom, Cos2016, mean)
aggregate(TolerHomo ~ AtRelig, Cos2016, mean)
summary(Cos2016$TolerHomo)

###Costa Rica
##2018
Cos2018 <- read_dta("Cos2018.dta")
Cos2018 <- remove_all_labels(Cos2018)
#Tolerância
Cos2018$TolerHomo <- Cos2018$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Cos2018$Denom <-  recode(Cos2018$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Cos2018$Denom <- as.factor(Cos2018$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Cos2018$AtRelig <- recode(Cos2018$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Cos2018$IntRelig <- recode(Cos2018$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Cos2018, mean)
aggregate(TolerHomo ~ AtRelig, Cos2018, mean)
summary(Cos2018$TolerHomo)

##########Condicionantes

#Adesão a democracia (churchiliana)
val_labels(Cos2018$ing4) <- NULL#retira rótulos dos valores para uso do recode
Cos2018$Dem <- Cos2018$ing4

#Confiança
Cos2018$conf <- as.numeric(Cos2018$it1)
val_labels(Cos2018$conf) <- NULL#retira rótulos dos valores para uso do recode
Cos2018$ConfInt <- recode(Cos2018$conf, 0 <- c(4), 1 <- c(3), 2 <- c(2), 3 <- c(1))


#Escolaridade
Cos2018$Ed_sup <- cut(Cos2018$ed, c(0,15,19))
Cos2018$Ed_sup <- as.numeric(Cos2018$Ed_sup)

#Idade
val_labels(Cos2018$q2) <- NULL#retira rótulos dos valores para uso do recode
Cos2018$Idade <- Cos2018$q2

#Sexo
val_labels(Cos2018$q1) <- NULL#retira rótulos dos valores para uso do recode
Cos2018$Sexo <- recode(Cos2018$q1, 0 <- c(1), 1 <- c(2))

val_labels(Cos2018$it1) <- NULL
Cos2018$it1 <- as.numeric(Cos2018$it1)
Cos2018$Conf_int <- recode(Cos2018$it1, 0 <- 4, 1 <- 3, 2 <-2, 3 <- 1)

save(Cos2018, file = "Cos2018.RData")

###El Salvador 
##2004
Els2004 <- read_dta("Els2004.dta")
Els2004 <- remove_all_labels(Els2004)
#Tolerância
Els2004$TolerHomo <- Els2004$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Els2004$Denom <-  recode(Els2004$q3, 1 <- 1, 2 <- 2, 3 <- 3, 4 <- 4)
Els2004$Denom <- as.factor(Els2004$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Els2004$AtRelig <- recode(Els2004$q4, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Els2004, mean)
aggregate(TolerHomo ~ AtRelig, Els2004, mean)
summary(Els2004$TolerHomo)


###El Salvador 
##2006
Els2006 <- read_dta("Els2006.dta")
Els2006 <- remove_all_labels(Els2006)
#Tolerância
Els2006$TolerHomo <- Els2006$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Els2006$Denom <-  recode(Els2006$q3, 1 <- 1, 2 <- 2, 3 <- 3, 4 <- 4)
Els2006$Denom <- as.factor(Els2006$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Els2006, mean)
aggregate(TolerHomo ~ AtRelig, Els2006, mean)
summary(Els2006$TolerHomo)

###El Salvador 
##2008
Els2008 <- read_dta("Els2008.dta")
Els2008 <- remove_all_labels(Els2008)
#Tolerância
Els2008$TolerHomo <- Els2008$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Els2008$Denom <-  recode(Els2008$q3, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Els2008$Denom <- as.factor(Els2008$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Els2008$AtRelig <- recode(Els2008$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Els2008, mean)
aggregate(TolerHomo ~ AtRelig, Els2008, mean)
summary(Els2008$TolerHomo)

###El Salvador 
##2010
Els2010 <- read_dta("Els2010.dta")
Els2010 <- remove_all_labels(Els2010)
#Tolerância
Els2010$TolerHomo <- Els2010$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Els2010$Denom <-  recode(Els2010$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Els2010$Denom <- as.factor(Els2010$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Els2010$AtRelig <- recode(Els2010$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Els2010$IntRelig <- recode(Els2010$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Els2010, mean)
aggregate(TolerHomo ~ AtRelig, Els2010, mean)
summary(Els2010$TolerHomo)

###El Salvador 
##2012
Els2012 <- read_dta("Els2012.dta")
Els2012 <- remove_all_labels(Els2012)
#Tolerância
Els2012$TolerHomo <- Els2012$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Els2012$Denom <-  recode(Els2012$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Els2012$Denom <- as.factor(Els2012$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Els2012$AtRelig <- recode(Els2012$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Els2012$IntRelig <- recode(Els2012$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Els2012, mean)
aggregate(TolerHomo ~ AtRelig, Els2012, mean)
summary(Els2012$TolerHomo)

###El Salvador 
##2014
Els2014 <- read_dta("Els2014.dta")
Els2014 <- remove_all_labels(Els2014)
#Tolerância
Els2014$TolerHomo <- Els2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Els2014$Denom <-  recode(Els2014$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Els2014$Denom <- as.factor(Els2014$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Els2014$IntRelig <- recode(Els2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Els2014, mean)
aggregate(TolerHomo ~ AtRelig, Els2014, mean)
summary(Els2014$TolerHomo)

###El Salvador 
##2016
Els2016 <- read_dta("Els2016.dta")
Els2016 <- remove_all_labels(Els2016)
#Tolerância
Els2016$TolerHomo <- Els2016$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Els2016$Denom <-  recode(Els2016$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Els2016$Denom <- as.factor(Els2016$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Els2016$AtRelig <- recode(Els2016$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Els2016$IntRelig <- recode(Els2016$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Els2016, mean)
aggregate(TolerHomo ~ AtRelig, Els2016, mean)
summary(Els2016$TolerHomo)

###El Salvador 
##2018
Els2018 <- read_dta("Els2018.dta")
Els2018 <- remove_all_labels(Els2018)
#Tolerância
Els2018$TolerHomo <- Els2018$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Els2018$Denom <-  recode(Els2018$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Els2018$Denom <- as.factor(Els2018$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Els2018$AtRelig <- recode(Els2018$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Els2018$IntRelig <- recode(Els2018$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Els2018, mean)
aggregate(TolerHomo ~ AtRelig, Els2018, mean)
summary(Els2018$TolerHomo)

##########Condicionantes

#Adesão a democracia (churchiliana)
val_labels(Els2018$ing4) <- NULL#retira rótulos dos valores para uso do recode
Els2018$Dem <- Els2018$ing4

#Confiança
Els2018$conf <- as.numeric(Els2018$it1)
val_labels(Els2018$conf) <- NULL#retira rótulos dos valores para uso do recode
Els2018$ConfInt <- recode(Els2018$conf, 0 <- c(4), 1 <- c(3), 2 <- c(2), 3 <- c(1))

#Escolaridade
Els2018$Ed_sup <- cut(Els2018$ed, c(0,15,19))
Els2018$Ed_sup <- as.numeric(Els2018$Ed_sup)

#Idade
val_labels(Els2018$q2) <- NULL#retira rótulos dos valores para uso do recode
Els2018$Idade <- Els2018$q2

#Sexo
val_labels(Els2018$q1) <- NULL#retira rótulos dos valores para uso do recode
Els2018$Sexo <- recode(Els2018$q1, 0 <- c(1), 1 <- c(2))

val_labels(Els2018$it1) <- NULL
Els2018$it1 <- as.numeric(Els2018$it1)
Els2018$Conf_int <- recode(Els2018$it1, 0 <- 4, 1 <- 3, 2 <-2, 3 <- 1)

save(Els2018, file = "Els2018.RData")

###Equador 
##2004
Equ2004 <- read_dta("Equ2004.dta")
Equ2004 <- remove_all_labels(Equ2004)
#Tolerância
Equ2004$TolerHomo <- Equ2004$newtol5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Equ2004$Denom <-  recode(Equ2004$q3, 1 <- c(1,2), 2 <- 3, 3 <- c(5,6), 4 <- 4)
Equ2004$Denom <- as.factor(Equ2004$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Equ2004, mean)
aggregate(TolerHomo ~ AtRelig, Equ2004, mean)
summary(Equ2004$TolerHomo)

###Equador 
##2006
Equ2006 <- read_dta("Equ2006.dta")
Equ2006 <- remove_all_labels(Equ2006)
#Tolerância
Equ2006$TolerHomo <- Equ2006$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Equ2006$Denom <-  recode(Equ2006$q3, 1 <- 1, 2 <- 3, 3 <- 2, 4 <- 4)
Equ2006$Denom <- as.factor(Equ2006$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Equ2006, mean)
aggregate(TolerHomo ~ AtRelig, Equ2006, mean)
summary(Equ2006$TolerHomo)

###Equador 
##2008
Equ2008 <- read_dta("Equ2008.dta")
Equ2008 <- remove_all_labels(Equ2008)
#Tolerância
Equ2008$TolerHomo <- Equ2008$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Equ2008$Denom <-  recode(Equ2008$q3, 1 <- 1, 2 <- 3, 3 <- 2, 4 <- 4)
Equ2008$Denom <- as.factor(Equ2008$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Equ2008$AtRelig <- recode(Equ2008$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Equ2008, mean)
aggregate(TolerHomo ~ AtRelig, Equ2008, mean)
summary(Equ2008$TolerHomo)

###Equador 
##2010
Equ2010 <- read_dta("Equ2010.dta")
Equ2010 <- remove_all_labels(Equ2010)
#Tolerância
Equ2010$TolerHomo <- Equ2010$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Equ2010$Denom <-  recode(Equ2010$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Equ2010$Denom <- as.factor(Equ2010$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Equ2010$AtRelig <- recode(Equ2010$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Equ2010$IntRelig <- recode(Equ2010$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Equ2010, mean)
aggregate(TolerHomo ~ AtRelig, Equ2010, mean)
summary(Equ2010$TolerHomo)

###Equador 
##2012
Equ2012 <- read_dta("Equ2012.dta")
Equ2012 <- remove_all_labels(Equ2012)
#Tolerância
Equ2012$TolerHomo <- Equ2012$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Equ2012$Denom <-  recode(Equ2012$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Equ2012$Denom <- as.factor(Equ2012$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Equ2012$AtRelig <- recode(Equ2012$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Equ2012$IntRelig <- recode(Equ2012$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Equ2012, mean)
aggregate(TolerHomo ~ AtRelig, Equ2012, mean)
summary(Equ2012$TolerHomo)

###Equador 
##2014
Equ2014 <- read_dta("Equ2014.dta")
Equ2014 <- remove_all_labels(Equ2014)
#Tolerância
Equ2014$TolerHomo <- Equ2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Equ2014$Denom <-  recode(Equ2014$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Equ2014$Denom <- as.factor(Equ2014$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Equ2014$IntRelig <- recode(Equ2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Equ2014, mean)
aggregate(TolerHomo ~ AtRelig, Equ2014, mean)
summary(Equ2014$TolerHomo)

###Equador 
##2016
Equ2016 <- read_dta("Equ2016.dta")
Equ2016 <- remove_all_labels(Equ2016)
#Tolerância
Equ2016$TolerHomo <- Equ2016$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Equ2016$Denom <-  recode(Equ2016$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Equ2016$Denom <- as.factor(Equ2016$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Equ2016$AtRelig <- recode(Equ2016$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Equ2016$IntRelig <- recode(Equ2016$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Equ2016, mean)
aggregate(TolerHomo ~ AtRelig, Equ2016, mean)
summary(Equ2016$TolerHomo)

###Equador 
##2018
Equ2018 <- read_dta("Equ2018.dta")
Equ2018 <- remove_all_labels(Equ2018)
#Tolerância
Equ2018$TolerHomo <- Equ2018$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Equ2018$Denom <-  recode(Equ2018$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Equ2018$Denom <- as.factor(Equ2018$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Equ2018$AtRelig <- recode(Equ2018$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Equ2018$IntRelig <- recode(Equ2018$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Equ2018, mean)
aggregate(TolerHomo ~ AtRelig, Equ2018, mean)
summary(Equ2018$TolerHomo)

##########Condicionantes

#Adesão a democracia (churchiliana)
val_labels(Equ2018$ing4) <- NULL#retira rótulos dos valores para uso do recode
Equ2018$Dem <- Equ2018$ing4

#Confiança
Equ2018$conf <- as.numeric(Equ2018$it1)
val_labels(Equ2018$conf) <- NULL#retira rótulos dos valores para uso do recode
Equ2018$ConfInt <- recode(Equ2018$conf, 0 <- c(4), 1 <- c(3), 2 <- c(2), 3 <- c(1))


#Escolaridade
Equ2018$Ed_sup <- cut(Equ2018$ed, c(0,15,19))
Equ2018$Ed_sup <- as.numeric(Equ2018$Ed_sup)

#Idade
val_labels(Equ2018$q2) <- NULL#retira rótulos dos valores para uso do recode
Equ2018$Idade <- Equ2018$q2

#Sexo
val_labels(Equ2018$q1) <- NULL#retira rótulos dos valores para uso do recode
Equ2018$Sexo <- recode(Equ2018$q1, 0 <- c(1), 1 <- c(2))

val_labels(Equ2018$it1) <- NULL
Equ2018$it1 <- as.numeric(Equ2018$it1)
Equ2018$Conf_int <- recode(Equ2018$it1, 0 <- 4, 1 <- 3, 2 <-2, 3 <- 1)

save(Equ2018, file = "Equ2018.RData")

###Guatemala 
##2004
Gua2004 <- read_dta("Gua2004.dta")
Gua2004 <- remove_all_labels(Gua2004)
#Tolerância
Gua2004$TolerHomo <- Gua2004$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Gua2004$Denom <-  recode(Gua2004$q3, 1 <- 1, 2 <- 2, 3 <- 3, 4 <- 4)
Gua2004$Denom <- as.factor(Gua2004$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Gua2004$AtRelig <- recode(Gua2004$q4, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Gua2004, mean)
aggregate(TolerHomo ~ AtRelig, Gua2004, mean)
summary(Gua2004$TolerHomo)

###Guatemala 
##2006
Gua2006 <- read_dta("Gua2006.dta")
Gua2006 <- remove_all_labels(Gua2006)
#Tolerância
Gua2006$TolerHomo <- Gua2006$D5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Gua2006$Denom <-  recode(Gua2006$Q3,  1 <- 1, 2 <- 2, 3 <- 3, 4 <- 4)
Gua2006$Denom <- as.factor(Gua2006$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Gua2006$AtRelig <- recode(Gua2006$GUAQ4, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Gua2006, mean)
aggregate(TolerHomo ~ AtRelig, Gua2006, mean)
summary(Gua2006$TolerHomo)

###Guatemala 
##2008
Gua2008 <- read_dta("Gua2008.dta")
Gua2008 <- remove_all_labels(Gua2008)
#Tolerância
Gua2008$TolerHomo <- Gua2008$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Gua2008$Denom <-  recode(Gua2008$q3, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Gua2008$Denom <- as.factor(Gua2008$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Gua2008$AtRelig <- recode(Gua2008$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Gua2008, mean)
aggregate(TolerHomo ~ AtRelig, Gua2008, mean)
summary(Gua2008$TolerHomo)

###Guatemala 
##2010
Gua2010 <- read_dta("Gua2010.dta")
Gua2010 <- remove_all_labels(Gua2010)
#Tolerância
Gua2010$TolerHomo <- Gua2010$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Gua2010$Denom <-  recode(Gua2010$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Gua2010$Denom <- as.factor(Gua2010$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Gua2010$AtRelig <- recode(Gua2010$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Gua2010$IntRelig <- recode(Gua2010$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Gua2010, mean)
aggregate(TolerHomo ~ AtRelig, Gua2010, mean)
summary(Gua2010$TolerHomo)

###Guatemala 
##2012
Gua2012 <- read_dta("Gua2012.dta")
Gua2012 <- remove_all_labels(Gua2012)
#Tolerância
Gua2012$TolerHomo <- Gua2012$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Gua2012$Denom <-  recode(Gua2012$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Gua2012$Denom <- as.factor(Gua2012$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Gua2012$AtRelig <- recode(Gua2012$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Gua2012$IntRelig <- recode(Gua2012$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Gua2012, mean)
aggregate(TolerHomo ~ AtRelig, Gua2012, mean)
summary(Gua2012$TolerHomo)

###Guatemala 
##2014
Gua2014 <- read_dta("Gua2014.dta")
Gua2014 <- remove_all_labels(Gua2014)
#Tolerância
Gua2014$TolerHomo <- Gua2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Gua2014$Denom <-  recode(Gua2014$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Gua2014$Denom <- as.factor(Gua2014$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Gua2014$IntRelig <- recode(Gua2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Gua2014, mean)
aggregate(TolerHomo ~ AtRelig, Gua2014, mean)
summary(Gua2014$TolerHomo)

##2016
Gua2016 <- read_dta("Gua2016.dta")
Gua2016 <- remove_all_labels(Gua2016)
#Tolerância
Gua2016$TolerHomo <- Gua2016$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Gua2016$Denom <-  recode(Gua2016$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Gua2016$Denom <- as.factor(Gua2016$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Gua2016$AtRelig <- recode(Gua2016$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Gua2016$IntRelig <- recode(Gua2016$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Gua2016, mean)
aggregate(TolerHomo ~ AtRelig, Gua2016, mean)
summary(Gua2016$TolerHomo)

###Guatemala 
##2018
Gua2018 <- read_dta("Gua2018.dta")
Gua2018 <- remove_all_labels(Gua2018)
#Tolerância
Gua2018$TolerHomo <- Gua2018$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Gua2018$Denom <-  recode(Gua2018$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Gua2018$Denom <- as.factor(Gua2018$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Gua2018$AtRelig <- recode(Gua2018$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Gua2018$IntRelig <- recode(Gua2018$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Gua2018, mean)
aggregate(TolerHomo ~ AtRelig, Gua2018, mean)
summary(Gua2018$TolerHomo)

##########Condicionantes

#Adesão a democracia (churchiliana)
val_labels(Gua2018$ing4) <- NULL#retira rótulos dos valores para uso do recode
Gua2018$Dem <- Gua2018$ing4

#Confiança
Gua2018$conf <- as.numeric(Gua2018$it1)
val_labels(Gua2018$conf) <- NULL#retira rótulos dos valores para uso do recode
Gua2018$ConfInt <- recode(Gua2018$conf, 0 <- c(4), 1 <- c(3), 2 <- c(2), 3 <- c(1))


#Escolaridade
Gua2018$Ed_sup <- cut(Gua2018$ed, c(0,15,19))
Gua2018$Ed_sup <- as.numeric(Gua2018$Ed_sup)

#Idade
val_labels(Gua2018$q2) <- NULL#retira rótulos dos valores para uso do recode
Gua2018$Idade <- Gua2018$q2

#Sexo
val_labels(Gua2018$q1) <- NULL#retira rótulos dos valores para uso do recode
Gua2018$Sexo <- recode(Gua2018$q1, 0 <- c(1), 1 <- c(2))

val_labels(Gua2018$it1) <- NULL
Gua2018$it1 <- as.numeric(Gua2018$it1)
Gua2018$Conf_int <- recode(Gua2018$it1, 0 <- 4, 1 <- 3, 2 <-2, 3 <- 1)

save(Gua2018, file = "Gua2018.RData")

###Haiti
##2006
Hai2006 <- read_dta("Hai2006.dta")
Hai2006 <- remove_all_labels(Hai2006)
#Tolerância
Hai2006$TolerHomo <- Hai2006$D5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Hai2006$Denom <-  recode(Hai2006$Q3,  1 <- 1, 2 <- c(2,5), 3 <- c(3,6), 4 <- 4)
Hai2006$Denom <- as.factor(Hai2006$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Hai2006, mean)
aggregate(TolerHomo ~ AtRelig, Hai2006, mean)
summary(Hai2006$TolerHomo)



###Haiti
##2008
Hai2008 <- read_dta("Hai2008.dta")
Hai2008 <- remove_all_labels(Hai2008)
#Tolerância
Hai2008$TolerHomo <- Hai2008$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Hai2008$Denom <-  recode(Hai2008$q3,  1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Hai2008$Denom <- as.factor(Hai2008$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Hai2008$AtRelig <- recode(Hai2008$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Hai2008, mean)
aggregate(TolerHomo ~ AtRelig, Hai2008, mean)
summary(Hai2008$TolerHomo)

###Haiti
##2010
Hai2010 <- read_dta("Hai2010.dta")
Hai2010 <- remove_all_labels(Hai2010)
#Tolerância
Hai2010$TolerHomo <- Hai2010$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Hai2010$Denom <-  recode(Hai2010$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Hai2010$Denom <- as.factor(Hai2010$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Hai2010$AtRelig <- recode(Hai2010$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Hai2010$IntRelig <- recode(Hai2010$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Hai2010, mean)
aggregate(TolerHomo ~ AtRelig, Hai2010, mean)
summary(Hai2010$TolerHomo)

###Haiti
##2012
Hai2012 <- read_dta("Hai2012.dta")
Hai2012 <- remove_all_labels(Hai2012)
#Tolerância
Hai2012$TolerHomo <- Hai2012$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Hai2012$Denom <-  recode(Hai2012$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Hai2012$Denom <- as.factor(Hai2012$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Hai2012$AtRelig <- recode(Hai2012$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Hai2012$IntRelig <- recode(Hai2012$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Hai2012, mean)
aggregate(TolerHomo ~ AtRelig, Hai2012, mean)
summary(Hai2012$TolerHomo)

###Haiti
##2014
Hai2014 <- read_dta("Hai2014.dta")
Hai2014 <- remove_all_labels(Hai2014)
#Tolerância
Hai2014$TolerHomo <- Hai2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Hai2014$Denom <-  recode(Hai2014$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Hai2014$Denom <- as.factor(Hai2014$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Hai2014$IntRelig <- recode(Hai2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Hai2014, mean)
aggregate(TolerHomo ~ AtRelig, Hai2014, mean)
summary(Hai2014$TolerHomo)

###Haiti
##2016
Hai2016 <- read_dta("Hai2016.dta")
Hai2016 <- remove_all_labels(Hai2016)
#Tolerância
Hai2016$TolerHomo <- Hai2016$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Hai2016$Denom <-  recode(Hai2016$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Hai2016$Denom <- as.factor(Hai2016$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Hai2016$AtRelig <- recode(Hai2016$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Hai2016$IntRelig <- recode(Hai2016$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Hai2016, mean)
aggregate(TolerHomo ~ AtRelig, Hai2016, mean)
summary(Hai2016$TolerHomo)

##########Condicionantes

#Adesão a democracia (churchiliana)
val_labels(Hai2016$ing4) <- NULL#retira rótulos dos valores para uso do recode
Hai2016$Dem <- Hai2016$ing4

#Confiança
Hai2016$conf <- as.numeric(Hai2016$it1)
val_labels(Hai2016$conf) <- NULL#retira rótulos dos valores para uso do recode
Hai2016$ConfInt <- recode(Hai2016$conf, 0 <- c(4), 1 <- c(3), 2 <- c(2), 3 <- c(1))


#Escolaridade
Hai2016$Ed_sup <- cut(Hai2016$ed, c(0,15,19))
Hai2016$Ed_sup <- as.numeric(Hai2016$Ed_sup)

#Idade
val_labels(Hai2016$q2) <- NULL#retira rótulos dos valores para uso do recode
Hai2016$Idade <- Hai2016$q2

#Sexo
val_labels(Hai2016$q1) <- NULL#retira rótulos dos valores para uso do recode
Hai2016$Sexo <- recode(Hai2016$q1, 0 <- c(1), 1 <- c(2))

val_labels(Hai2016$it1) <- NULL
Hai2016$it1 <- as.numeric(Hai2016$it1)
Hai2016$Conf_int <- recode(Hai2016$it1, 0 <- 4, 1 <- 3, 2 <-2, 3 <- 1)

save(Hai2016, file = "Hai2016.RData")

###Honduras
##2004
Hon2004 <- read_dta("Hon2004.dta")
Hon2004 <- remove_all_labels(Hon2004)
#Tolerância
Hon2004$TolerHomo <- Hon2004$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Hon2004$Denom <-  recode(Hon2004$q3,  1 <- 1, 2 <- 2, 3 <- 3, 4 <- 4)
Hon2004$Denom <- as.factor(Hon2004$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Hon2004$AtRelig <- recode(Hon2004$q4, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Hon2004, mean)
aggregate(TolerHomo ~ AtRelig, Hon2004, mean)
summary(Hon2004$TolerHomo)

###Honduras
##2006
Hon2006 <- read_dta("Hon2006.dta")
Hon2006 <- remove_all_labels(Hon2006)
#Tolerância
Hon2006$TolerHomo <- Hon2006$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Hon2006$Denom <-  recode(Hon2006$q3, 1 <- 1, 2 <- 5, 3 <- c(2,3), 4 <- 4)
Hon2006$Denom <- as.factor(Hon2006$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Hon2006, mean)
aggregate(TolerHomo ~ AtRelig, Hon2006, mean)
summary(Hon2006$TolerHomo)

###Honduras
##2008
Hon2008 <- read_dta("Hon2008.dta")
Hon2008 <- remove_all_labels(Hon2008)
#Tolerância
Hon2008$TolerHomo <- Hon2008$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Hon2008$Denom <-  recode(Hon2008$q3, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Hon2008$Denom <- as.factor(Hon2008$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Hon2008$AtRelig <- recode(Hon2008$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Hon2008, mean)
aggregate(TolerHomo ~ AtRelig, Hon2008, mean)
summary(Hon2008$TolerHomo)

###Honduras
##2010
Hon2010 <- read_dta("Hon2010.dta")
Hon2010 <- remove_all_labels(Hon2010)
#Tolerância
Hon2010$TolerHomo <- Hon2010$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Hon2010$Denom <-  recode(Hon2010$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Hon2010$Denom <- as.factor(Hon2010$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Hon2010$AtRelig <- recode(Hon2010$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Hon2010$IntRelig <- recode(Hon2010$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Hon2010, mean)
aggregate(TolerHomo ~ AtRelig, Hon2010, mean)
summary(Hon2010$TolerHomo)

###Honduras
##2012
Hon2012 <- read_dta("Hon2012.dta")
Hon2012 <- remove_all_labels(Hon2012)
#Tolerância
Hon2012$TolerHomo <- Hon2012$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Hon2012$Denom <-  recode(Hon2012$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Hon2012$Denom <- as.factor(Hon2012$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Hon2012$AtRelig <- recode(Hon2012$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Hon2012$IntRelig <- recode(Hon2012$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Hon2012, mean)
aggregate(TolerHomo ~ AtRelig, Hon2012, mean)
summary(Hon2012$TolerHomo)

###Honduras
##2014
Hon2014 <- read_dta("Hon2014.dta")
Hon2014 <- remove_all_labels(Hon2014)
#Tolerância
Hon2014$TolerHomo <- Hon2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Hon2014$Denom <-  recode(Hon2014$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Hon2014$Denom <- as.factor(Hon2014$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Hon2014$IntRelig <- recode(Hon2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Hon2014, mean)
aggregate(TolerHomo ~ AtRelig, Hon2014, mean)
summary(Hon2014$TolerHomo)

###Honduras
##2016
Hon2016 <- read_dta("Hon2016.dta")
Hon2016 <- remove_all_labels(Hon2016)
#Tolerância
Hon2016$TolerHomo <- Hon2016$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Hon2016$Denom <-  recode(Hon2016$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Hon2016$Denom <- as.factor(Hon2016$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Hon2016$AtRelig <- recode(Hon2016$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Hon2016$IntRelig <- recode(Hon2016$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Hon2016, mean)
aggregate(TolerHomo ~ AtRelig, Hon2016, mean)
summary(Hon2016$TolerHomo)

###Honduras
##2018
Hon2018 <- read_dta("Hon2018.dta")
Hon2018 <- remove_all_labels(Hon2018)
#Tolerância
Hon2018$TolerHomo <- Hon2018$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Hon2018$Denom <-  recode(Hon2018$q3cn, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Hon2018$Denom <- as.factor(Hon2018$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Hon2018$AtRelig <- recode(Hon2018$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Hon2018$IntRelig <- recode(Hon2018$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Hon2018, mean)
aggregate(TolerHomo ~ AtRelig, Hon2018, mean)
summary(Hon2018$TolerHomo)

##########Condicionantes

#Adesão a democracia (churchiliana)
val_labels(Hon2018$ing4) <- NULL#retira rótulos dos valores para uso do recode
Hon2018$Dem <- Hon2018$ing4

#Confiança
Hon2018$conf <- as.numeric(Hon2018$it1)
val_labels(Hon2018$conf) <- NULL#retira rótulos dos valores para uso do recode
Hon2018$ConfInt <- recode(Hon2018$conf, 0 <- c(4), 1 <- c(3), 2 <- c(2), 3 <- c(1))


#Escolaridade
Hon2018$Ed_sup <- cut(Hon2018$ed, c(0,15,19))
Hon2018$Ed_sup <- as.numeric(Hon2018$Ed_sup)

#Idade
val_labels(Hon2018$q2) <- NULL#retira rótulos dos valores para uso do recode
Hon2018$Idade <- Hon2018$q2

#Sexo
val_labels(Hon2018$q1) <- NULL#retira rótulos dos valores para uso do recode
Hon2018$Sexo <- recode(Hon2018$q1, 0 <- c(1), 1 <- c(2))

val_labels(Hon2018$it1) <- NULL
Hon2018$it1 <- as.numeric(Hon2018$it1)
Hon2018$Conf_int <- recode(Hon2018$it1, 0 <- 4, 1 <- 3, 2 <-2, 3 <- 1)

save(Hon2018, file = "Hon2018.RData")


###México
##2004
Mex2004 <- read_dta("Mex2004.dta")
Mex2004 <- remove_all_labels(Mex2004)
#Tolerância
Mex2004$TolerHomo <- Mex2004$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Mex2004$Denom <-  recode(Mex2004$q3,  1 <- 1, 2 <- 2, 3 <- 3, 4 <- 4)
Mex2004$Denom <- as.factor(Mex2004$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Mex2004$AtRelig <- recode(Mex2004$q4, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Mex2004, mean)
aggregate(TolerHomo ~ AtRelig, Mex2004, mean)
summary(Mex2004$TolerHomo)

###México
##2006
Mex2006 <- read_dta("Mex2006.dta")
Mex2006 <- remove_all_labels(Mex2006)
#Tolerância
Mex2006$TolerHomo <- Mex2006$D5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Mex2006$Denom <-  recode(Mex2006$Q3,  1 <- 1, 2 <- 5, 3 <- c(2,3), 4 <- 4)
Mex2006$Denom <- as.factor(Mex2006$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Mex2006, mean)
aggregate(TolerHomo ~ AtRelig, Mex2006, mean)
summary(Mex2006$TolerHomo)


###México
##2008
Mex2008 <- read_dta("Mex2008.dta")
Mex2008 <- remove_all_labels(Mex2008)
#Tolerância
Mex2008$TolerHomo <- Mex2008$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Mex2008$Denom <-  recode(Mex2008$q3,  1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Mex2008$Denom <- as.factor(Mex2008$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Mex2008$AtRelig <- recode(Mex2008$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Mex2008, mean)
aggregate(TolerHomo ~ AtRelig, Mex2008, mean)
summary(Mex2008$TolerHomo)

###México
##2010
Mex2010 <- read_dta("Mex2010.dta")
Mex2010 <- remove_all_labels(Mex2010)
#Tolerância
Mex2010$TolerHomo <- Mex2010$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Mex2010$Denom <-  recode(Mex2010$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Mex2010$Denom <- as.factor(Mex2010$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Mex2010$AtRelig <- recode(Mex2010$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Mex2010$IntRelig <- recode(Mex2010$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Mex2010, mean)
aggregate(TolerHomo ~ AtRelig, Mex2010, mean)
summary(Mex2010$TolerHomo)


###México
##2012
Mex2012 <- read_dta("Mex2012.dta")
Mex2012 <- remove_all_labels(Mex2012)
#Tolerância
Mex2012$TolerHomo <- Mex2012$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Mex2012$Denom <-  recode(Mex2012$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Mex2012$Denom <- as.factor(Mex2012$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Mex2012$AtRelig <- recode(Mex2012$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Mex2012$IntRelig <- recode(Mex2012$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Mex2012, mean)
aggregate(TolerHomo ~ AtRelig, Mex2012, mean)
summary(Mex2012$TolerHomo)

###México
##2014
Mex2014 <- read_dta("Mex2014.dta")
Mex2014 <- remove_all_labels(Mex2014)
#Tolerância
Mex2014$TolerHomo <- Mex2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Mex2014$Denom <-  recode(Mex2014$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Mex2014$Denom <- as.factor(Mex2014$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Mex2014$IntRelig <- recode(Mex2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Mex2014, mean)
aggregate(TolerHomo ~ AtRelig, Mex2014, mean)
summary(Mex2014$TolerHomo)


###México
##2016
Mex2016 <- read_dta("Mex2016.dta")
Mex2016 <- remove_all_labels(Mex2016)
#Tolerância
Mex2016$TolerHomo <- Mex2016$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Mex2016$Denom <-  recode(Mex2016$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Mex2016$Denom <- as.factor(Mex2016$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Mex2016$AtRelig <- recode(Mex2016$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Mex2016$IntRelig <- recode(Mex2016$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Mex2016, mean)
aggregate(TolerHomo ~ AtRelig, Mex2016, mean)
summary(Mex2016$TolerHomo)

###México
##2018
Mex2018 <- read_dta("Mex2018.dta")
Mex2018 <- remove_all_labels(Mex2018)
#Tolerância
Mex2018$TolerHomo <- Mex2018$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Mex2018$Denom <-  recode(Mex2018$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Mex2018$Denom <- as.factor(Mex2018$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Mex2018$AtRelig <- recode(Mex2018$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Mex2018$IntRelig <- recode(Mex2018$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Mex2018, mean)
aggregate(TolerHomo ~ AtRelig, Mex2018, mean)
summary(Mex2018$TolerHomo)

##########Condicionantes

#Adesão a democracia (churchiliana)
val_labels(Mex2018$ing4) <- NULL#retira rótulos dos valores para uso do recode
Mex2018$Dem <- Mex2018$ing4

#Confiança
Mex2018$conf <- as.numeric(Mex2018$it1)
val_labels(Mex2018$conf) <- NULL#retira rótulos dos valores para uso do recode
Mex2018$ConfInt <- recode(Mex2018$conf, 0 <- c(4), 1 <- c(3), 2 <- c(2), 3 <- c(1))

#Escolaridade
Mex2018$Ed_sup <- cut(Mex2018$ed, c(0,15,19))
Mex2018$Ed_sup <- as.numeric(Mex2018$Ed_sup)

#Idade
val_labels(Mex2018$q2) <- NULL#retira rótulos dos valores para uso do recode
Mex2018$Idade <- Mex2018$q2

#Sexo
val_labels(Mex2018$q1) <- NULL#retira rótulos dos valores para uso do recode
Mex2018$Sexo <- recode(Mex2018$q1, 0 <- c(1), 1 <- c(2))

val_labels(Mex2018$it1) <- NULL
Mex2018$it1 <- as.numeric(Mex2018$it1)
Mex2018$Conf_int <- recode(Mex2018$it1, 0 <- 4, 1 <- 3, 2 <-2, 3 <- 1)

save(Mex2018, file = "Mex2018.RData")

###Nicarágua
Nic2004 <- read_dta("Nic2004.dta")
Nic2004 <- remove_all_labels(Nic2004)
#Tolerância
Nic2004$TolerHomo <- Nic2004$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Nic2004$Denom <-  recode(Nic2004$q3,  1 <- 1, 2 <- 2, 3 <- 3, 4 <- 4)
Nic2004$Denom <- as.factor(Nic2004$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Nic2004$AtRelig <- recode(Nic2004$q4, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Nic2004, mean)
aggregate(TolerHomo ~ AtRelig, Nic2004, mean)
summary(Nic2004$TolerHomo)

###Nicarágua
##2006
Nic2006 <- read_dta("Nic2006.dta")
Nic2006 <- remove_all_labels(Nic2006)
#Tolerância
Nic2006$TolerHomo <- Nic2006$D5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Nic2006$Denom <-  recode(Nic2006$Q3,  1 <- 1, 2 <- 5, 3 <- c(2,3), 4 <- 4)
Nic2006$Denom <- as.factor(Nic2006$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Nic2006, mean)
aggregate(TolerHomo ~ AtRelig, Nic2006, mean)
summary(Nic2006$TolerHomo)

###Nicarágua
##2008
Nic2008 <- read_dta("Nic2008.dta")
Nic2008 <- remove_all_labels(Nic2008)
#Tolerância
Nic2008$TolerHomo <- Nic2008$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Nic2008$Denom <-  recode(Nic2008$q3,  1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Nic2008$Denom <- as.factor(Nic2008$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Nic2008$AtRelig <- recode(Nic2008$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Nic2008, mean)
aggregate(TolerHomo ~ AtRelig, Nic2008, mean)
summary(Nic2008$TolerHomo)

###Nicarágua
##2010
Nic2010 <- read_dta("Nic2010.dta")
Nic2010 <- remove_all_labels(Nic2010)
#Tolerância
Nic2010$TolerHomo <- Nic2010$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Nic2010$Denom <-  recode(Nic2010$q3c,  1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Nic2010$Denom <- as.factor(Nic2010$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Nic2010$AtRelig <- recode(Nic2010$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Nic2010, mean)
aggregate(TolerHomo ~ AtRelig, Nic2010, mean)
summary(Nic2010$TolerHomo)

###Nicarágua
##2012
Nic2012 <- read_dta("Nic2012.dta")
Nic2012 <- remove_all_labels(Nic2012)
#Tolerância
Nic2012$TolerHomo <- Nic2012$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Nic2012$Denom <-  recode(Nic2012$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Nic2012$Denom <- as.factor(Nic2012$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Nic2012$AtRelig <- recode(Nic2012$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Nic2012$IntRelig <- recode(Nic2012$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Nic2012, mean)
aggregate(TolerHomo ~ AtRelig, Nic2012, mean)
summary(Nic2012$TolerHomo)

###Nicarágua
##2014
Nic2014 <- read_dta("Nic2014.dta")
Nic2014 <- remove_all_labels(Nic2014)
#Tolerância
Nic2014$TolerHomo <- Nic2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Nic2014$Denom <-  recode(Nic2014$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Nic2014$Denom <- as.factor(Nic2014$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Nic2014$IntRelig <- recode(Nic2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Nic2014, mean)
aggregate(TolerHomo ~ AtRelig, Nic2014, mean)
summary(Nic2014$TolerHomo)

###Nicarágua
##2016
Nic2016 <- read_dta("Nic2016.dta")
Nic2016 <- remove_all_labels(Nic2016)
#Tolerância
Nic2016$TolerHomo <- Nic2016$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Nic2016$Denom <-  recode(Nic2016$q3c, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Nic2016$Denom <- as.factor(Nic2016$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Nic2016$AtRelig <- recode(Nic2016$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Nic2016$IntRelig <- recode(Nic2016$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Nic2016, mean)
aggregate(TolerHomo ~ AtRelig, Nic2016, mean)
summary(Nic2016$TolerHomo)

###Nicarágua
##2018
Nic2018 <- read_dta("Nic2018.dta")
Nic2018 <- remove_all_labels(Nic2018)
#Tolerância
Nic2018$TolerHomo <- Nic2018$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Nic2018$Denom <-  recode(Nic2018$q3cn, 1 <- 1, 2 <- 2, 3 <- c(3,5,6,7,10,12), 4 <-c(4,11))
Nic2018$Denom <- as.factor(Nic2018$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Nic2018$AtRelig <- recode(Nic2018$q5a, 0 <- 5, 1 <- 4, 2 <- 3, 3 <- 2, 4 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Nic2018$IntRelig <- recode(Nic2018$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Nic2018, mean)
aggregate(TolerHomo ~ AtRelig, Nic2018, mean)
summary(Nic2018$TolerHomo)

##########Condicionantes

#Adesão a democracia (churchiliana)
val_labels(Nic2018$ing4) <- NULL#retira rótulos dos valores para uso do recode
Nic2018$Dem <- Nic2018$ing4

#Confiança
Nic2018$conf <- as.numeric(Nic2018$it1)
val_labels(Nic2018$conf) <- NULL#retira rótulos dos valores para uso do recode
Nic2018$ConfInt <- recode(Nic2018$conf, 0 <- c(4), 1 <- c(3), 2 <- c(2), 3 <- c(1))


#Escolaridade
Nic2018$Ed_sup <- cut(Nic2018$ed, c(0,15,19))
Nic2018$Ed_sup <- as.numeric(Nic2018$Ed_sup)

#Idade
val_labels(Nic2018$q2) <- NULL#retira rótulos dos valores para uso do recode
Nic2018$Idade <- Nic2018$q2

#Sexo
val_labels(Nic2018$q1) <- NULL#retira rótulos dos valores para uso do recode
Nic2018$Sexo <- recode(Nic2018$q1, 0 <- c(1), 1 <- c(2))

val_labels(Nic2018$it1) <- NULL
Nic2018$it1 <- as.numeric(Nic2018$it1)
Nic2018$Conf_int <- recode(Nic2018$it1, 0 <- 4, 1 <- 3, 2 <-2, 3 <- 1)

save(Nic2018, file = "Nic2018.RData")

###Panamá
##2004
Pan2004 <- read_dta("Pan2004.dta")
Pan2004 <- remove_all_labels(Pan2004)
#Tolerância
Pan2004$TolerHomo <- Pan2004$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Pan2004$Denom <-  recode(Pan2004$q3,  1 <- 1, 2 <- 2, 3 <- 3, 4 <- 4)
Pan2004$Denom <- as.factor(Pan2004$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Pan2004$AtRelig <- recode(Pan2004$q4, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Pan2004, mean)
aggregate(TolerHomo ~ AtRelig, Pan2004, mean)
summary(Pan2004$TolerHomo)

###Panamá
##2006
Pan2006 <- read_dta("Pan2006.dta")
Pan2006 <- remove_all_labels(Pan2006)
#Tolerância
Pan2006$TolerHomo <- Pan2006$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Pan2006$Denom <-  recode(Pan2006$q3,  1 <- 1, 2 <- 5, 3 <- c(3,2), 4 <- 4)
Pan2006$Denom <- as.factor(Pan2006$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Pan2006, mean)
aggregate(TolerHomo ~ AtRelig, Pan2006, mean)
summary(Pan2006$TolerHomo)

###Panamá
##2008
Pan2008 <- read_dta("Pan2008.dta")
Pan2008 <- remove_all_labels(Pan2008)
#Tolerância
Pan2008$TolerHomo <- Pan2008$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Pan2008$Denom <-  recode(Pan2008$q3,  1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Pan2008$Denom <- as.factor(Pan2008$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Pan2008$AtRelig <- recode(Pan2008$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Pan2008, mean)
aggregate(TolerHomo ~ AtRelig, Pan2008, mean)
summary(Pan2008$TolerHomo)

###Panamá
##20010
Pan2010 <- read_dta("Pan2010.dta")
Pan2010 <- remove_all_labels(Pan2010)
#Tolerância
Pan2010$TolerHomo <- Pan2010$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Pan2010$Denom <-  recode(Pan2010$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,12), 4 <-c(4,11))
Pan2010$Denom <- as.factor(Pan2010$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Pan2010$AtRelig <- recode(Pan2010$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Pan2010$IntRelig <- recode(Pan2010$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Pan2010, mean)
aggregate(TolerHomo ~ AtRelig, Pan2010, mean)
summary(Pan2010$TolerHomo)

###Panamá
##2012
Pan2012 <- read_dta("Pan2012.dta")
Pan2012 <- remove_all_labels(Pan2012)
#Tolerância
Pan2012$TolerHomo <- Pan2012$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Pan2012$Denom <-  recode(Pan2012$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,12), 4 <-c(4,11))
Pan2012$Denom <- as.factor(Pan2012$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Pan2012$AtRelig <- recode(Pan2012$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Pan2012$IntRelig <- recode(Pan2012$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Pan2012, mean)
aggregate(TolerHomo ~ AtRelig, Pan2012, mean)
summary(Pan2012$TolerHomo)

###Panamá
##2014
Pan2014 <- read_dta("Pan2014.dta")
Pan2014 <- remove_all_labels(Pan2014)
#Tolerância
Pan2014$TolerHomo <- Pan2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Pan2014$Denom <-  recode(Pan2014$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,12), 4 <-c(4,11))
Pan2014$Denom <- as.factor(Pan2014$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Pan2014$IntRelig <- recode(Pan2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Pan2014, mean)
aggregate(TolerHomo ~ AtRelig, Pan2014, mean)
summary(Pan2014$TolerHomo)

###Panamá
##2016
Pan2016 <- read_dta("Pan2016.dta")
Pan2016 <- remove_all_labels(Pan2016)
#Tolerância
Pan2016$TolerHomo <- Pan2016$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Pan2016$Denom <-  recode(Pan2016$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,12), 4 <-c(4,11))
Pan2016$Denom <- as.factor(Pan2016$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Pan2016$AtRelig <- recode(Pan2016$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Pan2016$IntRelig <- recode(Pan2016$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Pan2016, mean)
aggregate(TolerHomo ~ AtRelig, Pan2016, mean)
summary(Pan2016$TolerHomo)

###Panamá
##2018
Pan2018 <- read_dta("Pan2018.dta")
Pan2018 <- remove_all_labels(Pan2018)
#Tolerância
Pan2018$TolerHomo <- Pan2018$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Pan2018$Denom <-  recode(Pan2018$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,7,11,77), 4 <-c(4))
Pan2018$Denom <- as.factor(Pan2018$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Pan2018$AtRelig <- recode(Pan2018$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Pan2018$IntRelig <- recode(Pan2018$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Pan2018, mean)
aggregate(TolerHomo ~ AtRelig, Pan2018, mean)
summary(Pan2018$TolerHomo)

##########Condicionantes

#Adesão a democracia (churchiliana)
val_labels(Pan2018$ing4) <- NULL#retira rótulos dos valores para uso do recode
Pan2018$Dem <- Pan2018$ing4

#Confiança
Pan2018$conf <- as.numeric(Pan2018$it1)
val_labels(Pan2018$conf) <- NULL#retira rótulos dos valores para uso do recode
Pan2018$ConfInt <- recode(Pan2018$conf, 0 <- c(4), 1 <- c(3), 2 <- c(2), 3 <- c(1))


#Escolaridade
Pan2018$Ed_sup <- cut(Pan2018$ed, c(0,15,19))
Pan2018$Ed_sup <- as.numeric(Pan2018$Ed_sup)

#Idade
val_labels(Pan2018$q2) <- NULL#retira rótulos dos valores para uso do recode
Pan2018$Idade <- Pan2018$q2

#Sexo
val_labels(Pan2018$q1) <- NULL#retira rótulos dos valores para uso do recode
Pan2018$Sexo <- recode(Pan2018$q1, 0 <- c(1), 1 <- c(2))

val_labels(Pan2018$it1) <- NULL
Pan2018$it1 <- as.numeric(Pan2018$it1)
Pan2018$Conf_int <- recode(Pan2018$it1, 0 <- 4, 1 <- 3, 2 <-2, 3 <- 1)

save(Pan2018, file = "Pan2018.RData")

###Paraguai
##2006
Par2006 <- read_dta("Par2006.dta")
Par2006 <- remove_all_labels(Par2006)
#Tolerância
Par2006$TolerHomo <- Par2006$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Par2006$Denom <-  recode(Par2006$q3, 1 <- 1, 2 <- 4, 3 <- c(2,3), 4 <- 5)
Par2006$Denom <- as.factor(Par2006$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Par2006, mean)
aggregate(TolerHomo ~ AtRelig, Par2006, mean)
summary(Par2006$TolerHomo)

###Paraguai
##2008
Par2008 <- read_dta("Par2008.dta")
Par2008 <- remove_all_labels(Par2008)
#Tolerância
Par2008$TolerHomo <- Par2008$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Par2008$Denom <-  recode(Par2008$q3, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Par2008$Denom <- as.factor(Par2008$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Par2008$AtRelig <- recode(Par2008$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Par2008, mean)
aggregate(TolerHomo ~ AtRelig, Par2008, mean)
summary(Par2008$TolerHomo)

###Paraguai
##2010
Par2010 <- read_dta("Par2010.dta")
Par2010 <- remove_all_labels(Par2010)
#Tolerância
Par2010$TolerHomo <- Par2010$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Par2010$Denom <-  recode(Par2010$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <-c(4))
Par2010$Denom <- as.factor(Par2010$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Par2010$AtRelig <- recode(Par2010$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Par2010$IntRelig <- recode(Par2010$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Par2010, mean)
aggregate(TolerHomo ~ AtRelig, Par2010, mean)
summary(Par2010$TolerHomo)

###Paraguai
##2012
Par2012 <- read_dta("Par2012.dta")
Par2012 <- remove_all_labels(Par2012)
#Tolerância
Par2012$TolerHomo <- Par2012$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Par2012$Denom <-  recode(Par2012$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <-c(4))
Par2012$Denom <- as.factor(Par2012$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Par2012$AtRelig <- recode(Par2012$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Par2012$IntRelig <- recode(Par2012$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Par2012, mean)
aggregate(TolerHomo ~ AtRelig, Par2012, mean)
summary(Par2012$TolerHomo)

###Paraguai
##2014
Par2014 <- read_dta("Par2014.dta")
Par2014 <- remove_all_labels(Par2014)
#Tolerância
Par2014$TolerHomo <- Par2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Par2014$Denom <-  recode(Par2014$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <-c(4))
Par2014$Denom <- as.factor(Par2014$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Par2014$IntRelig <- recode(Par2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Par2014, mean)
aggregate(TolerHomo ~ AtRelig, Par2014, mean)
summary(Par2014$TolerHomo)

###Paraguai
##2016
Par2016 <- read_dta("Par2016.dta")
Par2016 <- remove_all_labels(Par2016)
#Tolerância
Par2016$TolerHomo <- Par2016$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Par2016$Denom <-  recode(Par2016$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <-c(4))
Par2016$Denom <- as.factor(Par2016$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Par2016$AtRelig <- recode(Par2016$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Par2016$IntRelig <- recode(Par2016$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Par2016, mean)
aggregate(TolerHomo ~ AtRelig, Par2016, mean)
summary(Par2016$TolerHomo)

###Paraguai
##2018
Par2018 <- read_dta("Par2018.dta")
Par2018 <- remove_all_labels(Par2018)
#Tolerância
Par2018$TolerHomo <- Par2018$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Par2018$Denom <-  recode(Par2018$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,7,11,77), 4 <-c(4))
Par2018$Denom <- as.factor(Par2018$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Par2018$AtRelig <- recode(Par2018$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Par2018$IntRelig <- recode(Par2018$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Par2018, mean)
aggregate(TolerHomo ~ AtRelig, Par2018, mean)
summary(Par2018$TolerHomo)

##########Condicionantes

#Adesão a democracia (churchiliana)
val_labels(Par2018$ing4) <- NULL#retira rótulos dos valores para uso do recode
Par2018$Dem <- Par2018$ing4

#Confiança
Par2018$conf <- as.numeric(Par2018$it1)
val_labels(Par2018$conf) <- NULL#retira rótulos dos valores para uso do recode
Par2018$ConfInt <- recode(Par2018$conf, 0 <- c(4), 1 <- c(3), 2 <- c(2), 3 <- c(1))


#Escolaridade
Par2018$Ed_sup <- cut(Par2018$ed, c(0,15,19))
Par2018$Ed_sup <- as.numeric(Par2018$Ed_sup)

#Idade
val_labels(Par2018$q2) <- NULL#retira rótulos dos valores para uso do recode
Par2018$Idade <- Par2018$q2

#Sexo
val_labels(Par2018$q1) <- NULL#retira rótulos dos valores para uso do recode
Par2018$Sexo <- recode(Par2018$q1, 0 <- c(1), 1 <- c(2))

val_labels(Par2018$it1) <- NULL
Par2018$it1 <- as.numeric(Par2018$it1)
Par2018$Conf_int <- recode(Par2018$it1, 0 <- 4, 1 <- 3, 2 <-2, 3 <- 1)

save(Par2018, file = "Par2018.RData")

###Peru
##2006
Per2006 <- read_dta("Per2006.dta")
Per2006 <- remove_all_labels(Per2006)
#Tolerância
Per2006$TolerHomo <- Per2006$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Per2006$Denom <-  recode(Per2006$q3, 1 <- 1, 2 <- 5, 3 <- c(3,2), 4 <-c(4))
Per2006$Denom <- as.factor(Per2006$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Per2006, mean)
aggregate(TolerHomo ~ AtRelig, Per2006, mean)
summary(Per2006$TolerHomo)

###Peru
##2008
Per2008 <- read_dta("Per2008.dta")
Per2008 <- remove_all_labels(Per2008)
#Tolerância
Per2008$TolerHomo <- Per2008$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Per2008$Denom <-  recode(Per2008$q3, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <-c(4))
Per2008$Denom <- as.factor(Per2008$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Per2008$AtRelig <- recode(Per2008$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Per2008, mean)
aggregate(TolerHomo ~ AtRelig, Per2008, mean)
summary(Per2008$TolerHomo)


###Peru
##2010
Per2010 <- read_dta("Per2010.dta")
Per2010 <- remove_all_labels(Per2010)
#Tolerância
Per2010$TolerHomo <- Per2010$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Per2010$Denom <-  recode(Per2010$q3, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <-c(4))
Per2010$Denom <- as.factor(Per2010$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Per2010$AtRelig <- recode(Per2010$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Per2010$IntRelig <- recode(Per2010$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Per2010, mean)
aggregate(TolerHomo ~ AtRelig, Per2010, mean)
summary(Per2010$TolerHomo)


###Peru
##2012
Per2012 <- read_dta("Per2012.dta")
Per2012 <- remove_all_labels(Per2012)
#Tolerância
Per2012$TolerHomo <- Per2012$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Per2012$Denom <-  recode(Per2012$q3, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <-c(4))
Per2012$Denom <- as.factor(Per2012$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Per2012$AtRelig <- recode(Per2012$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Per2012$IntRelig <- recode(Per2012$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Per2012, mean)
aggregate(TolerHomo ~ AtRelig, Per2012, mean)
summary(Per2012$TolerHomo)

###Peru
##2014
Per2014 <- read_dta("Per2014.dta")
Per2014 <- remove_all_labels(Per2014)
#Tolerância
Per2014$TolerHomo <- Per2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Per2014$Denom <-  recode(Per2014$q3, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <-c(4))
Per2014$Denom <- as.factor(Per2014$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Per2014$AtRelig <- recode(Per2014$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Per2014, mean)
aggregate(TolerHomo ~ AtRelig, Per2014, mean)
summary(Per2014$TolerHomo)


###Peru
##2016
Per2016 <- read_dta("Per2016.dta")
Per2016 <- remove_all_labels(Per2016)
#Tolerância
Per2016$TolerHomo <- Per2016$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Per2016$Denom <-  recode(Per2016$q3, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12,77), 4 <-c(4))
Per2016$Denom <- as.factor(Per2016$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Per2016$AtRelig <- recode(Per2016$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Per2016$IntRelig <- recode(Per2016$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Per2016, mean)
aggregate(TolerHomo ~ AtRelig, Per2016, mean)
summary(Per2016$TolerHomo)


###Peru
##2018
Per2018 <- read_dta("Per2018.dta")
Per2018 <- remove_all_labels(Per2018)
#Tolerância
Per2018$TolerHomo <- Per2018$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Per2018$Denom <-  recode(Per2018$q3, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12,77), 4 <-c(4))
Per2018$Denom <- as.factor(Per2018$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Per2018$AtRelig <- recode(Per2018$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Per2018$IntRelig <- recode(Per2018$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Per2018, mean)
aggregate(TolerHomo ~ AtRelig, Per2018, mean)
summary(Per2018$TolerHomo)


##########Condicionantes

#Adesão a democracia (churchiliana)
val_labels(Per2018$ing4) <- NULL#retira rótulos dos valores para uso do recode
Per2018$Dem <- Per2018$ing4

#Confiança
Per2018$conf <- as.numeric(Per2018$it1)
val_labels(Per2018$conf) <- NULL#retira rótulos dos valores para uso do recode
Per2018$ConfInt <- recode(Per2018$conf, 0 <- c(4), 1 <- c(3), 2 <- c(2), 3 <- c(1))


#Escolaridade
Per2018$Ed_sup <- cut(Per2018$ed, c(0,15,19))
Per2018$Ed_sup <- as.numeric(Per2018$Ed_sup)

#Idade
val_labels(Per2018$q2) <- NULL#retira rótulos dos valores para uso do recode
Per2018$Idade <- Per2018$q2

#Sexo
val_labels(Per2018$q1) <- NULL#retira rótulos dos valores para uso do recode
Per2018$Sexo <- recode(Per2018$q1, 0 <- c(1), 1 <- c(2))

val_labels(Per2018$it1) <- NULL
Per2018$it1 <- as.numeric(Per2018$it1)
Per2018$Conf_int <- recode(Per2018$it1, 0 <- 4, 1 <- 3, 2 <-2, 3 <- 1)

save(Per2018, file = "Per2018.RData")

###República Dominicana
##2004
Rep2004 <- read_dta("Rep2004.dta")
Rep2004 <- remove_all_labels(Rep2004)
#Tolerância
Rep2004$TolerHomo <- Rep2004$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Rep2004$Denom <-  recode(Rep2004$q3, 1 <- 1, 2 <- 2, 4 <- 4)
Rep2004$Denom <- as.factor(Rep2004$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Rep2004, mean)
aggregate(TolerHomo ~ AtRelig, Rep2004, mean)
summary(Rep2004$TolerHomo)

###República Dominicana
##2006
Rep2006 <- read_dta("Rep2006.dta")
Rep2006 <- remove_all_labels(Rep2006)
#Tolerância
Rep2006$TolerHomo <- Rep2006$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Rep2006$Denom <-  recode(Rep2006$q3, 1 <- 1, 2 <- 5, 3 <- 3, 4 <- 4)
Rep2006$Denom <- as.factor(Rep2006$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Rep2006, mean)
aggregate(TolerHomo ~ AtRelig, Rep2006, mean)
summary(Rep2006$TolerHomo)

###República Dominicana
##2008
Rep2008 <- read_dta("Rep2008.dta")
Rep2008 <- remove_all_labels(Rep2008)
#Tolerância
Rep2008$TolerHomo <- Rep2008$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Rep2008$Denom <-  recode(Rep2008$q3, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7), 4 <- 4)
Rep2008$Denom <- as.factor(Rep2008$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Rep2008$AtRelig <- recode(Rep2008$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Rep2008, mean)
aggregate(TolerHomo ~ AtRelig, Rep2008, mean)
summary(Rep2008$TolerHomo)

###República Dominicana
##2010
Rep2010 <- read_dta("Rep2010.dta")
Rep2010 <- remove_all_labels(Rep2010)
#Tolerância
Rep2010$TolerHomo <- Rep2010$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Rep2010$Denom <-  recode(Rep2010$q3c,  1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <-c(4))
Rep2010$Denom <- as.factor(Rep2010$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Rep2010$AtRelig <- recode(Rep2010$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Rep2010$IntRelig <- recode(Rep2010$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Rep2010, mean)
aggregate(TolerHomo ~ AtRelig, Rep2010, mean)
summary(Rep2010$TolerHomo)

###República Dominicana
##2012
Rep2012 <- read_dta("Rep2012.dta")
Rep2012 <- remove_all_labels(Rep2012)
#Tolerância
Rep2012$TolerHomo <- Rep2012$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Rep2012$Denom <-  recode(Rep2012$q3c,  1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <-c(4))
Rep2012$Denom <- as.factor(Rep2012$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Rep2012$AtRelig <- recode(Rep2012$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Rep2012$IntRelig <- recode(Rep2012$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Rep2012, mean)
aggregate(TolerHomo ~ AtRelig, Rep2012, mean)
summary(Rep2012$TolerHomo)

###República Dominicana
##2014
Rep2014 <- read_dta("Rep2014.dta")
Rep2014 <- remove_all_labels(Rep2014)
#Tolerância
Rep2014$TolerHomo <- Rep2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Rep2014$Denom <-  recode(Rep2014$q3c,  1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <-c(4))
Rep2014$Denom <- as.factor(Rep2014$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Rep2014$IntRelig <- recode(Rep2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Rep2014, mean)
aggregate(TolerHomo ~ AtRelig, Rep2014, mean)
summary(Rep2014$TolerHomo)

###República Dominicana
##2016
Rep2016 <- read_dta("Rep2016.dta")
Rep2016 <- remove_all_labels(Rep2016)
#Tolerância
Rep2016$TolerHomo <- Rep2016$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Rep2016$Denom <-  recode(Rep2016$q3c,  1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <-c(4))
Rep2016$Denom <- as.factor(Rep2016$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Rep2016$AtRelig <- recode(Rep2016$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Rep2016$IntRelig <- recode(Rep2016$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Rep2016, mean)
aggregate(TolerHomo ~ AtRelig, Rep2016, mean)
summary(Rep2016$TolerHomo)

###República Dominicana
##2018
Rep2018 <- read_dta("Rep2018.dta")
Rep2018 <- remove_all_labels(Rep2018)
#Tolerância
Rep2018$TolerHomo <- Rep2018$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Rep2018$Denom <-  recode(Rep2018$q3c,  1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <-c(4))
Rep2018$Denom <- as.factor(Rep2018$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Rep2018$AtRelig <- recode(Rep2018$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Rep2018$IntRelig <- recode(Rep2018$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Rep2018, mean)
aggregate(TolerHomo ~ AtRelig, Rep2018, mean)
summary(Rep2018$TolerHomo)

##########Condicionantes

#Adesão a democracia (churchiliana)
val_labels(Rep2018$ing4) <- NULL#retira rótulos dos valores para uso do recode
Rep2018$Dem <- Rep2018$ing4

#Confiança
Rep2018$conf <- as.numeric(Rep2018$it1)
val_labels(Rep2018$conf) <- NULL#retira rótulos dos valores para uso do recode
Rep2018$ConfInt <- recode(Rep2018$conf, 0 <- c(4), 1 <- c(3), 2 <- c(2), 3 <- c(1))


#Escolaridade
Rep2018$Ed_sup <- cut(Rep2018$ed, c(0,15,19))
Rep2018$Ed_sup <- as.numeric(Rep2018$Ed_sup)

#Idade
val_labels(Rep2018$q2) <- NULL#retira rótulos dos valores para uso do recode
Rep2018$Idade <- Rep2018$q2

#Sexo
val_labels(Rep2018$q1) <- NULL#retira rótulos dos valores para uso do recode
Rep2018$Sexo <- recode(Rep2018$q1, 0 <- c(1), 1 <- c(2))

val_labels(Rep2018$it1) <- NULL
Rep2018$it1 <- as.numeric(Rep2018$it1)
Rep2018$Conf_int <- recode(Rep2018$it1, 0 <- 4, 1 <- 3, 2 <-2, 3 <- 1)

save(Rep2018, file = "Rep2018.RData")

###Uruguai
##2006
Uru2006 <- read_dta("Uru2006.dta")
Uru2006 <- remove_all_labels(Uru2006)
#Tolerância
Uru2006$TolerHomo <- Uru2006$D5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Uru2006$Denom <-  recode(Uru2006$Q3,  1 <- 1, 2 <- 5, 3 <- c(2,3,6,7), 4 <- 4)
Uru2006$Denom <- as.factor(Uru2006$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Uru2006, mean)
aggregate(TolerHomo ~ AtRelig, Uru2006, mean)
summary(Uru2006$TolerHomo)

###Uruguai
##2008
Uru2008 <- read_dta("Uru2008.dta")
Uru2008 <- remove_all_labels(Uru2008)
#Tolerância
Uru2008$TolerHomo <- Uru2008$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Uru2008$Denom <-  recode(Uru2008$q3,  1 <- 1, 2 <- 5, 3 <- c(2,3,6,7), 4 <- 4)
Uru2008$Denom <- as.factor(Uru2008$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Uru2008$AtRelig <- recode(Uru2008$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Uru2008, mean)
aggregate(TolerHomo ~ AtRelig, Uru2008, mean)
summary(Uru2008$TolerHomo)

###Uruguai
##2010
Uru2010 <- read_dta("Uru2010.dta")
Uru2010 <- remove_all_labels(Uru2010)
#Tolerância
Uru2010$TolerHomo <- Uru2010$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Uru2010$Denom <-  recode(Uru2010$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <-c(4))
Uru2010$Denom <- as.factor(Uru2010$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Uru2010$AtRelig <- recode(Uru2010$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Uru2010$IntRelig <- recode(Uru2010$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Uru2010, mean)
aggregate(TolerHomo ~ AtRelig, Uru2010, mean)
summary(Uru2010$TolerHomo)

###Uruguai
##2012
Uru2012 <- read_dta("Uru2012.dta")
Uru2012 <- remove_all_labels(Uru2012)
#Tolerância
Uru2012$TolerHomo <- Uru2012$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Uru2012$Denom <-  recode(Uru2012$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <-c(4))
Uru2012$Denom <- as.factor(Uru2012$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Uru2012$AtRelig <- recode(Uru2012$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Uru2012$IntRelig <- recode(Uru2012$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Uru2012, mean)
aggregate(TolerHomo ~ AtRelig, Uru2012, mean)
summary(Uru2012$TolerHomo)

###Uruguai
##2014
Uru2014 <- read_dta("Uru2014.dta")
Uru2014 <- remove_all_labels(Uru2014)
#Tolerância
Uru2014$TolerHomo <- Uru2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Uru2014$Denom <-  recode(Uru2014$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <-c(4))
Uru2014$Denom <- as.factor(Uru2014$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Uru2014$IntRelig <- recode(Uru2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Uru2014, mean)
aggregate(TolerHomo ~ AtRelig, Uru2014, mean)
summary(Uru2014$TolerHomo)

###Uruguai
##2016
Uru2016 <- read_dta("Uru2016.dta")
Uru2016 <- remove_all_labels(Uru2016)
#Tolerância
Uru2016$TolerHomo <- Uru2016$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Uru2016$Denom <-  recode(Uru2016$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <-c(4))
Uru2016$Denom <- as.factor(Uru2016$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Uru2016$AtRelig <- recode(Uru2016$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Uru2016$IntRelig <- recode(Uru2016$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Uru2016, mean)
aggregate(TolerHomo ~ AtRelig, Uru2016, mean)
summary(Uru2016$TolerHomo)

###Uruguai
##2018
Uru2018 <- read_dta("Uru2018.dta")
Uru2018 <- remove_all_labels(Uru2018)
#Tolerância
Uru2018$TolerHomo <- Uru2018$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Uru2018$Denom <-  recode(Uru2018$q3c, 1 <- 1, 2 <- c(2,5), 3 <- c(3,7,11,77), 4 <-c(4))
Uru2018$Denom <- as.factor(Uru2018$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Uru2018$AtRelig <- recode(Uru2018$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Uru2018$IntRelig <- recode(Uru2018$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Uru2018, mean)
aggregate(TolerHomo ~ AtRelig, Uru2018, mean)
summary(Uru2018$TolerHomo)

##########Condicionantes

#Adesão a democracia (churchiliana)
val_labels(Uru2018$ing4) <- NULL#retira rótulos dos valores para uso do recode
Uru2018$Dem <- Uru2018$ing4

#Confiança
Uru2018$conf <- as.numeric(Uru2018$it1)
val_labels(Uru2018$conf) <- NULL#retira rótulos dos valores para uso do recode
Uru2018$ConfInt <- recode(Uru2018$conf, 0 <- c(4), 1 <- c(3), 2 <- c(2), 3 <- c(1))


#Escolaridade
Uru2018$Ed_sup <- cut(Uru2018$ed, c(0,15,19))
Uru2018$Ed_sup <- as.numeric(Uru2018$Ed_sup)

#Idade
val_labels(Uru2018$q2) <- NULL#retira rótulos dos valores para uso do recode
Uru2018$Idade <- Uru2018$q2

#Sexo
val_labels(Uru2018$q1) <- NULL#retira rótulos dos valores para uso do recode
Uru2018$Sexo <- recode(Uru2018$q1, 0 <- c(1), 1 <- c(2))

val_labels(Uru2018$it1) <- NULL
Uru2018$it1 <- as.numeric(Uru2018$it1)
Uru2018$Conf_int <- recode(Uru2018$it1, 0 <- 4, 1 <- 3, 2 <-2, 3 <- 1)

save(Uru2018, file = "Uru2018.RData")


###Venezuela
##2007
Ven2007 <- read_dta("Ven2007.dta")
Ven2007 <- remove_all_labels(Ven2007)
#Tolerância
Ven2007$TolerHomo <- Ven2007$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Ven2007$Denom <-  recode(Ven2007$q3, 1 <- 1, 2 <- 5, 3 <- c(2,3,6,7), 4 <-c(4))
Ven2007$Denom <- as.factor(Ven2007$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (NA)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Ven2007, mean)
aggregate(TolerHomo ~ AtRelig, Ven2007, mean)
summary(Ven2007$TolerHomo)

###Venezuela
##2008
Ven2008 <- read_dta("Ven2008.dta")
Ven2008 <- remove_all_labels(Ven2008)
#Tolerância
Ven2008$TolerHomo <- Ven2008$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Ven2008$Denom <-  recode(Ven2008$q3, 1 <- 1, 2 <- 5, 3 <- c(2,3,6,7), 4 <-c(4))
Ven2008$Denom <- as.factor(Ven2008$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Ven2008$AtRelig <- recode(Ven2008$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Ven2008$IntRelig <- recode(Ven2008$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Ven2008, mean)
aggregate(TolerHomo ~ AtRelig, Ven2008, mean)
summary(Ven2008$TolerHomo)

###Venezuela
##2010
Ven2010 <- read_dta("Ven2010.dta")
Ven2010 <- remove_all_labels(Ven2010)
#Tolerância
Ven2010$TolerHomo <- Ven2010$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Ven2010$Denom <-  recode(Ven2010$q3, 1 <- 1, 2 <- 5, 3 <- c(2,3,6,7), 4 <-c(4))
Ven2010$Denom <- as.factor(Ven2010$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Ven2010$AtRelig <- recode(Ven2010$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Ven2010$IntRelig <- recode(Ven2010$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Ven2010, mean)
aggregate(TolerHomo ~ AtRelig, Ven2010, mean)
summary(Ven2010$TolerHomo)

###Venezuela
##2012
Ven2012 <- read_dta("Ven2012.dta")
Ven2012 <- remove_all_labels(Ven2012)
#Tolerância
Ven2012$TolerHomo <- Ven2012$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Ven2012$Denom <-  recode(Ven2012$q3, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <-c(4))
Ven2012$Denom <- as.factor(Ven2012$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Ven2012$AtRelig <- recode(Ven2012$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Ven2012$IntRelig <- recode(Ven2012$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Ven2012, mean)
aggregate(TolerHomo ~ AtRelig, Ven2012, mean)
summary(Ven2012$TolerHomo)

###Venezuela
##2014
Ven2014 <- read_dta("Ven2014.dta")
Ven2014 <- remove_all_labels(Ven2014)
#Tolerância
Ven2014$TolerHomo <- Ven2014$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Ven2014$Denom <-  recode(Ven2014$q3, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <-c(4))
Ven2014$Denom <- as.factor(Ven2014$Denom)
#Ativismo Religioso (NA)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Ven2014$IntRelig <- recode(Ven2014$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Ven2014, mean)
aggregate(TolerHomo ~ AtRelig, Ven2014, mean)
summary(Ven2014$TolerHomo)

###Venezuela
##2016
Ven2016 <- read_dta("Ven2016.dta")
Ven2016 <- remove_all_labels(Ven2016)
#Tolerância
Ven2016$TolerHomo <- Ven2016$d5
#Denominação Religiosao (catolico=1, evangelico/protestante=2, outras=3 e ateu/agnóstico=4)
Ven2016$Denom <-  recode(Ven2016$q3, 1 <- 1, 2 <- c(2,5), 3 <- c(3,6,7,10,11,12), 4 <-c(4))
Ven2016$Denom <- as.factor(Ven2016$Denom)
#Ativismo Religioso (0=Nunca, 1=1 ou 2 vezes ao ano, 2=1 vez ao mês, 3=1 vez por semana e 4=mais de 1 vez por semana)
Ven2016$AtRelig <- recode(Ven2016$q5a, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Intensidade Religiosa (0=Nada importante, 1=pouco importante, 2=algo importante e 3=muito importante)
Ven2016$IntRelig <- recode(Ven2016$q5b, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Médias por Grupo 
aggregate(TolerHomo ~ Denom, Ven2016, mean)
aggregate(TolerHomo ~ AtRelig, Ven2016, mean)
summary(Ven2016$TolerHomo)

##########Condicionantes

#Adesão a democracia (churchiliana)
val_labels(Ven2016$ing4) <- NULL#retira rótulos dos valores para uso do recode
Ven2016$Dem <- Ven2016$ing4

#Confiança
Ven2016$conf <- as.numeric(Ven2016$it1)
val_labels(Ven2016$conf) <- NULL#retira rótulos dos valores para uso do recode
Ven2016$ConfInt <- recode(Ven2016$conf, 0 <- c(4), 1 <- c(3), 2 <- c(2), 3 <- c(1))


#Escolaridade
Ven2016$Ed_sup <- cut(Ven2016$ed, c(0,15,19))
Ven2016$Ed_sup <- as.numeric(Ven2016$Ed_sup)

table(BancoLAPOPCompleto$Ed_sup)
table(BancoLAPOPCompleto$ed2)

#Idade
val_labels(Ven2016$q2) <- NULL#retira rótulos dos valores para uso do recode
Ven2016$Idade <- Ven2016$q2

#Sexo
val_labels(Ven2016$q1) <- NULL#retira rótulos dos valores para uso do recode
# 1 é mulher
Ven2016$Sexo <- recode(Ven2016$q1, 0 <- c(1), 1 <- c(2))

val_labels(Ven2016$it1) <- NULL
Ven2016$it1 <- as.numeric(Ven2016$it1)
Ven2016$Conf_int <- recode(Ven2016$it1, 0 <- 4, 1 <- 3, 2 <-2, 3 <- 1)

save(Ven2016, file = "Ven2016.RData")


##Banco <- merge(Banco 1, Banco2, all = T)
Banco <- merge(Arg2019, Bol2019, all = T)
Banco1 <- merge(Banco, Bra2019, all = T)
Banco2 <- merge(Banco1, Cos2018, all = T)
Banco3 <- merge(Banco2, Els2018, all = T)
Banco4 <- merge(Banco3, Equ2018, all=T)
Banco5 <- merge(Banco4, Gua2018, all = T)
Banco6 <- merge(Banco5, Hai2016, all = T)
Banco7 <- merge(Banco6, Hon2018, all = T)
Banco8 <- merge(Banco7, Mex2018, all = T)
Banco9 <- merge(Banco8, Nic2018, all = T)
Banco10 <- merge(Banco9, Pan2018, all = T)
Banco11 <- merge(Banco10, Par2018, all = T)
Banco12 <- merge(Banco11, Chi2018, all = T)
Banco13 <- merge(Banco12, Per2018, all = T )
Banco14 <- merge(Banco13, Rep2018, all =  T)
Banco15 <- merge(Banco14, Uru2018, all = T)
BancoLAPOPCompleto <- merge(Banco15, Ven2016, all = T)


#Recodifiquei a variável Denominação religiosa, para que os ateus fiquem como grupo de rerência
# Ficou: 1- Ateus, 2- Católicos, 3- Evangélicos e 4- Outras religiões
BancoLAPOPCompleto$Denom <-  recode(BancoLAPOPCompleto$Denom,
                                    1 <- 4, 2 <- 1, 3 <- 2, 4 <- 3)
save(BancoLAPOPCompleto, file = "BancoLAPOPCompleto.RData")


BancoLAPOPCompleto <- merge(BancoLAPOPCompleto, DadosMacro, by= "pais")

save(BancoLAPOPCompleto, file = "BancoLAPOPCompleto.RData")

#Nível macro:
#O nível 2 é composto por 10 variáveis, mas somente 2 apresentaram relação estatísticamente 
#significante com as variáveis de nível 1, quais sejam: TNaH e Desemprego.
#A variável que rodamos para verificar nossa hipótese de pesquisa é a TNaH
#O Índice de Tolerância Nacional aos Homossexuais é medida em uma escala de 0 a 2
#Em que 0 representa os países que não possuem nenhum direito legalizado
# 1 os países que possuem só o casamento legalizado e 2 países que possuem casamento 
# e adoção legalizados.

