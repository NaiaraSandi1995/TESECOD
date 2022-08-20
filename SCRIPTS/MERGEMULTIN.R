#Os bancos do arquivo "Bancos da tese" já estão recodificados
#Usando esses bancos fazemos o merge

#MERGE####
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

BancoGeral <- merge(Banco15, Banco16, all=T)
save(BancoGeral, file= "BancoGeral.RData")

BancoGeral2 <- merge(BancoGeral, BancoMacro, all = T )

table(BancoGeral2$pais)
# 1    2    3    4    5    6    7    9   10   11   12   13   14   15 
# 1580 1596 1511 1560 1547 1501 1559 1533 1682 1521 1515 1638 1581 1498 
# 16   17   21   22 
# 1558 1528 1516 2221 
table(BancoGeral2$Denom)

# 1     2     3     4 
# 15381  5041  3260  3642

save(BancoGeral2, file= "BancoGeral2.RData")


#TeseModelosFinais####
#Análise multinível usando no nível 2 as variáveis TNaH
#PIBper capita e a Média de Tolerância aos Homossexuais do ano de 
#2008 e 2009


library(sjPlot)
library(multilevel)#ativa o pacote para execução do modelo ANOVA

#Banco contendo o nível 1 e 2: BancoGeral2

options(scipen = 999)

##ModeloNULO####
#TolerHomo


Null.Model <- lme(TolerHomo~1, random = ~1|pais, data = BancoGeral2,
                  control = list(opt="optim"), na.action=na.omit)
Null.Model
#Linear mixed-effects model fit by REML
#Data: BancoFinal1 
#Log-restricted-likelihood: -71576.67
#Fixed: TolerHomo ~ 1 
#(Intercept) 
#5.122385 

#Random effects:
#  Formula: ~1 | pais
#           (Intercept) Residual
#StdDev:    1.463603 3.133621

#Number of Observations: 27927
#Number of Groups: 18


summary(Null.Model)
#Linear mixed-effects model fit by REML
#Data: BancoFinal1 
#AIC      BIC        logLik
#143159.3 143184.1 -71576.67

#Random effects:
#  Formula: ~1 | pais
#           (Intercept) Residual
#StdDev:    1.463603 3.133621

#Fixed effects:  TolerHomo ~ 1 
#              Value  Std.Error    DF  t-value p-value
#(Intercept) 5.122385 0.3454875 27909 14.82654       0

#Standardized Within-Group Residuals:
#  Min         Q1        Med         Q3        Max 
#-2.1224434 -0.9112838 -0.0454174  0.8866034  2.5884081 

#Number of Observations: 27927
#Number of Groups: 18 

##ICC####

VarCorr(Null.Model)

#pais = pdLogChol(1) 
#          Variance StdDev  
#(Intercept) 2.142135 1.463603
#Residual    9.819581 3.133621
2.142135/(2.142135+9.819581) # A partir desses dois valores deve<-se calcular o Coeficiente
#de Correlação Intraclasse
#[1] 0.1790826 o resultado indica que 17,9% da variação em nossa variável dependente se deve a fatores 
# que se localizam no nível dos países, o que justifica o emprego da modelagem multinível.
# Para determinar se a variância do intercepto é significativamente diferente de 0 podemos comparar 
#os valores de <-2 log likelihood do modelo com interceptos aleatórios e do modelo sem essa randomização. 

##SemVariação####
#####Estamos criando um modelo sem variação de nível 2
Null.gls<-gls(TolerHomo ~1,data=BancoGeral2, control=list(opt="optim"),
              na.action=na.omit)
logLik(Null.gls)*-2
#'log Lik.' 148741  (df=2)
logLik(Null.Model)*-2
#'log Lik.' 143153.3 (df=3)
y= 148741 - 143153.3
y
y = 5587.7
#5587.7 é a diferença entre os dois valores, que é significativamente em uma distribuição de 
#qui<-quadrado com 1 grau de liberdade, ou seja, indica significativa variação do intercepto.
anova(Null.gls, Null.Model)
#Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#Null.gls       1  2 148745.0 148761.5 -74370.50                        
#Null.Model     2  3 143159.3 143184.0 -71576.67 1 vs 2 5587.667  <.0001

##1ºTESTE*####
#Modelo de nível 1 
Model1.Tol <- lme(TolerHomo~ Denom + AtRelig + IntRelig + Dem + ConfInt +
                    Ed_sup + Idade + Sexo,
                  random = ~1|pais, data = BancoGeral2, na.action=na.omit,
                  control = list(opt="optim"))

View(Model1.Tol)

#Para gerar tabelas:
Model1.Tol <- lm(TolerHomo~ Denom + AtRelig + IntRelig + Dem + ConfInt + 
                   Ed_sup + Idade + Sexo,
                 random = ~1|pais, data = BancoGeral2, na.action=na.omit,
                 control = list(opt="optim"))

#Para visualizar a saída do modelo, com o sjPlot
tab_model(Model1.Tol, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60,
          p.style ="stars")

summary(Model1.Tol) #Esse é o summary do primeiro modelo gerado
Model1.Tol

#Linear mixed-effects model fit by REML
#Data: BancoFinal1 
#AIC      BIC    logLik
#108135.6 108239.3 -54054.79

#Random effects:
#  Formula: ~1 | pais
#(Intercept) Residual
#StdDev:    1.240673 2.969409

#Fixed effects: TolerHomo ~ Denom + AtRelig + IntRelig + Dem + ConfInt + Ed_sup +      Idade + Sexo 
#Value Std.Error    DF    t-value p-value
#(Intercept)  4.511916 0.3201211 21503  14.094405  0.0000
#Denom2       0.197557 0.0772502 21503   2.557358  0.0106
#Denom3      -0.827761 0.0903940 21503  -9.157254  0.0000
#Denom4      -0.461468 0.1010168 21503  -4.568225  0.0000
#AtRelig     -0.211465 0.0184439 21503 -11.465293  0.0000
#IntRelig    -0.284674 0.0308735 21503  -9.220688  0.0000
#Dem          0.197846 0.0121340 21503  16.305073  0.0000
#ConfInt      0.241486 0.0216871 21503  11.135006  0.0000
#Ed_sup       0.972012 0.0632091 21503  15.377727  0.0000
#Idade       -0.023777 0.0013074 21503 -18.186614  0.0000
#Sexo         0.520143 0.0411438 21503  12.642054  0.0000
#Correlation: 
#  (Intr) Denom2 Denom3 Denom4 AtRelg IntRlg Dem    CnfInt Ed_sup Idade 
#Denom2   -0.107                                                               
#Denom3   -0.102  0.787                                                        
#Denom4   -0.093  0.690  0.630                                                 
#AtRelig   0.044 -0.163 -0.282 -0.243                                          
#IntRelig -0.149 -0.292 -0.272 -0.221 -0.271                                   
#Dem      -0.143 -0.003 -0.003  0.006 -0.004 -0.005                            
#ConfInt  -0.078 -0.013  0.013  0.003 -0.032 -0.018 -0.091                     
#Ed_sup   -0.211 -0.009  0.021  0.015 -0.018  0.047 -0.072 -0.068              
#Idade    -0.119 -0.054  0.015  0.001 -0.114 -0.063 -0.071  0.005  0.002       
#Sexo     -0.061 -0.025 -0.007 -0.012 -0.092 -0.058  0.037  0.079 -0.004  0.056

#Standardized Within-Group Residuals:
#  Min          Q1         Med          Q3         Max 
#-2.65934656 -0.80632700 -0.06671797  0.77748374  3.19683989 

#Number of Observations: 21531
#Number of Groups: 18

##Calculo do rendimento do teste####
C <- 1.240673
N <- 1.463603
x= C/N
x
x <- 0.8476841
y= 1 - x
y
y = 0.1523159

VarCorr(Model1.Tol)
#pais = pdLogChol(1) 
#Variance StdDev  
#(Intercept) 1.539269 1.240673
#Residual    8.817391 2.969409

##2ºTESTE####
# Modelo com inclinações variaveis
#Modelo 1 
Model.1.Aleatorio.Denom<-lme(TolerHomo~ Denom + AtRelig + IntRelig + Dem + ConfInt 
                             + Ed_sup + Idade + Sexo, random=~Denom|pais, 
                             data=BancoGeral2, na.action=na.omit, control=list(opt="optim"))

summary(Model.1.Aleatorio.Denom)


tab_model(Model.1.Aleatorio.Denom, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, p.style ="stars")

Model.1.Aleatorio.DenomA <-update(Model.1.Aleatorio.Denom,random=~1|pais)

tab_model(Model.1.Aleatorio.DenomA, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, p.style ="stars")

anova(Model.1.Aleatorio.Denom,Model.1.Aleatorio.DenomA)
#                            Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#Model.1.Aleatorio.Denom      1 22 108116.4 108291.9 -54036.20                        
#Model.1.Aleatorio.DenomA     2 13 108135.6 108239.3 -54054.79 1 vs 2 37.17739  <.0001

##3ºTESTE####
#Modelo 2
Model.2.Aleatorio.AtRelig<-lme(TolerHomo~ Denom + AtRelig + IntRelig + Dem + ConfInt 
                               + Ed_sup + Idade + Sexo, random=~AtRelig|pais, 
                               data=BancoGeral2, na.action=na.omit,
                               control=list(opt="optim"))

summary(Model.2.Aleatorio.AtRelig)

Model.2.Aleatorio.AtReligA <-update(Model.2.Aleatorio.AtRelig,random=~1|pais)
anova(Model.2.Aleatorio.AtReligA,Model.2.Aleatorio.AtRelig)

#                           Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#Model.2.Aleatorio.AtReligA     1 13 108135.6 108239.3 -54054.79                        
#Model.2.Aleatorio.AtRelig      2 15 108109.7 108229.3 -54039.84 1 vs 2 29.89982  <.0001

##4ºTESTE####
#Modelo 3
Model.3.Aleatorio.IntRelig<-lme(TolerHomo~ Denom + AtRelig + IntRelig + Dem + ConfInt 
                                + Ed_sup + Idade + Sexo, random=~IntRelig|pais, 
                                data=BancoGeral2, na.action=na.omit, control=list(opt="optim"))

summary(Model.3.Aleatorio.IntRelig)
Model.3.Aleatorio.IntReligA <-update(Model.3.Aleatorio.IntRelig,random=~1|pais)
anova(Model.3.Aleatorio.IntReligA,Model.3.Aleatorio.IntRelig)

#Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#Model.3.Aleatorio.IntReligA     1 13 108135.6 108239.3 -54054.79                        
#Model.3.Aleatorio.IntRelig      2 15 108128.9 108248.5 -54049.44 1 vs 2 10.68332  0.0048


## ####
#####
#####
##MUDANÇA####

##5ºTESTE*####
BancoGeral2$TNaH
BancoGeral2$MedToler
BancoGeral2$PIBpc
#modelo completo (Aqui coloco todas as variáveis de Nível 2)
BancoGeral2$MedToler <- as.numeric(BancoGeral2$MedToler)

Model.Comp1 <-lme(TolerHomo~ Denom + AtRelig + IntRelig + Dem + ConfInt 
                  + Ed_sup + Idade + Sexo + TNaH + MedToler +
                    PIBpc, random=~1|pais, 
                  data=BancoGeral2, na.action=na.omit, control=list(opt="optim"))


summary(Model.Comp1)
# Linear mixed-effects model fit by REML
# Data: BancoGeral2 
# AIC      BIC    logLik
# 108138.8 108266.4 -54053.39
# 
# Random effects:
#   Formula: ~1 | pais
# (Intercept) Residual
# StdDev:   0.6783962 2.969408
# 
# Fixed effects:  TolerHomo ~ Denom + AtRelig + IntRelig + Dem + ConfInt + Ed_sup +      Idade + Sexo + TNaH + MedToler + PIBpc 
# Value Std.Error    DF    t-value p-value
# (Intercept)  0.8111601 0.9842485 21503   0.824142  0.4099
# Denom2      -1.0278731 0.0561336 21503 -18.311182  0.0000
# Denom3      -0.6575024 0.0734177 21503  -8.955635  0.0000
# Denom4      -0.1941201 0.0772336 21503  -2.513415  0.0120
# AtRelig     -0.2096161 0.0184388 21503 -11.368205  0.0000
# IntRelig    -0.2837393 0.0308659 21503  -9.192653  0.0000
# Dem          0.1975102 0.0121341 21503  16.277248  0.0000
# ConfInt      0.2411514 0.0216844 21503  11.120971  0.0000
# Ed_sup       0.9720699 0.0632048 21503  15.379689  0.0000
# Idade       -0.0238481 0.0013074 21503 -18.240747  0.0000
# Sexo         0.5189930 0.0411442 21503  12.614003  0.0000
# TNaH         0.0269444 0.3732296    14   0.072193  0.9435
# MedToler     0.8431301 0.2739026    14   3.078211  0.0082
# PIBpc        0.0000033 0.0000472    14   0.069865  0.9453
# Correlation: 
#   (Intr) Denom2 Denom3 Denom4 AtRelg IntRlg Dem    CnfInt Ed_sup Idade  Sexo   TNaH  
# Denom2   -0.013                                                                             
# Denom3   -0.016  0.205                                                                      
# Denom4   -0.044  0.109  0.102                                                               
# AtRelig  -0.010 -0.230 -0.163  0.163                                                        
# IntRelig -0.087 -0.037  0.003  0.292 -0.270                                                 
# Dem      -0.041  0.000  0.011  0.003 -0.004 -0.005                                          
# ConfInt  -0.017  0.038  0.018  0.013 -0.032 -0.018 -0.091                                   
# Ed_sup   -0.068  0.046  0.030  0.009 -0.018  0.047 -0.072 -0.067                            
# Idade    -0.035  0.100  0.058  0.054 -0.114 -0.063 -0.071  0.005  0.002                     
# Sexo     -0.017  0.022  0.009  0.025 -0.092 -0.058  0.038  0.079 -0.004  0.056              
# TNaH      0.748 -0.007  0.003  0.002 -0.002 -0.002 -0.005  0.005  0.005 -0.005 -0.001       
# MedToler -0.939  0.005 -0.001 -0.003  0.001  0.014 -0.001 -0.007 -0.001 -0.002 -0.002 -0.801
# PIBpc     0.371 -0.016  0.013  0.010  0.029 -0.006 -0.011 -0.004 -0.002 -0.014 -0.008  0.355
# MedTlr
# Denom2         
# Denom3         
# Denom4         
# AtRelig        
# IntRelig       
# Dem            
# ConfInt        
# Ed_sup         
# Idade          
# Sexo           
# TNaH           
# MedToler       
# PIBpc    -0.611
# 
# Standardized Within-Group Residuals:
#   Min          Q1         Med          Q3         Max 
# -2.65805088 -0.80631582 -0.06511007  0.77817118  3.19676144 
# 
# Number of Observations: 21531
# Number of Groups: 18 


#Para visualizar a saída do modelo, com o sjPlot
tab_model(Model.Comp1, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

library(memisc) #Para utilizar o string Table
tapply(BancoGeral2$TNaH, BancoGeral2$pais, Table, percent = T)
#Com essa função nós verificamos a variação da variável Índice de Tolerância Nacional
#a homossexuais, sendo que essa variável está organizada de 0 a 2
#Sendo 0 nenhum direito, 1 direito ao casamento e 2 direito a casamento e adoção.



####
#Igual####
##6°TESTE####
# Modelo com inclinações variaveis
#Denominação religiosa 
# Estamos verificando se o efeito da denominação é diferente entre os países.
#Se tiver efeito tentaremos explicar o que explica essa variação entre os
Model.2.Tol.Denom<-lme(TolerHomo~Denom + AtRelig + IntRelig + Dem + ConfInt + Ed_sup + Idade + Sexo,
                       random=~Denom|pais, data=BancoGeral2, na.action=na.omit, 
                       control=list(opt="optim"))
summary(Model.2.Tol.Denom)



Model.A.Tol.Denom<-update(Model.2.Tol.Denom,random=~1|pais)
anova(Model.A.Tol.Denom,Model.2.Tol.Denom)
#                  Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#Model.A.Tol.Denom     1 13 108135.6 108239.3 -54054.79                        
#Model.2.Tol.Denom     2 22 108116.4 108291.9 -54036.20 1 vs 2 37.17739  <.0001

tab_model(Model.A.Tol.Denom, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

#
#Igual####
##7°TESTE####
Model.3.Tol.At<-lme(TolerHomo~Denom + AtRelig + IntRelig + Dem + ConfInt + Ed_sup + Idade + Sexo,
                    random=~AtRelig|pais, data=BancoGeral2, na.action=na.omit, 
                    control=list(opt="optim"))
summary(Model.3.Tol.At)
Model.B.Tol.At<-update(Model.3.Tol.At,random=~1|pais)
anova(Model.B.Tol.At,Model.3.Tol.At)

#               Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#Model.B.Tol.At     1 13 108135.6 108239.3 -54054.79                        
#Model.3.Tol.At     2 15 108109.7 108229.3 -54039.84 1 vs 2 29.89982  <.0001
#

##8°TESTE####
#Igual####
Model.4.Tol.Int<-lme(TolerHomo~Denom + AtRelig + IntRelig + Dem + ConfInt + Ed_sup + Idade + Sexo,
                     random=~IntRelig|pais, data=BancoGeral2, na.action=na.omit, 
                     control=list(opt="optim"))
summary(Model.4.Tol.Int)
Model.C.Tol.Int<-update(Model.4.Tol.Int,random=~1|pais)
anova(Model.C.Tol.Int,Model.4.Tol.Int)

#               Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#Model.C.Tol.Int     1 13 108135.6 108239.3 -54054.79                        
#Model.4.Tol.Int     2 15 108128.9 108248.5 -54049.44 1 vs 2 10.68332  0.0048

#MUDANÇA####
##9°TESTE*####
#Interação Denom e TNaH####
ModelTol.Denom.TNaH <-lme(TolerHomo~ Denom + Denom:TNaH + AtRelig + IntRelig + 
                            Dem + ConfInt  + Ed_sup + Idade + Sexo + 
                            MedToler + PIBpc,random=~1|pais, 
                          data=BancoGeral2, na.action=na.omit, control=list(opt="optim"))

round(summary(ModelTol.Denom.TNaH)$tTable,dig=3)
#             Value Std.Error    DF t-value p-value
#(Intercept)  0.812     0.997 21499   0.815   0.415
# Denom2      -0.991     0.061 21499 -16.108   0.000
# Denom3      -0.723     0.077 21499  -9.411   0.000
# Denom4      -0.243     0.086 21499  -2.839   0.005
# AtRelig     -0.208     0.018 21499 -11.261   0.000
# IntRelig    -0.280     0.031 21499  -9.057   0.000
# Dem          0.197     0.012 21499  16.256   0.000
# ConfInt      0.241     0.022 21499  11.121   0.000
# Ed_sup       0.972     0.063 21499  15.384   0.000
# Idade       -0.024     0.001 21499 -18.207   0.000
# Sexo         0.519     0.041 21499  12.617   0.000
# MedToler     0.844     0.277    15   3.043   0.008
# PIBpc        0.000     0.000    15   0.029   0.978
# Denom1:TNaH  0.015     0.379 21499   0.041   0.968
# Denom2:TNaH -0.086     0.381 21499  -0.226   0.822
# Denom3:TNaH  0.433     0.401 21499   1.079   0.281
# Denom4:TNaH  0.144     0.386 21499   0.374   0.709

summary(ModelTol.Denom.TNaH)

tab_model(ModelTol.Denom.TNaH, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")


#MUDANÇA####
##10°TESTE####
ModelTol.At.TNaH <-lme(TolerHomo~ Denom  + AtRelig + AtRelig:TNaH + IntRelig + Dem + ConfInt 
                       + Ed_sup + Idade + Sexo + PIBpc + MedToler,random=~1|pais, 
                       data=BancoGeral2, na.action=na.omit, control=list(opt="optim"))
round(summary(ModelTol.At.TNaH)$tTable,dig=3)

# Value Std.Error    DF t-value p-value
# (Intercept)   0.563     0.650 21502   0.867   0.386
# Denom2       -1.020     0.056 21502 -18.140   0.000
# Denom3       -0.665     0.073 21502  -9.056   0.000
# Denom4       -0.192     0.077 21502  -2.486   0.013
# AtRelig      -0.193     0.020 21502  -9.728   0.000
# IntRelig     -0.278     0.031 21502  -8.984   0.000
# Dem           0.197     0.012 21502  16.258   0.000
# ConfInt       0.240     0.022 21502  11.065   0.000
# Ed_sup        0.973     0.063 21502  15.391   0.000
# Idade        -0.024     0.001 21502 -18.290   0.000
# Sexo          0.518     0.041 21502  12.586   0.000
# PIBpc         0.000     0.000    15  -0.020   0.985
# MedToler      0.904     0.163    15   5.556   0.000
# AtRelig:TNaH -0.051     0.022 21502  -2.333   0.020
summary(ModelTol.At.TNaH)

tab_model(ModelTol.At.TNaH, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

#MUDANÇA####
#Extra####
##10°TESTE#### Agr com o PIBpc
ModelTol.At.PIBpc <-lme(TolerHomo~ Denom  + AtRelig + AtRelig:TNaH + 
                               AtRelig:PIBpc + IntRelig + Dem + ConfInt 
                             + Ed_sup + Idade + Sexo + PIBpc + MedToler,
                               random=~1|pais, 
                             data=BancoGeral2, na.action=na.omit, control=list(opt="optim"))
round(summary(ModelTol.At.PIBpc)$tTable,dig=3)
summary(ModelTol.At.PIBpc)

tab_model(ModelTol.At.PIBpc, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

#
##11°TESTE####
ModelTol.Int.TNaH <-lme(TolerHomo~ Denom  + AtRelig  + IntRelig + 
                          IntRelig:TNaH + Dem + ConfInt 
                        + Ed_sup + Idade + Sexo + PIBpc + 
                          MedToler,random=~1|pais, 
                        data=BancoGeral2, na.action=na.omit, 
                        control=list(opt="optim"))
round(summary(ModelTol.Int.TNaH)$tTable,dig=3)
summary(ModelTol.Int.TNaH)

tab_model(ModelTol.Int.TNaH, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

#MUDANÇA####
#Extra####
##11°TESTE####
ModelTol.Int.PIBpc <-lme(TolerHomo~ Denom  + AtRelig  + IntRelig +  
                                IntRelig:TNaH + IntRelig:PIBpc + Dem + ConfInt 
                              + Ed_sup + Idade + Sexo + PIBpc,random=~1|pais, 
                              data=BancoGeral2, na.action=na.omit, 
                         control=list(opt="optim"))

round(summary(ModelTol.Int.PIBpc )$tTable,dig=3)
summary(ModelTol.Int.PIBpc )

tab_model(ModelTol.Int.PIBpc, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")


#NÃORODEI####
#O que temos que fazer é criar um banco de dados, 
#com apenas 4 casos, em que vamos deixar instável todas as v.
#Menos a que estamos a estamos interagindo.

##Análise de valores preditos
#Criação do banco

#Interação entre denominação e tolerância nacional aos homossexuais 
PRED.Denom.TNaH <- data.frame(Denom=c("1", "1", "2", "2", "3", "3", "4", "4"),
                              AtRelig=c(0,0,2,2,2,2,2,2),
                              IntRelig=c(0,0,2,2,2,2,2,2),
                              Dem=c(4, 4, 4, 4, 4, 4, 4, 4),
                              ConfInt=c(2, 2, 2, 2, 2, 2, 2, 2),
                              Ed_sup=c(0, 0, 0, 0, 0, 0, 0, 0),
                              Idade=c(40, 40, 40, 40, 40, 40, 40, 40),
                              Sexo=c(0,0,0,0,0,0,0,0),
                              TNaH=c(0, 2,0, 2,0, 2,0, 2),
                              Desemprego= c(11,11,11,11,11,11,11,11))

predict(Model.Comp1,PRED.Denom.TNaH,level=0)

#[1] 4.392273 6.506973 3.596960 5.711660 2.571991 4.686691 2.937430 5.052130
#attr(,"label")
#[1] "Predicted values"


#3.4 é a medida de tolerância de um ateu que vive em um lugar com contexto sem direitos 
#5.5 contexto com tolerância
#Naiara cada dupla é uma categoria de Denom.


PRED.Denom.TNaH$TolerHomo <-predict(Model.Comp1,PRED.Denom.TNaH,level=0)
with(PRED.Denom.TNaH,interaction.plot(Denom,TNaH,TolerHomo, legend=F,xlab="Denominação", 
                                      ylab="Média de tolerância política", 
                                      main="Interação entre Denominações religiosas 
e Direitos dos Homossexuais (TNaH)"))
#####

####

# Segunda opção de gráfico:
PRED.Denom.TNaH$TolerHomo <-predict(Model.Comp1,
                                    PRED.Denom.TNaH,level=0)
with(PRED.Denom.TNaH,
     interaction.plot(Denom,TNaH,TolerHomo,
                      xlab="Denominação", 
                      ylab="Média de tolerância política", 
                      legend=T, col = "steelblue",
                      main="Interação entre Denominações religiosas 
e Direitos dos Homossexuais (TNaH)")) 

#Linha tracejada é sem direitos, e a sólida é a média de tolerância 
#para todas as religiões nos contextos com direitos 


#Interação entre ativismo e tolerância aos homossexuais
PRED.At.TNaH <- data.frame(Denom=c("1", "1", "2", "2", "2", "2", "3", "3","3","3", "4", "4","4","4"),
                           AtRelig=c(0,0, 0,0,4,4, 0,0,4,4, 0,0,4,4),
                           IntRelig=c(2,2, 2,2,2,2, 2,2,2,2, 2,2,2,2),
                           Dem=c(4,4, 4,4,4,4, 4,4,4,4, 4,4,4,4),
                           ConfInt=c(2,2, 2,2,2,2, 2,2,2,2, 2,2,2,2),
                           Ed_sup=c(0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0),
                           Idade=c(40,40, 40,40,40,40, 40,40,40,40, 40,40,40,40),
                           Sexo=c(0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0),
                           TNaH=c(0,2, 0,2,0,2, 0,2,0,2, 0,2,0,2),
                           Desemprego= c(11,11, 11,11,11,11, 11,11,11,11, 11,11,11,11))

predict(Model.Comp1,PRED.At.TNaH,level=0)

# [1] 3.823698 5.938398 4.019819 6.134518 3.174102 5.288802 2.994850 5.109549 2.149133 4.263833
#[11] 3.360289 5.474988 2.514572 4.629272
#attr(,"label")
#[1] "Predicted values"

#Agora estamos analisando o ativismo religioso. 
#Tracejada contexto que não tem direito. E a sólida é o contexto onde tem direito

PRED.At.TNaH$TolerHomo <-predict(Model.Comp1,PRED.At.TNaH,level=0)
with(PRED.At.TNaH,interaction.plot(AtRelig,TNaH,TolerHomo, legend=T,xlab="Ativismo", 
                                   ylab="Média de tolerância política", 
                                   main="Interação entre Ativismo e 
  Direitos dos Homossexuais (TNaH)",
                                   col = "steelblue"))



# 20/07/2020


#Interação entre intensidade e tolerância nacional aos homossexuais 
PRED.Int.TNaH <- data.frame(Denom=c("1", "1", "2", "2", "2", "2", "3", "3","3","3", "4", "4","4","4"),
                            AtRelig=c(0,0, 2,2,2,2, 2,2,2,2, 2,2,2,2),
                            IntRelig=c(0,0, 0,0,3,3, 0,0,3,3, 0,0,3,3),
                            Dem=c(4,4, 4,4,4,4, 4,4,4,4, 4,4,4,4),
                            ConfInt=c(2,2, 2,2,2,2, 2,2,2,2, 2,2,2,2),
                            Ed_sup=c(0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0),
                            Idade=c(40,40, 40,40,40,40, 40,40,40,40, 40,40,40,40),
                            Sexo=c(0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0),
                            TNaH=c(0,2, 0,2,0,2, 0,2,0,2, 0,2,0,2),
                            Desemprego= c(11,11, 11,11,11,11, 11,11,11,11, 11,11,11,11))

predict(Model.Comp1,PRED.Int.TNaH,level=0)

#[1] 4.392273 6.506973 4.165536 6.280236 3.312673 5.427372 3.140567 5.255267 2.287704 4.402404 3.506006
#[12] 5.620706 2.653143 4.767843
#attr(,"label")
#[1] "Predicted values"


PRED.Int.TNaH$TolerHomo <-predict(Model.Comp1,PRED.Int.TNaH,level=0)
with(PRED.Int.TNaH,interaction.plot(IntRelig,TNaH,TolerHomo, legend=T,xlab="Intensidade", 
                                    ylab="Média de tolerância política", 
                                    main="Interação entre intensidade religiosa e Direitos 
dos Homossexuais (TNaH)",
                                    col = "steelblue"))

#Sem/a/V/MedTol####
#Sem/a/V/MedTol####
#Sem/a/V/MedTol####
#Sem/a/V/MedTol####
#Sem/a/V/MedTol####
#Sem/a/V/MedTol####

##5ºTESTE*####
BancoGeral2$TNaH
BancoGeral2$MedToler
BancoGeral2$PIBpc
#modelo completo (Aqui coloco todas as variáveis de Nível 2)
Model.Comp1 <-lme(TolerHomo~ Denom + AtRelig + IntRelig + Dem + ConfInt 
                  + Ed_sup + Idade + Sexo + MedToler +
                    PIBpc, random=~1|pais, 
                  data=BancoGeral2, na.action=na.omit, control=list(opt="optim"))

summary(Model.Comp1)


#Para visualizar a saída do modelo, com o sjPlot
tab_model(Model.Comp1, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

##############################
#Testando novamente####

##5ºTESTE*####
BancoGeral2$TNaH
BancoGeral2$MedToler
BancoGeral2$PIBpc
#modelo completo (Aqui coloco todas as variáveis de Nível 2)
Model.Comp1 <-lme(TolerHomo~ Denom + AtRelig + IntRelig + Dem + ConfInt 
                  + Ed_sup + Idade + Sexo + TNaH + MedToler +
                    PIBpc, random=~1|pais, 
                  data=BancoGeral2, na.action=na.omit, control=list(opt="optim"))

summary(Model.Comp1)


#Para visualizar a saída do modelo, com o sjPlot
tab_model(Model.Comp1, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

library(memisc) #Para utilizar o string Table
tapply(BancoGeral2$TNaH, BancoGeral2$pais, Table, percent = T)
#Com essa função nós verificamos a variação da variável Índice de Tolerância Nacional
#a homossexuais, sendo que essa variável está organizada de 0 a 2
#Sendo 0 nenhum direito, 1 direito ao casamento e 2 direito a casamento e adoção.



####
#Igual####
##6°TESTE####
# Modelo com inclinações variaveis
#Denominação religiosa 
# Estamos verificando se o efeito da denominação é diferente entre os países.
#Se tiver efeito tentaremos explicar o que explica essa variação entre os
Model.2.Tol.Denom<-lme(TolerHomo~Denom + AtRelig + IntRelig + Dem + ConfInt + Ed_sup + Idade + Sexo,
                       random=~Denom|pais, data=BancoGeral2, na.action=na.omit, 
                       control=list(opt="optim"))
summary(Model.2.Tol.Denom)



Model.A.Tol.Denom<-update(Model.2.Tol.Denom,random=~1|pais)
anova(Model.A.Tol.Denom,Model.2.Tol.Denom)


tab_model(Model.A.Tol.Denom, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

#
#Igual####
##7°TESTE####
Model.3.Tol.At<-lme(TolerHomo~Denom + AtRelig + IntRelig + Dem + ConfInt + Ed_sup + Idade + Sexo,
                    random=~AtRelig|pais, data=BancoGeral2, na.action=na.omit, 
                    control=list(opt="optim"))
summary(Model.3.Tol.At)
Model.B.Tol.At<-update(Model.3.Tol.At,random=~1|pais)
anova(Model.B.Tol.At,Model.3.Tol.At)



##8°TESTE####
#Igual####
Model.4.Tol.Int<-lme(TolerHomo~Denom + AtRelig + IntRelig + Dem + ConfInt + Ed_sup + Idade + Sexo,
                     random=~IntRelig|pais, data=BancoGeral2, na.action=na.omit, 
                     control=list(opt="optim"))
summary(Model.4.Tol.Int)
Model.C.Tol.Int<-update(Model.4.Tol.Int,random=~1|pais)
anova(Model.C.Tol.Int,Model.4.Tol.Int)


#MUDANÇA####
##9°TESTE*####
#Interação Denom e MedToler####
ModelTol.Denom.MedToler <-lme(TolerHomo~ Denom + Denom:MedToler + AtRelig + IntRelig + 
                                Dem + ConfInt  + Ed_sup + Idade + Sexo + 
                                MedToler + PIBpc,random=~1|pais, 
                              data=BancoGeral2, na.action=na.omit, control=list(opt="optim"))

round(summary(ModelTol.Denom.MedToler)$tTable,dig=3)


summary(ModelTol.Denom.MedToler)

tab_model(ModelTol.Denom.MedToler, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")


#MUDANÇA####
##10°TESTE####
ModelTol.At.MedToler <-lme(TolerHomo~ Denom  + AtRelig + AtRelig:MedToler + IntRelig + Dem + ConfInt 
                           + Ed_sup + Idade + Sexo + PIBpc + MedToler,random=~1|pais, 
                           data=BancoGeral2, na.action=na.omit, control=list(opt="optim"))
round(summary(ModelTol.At.MedToler)$tTable,dig=3)


summary(ModelTol.At.MedToler)

tab_model(ModelTol.At.MedToler, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

#MUDANÇA####
#Extra####
##10°TESTE#### Agr com o PIBpc
ModelTol.At.PIBpc <-lme(TolerHomo~ Denom  + AtRelig + AtRelig:MedToler + 
                          AtRelig:PIBpc + IntRelig + Dem + ConfInt 
                        + Ed_sup + Idade + Sexo + PIBpc + MedToler,
                        random=~1|pais, 
                        data=BancoGeral2, na.action=na.omit, control=list(opt="optim"))
round(summary(ModelTol.At.PIBpc)$tTable,dig=3)
summary(ModelTol.At.PIBpc)

tab_model(ModelTol.At.PIBpc, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

#
##11°TESTE####
ModelTol.Int.MedToler <-lme(TolerHomo~ Denom  + AtRelig  + IntRelig + 
                              IntRelig:MedToler + Dem + ConfInt 
                            + Ed_sup + Idade + Sexo + PIBpc + 
                              MedToler,random=~1|pais, 
                            data=BancoGeral2, na.action=na.omit, 
                            control=list(opt="optim"))
round(summary(ModelTol.Int.MedToler)$tTable,dig=3)
summary(ModelTol.Int.MedToler)

tab_model(ModelTol.Int.MedToler, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

#MUDANÇA####
#Extra####
##11°TESTE####
ModelTol.Int.PIBpc <-lme(TolerHomo~ Denom  + AtRelig  + IntRelig +  
                           IntRelig:MedToler + IntRelig:PIBpc + Dem + ConfInt 
                         + Ed_sup + Idade + Sexo + PIBpc,random=~1|pais, 
                         data=BancoGeral2, na.action=na.omit, 
                         control=list(opt="optim"))

round(summary(ModelTol.Int.PIBpc)$tTable,dig=3)
summary(ModelTol.Int.PIBpc)

tab_model(ModelTol.Int.PIBpc, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

#IVE####
#IVE####
#IVE####
#IVE####
#IVE####
#IVE####
#IVE####
#IVE####
#IVE####
#IVE####
table(BancoGeral2$IVE2012)


BancoGeral2$IVE2012 <- as.numeric(BancoGeral2$IVE2012)

##5ºTESTE*####
BancoGeral2$TNaH
BancoGeral2$IVE2012
BancoGeral2$PIBpc
#modelo completo (Aqui coloco todas as variáveis de Nível 2)
Model.Comp1 <-lme(TolerHomo~ Denom + AtRelig + IntRelig + Dem + ConfInt 
                  + Ed_sup + Idade + Sexo + TNaH + IVE2012 +
                    PIBpc, random=~1|pais, 
                  data=BancoGeral2, na.action=na.omit, control=list(opt="optim"))

summary(Model.Comp1)


#Para visualizar a saída do modelo, com o sjPlot
tab_model(Model.Comp1, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")


library(memisc) #Para utilizar o string Table
tapply(BancoGeral2$TNaH, BancoGeral2$pais, Table, percent = T)
#Com essa função nós verificamos a variação da variável Índice de Tolerância Nacional
#a homossexuais, sendo que essa variável está organizada de 0 a 2
#Sendo 0 nenhum direito, 1 direito ao casamento e 2 direito a casamento e adoção.



####
#Igual####
##6°TESTE####
# Modelo com inclinações variaveis
#Denominação religiosa 
# Estamos verificando se o efeito da denominação é diferente entre os países.
#Se tiver efeito tentaremos explicar o que explica essa variação entre os
Model.2.Tol.Denom<-lme(TolerHomo~Denom + AtRelig + IntRelig + 
                         Dem + ConfInt + Ed_sup + Idade + Sexo,
                       random=~Denom|pais, data=BancoGeral2,
                       na.action=na.omit, 
                       control=list(opt="optim"))
summary(Model.2.Tol.Denom)



Model.A.Tol.Denom<-update(Model.2.Tol.Denom,random=~1|pais)
anova(Model.A.Tol.Denom,Model.2.Tol.Denom)


tab_model(Model.A.Tol.Denom, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

#
#Igual####
##7°TESTE####
Model.3.Tol.At<-lme(TolerHomo~Denom + AtRelig + IntRelig + Dem + ConfInt + Ed_sup + Idade + Sexo,
                    random=~AtRelig|pais, data=BancoGeral2, na.action=na.omit, 
                    control=list(opt="optim"))
summary(Model.3.Tol.At)
Model.B.Tol.At<-update(Model.3.Tol.At,random=~1|pais)
anova(Model.B.Tol.At,Model.3.Tol.At)



##8°TESTE####
#Igual####
Model.4.Tol.Int<-lme(TolerHomo~Denom + AtRelig + IntRelig + Dem + ConfInt + Ed_sup + Idade + Sexo,
                     random=~IntRelig|pais, data=BancoGeral2, na.action=na.omit, 
                     control=list(opt="optim"))
summary(Model.4.Tol.Int)
Model.C.Tol.Int<-update(Model.4.Tol.Int,random=~1|pais)
anova(Model.C.Tol.Int,Model.4.Tol.Int)


#MUDANÇA####
##9°TESTE*####
#Interação Denom e IVE####
ModelTol.Denom.IVE <-lme(TolerHomo~ Denom + Denom:IVE2012 + AtRelig + IntRelig + 
                                Dem + ConfInt  + Ed_sup + Idade + Sexo + 
                           IVE2012 + PIBpc,random=~1|pais, 
                              data=BancoGeral2, na.action=na.omit, control=list(opt="optim"))

round(summary(ModelTol.Denom.IVE)$tTable,dig=3)


summary(ModelTol.Denom.IVE)

tab_model(ModelTol.Denom.IVE, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")


#MUDANÇA####
##10°TESTE####
ModelTol.At.IVE <-lme(TolerHomo~ Denom  + AtRelig + AtRelig:IVE2012 + 
                        IntRelig + Dem + ConfInt 
                           + Ed_sup + Idade + Sexo + 
                        PIBpc + IVE2012,random=~1|pais, 
                           data=BancoGeral2, 
                      na.action=na.omit, control=list(opt="optim"))
round(summary(ModelTol.At.IVE)$tTable,dig=3)


summary(ModelTol.At.IVE)

tab_model(ModelTol.At.IVE, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

#MUDANÇA####
#Extra####
##10°TESTE#### Agr com o PIBpc
ModelTol.At.PIBpc <-lme(TolerHomo~ Denom  + AtRelig + 
                          AtRelig:IVE2012 + 
                          AtRelig:PIBpc + IntRelig + Dem + 
                          ConfInt 
                        + Ed_sup + Idade + Sexo + PIBpc + 
                          IVE2012,
                        random=~1|pais, 
                        data=BancoGeral2, na.action=na.omit,
                        control=list(opt="optim"))
round(summary(ModelTol.At.PIBpc)$tTable,dig=3)
summary(ModelTol.At.PIBpc)

tab_model(ModelTol.At.PIBpc, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

#
##11°TESTE####
ModelTol.Int.IVE <-lme(TolerHomo~ Denom  + AtRelig  + IntRelig + 
                              IntRelig:IVE2012 + Dem + ConfInt 
                            + Ed_sup + Idade + Sexo + PIBpc + 
                         IVE2012,random=~1|pais, 
                            data=BancoGeral2, na.action=na.omit, 
                            control=list(opt="optim"))
round(summary(ModelTol.Int.IVE)$tTable,dig=3)
summary(ModelTol.Int.IVE)

tab_model(ModelTol.Int.IVE, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

#MUDANÇA####
#Extra####
##11°TESTE####
ModelTol.Int.PIBpc <-lme(TolerHomo~ Denom  + AtRelig  + IntRelig +  
                           IntRelig:IVE2012 + IntRelig:PIBpc + 
                           Dem + ConfInt 
                         + Ed_sup + Idade + Sexo + PIBpc,
                         random=~1|pais, 
                         data=BancoGeral2, na.action=na.omit, 
                         control=list(opt="optim"))

round(summary(ModelTol.Int.PIBpc)$tTable,dig=3)
summary(ModelTol.Int.PIBpc)

tab_model(ModelTol.Int.PIBpc, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

####################
#######################
##################
################
#RODADNDO TODOS OS MODELOS DE NOVO####
#cOM O DESEMPREGO

BancoGeral2 <- merge(BancoGeral, BancoMacro, all = T )

table(BancoGeral2$pais)
# 1    2    3    4    5    6    7    9   10   11   12   13   14   15 
# 1580 1596 1511 1560 1547 1501 1559 1533 1682 1521 1515 1638 1581 1498 
# 16   17   21   22 
# 1558 1528 1516 2221 
table(BancoGeral2$Denom)

# 1     2     3     4 
# 15381  5041  3260  3642

save(BancoGeral2, file= "BancoGeral2.RData")


##MUDANÇA####

library(sjPlot)
library(multilevel)#ativa o pacote para execução do modelo ANOVA

##5ºTESTE*####
BancoGeral2$TNaH
BancoGeral2$MedToler
BancoGeral2$Desemprego
#modelo completo (Aqui coloco todas as variáveis de Nível 2)
BancoGeral2$MedToler <- as.numeric(BancoGeral2$MedToler)

Model.Comp1 <-lme(TolerHomo~ Denom + AtRelig + IntRelig + Dem + ConfInt +
                   Ed_sup + Idade + Sexo + TNaH + MedToler +
                    Desemprego, random=~1|pais, 
                  data=BancoGeral2, na.action=na.omit, control=list(opt="optim"))


summary(Model.Comp1)

#Para visualizar a saída do modelo, com o sjPlot
tab_model(Model.Comp1, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

library(memisc) #Para utilizar o string Table
tapply(BancoGeral2$TNaH, BancoGeral2$pais, Table, percent = T)
#Com essa função nós verificamos a variação da variável Índice de Tolerância Nacional
#a homossexuais, sendo que essa variável está organizada de 0 a 2
#Sendo 0 nenhum direito, 1 direito ao casamento e 2 direito a casamento e adoção.

##5ºTESTE*####
BancoGeral2$TNaH
BancoGeral2$IVE2012
BancoGeral2$Desemprego
#modelo completo (Aqui coloco todas as variáveis de Nível 2)
Model.Comp1 <-lme(TolerHomo~ Denom + AtRelig + IntRelig + Dem + ConfInt 
                  + Ed_sup + Idade + Sexo + TNaH + IVE2012 +
                    Desemprego, random=~1|pais, 
                  data=BancoGeral2, na.action=na.omit, control=list(opt="optim"))

summary(Model.Comp1)


#Para visualizar a saída do modelo, com o sjPlot
tab_model(Model.Comp1, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")


library(memisc) #Para utilizar o string Table
tapply(BancoGeral2$TNaH, BancoGeral2$pais, Table, percent = T)
#Com essa função nós verificamos a variação da variável Índice de Tolerância Nacional
#a homossexuais, sendo que essa variável está organizada de 0 a 2
#Sendo 0 nenhum direito, 1 direito ao casamento e 2 direito a casamento e adoção.



####
#DEM
#MUDANÇA####
##9°TESTE*####
#Interação Denom e MedToler####
ModelTol.Denom.MedToler <-lme(TolerHomo~ Denom + Denom:MedToler + AtRelig + IntRelig + 
                                Dem + ConfInt  + Ed_sup + Idade + Sexo + 
                                MedToler + Desemprego,random=~1|pais, 
                              data=BancoGeral2, na.action=na.omit, control=list(opt="optim"))

round(summary(ModelTol.Denom.MedToler)$tTable,dig=3)


summary(ModelTol.Denom.MedToler)

tab_model(ModelTol.Denom.MedToler, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")


#MUDANÇA####
##10°TESTE####
ModelTol.At.MedToler <-lme(TolerHomo~ Denom  + AtRelig + AtRelig:MedToler + IntRelig + Dem + ConfInt 
                           + Ed_sup + Idade + Sexo + Desemprego + MedToler,random=~1|pais, 
                           data=BancoGeral2, na.action=na.omit, control=list(opt="optim"))
round(summary(ModelTol.At.MedToler)$tTable,dig=3)


summary(ModelTol.At.MedToler)

tab_model(ModelTol.At.MedToler, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")



#
##11°TESTE####
ModelTol.Int.MedToler <-lme(TolerHomo~ Denom  + AtRelig  + IntRelig + 
                              IntRelig:MedToler + Dem + ConfInt 
                            + Ed_sup + Idade + Sexo + Desemprego + 
                              MedToler,random=~1|pais, 
                            data=BancoGeral2, na.action=na.omit, 
                            control=list(opt="optim"))
round(summary(ModelTol.Int.MedToler)$tTable,dig=3)
summary(ModelTol.Int.MedToler)

tab_model(ModelTol.Int.MedToler, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")


#MUDANÇA####
##9°TESTE*####
#Interação Denom e TNaH####
ModelTol.Denom.TNaH <-lme(TolerHomo~ Denom + Denom:TNaH + AtRelig + IntRelig + 
                                Dem + ConfInt  + Ed_sup + Idade + Sexo + 
                                MedToler + Desemprego,random=~1|pais, 
                              data=BancoGeral2, na.action=na.omit, control=list(opt="optim"))

round(summary(ModelTol.Denom.TNaH)$tTable,dig=3)


summary(ModelTol.Denom.TNaH)

tab_model(ModelTol.Denom.TNaH, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")


#TNAH
BancoGeral2$TNaH
#MUDANÇA####
##10°TESTE####
ModelTol.At.TNaH <-lme(TolerHomo~ Denom  + AtRelig + AtRelig:TNaH + IntRelig + Dem + ConfInt 
                           + Ed_sup + Idade + Sexo + Desemprego + MedToler,random=~1|pais, 
                           data=BancoGeral2, na.action=na.omit, control=list(opt="optim"))
round(summary(ModelTol.At.TNaH)$tTable,dig=3)


summary(ModelTol.At.TNaH)

tab_model(ModelTol.At.TNaH, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")



#
##11°TESTE####
ModelTol.Int.TNaH <-lme(TolerHomo~ Denom  + AtRelig  + IntRelig + 
                              IntRelig:TNaH + Dem + ConfInt 
                            + Ed_sup + Idade + Sexo + Desemprego + 
                              TNaH,random=~1|pais, 
                            data=BancoGeral2, na.action=na.omit, 
                            control=list(opt="optim"))
round(summary(ModelTol.Int.TNaH)$tTable,dig=3)
summary(ModelTol.Int.TNaH)

tab_model(ModelTol.Int.TNaH, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

