
##TeseNai####

library(multilevel)#ativa o pacote para execução do modelo ANOVA
data(BancoFinal)#Indica o banco de dados integrado



##ModeloNULO####
#TolerHomo
Null.Model <- lme(TolerHomo~1, random = ~1|pais, data = BancoFinal,
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
Null.gls<-gls(TolerHomo ~1,data=BancoFinal, control=list(opt="optim"),
              na.action=na.omit)
logLik(Null.gls)*-2
#'log Lik.' 148741  (df=2)
logLik(Null.Model)*-2
#'log Lik.' 143153.3 (df=3)
148741 - 143153.3 = y
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
                  random = ~1|pais, data = BancoFinal, na.action=na.omit,
                  control = list(opt="optim"))

View(Model1.Tol)

#Para gerar tabelas:
Model1.Tol <- lm(TolerHomo~ Denom + AtRelig + IntRelig + Dem + ConfInt + 
                   Ed_sup + Idade + Sexo,
             random = ~1|pais, data = BancoFinal, na.action=na.omit,
             control = list(opt="optim"))

#Para visualizar a saída do modelo, com o sjPlot
tab_model(Model1.Tol, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60,
          p.style ="stars")

summary(Model1.Tol)

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
C/N = x
x <- 0.8476841
1 - x = y
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
                    data=BancoFinal, na.action=na.omit, control=list(opt="optim"))
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
                             data=BancoFinal, na.action=na.omit,
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
                               data=BancoFinal, na.action=na.omit, control=list(opt="optim"))

summary(Model.3.Aleatorio.IntRelig)
Model.3.Aleatorio.IntReligA <-update(Model.3.Aleatorio.IntRelig,random=~1|pais)
anova(Model.3.Aleatorio.IntReligA,Model.3.Aleatorio.IntRelig)

#Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#Model.3.Aleatorio.IntReligA     1 13 108135.6 108239.3 -54054.79                        
#Model.3.Aleatorio.IntRelig      2 15 108128.9 108248.5 -54049.44 1 vs 2 10.68332  0.0048

##5ºTESTE*####
#modelo completo (Irei utilizar as duas variáveis mais importantes)
Model.Comp1 <-lme(TolerHomo~ Denom + AtRelig + IntRelig + Dem + ConfInt 
+ Ed_sup + Idade + Sexo + TNaH + Desemprego, random=~1|pais, 
 data=BancoFinal, na.action=na.omit, control=list(opt="optim"))

summary(Model.Comp1)


#Para visualizar a saída do modelo, com o sjPlot
tab_model(Model.Comp1, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

tapply(BancoFinal$TNaH, BancoFinal$pais, Table, percent = T)
#Com essa função nós verificamos a variação da variável Índice de Tolerância Nacional
#a homossexuais, sendo que essa variável está organizada de 0 a 2
#Sendo 0 nenhum direito, 1 direito ao casamento e 2 direito a casamento e adoção.


#######ModeloMário####
###Vou tentar fazer esse mesmo modelo de outra forma:
#Fixed effects:  TolerHomo ~ Denom + AtRelig + IntRelig + Dem + ConfInt + Ed_sup +
#  Idade + Sexo + TNaH + Desemprego 

stargazer(Model.Comp1, type="html",
          title = "Teste",
          dep.var.labels=c("TolerHomo"),
          covariate.labels=c("Denom", 
                             "AtRelig",
                             "IntRelig", 
                             "Dem",
                             "ConfInt",
                             "Ed\\_sup",
                             "Ed\\_sup",
                             "Idade",
                             "Sexo",
                             "TNaH",
                             "Desemprego"), 
         out="Model.Comp1.htm", apply.coef = exp, digits = 2, p.auto = T) 

####
##6°TESTE####
# Modelo com inclinações variaveis
#Denominação religiosa 
# Estamos verificando se o efeito da denominação é diferente entre os países.
#Se tiver efeito tentaremos explicar o que explica essa variação entre os
Model.2.Tol.Denom<-lme(TolerHomo~Denom + AtRelig + IntRelig + Dem + ConfInt + Ed_sup + Idade + Sexo,
                    random=~Denom|pais, data=BancoFinal, na.action=na.omit, 
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
##7°TESTE####
Model.3.Tol.At<-lme(TolerHomo~Denom + AtRelig + IntRelig + Dem + ConfInt + Ed_sup + Idade + Sexo,
                       random=~AtRelig|pais, data=BancoFinal, na.action=na.omit, 
                       control=list(opt="optim"))
summary(Model.3.Tol.At)
Model.B.Tol.At<-update(Model.3.Tol.At,random=~1|pais)
anova(Model.B.Tol.At,Model.3.Tol.At)
  
#               Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#Model.B.Tol.At     1 13 108135.6 108239.3 -54054.79                        
#Model.3.Tol.At     2 15 108109.7 108229.3 -54039.84 1 vs 2 29.89982  <.0001
#

##8°TESTE####
Model.4.Tol.Int<-lme(TolerHomo~Denom + AtRelig + IntRelig + Dem + ConfInt + Ed_sup + Idade + Sexo,
                    random=~IntRelig|pais, data=BancoFinal, na.action=na.omit, 
                    control=list(opt="optim"))
summary(Model.4.Tol.Int)
Model.C.Tol.Int<-update(Model.4.Tol.Int,random=~1|pais)
anova(Model.C.Tol.Int,Model.4.Tol.Int)

#               Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#Model.C.Tol.Int     1 13 108135.6 108239.3 -54054.79                        
#Model.4.Tol.Int     2 15 108128.9 108248.5 -54049.44 1 vs 2 10.68332  0.0048


##9°TESTE*####
#Interação Denom e TNaH####
ModelTol.Denom.TNaH <-lme(TolerHomo~ Denom + Denom:TNaH + AtRelig + IntRelig + Dem + ConfInt 
                          + Ed_sup + Idade + Sexo + Desemprego,random=~1|pais, 
                     data=BancoFinal, na.action=na.omit, control=list(opt="optim"))

round(summary(ModelTol.Denom.TNaH)$tTable,dig=3)
#             Value Std.Error    DF t-value p-value
#(Intercept)  4.499     0.338 21499  13.322   0.000
#Denom2       0.245     0.086 21499   2.865   0.004
#Denom3      -0.742     0.099 21499  -7.533   0.000
#Denom4      -0.480     0.108 21499  -4.450   0.000
#AtRelig     -0.210     0.018 21499 -11.358   0.000
#IntRelig    -0.280     0.031 21499  -9.073   0.000
#Dem          0.198     0.012 21499  16.283   0.000
#ConfInt      0.241     0.022 21499  11.134   0.000
#Ed_sup       0.973     0.063 21499  15.396   0.000
#Idade       -0.024     0.001 21499 -18.169   0.000
#Sexo         0.520     0.041 21499  12.640   0.000
#Desemprego  -0.044     0.020    16  -2.181   0.044
#Denom1:TNaH  1.173     0.275 21499   4.271   0.000
#Denom2:TNaH  1.043     0.264 21499   3.947   0.000
#Denom3:TNaH  0.941     0.268 21499   3.510   0.000
#Denom4:TNaH  1.465     0.295 21499   4.966   0.000

summary(ModelTol.Denom.TNaH)

tab_model(ModelTol.Denom.TNaH, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

#
##10°TESTE####
ModelTol.At.TNaH <-lme(TolerHomo~ Denom  + AtRelig + AtRelig:TNaH + IntRelig + Dem + ConfInt 
                          + Ed_sup + Idade + Sexo + Desemprego,random=~1|pais, 
                          data=BancoFinal, na.action=na.omit, control=list(opt="optim"))
round(summary(ModelTol.At.TNaH)$tTable,dig=3)
summary(ModelTol.At.TNaH)

tab_model(ModelTol.At.TNaH, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

#Extra####
##10°TESTE#### Agr com o desemprego
ModelTol.At.Desemprego <-lme(TolerHomo~ Denom  + AtRelig + AtRelig:TNaH + 
                        AtRelig:Desemprego + IntRelig + Dem + ConfInt 
                       + Ed_sup + Idade + Sexo + Desemprego,random=~1|pais, 
                       data=BancoFinal, na.action=na.omit, control=list(opt="optim"))
round(summary(ModelTol.At.Desemprego)$tTable,dig=3)
summary(ModelTol.At.Desemprego)

tab_model(ModelTol.At.Desemprego, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

#
##11°TESTE####
ModelTol.Int.TNaH <-lme(TolerHomo~ Denom  + AtRelig  + IntRelig +  IntRelig:TNaH + Dem + ConfInt 
                       + Ed_sup + Idade + Sexo + Desemprego,random=~1|pais, 
                       data=BancoFinal, na.action=na.omit, control=list(opt="optim"))
round(summary(ModelTol.Int.TNaH)$tTable,dig=3)
summary(ModelTol.Int.TNaH)

tab_model(ModelTol.Int.TNaH, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")
#
#Extra####
##11°TESTE####
ModelTol.Int.Desemprego <-lme(TolerHomo~ Denom  + AtRelig  + IntRelig +  
                                IntRelig:TNaH + IntRelig:Desemprego + Dem + ConfInt 
                        + Ed_sup + Idade + Sexo + Desemprego,random=~1|pais, 
                        data=BancoFinal, na.action=na.omit, control=list(opt="optim"))
round(summary(ModelTol.Int.Desemprego )$tTable,dig=3)
summary(ModelTol.Int.Desemprego )

tab_model(ModelTol.Int.Desemprego, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")


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

#Descritiva?####
table(BancoFinal$Denom)
freq(BancoFinal$Denom_Desc)
#                 Frequência Percentual Perc. Válido
# Católico         15381     53.695        56.29
# Protestante       5041     17.598        18.45
# Outras            3260     11.381        11.93
# Ateu              3642     12.714        13.33
# NA's              1321      4.612             
# Total            28645    100.000       100.00


BancoFinal$Denom_Desc <- recode(BancoFinal$Denom, "Católico" <- 1, 
                                "Protestante" <- 2, "Outras" <- 3, "Ateu" <- 4)
table(BancoFinal$Denom_Desc)

tabela <- table(BancoFinal$TolerHomo, BancoFinal$Denom_Desc)
ca(tabela)

# Principal inertias (eigenvalues):
#              1        2        3       
# Value      0.047528 0.016055 0.000395
# Percentage 74.29%   25.09%   0.62%   
# 
# 
# Rows:
#   1         2         3         4        5        6        7        8        9
# Mass     0.285489  0.069330  0.053290  0.052391 0.097549 0.063746 0.066594 0.070454 0.052166
# ChiDist  0.310707  0.135316  0.143280  0.109470 0.118556 0.167466 0.169098 0.215162 0.257509
# Inertia  0.027561  0.001269  0.001094  0.000628 0.001371 0.001788 0.001904 0.003262 0.003459
# Dim. 1  -1.385047 -0.375804 -0.179026 -0.185844 0.360612 0.375268 0.460070 0.863199 1.050161
# Dim. 2  -0.575978  0.725599  1.083473  0.802007 0.699548 1.097802 1.064176 0.822005 0.929899
# 10
# Mass     0.188990
# ChiDist  0.338398
# Inertia  0.021642
# Dim. 1   1.245632
# Dim. 2  -1.593413
# 
# 
# Columns:
#   Católico Protestante    Outras      Ateu
# Mass    0.563896    0.183706  0.118198  0.134200
# ChiDist 0.128565    0.337521  0.280980  0.426381
# Inertia 0.009321    0.020928  0.009332  0.024398
# Dim. 1  0.362243   -1.514825 -1.180394  1.591164
# Dim. 2  0.800645   -0.506032 -0.812284 -1.956102

plot(ca(tabela), col = c("#2297E6", "#DF536B"), 
             arrows=c(TRUE, FALSE),
     col.lab = c("black", "black"), pch = c(16, 1, 18, 9),
     lwd=1, what = c("active", "active"))

palette()

pchlist() 
 
#plot3d.ca

## TMC
tabela1 <- table(BancoFinal$TolerHomo,
                 BancoFinal$IntRelig, BancoFinal$AtRelig)
ca(tabela1)
ellipseCA(tabela1, ncp =5, row.sup = NULL, col.sup = NULL, graph = F)

#Descritiva2####

table(BancoFinal$pais)
table(BancoFinal$TolerHomo)
summary(BancoFinal$TolerHomo)
BancoFinal$TolerHomo <- as.factor(BancoFinal$TolerHomo)

BancoFinal$TolerHomoP <- recode(BancoFinal$TolerHomo, 
          "Intolerante" <- c(1, 2, 3, 4,5), "Tolerantes" <- 
            c(6,7,8,9,10))

table(BancoFinal$TolerHomoP)
summary(BancoFinal$TolerHomoP)

save(BancoFinal, file = "BancoFinal.RData")

library(tidyr)
library(dplyr)
#Remove NAs apenas da coluna TolerHomoP
BancoFinal <- drop_na(BancoFinal, TolerHomoP)
BancoFinal <- drop_na(BancoFinal, Denom)


BancoFinal$Denom

Gráfico <-  ggplot(BancoFinal, aes(x = TolerHomoP, 
                                   fill = Denom1)) +
  geom_bar(width = .75) + 
  labs(x ="Tolerância Homossexuais")

Gra2 <- Gráfico  +
  theme_grey() + facet_wrap( ~ PaisD, nrow = 3) +
  theme(axis.text.x = 
          element_text(angle = 90, 
             vjust = 0, size= 10,  hjust = 1, 
             color = "black", family = "Times New 
             Roman")) +
  scale_fill_brewer("BrBg", type = "div") 

Gra2 + 
  theme(text = element_text(
    family = "Times New Roman", size = 13)) 


table(BancoFinal$Denom)
BancoFinal$Denom <- as.factor(BancoFinal$Denom)
BancoFinal$Denom1 <- recode(BancoFinal$Denom,
"Catól" <- 1, "Protes." <- 2, "Outras." <- 3, 
"Ateu" <- 4)


table(BancoFinal$PaisD)

# 1    2    3    4    5    6    7    9   10   11   12   13   14   15 
# 1524 1500 1480 1516 1498 1471 1532 1513 1644 1502 1484 1594 1563 1471 
# 16   17   21   22 
# 1510 1502 1479 2144 

BancoFinal$PaisD <- recode(BancoFinal$pais, 
 "México" <- 1, "Guat." <- 2, "ElSalv." <-3,
 "Hondu." <- 4, "Nicar." <- 5, "Cost.R" <-6,
 "Panamá" <- 7, "Ecuador" <- 9, "Bolívia" <- 10,
 "Peru"<- 11, "Parag." <- 12, "Chile" <- 13, 
 "Urug."<- 14, "Brasil" <- 15, "Venez." <-16,
 "Argent." <- 17, "Rep.Dom." <- 21, "Haiti" <-22)

BancoFinal$PaisD <- as.factor(BancoFinal$PaisD)
table(BancoFinal$PaisD)

library(RColorBrewer) # Tem um site 
#scale_fill_brewer(palette = 2, type = "seq")
#Para dados divergentes: type = "div"; 
#para dados em sequência: type = "seq"; para dados 
#categóricos em geral: type = "qual".


