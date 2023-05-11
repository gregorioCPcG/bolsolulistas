rm(list=ls())


library(haven)
q2018 <- read_sav("q2018_.sav")#banco eseb bruto

q2018$LavaJato_antiPt <- q2018$P19 == 3
summary(q2018$LavaJato_antiPt)
q2018$LavaJato_antiPt <- as.numeric(q2018$LavaJato_antiPt)
table(q2018$LavaJato_antiPt)
q2018$p19_recod <- q2018$LavaJato_antiPt
q2018$cont <- 1
temp1<- subset(q2018, select = c(cont,
                                 P18,Q502,Q405,P1102,P1103,Q404,Q8,Q21,P307,
                                 P1002,P1104,P1105,P12,P1705,P1707,
                                 P1712,p19_recod))



summary(temp1)
temp1$P18[temp1$P18  == 8] <- NA
temp1$P18[temp1$P18  == 9] <- NA
temp1$Q502[temp1$Q502  == 8] <- NA
temp1$Q502[temp1$Q502  == 7] <- NA
temp1$Q405[temp1$Q405  == 8] <- NA
temp1$Q405[temp1$Q405  == 7] <- NA
temp1$P1102[temp1$P1102  == 8] <- NA
temp1$P1102[temp1$P1102  == 9] <- NA
temp1$P1103[temp1$P1103  == 8] <- NA
temp1$P1103[temp1$P1103  == 9] <- NA
temp1$Q404[temp1$Q404  == 8] <- NA
temp1$Q404[temp1$Q404  == 7] <- NA
temp1$Q8[temp1$Q8  == 8] <- NA
temp1$Q8[temp1$Q8  == 7] <- NA
temp1$Q21[temp1$Q21  == 8] <- NA
temp1$Q21[temp1$Q21  == 7] <- NA
temp1$P307[temp1$P307  == 8] <- NA
temp1$P307[temp1$P307  == 9] <- NA
temp1$P1002[temp1$P1002  == 8] <- NA
temp1$P1002[temp1$P1002  == 9] <- NA
temp1$P1104[temp1$P1104  == 8] <- NA
temp1$P1104[temp1$P1104  == 9] <- NA
temp1$P1105[temp1$P1105  == 8] <- NA
temp1$P1105[temp1$P1105  == 9] <- NA
temp1$P12[temp1$P12  == 8] <- NA
temp1$P12[temp1$P12  == 9] <- NA
temp1$P1705[temp1$P1705  == 8] <- NA
temp1$P1705[temp1$P1705  == 9] <- NA
temp1$P1707[temp1$P1707  == 8] <- NA
temp1$P1707[temp1$P1707  == 9] <- NA
temp1$P1712[temp1$P1712  == 8] <- NA
temp1$P1712[temp1$P1712  == 9] <- NA
summary(temp1[,2:18])


#
library(psych)
library(mirt)
KMO(temp1[,2:18])
scree(temp1[,2:18])#2, 3 OU 4


mod2 <- mirt(temp1[,2:18], 2, itemtype = "graded")
#Iteration: 168, Log-Lik: -48452.467, Max-Change: 0.00008
summary(mod2)
summary(mod2,rotate="varimax",suppress=0.4)# deu CONFUSO, EM ESPECIAL F1

mod3 <- mirt(temp1[,2:18], 3, itemtype = "graded")
#Iteration: 136, Log-Lik: -48377.141, Max-Change: 0.00009
summary(mod3)
summary(mod3,rotate="varimax",suppress=0.32)# melhorou, mas ainda um pouco confuso
#consta na planilha 4

mirtCluster()
mod4 <- mirt(temp1[,2:18], 4)
mod4 <- mirt(temp1[,2:18], 4, itemtype = "graded")# se precisar
#Iteration: 240, Log-Lik: -48288.160, Max-Change: 0.00009
summary(mod4,rotate="varimax",suppress=0.32)# 
#consta na planilha 4

anova(mod2,mod3,mod4)#comparando modelos


#com 4 - ver na planilha 4
#F1 não igualitário
#F2 não crítico à justiça (inverter para crítico)
#F3 não majoritário (inverter para tornar majoritário)
#F4 descontente_partidos-e_democracia


a <- fscores(mod4, QMC=TRUE)
#a
temp1$nao_igualitario <- a[,1]#f1
temp1$critico_justiça <- -1*a[,2]#f2
temp1$majoritarian <- -1*a[,3]#f3
temp1$descontente_democracia_e_partidos <- a[,4]#f4

df <- subset(temp1, select=c(nao_igualitario,critico_justiça,
                                majoritarian,descontente_democracia_e_partidos))
write.csv(df, "C:/Users/grego/OneDrive/Desktop/work/paper 7o_workshop/df.csv")



#robustez

#análise dos componentes principais
x <- psych::principal(temp1[,2:18],4)
x$loadings


