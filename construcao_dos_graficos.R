# CONSTRUÇÃO DE GRÁFICOS
library(tidyverse)
library(haven)
rm(list=ls())#limpar tudo
q2018 <- read_sav("C:/Users/grego/OneDrive/Desktop/work/SCRIPTS TALK ABOUT/ESEB Novo fev 23/q2018_.sav")
#OBS q2018 é a base bruta original

df <- read_csv("df.csv")
#df é a base gerada em divisoes.R, portanto divisoes.R deve ser rodado antes


#confeccçao dos perfis

#questões usadas
# Questão  = Q12P2_B Em quem o(a) sr(a) votou para presidente no segundo turno?
# questão = Q12P1_B Em quem o(a) sr(a) votou para presidente no primeiro turno?
# Questão = P22. Se o ex-presidente Lula tivesse sido candidato à Presidência, o(a) s(a):  

#perfis
# Bolsolulista convicto -> Q12P2_B = 2(bolsonaro) &  p22 = 1 Teria votado nele com certeza
# Bolsolulista relutante -> Q12P2_B =  2(bolsonaro) &  p22= 2 Possivelmente teria votado nele
# Bolsolulista -> Q12P2_B =  2(bolsonaro) & (p22 1 & 2) = certeza ou possivelmente votado nele
# Bolsonarista convicto ->  q2018$P22 == 4 & Q12P1_A = 9 q2018$Q12P2_B = 2 = Bolsonaro
# Não bolsonarista -> Q12P2_B != 2 & q2018$Q12P1_B != 9 -> Outros/branco/nulo/abstenção 
# A amostra toda (média nacional) 
q2018$P22 == 1 -> q2018$certeza_Lula
q2018$P22 == 2 -> q2018$possivel_Lula
q2018$P22 == 4 -> q2018$lulaNever

df$Bolsolulista_convicto <- q2018$certeza_Lula == TRUE & q2018$Q12P2_B == 2

table(df$Bolsolulista_convicto)#130 pessoas para fins de conferência
table(q2018$P22,q2018$Q12P2_B)# 1 e 1 = 130 = deu certo

df$Bolsolulista_relutante <- q2018$possivel_Lula == TRUE & q2018$Q12P2_B == 2
df$Bolsolulista_geral <- df$Bolsolulista_convicto + df$Bolsolulista_relutante
table(df$Bolsolulista_relutante)
table(df$Bolsolulista_geral)
prop.table(table(df$Bolsolulista_convicto))#5.1% do total
prop.table(table(df$Bolsolulista_relutante))#1,9% do total
prop.table(table(df$Bolsolulista_geral))#21.8% do toal
df$Bolsonarista_convicto <- q2018$Q12P2_B == 2 & q2018$Q12P1_B == 9 & q2018$lulaNever == TRUE
table(df$Bolsonarista_convicto)
prop.table(table(df$Bolsonarista_convicto))#25,6% do total

df$Nao_Bolsonarista <- q2018$Q12P2_B != 2 & q2018$Q12P1_B != 9

table(q2018$Q12P1_B)#838 
table(q2018$Q12P2_B)#1023
table(q2018$Q12P2_B,q2018$Q12P1_B)#789
(838+1023)-789 #(voto no primeiro + voto no segundo) - (q votou nos dois)
(2506-1072)#total - resultado = TRUE

table(df$Nao_Bolsonarista)
prop.table(table(df$Nao_Bolsonarista))#57,2%
#coluna 4
basevalid <- q2018
basevalid$x <- q2018$Q12P2_B < 3
table(basevalid$x)
table(basevalid$Q12P2_B)
711+1023#deu certo

basevalid <- basevalid%>%dplyr::filter(x == 1)

basevalid$Bolsolulista_convicto <- basevalid$certeza_Lula == TRUE & basevalid$Q12P2_B == 2
basevalid$Bolsolulista_relutante <- basevalid$possivel_Lula == TRUE & basevalid$Q12P2_B == 2
basevalid$Bolsolulista_geral <- basevalid$Bolsolulista_convicto + basevalid$Bolsolulista_relutante
basevalid$Bolsonarista_convicto <- basevalid$Q12P2_B == 2 & basevalid$Q12P1_B == 9 & basevalid$lulaNever == TRUE
basevalid$Nao_Bolsonarista <- basevalid$Q12P2_B != 2#só haddad
prop.table(table(basevalid$Bolsolulista_convicto))#28,7%  dos válidos
prop.table(table(basevalid$Bolsolulista_relutante))#2,8%  dos válidos
prop.table(table(basevalid$Bolsolulista_geral))#%  dos válidos
prop.table(table(basevalid$Bolsonarista_convicto))#%  dos válidos
prop.table(table(basevalid$Nao_Bolsonarista))#%  dos válidos




rm(basevalid)#remover pq não usará mais

df$Bolsolulista_geral <- as.logical(df$Bolsolulista_geral)


#cores
"#1cff00" # Bolsolulista convicto
"#fca311" #Boslolulista relutante
"#f72585" #Bolsolulista geral
"#6c584c" #Bolsonarista convicto
"#00b4d8" #Não Bolsonarista

#exploratório "opinães"
by(df$nao_igualitario, df$Bolsolulista_convicto, mean)*100# -4.7
by(df$nao_igualitario, df$Bolsolulista_relutante, mean)*100# -1
by(df$nao_igualitario, df$Bolsolulista_geral, mean)*100# -3.7
by(df$nao_igualitario, df$Bolsonarista_convicto, mean)*100# 18.9
by(df$nao_igualitario, df$Nao_Bolsonarista,mean)*100# -6.12

by(df$critico_justiça, df$Bolsolulista_convicto, mean)*100# -16
by(df$critico_justiça, df$Bolsolulista_relutante, mean)*100# -6.1
by(df$critico_justiça, df$Bolsolulista_geral, mean)*100# -13
by(df$critico_justiça, df$Bolsonarista_convicto, mean)*100# 5.01
by(df$critico_justiça, df$Nao_Bolsonarista,mean)*100# -4.32

by(df$majoritarian, df$Bolsolulista_convicto, mean)*100# 31.7
by(df$majoritarian, df$Bolsolulista_relutante, mean)*100# 7.9
by(df$majoritarian, df$Bolsolulista_geral, mean)*100# 25
by(df$majoritarian, df$Bolsonarista_convicto, mean)*100# 8.12
by(df$majoritarian, df$Nao_Bolsonarista,mean)*100# -8.73

by(df$descontente_democracia_e_partidos, df$Bolsolulista_convicto, mean)*100# -22.8
by(df$descontente_democracia_e_partidos, df$Bolsolulista_relutante, mean)*100# -11.9
by(df$descontente_democracia_e_partidos, df$Bolsolulista_geral, mean)*100# -19
by(df$descontente_democracia_e_partidos, df$Bolsonarista_convicto, mean)*100# -4.16
by(df$descontente_democracia_e_partidos, df$Nao_Bolsonarista,mean)*100# 2.49


#decidi não usar mais o bolsolulista geral !

# dados instigantes

df <- subset(df, select=-c(Bolsolulista_geral))

#vamos voltar depois, por enquanto só explorações

# dados sociais

library(labelled)
library(memisc)
df$faixaidade <- memisc::recode(as.factor(q2018$D1A_FAIXAID), "16-24 anos" <- c(1,2),
                                "25-34 anos" <- c(3), "34-44"<-c(4),
                                "45-54 anos"<-c(5),"55-64 anos"<-c(6),
                                "65+ anos"<-c(7))

df$Sexo <- memisc::recode(as.factor(q2018$D2_SEXO), "MASC" <- c(1),
                                "FEM" <- c(2))

df$temp <- q2018$D3_ESCOLA > 6
df$temp <- as.numeric(df$temp)
table(df$temp)
 
df$Superior_ou_acima <-df$temp
df <- subset(df, select=-c(temp))
df$Ensino_Medio <- memisc::recode(as.numeric(q2018$D3_ESCOLA), 1 <- c(6),
                                       0 <- c(0,1,2,3,4,5,7,8,9))

df$Ideologia <- q2018$Q18
df$Ideologia[df$Ideologia  == 95] <- NA
df$Ideologia[df$Ideologia  == 97] <- NA
df$Ideologia[df$Ideologia  == 98] <- NA
df$evangelico <- q2018$D10 == 5

#modelo parte dos atrivuos
#perfil = Escolaridade + Ideologia + evangélico +sexo +faixa_idade
library(sjPlot)

modelo1.1 <- glm(Bolsolulista_convicto~ faixaidade+
                   Sexo+Ensino_Medio+Superior_ou_acima+Ideologia+evangelico,data=df,
                family=binomial(link=logit))
modelo2.1 <- glm(Bolsolulista_relutante~ faixaidade+
                   Sexo+Ensino_Medio+Superior_ou_acima+Ideologia+evangelico,data=df,
                 family=binomial(link=logit))
modelo3.1 <- glm(Bolsonarista_convicto~ faixaidade+
                   Sexo+Ensino_Medio+Superior_ou_acima+Ideologia+evangelico,data=df,
                 family=binomial(link=logit))
modelo4.1 <- glm(Nao_Bolsonarista~ faixaidade+
                   Sexo+Ensino_Medio+Superior_ou_acima+Ideologia+evangelico,data=df,
                 family=binomial(link=logit))

tab_model(modelo1.1,modelo2.1,
          modelo3.1,modelo4.1, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")#pro apêndice

plot_models(modelo1.1,modelo2.1,
            modelo3.1,modelo4.1, vline.color = "orange", ci.lvl = 0.9)#só para conferir, tirar o 2.1
plot_models(modelo1.1,
            modelo3.1,modelo4.1, vline.color = "orange", ci.lvl = 0.9)#só para conferir


plot_models(modelo1.1,
            modelo3.1,modelo4.1,
            vline.color = "red",
            title="Probabilidade de voto (com condicionante), em 2018 no Brasil",
            legend.title = "Atitude",m.labels = c("Bolsolulista convicto",
                                                  "Bolsonarista convicto",
                                                  "Não Bolsonarista"),
            axis.labels=c("É Evangélico? SIM",
                          "Ideologia (valores positivos indicam autoposicionamento à direita)",
                          "Ensino Superior ou acima",
                          "Ensino Médio completo (cat. ref. abaixo de Ensino Médio completo)",
                          "Sexo: Fem",
                          "65+ anos",
                          "55-64 anos",
                          "45-54 anos", "34-44 anos", "25-34 anos (cat. ref. 16-24 anos)"),
            std.est = T,show.p = T,ci.lvl = 0.9, p.shape = TRUE, colors=c("#00b4d8","#6c584c",
                                                                                   "#1cff00"
            ))#modelo final


# gráfico de barras



Perfil <- c("Bolsolulista convicto", "Bolsolulista relutante",
            "Bolsonarista convicto", "Não Bolsonarista",
            "Bolsolulista convicto", "Bolsolulista relutante",
            "Bolsonarista convicto", "Não Bolsonarista",
            "Bolsolulista convicto", "Bolsolulista relutante",
            "Bolsonarista convicto", "Não Bolsonarista",
            "Bolsolulista convicto", "Bolsolulista relutante",
            "Bolsonarista convicto", "Não Bolsonarista")


m1 <- mean(df$nao_igualitario)*100

m2 <- mean(df$critico_justiça)*100

m3 <- mean(df$majoritarian)*100

m4 <- mean(df$descontente_democracia_e_partidos)*100


by(df$nao_igualitario, df$Bolsolulista_convicto, mean)*100# -4.7
by(df$nao_igualitario, df$Bolsolulista_relutante, mean)*100# -1
by(df$nao_igualitario, df$Bolsonarista_convicto, mean)*100# 18.9
by(df$nao_igualitario, df$Nao_Bolsonarista,mean)*100# -6.12

n1 <- c(-4.7-m1,-1.08-m1,18.9-m1,-6.12-m1)

by(df$critico_justiça, df$Bolsolulista_convicto, mean)*100# -16
by(df$critico_justiça, df$Bolsolulista_relutante, mean)*100# -6.1
by(df$critico_justiça, df$Bolsonarista_convicto, mean)*100# 5.01
by(df$critico_justiça, df$Nao_Bolsonarista,mean)*100# -4.32

n2 <-c(-16-m2,-6.1-m2,5-m2,-4.32-m2)

by(df$majoritarian, df$Bolsolulista_convicto, mean)*100# 31.7
by(df$majoritarian, df$Bolsolulista_relutante, mean)*100# 7.9
by(df$majoritarian, df$Bolsonarista_convicto, mean)*100# 8.12
by(df$majoritarian, df$Nao_Bolsonarista,mean)*100# -8.73

n3 <-c(31.7-m3,7.9-m3,8.12-m3,-8.73-m3)

by(df$descontente_democracia_e_partidos, df$Bolsolulista_convicto, mean)*100# -22.8
by(df$descontente_democracia_e_partidos, df$Bolsolulista_relutante, mean)*100# -11.9
by(df$descontente_democracia_e_partidos, df$Bolsonarista_convicto, mean)*100# -4.16
by(df$descontente_democracia_e_partidos, df$Nao_Bolsonarista,mean)*100# 2.49

n4 <-c(-22.8-m4,-11.9-m4,-4.16-m4,2.49-m4)

xx1 <- c(n1,n2,n3,n4)
`Divisão`<-c("Visão Não Igualitária","Visão Não Igualitária",
             "Visão Não Igualitária","Visão Não Igualitária",
             "Crítico à Justiça",
             "Crítico à Justiça",
             "Crítico à Justiça",
             "Crítico à Justiça",
             "Visão Majoritária","Visão Majoritária",
             "Visão Majoritária","Visão Majoritária",
             "Descontente com Democracia","Descontente com Democracia","Descontente com Democracia",
             "Descontente com Democracia")
# o voto dos temp1 em relação ao que ocupam

graf1 <- data.frame(xx1,`Divisão`,Perfil)
library(wesanderson)
a <- ggplot(graf1, aes(x= xx1, y=`Divisão`, fill=`Divisão`))
a+ geom_bar(stat="identity",position=position_dodge(preserve = "single"))+
  coord_flip()+ 
  annotate("segment", x = 0, xend = 0, y = 0, yend = 6,
           colour = "black",size=0.8)+
  facet_grid(.~Perfil) +
  scale_fill_manual(values = wes_palette("Moonrise2"))+
  theme(legend.position="bottom", axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  labs(title= "Distância entre a média dos perfilados e a média nacional em um conjunto de divisões no público",
       subtitle = "BRASIL, 2018",
       x = "Diferença em relação à média nacional",
       y = "",
       caption = "Valores positivos indicam maior propensão que a média nacional. 
       Por exemplo, os Bolsolulistas convictos são, em média, menos críticos à justiça. 
       Os dados são oriundos do ESEB 2018 e perguntavam sobre o voto no mesmo ano e sobre possibilidade de voto em Lula também no mesmo ano.
       .A linha preta indica a média nacional.
       ")#colocar no trabalho





# modeloes finais

modelo1.2 <- glm(Bolsolulista_convicto~ nao_igualitario+critico_justiça+
                   majoritarian + descontente_democracia_e_partidos+
                   faixaidade+Sexo+Ensino_Medio+Superior_ou_acima+
                     Ideologia+evangelico,data=df,
                 family=binomial(link=logit))
modelo2.2 <- glm(Bolsolulista_relutante~ nao_igualitario+critico_justiça+
                   majoritarian + descontente_democracia_e_partidos+
                   faixaidade+Sexo+Ensino_Medio+Superior_ou_acima+
                   Ideologia+evangelico,data=df,
                 family=binomial(link=logit))
modelo3.2 <- glm(Bolsonarista_convicto~ nao_igualitario+critico_justiça+
                   majoritarian + descontente_democracia_e_partidos+
                   faixaidade+Sexo+Ensino_Medio+Superior_ou_acima+
                   Ideologia+evangelico,data=df,
                 family=binomial(link=logit))
modelo4.2 <- glm(Nao_Bolsonarista~ nao_igualitario+critico_justiça+
                   majoritarian + descontente_democracia_e_partidos+
                   faixaidade+Sexo+Ensino_Medio+Superior_ou_acima+
                   Ideologia+evangelico,data=df,
                 family=binomial(link=logit))

tab_model(modelo1.2,modelo2.2,
          modelo3.2,modelo4.2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")#pro apêndice

#usar idade continua
q2018$D1A_ID -> df$idade_cont
remove_labels(df) -> df
modelo1.22 <- glm(Bolsolulista_convicto~ nao_igualitario+critico_justiça+
                   majoritarian + descontente_democracia_e_partidos+
                    idade_cont+Sexo+Ensino_Medio+Superior_ou_acima+
                   Ideologia+evangelico,data=df,
                 family=binomial(link=logit))
modelo2.22 <- glm(Bolsolulista_relutante~ nao_igualitario+critico_justiça+
                   majoritarian + descontente_democracia_e_partidos+
                    idade_cont+Sexo+Ensino_Medio+Superior_ou_acima+
                   Ideologia+evangelico,data=df,
                 family=binomial(link=logit))
modelo3.22 <- glm(Bolsonarista_convicto~ nao_igualitario+critico_justiça+
                   majoritarian + descontente_democracia_e_partidos+
                    idade_cont+Sexo+Ensino_Medio+Superior_ou_acima+
                   Ideologia+evangelico,data=df,
                 family=binomial(link=logit))
modelo4.22 <- glm(Nao_Bolsonarista~ nao_igualitario+critico_justiça+
                   majoritarian + descontente_democracia_e_partidos+
                    idade_cont+Sexo+Ensino_Medio+Superior_ou_acima+
                   Ideologia+evangelico,data=df,
                 family=binomial(link=logit))
plot_models(modelo1.22,modelo2.22,
            modelo3.22,modelo4.22, vline.color = "orange", ci.lvl = 0.9)#só para conferir, ñ tirar o 2.22
plot_models(modelo1.22,modelo2.22,
            modelo3.22,modelo4.22, vline.color = "orange",
            std.est = T,show.p = T,ci.lvl = 0.9, p.shape = TRUE)#só para conferir
#summary(modelo1.2)
a<- plot_models(modelo1.22,modelo2.22,
                modelo3.22,modelo4.22,
            vline.color = "red",
            title="",
            legend.title = "Atitude",m.labels = c("Bolsolulista convicto",
                                                  "Bolsolulista relutante",
                                                  "Bolsonarista convicto",
                                                  "Não Bolsonarista"),
            rm.terms=c("idade_cont",
                       "SexoFEM"),
            axis.labels=c("É Evangélico? SIM",
                          "Ideologia (valores positivos indicam autoposicionamento à direita)",
                          "Ensino Superior ou acima",
                          "Ensino Médio completo (cat. ref. abaixo de Ensino Médio completo)",
                          "Descontentamento com a democracia",
                          "Visão Majoritária","Crítico à Justiça",
                          "Visão não igualitária"),
            std.est = T,show.p = T,ci.lvl = 0.9, p.shape = TRUE, colors=c("#00b4d8","#6c584c",
                                                                                   "#fca311",
                                                                                   "#1cff00"
            ))#modelo final
a + labs(caption="Observação: Idade e Gênero removido do gráfico para obtenção de apresentação mais limpa")

# 

#prediciton

library(marginaleffects)

summary(df[,2:5])


predictions(modelo1.22, newdata = datagrid(Superior_ou_acima = 0,Ensino_Medio = 0,
                                           majoritarian = 0.55, nao_igualitario = -.4,
                                           critico_justiça = -.36,
                                           descontente_democracia_e_partidos = -0.43, 
                                           Ideologia = 10))
predictions(modelo1.22, newdata = datagrid(Superior_ou_acima = 1,Ensino_Medio = 0,
                                           majoritarian = -.6, nao_igualitario = .42,
                                           critico_justiça = .36,
                                           descontente_democracia_e_partidos = 0.42, 
                                           Ideologia = 0))

estimativa <-c(9.8,14.3,20.3,0.04,0.01,2)



Personagem <- c("A","A","A","B","B","B")
graf2 <- data.frame(Personagem,estimativa)

b <- ggplot(graf2, aes(x= estimativa, y=Personagem, fill=Personagem))
b+ geom_bar(stat = "summary", fun = "mean",
            position=position_dodge(preserve = "single"),size=0.4, alpha=0)+
  theme(legend.position="none", axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  coord_flip()+
  labs(title= "Predições de bolsolulista convicto",
       subtitle = "",
       x = "Estimativa",
       y = "",
       caption = "Observações: O modelo estima que um indivíduo com os atributos do personagem a tenha 14,3% (entre 9.8 e 20.3%) de chance de ser um Bolsolulista Convicto.
       a linha azul indica a presença de bolsolulistas convictos na amostra toda (5,1%).
       A linha vermelha indica quanto essa presença representa do total dos votos válidos no segundo turno na amostra (7,5%).
       O Personagem A foi simulado como tendo escolaridade baixa e autoposicionamento à direita, valores de quartil superior de visão majoritária e
       de quartil inferior de visão não igualitária, crítico a justiça e de descontetamento com a democracia .
       O Personagem B foi simulado como tendo escolaridade de nível superior e autoposicionamento à esqueda, valores de quartil inferior de visão majoritária e
       de quartil superior de visão não igualitária, crítico a justiça e de descontetamento com a democracia .
       O resultado completo da estimativa pode ser obtido no apêndice")+
  annotate("segment", x = 5.1, xend = 5.1, y = 0.5,
           yend = 2.5,colour = "blue",size=0.8)+
  annotate("segment", x = 7.5, xend = 7.5, y = 0.5,
           yend = 2.5,colour = "red",size=0.8)+
  annotate("text", x = 8.2, y = 2, label = "% dos bolsolulistas considerando apenas os que votaram em algum candidato", color="red")+
  annotate("text", x = 5.7, y = 2, label = "% dos bolsolulistas convictos no total da amostra", color="blue")+
  annotate("text", x = 14.3, y = 1.1, label = "14.3%", color="black")+
  annotate("text", x = 0.24, y = 2.12, label = "0.04%", color="black")+
  annotate("text", x = 15, y = 1.12, label = "Personagem A", color="#D8B70A")+
  annotate("text", x = 0.84, y = 2.12, label = "Personagem B", color="#02401B")+
  annotate("pointrange", x = 14.3, y = 1, xmin = 9.8, xmax = 20.3,
           colour = "#D8B70A", size = 1, linewidth = 1.5)+
  annotate("pointrange", x = 0.04, y = 2, xmin = 0.01, xmax = 2,
           colour = "#02401B", size = 1, linewidth = 1.5)+
  scale_fill_manual(values = wes_palette("Cavalcanti1"))


# casos parcos
qtemp <- remove_labels(q2018)
df$naovotouemBolsonaronoprimeiromasvotounosegundo <- q2018$Q12P2_B != 2 & q2018$Q12P1_B == 9
summary(df$naovotouemBolsonaronoprimeiromasvotounosegundo)


summary(df[,2:17])



#apêndice VII

a<- plot_cap(modelo2.22, condition="idade_cont")
a
b<- plot_cap(modelo3.22, condition = "idade_cont")
b
library(gridExtra)
grid.arrange(a,b,ncol=2)
