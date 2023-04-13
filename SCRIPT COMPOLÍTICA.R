#SCRIPT PARA COMPOLÍTICA 

#Dados quali:https://rkofreitag.github.io/Categorica.html

##Teste de qui-quadrado 1####
#EXISTE RELAÇÃO ENTRE 1- TIPO DE PÁGINAS X POSSUIR OU NÃO RT?

Teste1 <- table(coprus_compolitica_higienizado_Copia$PáginaRec, 
                coprus_compolitica_higienizado_Copia$`Possui RT X Não Possui`)


options(scipen = 999, digits = 4)

chisq.test(Teste1)

# Pearson's Chi-squared test
# 
# data:  Teste1
# X-squared = 687.7, df = 16, p-value < 0.00000000000000022



table(coprus_compolitica_higienizado_Copia$Página)


# AlineSleutjes        BOLSONEAS  CLAUDIAFREITA25  CLAUDIAFREITA30 
# 75                9                1                1 
# CLAUDIAFREITA32  CLAUDIAFREITA35  CLAUDIAFREITA36  CLAUDIAFREITA37 
# 1                1                1                1 
# CLAUDIAFREITA38         EuSouMBC   evertonsodario  GutoZacariasMBL 
# 1              664                9                1 
# GutoZacariasMBL;    JaniceTavares    KIM KATAGUIRI LuisPau78888694; 
# 94               59              398                1 
# MQEBrasil 
# 112 

library(memisc)


coprus_compolitica_higienizado_Copia$PáginaRec <- recode(
  coprus_compolitica_higienizado_Copia$Página,
  "CLAUDIAFREITA" <- c("CLAUDIAFREITA25",
                       "CLAUDIAFREITA30",
                       "CLAUDIAFREITA32",
                       "CLAUDIAFREITA35", 
                       "CLAUDIAFREITA36",
                       "CLAUDIAFREITA37",
                       "CLAUDIAFREITA38"),
  "Outros" <- c("BOLSONEAS", "evertonsodario", "LuisPau78888694"),
  "JaniceTavares" <- "JaniceTavares",
  "AlineSleutjes" <- "AlineSleutjes",
  "GutoZacariasMBL" <- c("GutoZacariasMBL", "GutoZacariasMBL"),
  "MQEBrasil" <- "MQEBrasil",
  "KIM KATAGUIRI" <- "KIM KATAGUIRI",
  "EuSouMBC" <- "EuSouMBC"
)

#Salvar a base
library(writexl)

#salvando
writexl::write_xlsx(coprus_compolitica_higienizado_Copia, 
                    path = "Base1.xlsx")

# Esta função realiza o teste de qui-quadrado (sem correção, 
# por isso há uma pequena diferença em relação ao que foi 
# calculado anteriormente) e calcula as estatísticas de associação possíveis.
# 
# Aogra que já sabemos o resultado, vamos reportá-lo. 
# Para variáveis categóricas, apresentamos a contagem e a frequência 
# e o resultado dos testes de independência e associação.
# 
# Podemos transpor as tabelas e os cálculos para o texto do relatório 
# (resumo, artigo, dissertacão, tese). Um dos pontos negativos evocados 
# para o não uso do R é que a apresentação gráfica dos resultados não é 
# boa e que a realização dos testes exige muitos procedimentos. Existe 
# um movimento dos usuários do R chamado EasyStats, com pacotes de funções 
# que facilitam a rotina 
# de procedimentos. Vamos usar alguns deles para ver as possibilidades.

#refazer o teste com a base nova

Teste1 <- table(Base1$PáginaRec, 
                Base1$`Possui RT X Não Possui`)


options(scipen = 999, digits = 4)

chisq.test(Teste1)

library(vcd)
assocstats(Teste1)

#                     X^2 df P(> X^2)
# Likelihood Ratio 796.17  7        0
# Pearson           674.64  7        0
# 
# Phi-Coefficient   : NA 
# Contingency Coeff.: 0.58 
# Cramer's V        : 0.711

#Para visualizar os dados em uma tabela
library(sjPlot)  

sjt.xtab(Base1$PáginaRec, 
         Base1$`Possui RT X Não Possui`,
  show.row.prc = T,  ### solicitamos que os resultados sejam apresentados em frequências
  show.summary = T, ### solicitamos o sumário dos testes inferenciais
  statistics = "cramer") ### informamos qual o teste solicitado; se esta linha for retirada, o pacote seleciona automaticamente um teste

Teste1 <- chisq.test(Base1$PáginaRec, 
                     Base1$`Possui RT X Não Possui`)

#ANÁLISE DOS RESÍDUOS
round(Teste1$residuals,3)

#ANÁLISE GRÁFICA 
library(corrplot)

corrplot(Teste1$residuals, is.corr = F)


# Nesta matriz de correlação, ou correlograma, o tamanho do círculo é proporcional à contribuição da célula para o efeito.
# 
# Resíduos positivos estão em azul e sinalizam uma associação positiva entre linha e coluna.
# Resíduos negativos estão em vermelho e sinalizam uma associação negativa entre linha e coluna.

##Teste de qui-quadrado 2####
#EXISTE RELAÇÃO ENTRE 2 - TIPO DE PÁGINAS X POSSUIR OU NÃO LINK?
Teste2 <- table(Base1$PáginaRec, 
                Base1$`Possui link X Não possuí`)


options(scipen = 999, digits = 4)

chisq.test(Teste2)

# Pearson's Chi-squared test
# 
# data:  Teste2
# X-squared = 342, df = 7, p-value <0.0000000000000002

library(vcd)
assocstats(Teste2)
#                     X^2 df P(> X^2)
# Likelihood Ratio 397.84  7        0
# Pearson          342.18  7        0
# 
# Phi-Coefficient   : NA 
# Contingency Coeff.: 0.44 
# Cramer's V        : 0.489 

#Para visualizar os dados em uma tabela
library(sjPlot)  

sjt.xtab(Base1$PáginaRec, 
         Base1$`Possui link X Não possuí`,
         show.row.prc = T,  ### solicitamos que os resultados sejam apresentados em frequências
         show.summary = T, ### solicitamos o sumário dos testes inferenciais
         statistics = "cramer") ### informamos qual o teste solicitado; se esta linha for retirada, o pacote seleciona automaticamente um teste

Teste2 <- chisq.test(Base1$PáginaRec, 
                     Base1$`Possui link X Não possuí`)

#ANÁLISE DOS RESÍDUOS
round(Teste2$residuals,3)

#ANÁLISE GRÁFICA 
library(corrplot)

corrplot(Teste2$residuals, is.corr = F)



