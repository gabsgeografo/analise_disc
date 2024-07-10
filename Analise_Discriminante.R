# UEPG - Programa de Pos-Graduacao em Agronomia
# Metodos em Estatista Multivariada

################################################################################
####                     Análise de Discriminante de Fischer (ADF)
################################################################################

#' # Carregar pacotes
library(MASS)
library(candisc) # Faz a ADF
library(ggplot2) # Constroi o grafico
library(heplots)
library(broom)


#' # Carregar dados
dados <- read.csv("Euroemp.csv", header = T, row.names = 1);  # 'row.names' a primeira coluna eh lida como rotulo 
head(dados)
attach(dados)


#' # MANOVA para obter o lambda de Wilks - verifica se tem diferenca estatistica entre os grupos
emp.mnv <- manova(as.matrix(dados[,-1])~ Group, data=dados)
summary.manova(emp.mnv, test="Wilks")


#' Análise discriminante - por modelo linear
mod <- lm(cbind(AGR,MIN,MAN,PS,CON,SER,FIN,SPS)~Group,data=dados) # Variável TC foi removida por causa da dep. linear
mod.can <- candisc(mod, data=dados) # Faz o calculo da ADF
summary(mod.can) # Expressa a saida da funcao 

# Para obter as variaveis canonicas
mod.can$structure

# Faz a correlacao com todas as variaveis
cor(cbind(AGR,MIN,MAN,PS,CON,SER,FIN,SPS,TC),mod.can$scores[,2:4]) # Incluindo a variável TC

# Plota o grafico da funcao Candisc
plot(mod.can, which = c(1,2))
plot(mod.can, which = c(1,3))

# Extraindo os escores
scores <- mod.can$scores
scores$Group <- dados$Group
scores


# Graficamente - usando o 'ggplot2'

ggplot(scores, aes(x = Can1, y = Can2, color = Group, label = rownames(scores))) +
  geom_point(size = 3) +
  stat_ellipse(level = 0.95) +
  labs(x = "Canônica 1",
       y = "Canônica 2") +
  theme_bw()+
  geom_label()+
  geom_text(check_overlap = TRUE)


# Adicionando a terceira canônica ao gráfico
ggplot(scores, aes(x = Can1, y = Can3, color = Group, label = rownames(scores))) +
  geom_point(size = 3) +
  stat_ellipse(level = 0.95) +
  labs(x = "Canônica 1",
       y = "Canônica 2") +
  theme_bw()+
  geom_label()+
  geom_text(check_overlap = TRUE)


# Plotando a correlação das variáveis originais com os escores canônic0s
correlations <- cor(cbind(AGR, MIN, MAN, PS, CON, SER, FIN, SPS, TC), mod.can$scores[, 2:4])
correlations_df <- as.data.frame(as.table(correlations))
colnames(correlations_df) <- c("Variable", "Canonical", "Correlation")

ggplot(correlations_df, aes(x = Canonical, y = Correlation, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Canônica",
       y = "Correlação") +
  theme_bw()

detach(dados)

