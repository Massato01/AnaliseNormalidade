# Instalando bibliotecas necessárias
# install.packages("corrplot")
# install.packages("ggplot2")
# ---------------------------------------------------------------------------------------------------
# Bibliotecas
library(corrplot) 
library(ggplot2)
library(readr)

# Importando o arquivo CSV - winequality-red
# O dataset já está limpo
df <- read.csv("winequality-red.csv")
View(df)
# ---------------------------------------------------------------------------------------------------
# Mapa de calor das correlações de todas as variáveis
grafico_correlacao <- corrplot(cor(df))
# ---------------------------------------------------------------------------------------------------
# Definindo as variáveis
X <- df$fixed.acidity
y <- df$citric.acid

# Gráfico de dispersão de Fixed Acidity e Citric Acid
plot(X, y, xlab = "Fixed Acidity", ylab = "Citric Acid", main = "Gráfico de Dispersão")
# ---------------------------------------------------------------------------------------------------
# Média
media_x <- mean(X) # 8.32
media_y <- mean(y) # 0.27

# Variância
variancia_x <- var(X) # 3.03 
variancia_y <- var(y) # 0.04

# Desvio Padrão
desvio_padrao_x <- sd(X) # 1.74
desvio_padrao_y <- sd(y) # 0.19

# Mediana
mediana_x <- median(X) # 7.9 
mediana_y <- median(y) # 0.26
# ---------------------------------------------------------------------------------------------------
# Histograma de Fixed Acidity
hist(X, main = "Histograma de Fixed Acidity", xlab = "x", ylab = "Frequência")

# Histograma de Citric Acid
hist(y, main = "Histograma de Citric Acid", xlab = "y", ylab = "Frequência")
# ---------------------------------------------------------------------------------------------------
# Boxplot de Fixed Acidity
boxplot(X, main = "Boxplot de Fixed Acidity", ylab = "Fixed Acidity", col = "royalblue")

# Boxplot de Citric Acid
boxplot(y, main = "Boxplot de Citric Acid", ylab = "Citric Acid", col = "royalblue")
# ---------------------------------------------------------------------------------------------------
# Correlação entre Fixed Acidity e Citric Acid
correlacao <- cor(X, y) # 0.67
# ---------------------------------------------------------------------------------------------------
# Teste de normalidade de Fixed Acidity
#  W = Quanto mais próximo de 1, mais normal é a distribuição. De Fixed Acidity é 0.94, então é bem normal
#  p-value = O quão compatível os dados são com a hipótese de que seguem uma dist. normal. De Fixed Acidity é quase 0, então
#           rejeitaria a hipótese nula (hipótese de que os dados vem de uma dist. normal). A dist. de X não é normal.
teste_normal_x <- shapiro.test(X) # W = 0.94203, p-value < 2.2e-16
print(teste_normal_x)

# Teste de normalidade de Citric Acid
teste_normal_y <- shapiro.test(y) # W = 0.95529, p-value < 2.2e-16
print(teste_normal_y)

# Gráfico de densidade junto com o histograma de Fixed Acidity
ggplot(data = df, aes(x = X)) +
geom_histogram(aes(y = after_stat(density)), fill = "royalblue", color = "black", binwidth = 0.5) +
geom_density(alpha = 0.2, color = "red")
# ---------------------------------------------------------------------------------------------------
# Regressão Linear Simples de Fixed Acidity e Citric Acid
modelo <- lm(y ~ X)

# Visualizando os resultados do modelo
summary(modelo)

ggplot(df, aes(x = citric.acid, y = fixed.acidity)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(x = "Citric Acid", y = "Fixed Acidity", title = "Regressão Linear Simples")