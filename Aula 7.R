# AULA 6
# COPIEM O CÓDIGO DA AULA 5 (AVA)

# API (PORTA/FORMA DE ACESSO REMOTO)
# ACESSAMOS OS DADOS DO BANCO MUNDIAL (WORLD BANK)
# WORLD DEVELOPMENT INDICATORS (WDI)
# INDICADORES DE DESENVOLVIMENTO MUNDIAL

# PIB (PRODUTO INTERNO BRUTO)

#install.packages('WDI')
library(WDI) # CARREGAR BIBLIOTECA/PACOTE

options(scipen = 999) # REMOVER A NOT. CIENT.

# DADOS EM PAINEL
dadospib <- WDI(country = 'all',
                indicator = 'NY.GDP.MKTP.CD')

paises <- c('BR', 'US')

dadosELECTRICITY <- WDI(country = paises,
                        indicator = 'EG.ELC.ACCS.ZS')

# CORTE TRANSVERSAL
dadosELECTRICITY2023 <- WDI(country = 'all',
                            indicator = 'EG.ELC.ACCS.ZS',
                            start = 2023, end = 2023)

# SÉRIE TEMPORAL
dadosELECTRICITYbr <- WDI(country = 'BR',
                          indicator = 'EG.ELC.ACCS.ZS')
# GRÁFICOS
# BIBLIOTECA ggplot2 (tidyverse)
#install.packages("tidyverse")
library(tidyverse)

# Carregar as bibliotecas necessárias
library(WDI)
library(ggplot2)
library(dplyr)

# Dados para o acesso à eletricidade (BR e US)
paises <- c('BR', 'US')

# Dados de acesso à eletricidade para Brasil e EUA
dadosELECTRICITY <- WDI(country = paises,
                        indicator = 'EG.ELC.ACCS.ZS')

# Ajuste no gráfico: removendo valores que não fazem sentido (e.g. anos sem dados relevantes)
dadosELECTRICITY <- dadosELECTRICITY %>% 
  filter(!is.na(EG.ELC.ACCS.ZS))  # Remover valores NA

# Gráfico de painel para o acesso à eletricidade
grafpainel <- ggplot(data = dadosELECTRICITY, 
                     mapping = aes(x = year, y = EG.ELC.ACCS.ZS)) +
  # Todos os países com pontos em cinza claro e transparência
  geom_point(alpha = 0.5, color = "gray70") +
  # Destacar o Brasil (pontos vermelhos)
  geom_point(data = filter(dadosELECTRICITY, country == "Brazil"),
             color = "red", size = 3) +
  # Destacar os EUA (pontos azuis)
  geom_point(data = filter(dadosELECTRICITY, country == "United States"),
             color = "blue", size = 3) +
  # Adicionar linha de tendência para o Brasil
  geom_smooth(data = filter(dadosELECTRICITY, country == "Brazil"), 
              aes(x = year, y = EG.ELC.ACCS.ZS), 
              color = "red", size = 1.5, method = "loess", se = FALSE) +
  # Adicionar linha de tendência para os EUA
  geom_smooth(data = filter(dadosELECTRICITY, country == "United States"), 
              aes(x = year, y = EG.ELC.ACCS.ZS), 
              color = "blue", size = 1.5, method = "loess", se = FALSE) +
  # Título e rótulos dos eixos
  labs(title = "Acesso à Eletricidade ao Longo do Tempo",
       x = "Ano",
       y = "Acesso à Eletricidade (%)") +
  # Tema mais moderno
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none"
  ) +
  # Limitar o eixo Y para valores entre 0 e 100
  ylim(0, 100)

# Exibir o gráfico
print(grafpainel)

