# Projeto 2 - Prevendo a Demanda de Estoque com Base em Vendas

# Rosane Moreira Barbosa

# Objetivo:
# Em resumo, neste projeto em linguagem R, o objetivo é desenvolver um modelo de
# aprendizado de máquina para prever a demanda de estoque com base nos dados
# históricos de vendas de produtos de panificação.


# ***** Descrição das Variáveis *****
# Semana —> Week number (From Thursday to Wednesday)
# Agencia_ID —> Sales Depot ID
# Canal_ID —> Sales Channel ID
# Ruta_SAK —> Route ID (Several routes = Sales Depot)
# Cliente_ID —> Client ID
# NombreCliente —> Client name
# Producto_ID —> Product ID
# NombreProducto —> Product Name
# Venta_uni_hoy —> Sales unit this week (integer)
# Venta_hoy —> Sales this week (unit: pesos)
# Dev_uni_proxima —> Returns unit next week (integer)
# Dev_proxima —> Returns next week (unit: pesos)
# Demanda_uni_equil —> Adjusted Demand (integer) (This is the target you will predict)



# ************************* Carregando os Dados *************************
# ******* Tamanho do arquivo de treino: 3.6 GB (muito grande!!!) ********
# ***********************************************************************

library(data.table)
library(dplyr)

treino <- fread("train.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE) 

dim(treino)
head(treino)



# ********** Criando uma amostra aleatória dos dados de treino **********
# *********** Utilizando a amostra para as análises e treino ************
# ***********************************************************************

set.seed(10)
amostra <- sample_n(treino, size = 500000, replace = FALSE)

# Gravando o arquivo de amostra
write.csv(amostra, "amostra.csv")

# Carregando a amostra gerada a partir do arquivo train do Kaggle
amostra <- fread("amostra.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

class(amostra)

dim(amostra)
head(amostra)

# Verificando se existem valores ausentes (missing) em cada coluna
sapply(amostra, function (x) sum(is.na(x)))

# Verificando os tipos das variáveis
str(amostra)

# Visualizando os dados
View(amostra)

# Verificando a quantidade de registros com a variável 'Demanda_uni_equil' zerada
count(amostra, Demanda_uni_equil == 0)
demanda_zero <- filter(amostra, Demanda_uni_equil == 0)
View(demanda_zero)

# Removendo o dataset treino para liberar a memória
rm(treino)


# ******************* Análise Exploratória dos Dados *******************
#
# **********************************************************************

# Medidas de tendência central das variáveis
summary(amostra)

# Boxplots
boxplot(amostra$Agencia_ID, main = "Boxplot de Agencia_ID", ylab = "Agencias - Depósito de Vendas")
boxplot(amostra$Ruta_SAK, main = "Boxplot de Ruta_SAK", ylab = "Rota - Depósito de Vendas")
boxplot(amostra$Producto_ID, main = "Boxplot dos Produtos", ylab = "Produtos")

# *** Comentário sobre os boxplots: 
# Verifica-se que existem alguns outliers nos boxplots das variáveis Agencia_ID e
# Ruta_SAK.


# Histograma da variável target
hist(amostra$Demanda_uni_equil,
     breaks = 100,
     xlab = "Unidades Demandadas",
     main = "Histograma da Demanda Ajustada",
     xlim = range(0:4500))


# Contagem das unidades demandadas
count <- table(amostra$Demanda_uni_equil)

barplot(count,
        main = "Demanda Ajustada",
        xlab = "Unidades",
        ylab = "Frequência")


library(ggplot2)

# Plot Venta_uni_hoy X Semana
ggplot(amostra, aes(x = Semana, y = Venta_uni_hoy )) +
  geom_point() +
  xlab("Semana") +
  ylab("Vendas (unidades)") +
  ggtitle("Unidades Vendidas por Semana") +
  theme(plot.title = element_text(size = 18))


# Plot Dev_uni_proxima X Semana
ggplot(amostra, aes(x = Semana, y = Dev_uni_proxima )) +
  geom_point() +
  xlab("Semana") +
  ylab("Devoluções (unidades)") +
  ggtitle("Unidades Devolvidas por Semana") +
  theme(plot.title = element_text(size = 18))


# Plot Demanda_uni_equil X Ruta_SAK
ggplot(amostra, aes_string(x = "Ruta_SAK", y = "Demanda_uni_equil" )) +
  geom_point() +
  xlab("Rota - Depósito de Vendas") +
  ylab("Demanda Ajustada (unidades)") +
  ggtitle("Demanda Unitária Ajustada - Rota") +
  theme(plot.title = element_text(size = 18))


# *** Comentários sobre os gráficos:
# 1) Na 3ª semana houve uma quantidade atípica vendida (mais de 4000 unidades). 
# 2) As devoluções ultrapassaram 300 unidades na 3ª, 7ª e 9ª semana, sendo que só
# na terceira semana houve mais de 900 unidades devolvidas.
# 3) Verifica-se no scatter plot que existem picos de demanda em algumas rotas de
# depósito.



# Matriz de Correlação
cor(amostra)

library(corrplot)

# Criando um corrplot
corrplot(cor(amostra), method = 'color')

# *** Comentários:
# 1) Verifica-se que as variáveis 'Venta_uni_hoy' e 'Venta_hoy' possuem uma forte
# correlação positiva com a variável target 'Demanda_uni_equil', contudo estas variáveis
# não existem no arquivo de teste.

# 2) Dentre as demais variáveis, verifica-se que a variável 'Canal_ID' apresentou uma
# correlação (positiva) com a variável target um pouco maior do que as outras. 



# ******************** Pré-processamento dos Dados *********************
# Juntando as tabelas aos dados e criando novas variáveis (médias das  *
# unidades vendidas para as combinações de duplas de variáveis).       * 
# **********************************************************************

# ***** Carregando as tabelas de produtos, local/estado e clientes *****

# Carregando a tabela de produtos
produtos <- read.csv("producto_tabla.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

dim(produtos)
View(produtos)


# Carregando a tabela de local/estados
local_estado <- read.csv("town_state.csv", header = TRUE, sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE)

dim(local_estado)
View(local_estado)


# Carregando a tabela de clientes
clientes <- read.csv("cliente_tabla.csv", header = TRUE, sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE)

dim(clientes)
View(clientes)
str(clientes)

# Eliminando as duplicidades da variável Cliente_ID e mantendo apenas o primeiro registro
clientes <- clientes %>% group_by(Cliente_ID) %>% filter(row_number(NombreCliente) == 1)


# ***** Merge das tabelas de produtos, local e clientes com os dados *****

#juntando a tabela de clientes aos dados
dados <- as.data.frame(amostra %>%
                         left_join(clientes, by = "Cliente_ID"))

# juntando a tabela de produtos aos dados
dados <- as.data.frame(dados %>%
                         inner_join(produtos, by = "Producto_ID"))

# juntando a tabela de local aos dados
dados <- as.data.frame(dados %>%
                         inner_join(local_estado, by ="Agencia_ID"))

dim(dados)
View(dados)
str(dados)



#***** Features Engineering ***** 

# Criando uma nova variável denominada custo_unidade:
dados$custo_unidade <- dados$Venta_hoy/dados$Venta_uni_hoy

# Visualizando a nova variável e checando valores missing
View(dados$custo_unidade)
sum(is.na(dados$custo_unidade))

# Substituindo por zero os valores NA da nova variável (gerados pela divisão por zero)
dados$custo_unidade <- lapply(dados$custo_unidade, function(x) x <- ifelse(is.na(x), 0, x))
dados$custo_unidade <- as.numeric(dados$custo_unidade)


# Sumarizando os dados das semanas de 3 a 9 e agrupando por produto e local para 
# calcular o valor médio das unidades vendidas e das devolvidas: 

tab_Venta_uni_Dev <- dados %>%
  group_by(NombreProducto, Town) %>% 
  summarise(Venta_uni_media = round(mean(Venta_uni_hoy), digits = 0),
            n = n(), DesvPad_Venta_uni = sd(Venta_uni_hoy),
            Dev_uni_media =round(mean(Dev_uni_proxima)))

# Adicionando a media das unidades vendidas e devolvidas por produto e local aos dados
dados <- as.data.frame(dados %>%
                         left_join(tab_Venta_uni_Dev, 
                                   by = c("NombreProducto", "Town"))) 

# Convertendo o tipo das variáveis adicionadas
dados$Venta_uni_media <- as.integer(dados$Venta_uni_media)
dados$Dev_uni_media <- as.integer(dados$Dev_uni_media)



# Sumarizando os dados das semanas de 3 a 9 e agrupando por Agencia_ID e Ruta_SAK:
tab_Agencia_Rota <- dados %>%
  group_by(Agencia_ID, Ruta_SAK) %>% 
  summarise(Venta_uni_AgRota_media = round(mean(Venta_uni_hoy), digits = 0),
            Dev_uni_AgRota_media = round(mean(Dev_uni_proxima)))

# Adicionando a media das unidades vendidas e devolvidas por Agencia e Rota aos dados
dados <- as.data.frame(dados %>%
                         left_join(tab_Agencia_Rota, 
                                   by = c("Agencia_ID", "Ruta_SAK"))) 

# Convertendo po tipo das variáveis adicionadas
dados$Venta_uni_AgRota_media <- as.integer(dados$Venta_uni_AgRota_media)
dados$Dev_uni_AgRota_media <- as.integer(dados$Dev_uni_AgRota_media)



# Sumarizando os dados das semanas de 3 a 9 e agrupando por Ruta_SAK e Producto_ID:

tab_Rota_Produto <- dados %>%
  group_by(Ruta_SAK, Producto_ID) %>% 
  summarise(Venta_uni_RotaProd_media = round(mean(Venta_uni_hoy), digits = 0),
            Dev_uni_RotaProd_media = round(mean(Dev_uni_proxima)))

# Adicionando a media das unidades vendidas e devolvidas por Rota e Produto aos dados 
dados <- as.data.frame(dados %>%
                         left_join(tab_Rota_Produto, 
                                   by = c("Ruta_SAK", "Producto_ID"))) 

# Convertendo po tipo das variáveis adicionadas
dados$Venta_uni_RotaProd_media <- as.integer(dados$Venta_uni_RotaProd_media)
dados$Dev_uni_RotaProd_media <- as.integer(dados$Dev_uni_RotaProd_media)



# Sumarizando os dados das semanas de 3 a 9 e agrupando por cliente e canal:
tab_Cliente_Canal <- dados %>%
  group_by(NombreCliente, Canal_ID) %>% 
  summarise(Venta_uni_CliCa_media = round(mean(Venta_uni_hoy), digits = 0),
            Dev_uni_CliCa_media = round(mean(Dev_uni_proxima)))

# Adicionando a media das unidades vendidas e devolvidas por cliente e rota aos dados
dados <- as.data.frame(dados %>%
                         left_join(tab_Cliente_Canal, 
                                   by = c("NombreCliente", "Canal_ID"))) 

# Convertendo po tipo das variáveis adicionadas
dados$Venta_uni_CliCa_media <- as.integer(dados$Venta_uni_CliCa_media)
dados$Dev_uni_CliCa_media <- as.integer(dados$Dev_uni_CliCa_media)


# Visualizado os tipos e os dados com as novas variáveis adicionadas
str(dados)
View(dados)



# ********* Seleção das variáveis mais relevantes para o modelo *********
# ***** Verificando a correlação com a inclusão das novas variáveis ***** 
# ***********************************************************************


# Definindo as colunas para verificar a correlação com a inclusão das novas variáveis
cols_dados <- c("Semana", "Agencia_ID", "Canal_ID", "Ruta_SAK", "Cliente_ID", 
                   "Producto_ID", "Venta_uni_hoy", "Venta_hoy", "Dev_uni_proxima",
                   "Dev_proxima", "custo_unidade", "Venta_uni_media", "Dev_uni_media",
                   "Venta_uni_AgRota_media", "Dev_uni_AgRota_media", 
                   "Venta_uni_RotaProd_media", "Dev_uni_RotaProd_media",
                   "Venta_uni_CliCa_media", "Dev_uni_CliCa_media",
                   "Demanda_uni_equil") 

cor(dados[cols_dados])


# *** Comentários:
# 1) As variáveis adicionadas aos dados 'Venta_uni_media' das combinações (Rota-Produto,
# Produto-Agência, Agência-Rota e Cliente-Canal apresentaram uma correlação mais forte 
# com a variável target Demanda_uni_equil.

# 2) As combinações da variável adicionada "Dev_uni" (unidades devolvidas) não apresentaram
# uma correlação significativa com a variável target "Demanda_uni_equil".



# ************** Divisão em Dados de Treino e de Validação **************
# ***** Treinaremos o modelo com os dados das semanas de 3 a 7 e os *****
#  dados das semanas 8 e 9 serão utilizados para a validação do modelo. * 
# ***********************************************************************

# Divisão dos dados (Treino: Semanas de 3 a 7; Validação: Semanas de 8 e 9)
dados_treino <- subset(dados, Semana <= 7)
dados_valida <- subset(dados, (Semana == 8) | (Semana == 9))

# Verificando a proporção obtida para os dados de treino e de validação
round((nrow(dados_treino)/nrow(dados))*100, digits = 0)
round((nrow(dados_valida)/nrow(dados))*100, digits = 0)



# ****************** Construção dos Modelos Preditivos ******************
#                                                                       *
# Treinando os modelos com as variáveis que constam no arquivo de teste *
# (Semana,Agencia_ID,Canal_ID,Ruta_SAK,Cliente_ID,Producto_ID) e com    *
# as variáveis adicionadas Venta_uni_media geradas pelas sumarizações e *
# agrupamentos das combinações das variáveis originais dos dados.       *
# ***********************************************************************

library(ModelMetrics)


# *** Primeiro Modelo: Regressão Linear sem Seleção de Variáveis (dados originais) ***

modelo_reg <- lm(Demanda_uni_equil ~ Semana + Agencia_ID + Canal_ID + Ruta_SAK + Cliente_ID +
                   Producto_ID, 
                 data = dados_treino)

# Avaliando a performance do modelo
summary(modelo_reg)

# Testando o modelo nos dados de validação
demanda_reg <- data.frame(actual = dados_valida$Demanda_uni_equil,
                         prediction = round(predict(modelo_reg, newdata = dados_valida)))

# Calculando a métrica RMSE (root mean squared error)
rmse(demanda_reg$actual, demanda_reg$prediction)

# Calculando a métrica RMSLE (root mean squared logarithmic error)
# ou seja, raiz do erro logarítmico quadrático médio
rmsle(demanda_reg$actual, demanda_reg$prediction)



# *** Segundo Modelo: Regressão Linear com Seleção das Variáveis Criadas (Novas) ***

modelo_reg2 <- lm(Demanda_uni_equil ~ Venta_uni_RotaProd_media + Venta_uni_media +
                    Venta_uni_AgRota_media + Venta_uni_CliCa_media,
                  data = dados_treino)

# Avaliando a performance do modelo 
summary(modelo_reg2)

# Testando o modelo nos dados de validação
demanda_reg2 <- data.frame(actual = dados_valida$Demanda_uni_equil,
                          prediction = round(predict(modelo_reg2, newdata = dados_valida)))

View(demanda_reg2)

# Calculando a métrica RMSE (root mean squared error)
rmse(demanda_reg2$actual, demanda_reg2$prediction)

# Substituindo por zero os (7) valores previstos negativos para a variável target
# (Demanda_uni_equil) a fim de calcular o RMSLE
demanda_reg2$prediction <- lapply(demanda_reg2$prediction, function (x) x <- ifelse(x < 0, 0, x ))
demanda_reg2$prediction <- as.integer(demanda_reg2$prediction)

# Calculando o RMSLE (root mean squared logarithmic error)
rmsle(demanda_reg2$actual, demanda_reg2$prediction)



# ***** Terceiro Modelo: XGBOOST (eXtreme Gradient Boosting) ***** 

library(caret)  # pacote de ML
library(xgboost)

# Suprimindo warninngs
warning = function() { return(NULL) }

# Vetor com as variáveis novas seleciondas (maior correlação com a variável target) 
new_vars <- c("Venta_uni_RotaProd_media", "Venta_uni_media",
          "Venta_uni_AgRota_media", "Venta_uni_CliCa_media")

# Transformando os datasets em matriz densa
dtreino <- xgb.DMatrix(data = as.matrix(dados_treino[ , new_vars]),
                      label = dados_treino$Demanda_uni_equil)
dvalida <- xgb.DMatrix(data = as.matrix(dados_valida[ , new_vars]),
                       label = dados_valida$Demanda_uni_equil)

# Watchlist com os datasets para a avaliação do RMSE em cada iteração
watchlist = list(train=dtreino, valid=dvalida)

# Treinando o modelo
xgb_model = xgb.train(data = dtreino,
                      max.depth=5,
                      eta = 0.1,     # controla a learning rate
                      nthread = 2, 
                      nround = 1000, 
                      watchlist = watchlist, 
                      objective = "reg:squarederror", 
                      early_stopping_rounds = 10,
                      print_every_n = 100)

# Print do modelo treinado
print(xgb_model)

# Previsões com os dados de validação
dvalida_vars <- xgb.DMatrix(data = as.matrix(dados_valida[ , new_vars]) )
pred_xgb <- round(predict(xgb_model, newdata = dvalida_vars))

View(pred_xgb)

# Substituindo por zero os valores previstos negativos para a variável target(Demanda_uni_equil) 
pred_xgb <- lapply(pred_xgb, function (x) x <- ifelse(x < 0, 0, x ))
pred_xgb <- as.integer(pred_xgb)

# Calculando o RMSLE (root mean squared logarithmic error)
rmsle(dados_valida$Demanda_uni_equil, pred_xgb)

# Calculando os resíduos
residuos <- pred_xgb - dados_valida$Demanda_uni_equil

# Plot dos resíduos
plot(residuos)

# Calculando o R2
demanda_media = mean(dados_valida$Demanda_uni_equil)
sqt = sum((dados_valida$Demanda_uni_equil - demanda_media)^2)
sqres = sum(residuos^2)
R2 = 1 - (sqres/sqt)

# Imprimindo o coeficiente de determinação (R2) calculado
print(paste("Coeficiente de determinação do modelo XGBoost:", R2))



# ****************** Avaliação dos Modelos Preditivos *******************
#
# ***********************************************************************

# Comparação dos modelos:

# 1) O coeficiente de determinação (R2) do modelo de regressão linear somente com as
# variáveis originais foi muito baixo (0.02), indicando que o modelo não se ajusta
# aos dados.
# O RMSE do modelo de regressão linear com as variáveis originais foi de 20.307.
# O RMSLE (raiz do erro logarítmico quadrático médio) de 0.96 indica que o desempenho
# do modelo de regressão linear com as variáveis originais foi muito ruim.

# 2) O modelo de regressão linear com seleção de novas variáveis melhorou o coeficiente
# de determinação (R2 de 0.7754).
# O RMSE do modelo com a seleção de novas variáveis foi menor (RMSE de 10.058). 
# O RMSLE do modelo com a seleção de novas variáveis resultou em 0.587, indicando
# que o desempenho deste modelo foi mais razoável comparado ao modelo anterior.

# 3) O modelo XGBoost apresentou uma melhor performance do que o modelo de regressão
# linear com seleção de novas variáveis. 
# O coeficiente de determinação (R2) calculado para o modelo XGBoost de 0.829 foi
# maior do que o R2 do modelo de regressão linear.
# As métricas RMSE e RMSLE obtidas com o XGBoost de 8.485 e 0.526 foram menores do
# que as do modelo de regressão linear.

# ***** Conclusão: Melhor Modelo -> XGBoost ***** 



# ******* Carregando os Dados de Teste para a Previsão do Modelo ********
#                                                                       *
# Aplicando o merge das tabelas e adicionando as variáveis criadas (mais*
# significantes) na etapa de treinamento do modelo.                     *
# ***********************************************************************

teste <- fread("test.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

dim(teste)
str(teste)
View(teste)

# Medidas de tendência central das variáveis dos dados de teste
summary(teste)


# *** Merge das tabelas de produtos, local_estado e clientes com os dados de teste ***

# juntando a tabela de clientes ao arquivo de teste

dados_teste <- as.data.frame(teste %>%
                               left_join(clientes, by = "Cliente_ID"))

# juntando a tabela de produtos aos dados de teste
dados_teste <- as.data.frame(dados_teste %>%
                         inner_join(produtos, by = "Producto_ID"))

# juntando a tabela de local aos dados de teste
dados_teste <- as.data.frame(dados_teste %>%
                         inner_join(local_estado, by ="Agencia_ID"))

dim(dados_teste)
View(dados_teste)
str(dados_teste)


# Removendo arquivos para liberar memória
rm(amostra)
rm(teste)
rm(clientes)



# *************** Pré-processamento dos Dados de Teste ****************
# ***** Adicionando as novas variáveis (médias das unidades vendidas  *
# para as combinações de duplas de variáveis).                        * 
# *********************************************************************

# Adicionando a tabela com o valor médio das unidades vendidas por produto e local
# das semanas de 3 a 9 ao arquivo de teste

dados_teste <- as.data.frame(dados_teste %>%
                         left_join(tab_Venta_uni_Dev, 
                                    by = c("NombreProducto", "Town"))) 

# Verificando o percentual de valores ausentes (missing) gerados com a adição da tabela 
(sum(is.na(dados_teste$Venta_uni_media))/nrow(dados_teste)) * 100

# Nota:
# Considerando que o percentual de valores NA é pequeno (aproximadamente 6% dos
# dados de teste), substituiremos os valores NA pela média da variável Venta_uni_hoy
# dos dados (semanas de 3 a 9).
media_uni_vend = round(mean(dados$Venta_uni_hoy), digits = 0)
media_uni_vend

dados_teste$Venta_uni_media <- lapply(dados_teste$Venta_uni_media, 
                                    function (x) x <- ifelse(is.na(x),
                                                             media_uni_vend,
                                                             x))
dados_teste$Venta_uni_media <- as.integer(dados_teste$Venta_uni_media)



# Adicionando a media das unidades vendidas por Agencia e Rota das semanas de 3 a 9 
# aos dados de teste
dados_teste <- as.data.frame(dados_teste %>%
                               left_join(tab_Agencia_Rota, 
                                         by = c("Agencia_ID", "Ruta_SAK"))) 

# Verificando o percentual de valores ausentes (missing) gerados
(sum(is.na(dados_teste$Venta_uni_AgRota_media))/nrow(dados_teste)) * 100

# Como o percentual de valores NA é pequeno (aproximadamente 0.7% dos dados de teste),
# substituiremos os valores NA pela média da variável Venta_uni_hoy dos dados (semanas de 3 a 9).
dados_teste$Venta_uni_AgRota_media <- lapply(dados_teste$Venta_uni_AgRota_media, 
                                         function (x) x <- ifelse(is.na(x),
                                                                  media_uni_vend,
                                                                  x))
dados_teste$Venta_uni_AgRota_media <- as.integer(dados_teste$Venta_uni_AgRota_media)



# Adicionando a tabela com da media das unidades vendidas por Rota e Produto das semanas
# de 3 a 9 ao arquivo de teste 
dados_teste <- as.data.frame(dados_teste %>%
                               left_join(tab_Rota_Produto, 
                                         by = c("Ruta_SAK", "Producto_ID"))) 

# Verificando o percentual de valores ausentes (missing) gerados
(sum(is.na(dados_teste$Venta_uni_RotaProd_media))/nrow(dados_teste)) * 100

# Como o percentual de valores NA é pequeno (aproximadamente 10% dos dados de teste),
# substituiremos os valores NA pela média da variável Venta_uni_hoy dos dados (semanas de 3 a 9).
dados_teste$Venta_uni_RotaProd_media <- lapply(dados_teste$Venta_uni_RotaProd_media, 
                                         function (x) x <- ifelse(is.na(x),
                                                                  media_uni_vend,
                                                                  x))
dados_teste$Venta_uni_RotaProd_media <- as.integer(dados_teste$Venta_uni_RotaProd_media)



# Adicionando a tabela com a media das unidades vendidas por cliente e canal das
# semanas de 3 a 9 aos dados de teste
dados_teste <- as.data.frame(dados_teste %>%
                         left_join(tab_Cliente_Canal, 
                                   by = c("NombreCliente", "Canal_ID"))) 

# Verificando o percentual de valores ausentes (missing) gerados
(sum(is.na(dados_teste$Venta_uni_CliCa_media))/nrow(dados_teste)) * 100

# Como o percentual de valores NA ainda é pequeno (aproximadamente 14% dos dados de teste),
# substituiremos os valores NA pela média da variável Venta_uni_hoy dos dados (semanas de 3 a 9).
dados_teste$Venta_uni_CliCa_media <- lapply(dados_teste$Venta_uni_CliCa_media, 
                                         function (x) x <- ifelse(is.na(x),
                                                                  media_uni_vend,
                                                                  x))
dados_teste$Venta_uni_CliCa_media <- as.integer(dados_teste$Venta_uni_CliCa_media)


View(dados_teste)

# Removendo o arquivo dados para liberar memória
rm(dados)



# *********** Previsão do Melhor Modelo com os Dados de Teste ************
#
# ************************************************************************


# Transformando em uma matriz densa de variáveis preditoras
dteste <-xgb.DMatrix(data = as.matrix(dados_teste[ , new_vars]))
class(dteste)

# Previsão com os dados de teste
previsao <- round(predict(xgb_model, newdata = dteste))

# Visualizando as previsões
View(previsao)

# Transformando em dataframe
df_submission <- data.frame(dados_teste$id, previsao)
names(df_submission) = c("id", "Demanda_uni_equil")


# Gravando o arquivo de submissão contendo as previsões
write.csv(df_submission, "submission.csv")

