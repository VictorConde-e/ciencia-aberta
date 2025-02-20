##########################
# IMPORTANDO BIBLIOTECAS #
##########################

library(nlme)
library(SYNCSA)
library(tidyverse)
library(gridExtra)
library(broom)
library(googlesheets4)


# Definir diretório
#setwd("~/R/analise resiliencia v1.1")

#####################################################
# ORGANIZANDO OS Os DATAs FRAMES DE CARACTERISTICAS #
#####################################################

# IMPORTANDO BASE DE DADOS DAS CARACTERISTICAS
caracteristicas_especies = read_sheet('https://docs.google.com/spreadsheets/d/1wS5IwOsNJV7Xj46A8jat6csWZSzBw1jRyu2Q_aJymsg/edit?usp=sharing')
spp = caracteristicas_especies$especies

# Dando uma classe aos tamanhos maximo do corpo
caracteristicas_especies$classe = cut(caracteristicas_especies$mbl, breaks = c(-Inf,20,40,Inf), labels = c("pequeno","medio","grande"), include.lowest = TRUE)

# Removendo a primeira e segunda colunas que não serão utilizadas
caracteristicas_especies = caracteristicas_especies[c(-1,-2)]

# Definindo todas as colunas com o tipo fator
caracteristicas_especies[1:5] = lapply(caracteristicas_especies[1:5], factor)

# Definindo o nome das linhas para os nomes das espécies
row.names(caracteristicas_especies) = spp

############################################
# ORGANIZANDO OS DATA FRAMES DE ABUNDANCIA #
############################################

# IMPORTANDO DADOS DE ABUNDANCIA COM CPUE CPUE
batata_cpue = read_sheet("https://docs.google.com/spreadsheets/d/1yJt3lB87ilaYK0AWPh5SSlYHYfP7mr0yodhJ278YHWg/edit?usp=sharing")

# REJEITO
batata_rejeito <- subset(batata_cpue, Area %in% "Rejeito")
# definindo o nome das linha para os anos
anos = batata_rejeito$Ano
# removendo primeira e seunda coluna
batata_rejeito = batata_rejeito[-c(1,2)]
# trocando os valores NA para 0
batata_rejeito = replace(x = batata_rejeito, list = is.na(batata_rejeito), values = 0)
rownames(batata_rejeito)= anos
# NATURAL
batata_natural <- subset(batata_cpue, Area %in% "Natural")
# removendo primeira e seunda coluna
batata_natural = batata_natural[-c(1,2)]
# trocando os valores NA para 0
batata_natural = replace(x = batata_natural, list = is.na(batata_natural), values = 0)
# definindo o nome das linha para os anos
rownames(batata_natural)= anos

# PLANTIO
batata_plantio <- subset(batata_cpue, Area %in% "Plantio")
# definindo o nome das linha para os anos
anos_plantio = batata_plantio$Ano
# removendo primeira e seunda coluna
batata_plantio = batata_plantio[-c(1,2)]
# trocando os valores NA para 0
batata_plantio = replace(x = batata_plantio, list = is.na(batata_plantio), values = 0)
rownames(batata_plantio) = anos_plantio

#####################################
# RODANDO O SYNCSA PARA OS 3 LOCAIS #
#####################################

# Rejeito
syncsa_rejeito = rao.diversity(batata_rejeito,caracteristicas_especies)
redundancia_rejeito = as.data.frame(syncsa_rejeito$FunRedundancy)
redundancia_rejeito$ano =  as.numeric(anos)
colnames(redundancia_rejeito)=c("redu","ano")
redundancia_rejeito$redu = as.numeric(redundancia_rejeito$redu)

# Natural
syncsa_natural=rao.diversity(batata_natural,caracteristicas_especies)
redundancia_natural= as.data.frame(syncsa_natural$FunRedundancy)
redundancia_natural$ano =  as.numeric(anos)
colnames(redundancia_natural)=c("redu","ano")
redundancia_natural$redu = as.numeric(redundancia_natural$redu)

# Plantio
syncsa_plantio=rao.diversity(batata_plantio,caracteristicas_especies)
redundancia_plantio = as.data.frame(syncsa_plantio$FunRedundancy)
redundancia_plantio$ano = as.numeric(anos_plantio)
colnames(redundancia_plantio)=c("redu","ano") 
redundancia_plantio$redu = as.numeric(redundancia_plantio$redu)


###########################################################################################
# Plotando a série temporal dos três locais usando GLM para traçar uma linha de tendência #
###########################################################################################

theme_set(theme_gray(base_family = "Times New Roman"))

ggplot() +
  
  # Dados do rejeito
  geom_point(data = redundancia_rejeito, aes(x = ano, y = redu, color = "Rejeito"), size = 3) +
  geom_line(data = redundancia_rejeito, aes(x = ano, y = redu, group = 1), color = "#6E6E6E", linetype = "longdash", size = 1.1, show.legend = FALSE) +
  geom_smooth(data = redundancia_rejeito, aes(x = ano, y = redu, color = "Rejeito"), method = "glm", se = FALSE, size = 1.8) +
  
  # Dados naturais
  geom_point(data = redundancia_natural, aes(x = ano, y = redu, color = "Natural"), size = 3) +
  geom_line(data = redundancia_natural, aes(x = ano, y = redu, group = 1), color = "#228B22", linetype = "longdash", size = 1.1, show.legend = FALSE) +
  geom_smooth(data = redundancia_natural, aes(x = ano, y = redu, color = "Natural"), method = "glm", se = FALSE, size = 1.8) +
  
  # Dados de plantio
  geom_point(data = redundancia_plantio, aes(x = ano, y = redu, color = "Regeneração Facilitada"), size = 3) +
  geom_line(data = redundancia_plantio, aes(x = ano, y = redu, group = 1), color = "#7EACB5", linetype = "longdash", size = 1.1, show.legend = FALSE) +
  geom_smooth(data = redundancia_plantio, aes(x = ano, y = redu, color = "Regeneração Facilitada"), method = "glm", se = FALSE, size = 1.8) +
  
  # Personalizações
  labs(x = "Ano", y = "Resiliência", title = "Variação da resiliência ao longo dos anos") +
  scale_color_manual(values = c("Rejeito" = "#6E6E6E", "Natural" = "#228B22", "Regeneração Facilitada" = "#7EACB5")) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 15),
    legend.title = element_blank(), 
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 28, hjust = 0.5),
    panel.grid.major = element_line(color = "lightgray", linetype = "dotted"), 
    panel.grid.minor = element_line(color = "lightgray", linetype = "dotted"),
    axis.line = element_line(color = "#212121"),  
    axis.ticks = element_line(color = "#212121")
  )




########################################################################
# Teste de Shapiro para analisar a distribuição normal (TODOS NORMAIS) #
########################################################################

shapiro.test(redundancia_natural$riqueza)
shapiro.test(redundancia_natural$redu)
shapiro.test(redundancia_rejeito$riqueza)
shapiro.test(redundancia_rejeito$redu)
shapiro.test(redundancia_plantio$riqueza)
shapiro.test(redundancia_plantio$redu)

#############
# TESTE GLM #
#############

# Natural
summary(glm(redu ~ ano, data = redundancia_natural, family = "gaussian"))

# Rejeito
summary(glm(redu ~ ano, data = redundancia_rejeito, family = "gaussian"))

# Plantio
summary(glm(redu ~ ano, data = redundancia_plantio, family = "gaussian"))


############################################
# GLS COM ESTRUTURA DE CORRELAÇÃO TEMPORAL #
############################################


redundancia_batata = data.frame(redundancia_natural$ano, redundancia_natural$redu,redundancia_rejeito$redu)
redundancia_batata = rename(redundancia_batata, ano = redundancia_natural.ano, redu.natural = redundancia_natural.redu, redu.rejeito = redundancia_rejeito.redu )

redundancia_batata = left_join(
  redundancia_batata,
  redundancia_plantio,
  by = join_by(ano == ano),
  copy = FALSE,
  suffix = c(".x", ".y"),
  keep = NULL
)

redundancia_batata = redundancia_batata[,-5]
redundancia_batata = rename(redundancia_batata, redu.plantio = redu)

#####
correlacao_natural <- cor.test(redundancia_natural$riqueza, redundancia_natural$redu, method = "spearman")
correlacao_plantio <- cor.test(redundancia_plantio$riqueza, redundancia_plantio$redu, method = "spearman")
correlacao_rejeito <- cor.test(redundancia_rejeito$riqueza, redundancia_rejeito$redu, method = "spearman")

print(correlacao_natural)
print(correlacao_plantio)
print(correlacao_rejeito)

plot1 = ggplot(redundancia_natural, aes(riqueza, redu)) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE, color = "#A3CB38", size = 2) +  # Ajuste o valor de 'size' conforme necessário
  labs(x = "", y = "Resiliência", title = "Área Natural") +
  theme_classic(base_size = 20, base_family = "serif") +  # Definir base_size e base_family
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),  # Tamanho do título
    axis.title.x = element_text(size = 25),             # Tamanho da label do eixo x
    axis.title.y = element_text(size = 25),             # Tamanho da label do eixo y (mesmo que esteja vazia)
    axis.text = element_text(size = 25)                 # Tamanho dos textos dos eixos
  )
plot2 = ggplot(redundancia_rejeito, aes(riqueza, redu)) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE, color = "#f5cd79", size = 2) +
  labs(x = "Riqueza", y = "", title = "Área Impactada") +
  theme_classic(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),  # Tamanho do título
    axis.title.x = element_text(size = 25),             # Tamanho da label do eixo x
    axis.title.y = element_text(size = 25),             # Tamanho da label do eixo y (mesmo que esteja vazia)
    axis.text = element_text(size = 25)                 # Tamanho dos textos dos eixos
  )

plot3 = ggplot(redundancia_plantio, aes(riqueza, redu)) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE, color = "#778beb", size = 2) +
  labs(x = "", y = "", title = "Área de Regeneração Facilitada") +
  theme_classic(base_size = 20, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),  # Tamanho do título
    axis.title.x = element_text(size = 25),             # Tamanho da label do eixo x
    axis.title.y = element_text(size = 25),             # Tamanho da label do eixo y (mesmo que esteja vazia)
    axis.text = element_text(size = 25)                 # Tamanho dos textos dos eixos
  )

grid.arrange(plot1 , plot2 , plot3, ncol=3)
#ANALISE TEMPORAL

shapiro_test = shapiro.test(redundancia_plantio$redu)
print(shapiro_test)

model_rejeito = lm(redu ~ ano, data = redundancia_rejeito) 
residuos_r = residuals(model_rejeito)
shapiro_test_result_r = shapiro.test(residuos_r)
print(shapiro_test_result_r)

model_plantio = lm(log(redu) ~ ano, data = redundancia_plantio)
residuos_p = residuals(model_plantio)
shapiro_test_result_p = shapiro.test(residuos_p)
print(shapiro_test_result_p)

model_natural = lm(redu ~ ano, data = redundancia_natural)
residuos_n = residuals(model_rejeito)
shapiro_test_result_n = shapiro.test(residuos_n)
print(shapiro_test_result_n)

summary(model_rejeito)
summary(model_plantio)
summary(model_natural)

#QUERO AVALIAR COMO CHEIAS ANORMAIS INFLUENCIAM NOS RESULTADOS
#COMPARAR OS NUMEROS DAS CHEIAS DE CADA ANO E COM O VALOR DE RESILÊNCIA




pluv = read_sheet("https://docs.google.com/spreadsheets/d/1QCKEVbHRezCMFy51HCK6_aO5fwR_gVH8dWW3WvCxeRw/edit?usp=sharing")



pluv_rejeito = filter(pluv, area == "Rejeito") 
redundancia_rejeito = left_join(redundancia_rejeito,pluv_rejeito[,c('profundidade', 'ano')], by = "ano", suffix = c("", ""))


pluv_natural = filter(pluv, area == "Natural") 
redundancia_natural = left_join(redundancia_natural,pluv_natural[,c('profundidade', 'ano')], by = "ano", suffix = c("", ""))

pluv_plantio = filter(pluv, area == "Plantio") 

redundancia_plantio %>%
  left_join(y = pluv_plantio[,c('profundidade', 'ano')], by = "ano", suffix = c("", "")) %>%
  filter(!is.na(profundidade) & !is.na(redu))



ggplot(redundancia_rejeito, aes(profundidade, redu)) +
  geom_point() +  
  geom_smooth() +  # Adicionado fechamento correto e ajuste no 'size'
  labs(x = "", y = "Resiliência", title = "Área Natural") +
  theme_classic(base_size = 20, base_family = "serif") +  # Definir base_size e base_family
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),  # Tamanho do título
    axis.title.x = element_text(size = 25),             # Tamanho da label do eixo x
    axis.title.y = element_text(size = 25),             # Tamanho da label do eixo y (mesmo que esteja vazia)
    axis.text = element_text(size = 25)                 # Tamanho dos textos dos eixos
  )


ggplot(redundancia_plantio, aes(profundidade, redu)) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE, color = "#A3CB38", size = 2) +  # Ajuste o valor de 'size' conforme necessário
  labs(x = "", y = "Resiliência", title = "Área Natural") +
  theme_classic(base_size = 20, base_family = "serif") +  # Definir base_size e base_family
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),  # Tamanho do título
    axis.title.x = element_text(size = 25),             # Tamanho da label do eixo x
    axis.title.y = element_text(size = 25),             # Tamanho da label do eixo y (mesmo que esteja vazia)
    axis.text = element_text(size = 25)                 # Tamanho dos textos dos eixos
  )

ggplot(redundancia_natural, aes(profundidade, redu)) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE, color = "#A3CB38", size = 2) +  # Ajuste o valor de 'size' conforme necessário
  labs(x = "", y = "Resiliência", title = "Área Natural") +
  theme_classic(base_size = 20, base_family = "serif") +  # Definir base_size e base_family
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),  # Tamanho do título
    axis.title.x = element_text(size = 25),             # Tamanho da label do eixo x
    axis.title.y = element_text(size = 25),             # Tamanho da label do eixo y (mesmo que esteja vazia)
    axis.text = element_text(size = 25)                 # Tamanho dos textos dos eixos
  )

#Modelo0 > dados com uma coluna de redundância e outra coluna dizendo se o valor é em rejeito, natural, ou plantio
#objetivo = ver qual área imapcta a redundância, e onde a redundância é maior
#talvez usar os anos como variável randômica aqui, pq não queremos diferenciar entre os anos
model0 = glm(redu ~ natural + plantio + rejeito, data = dados)

#Modelo1 > separar os dados por área, como vc fez, e ver como a redundância varia ao longo do tempo2





#BOXPLOT REDUNDANCIA 


df_boxplot <- rbind(data.frame(Valor = redundancia_natural$redu, Categoria = factor("Natural")),
                    data.frame(Valor = redundancia_rejeito$redu, Categoria = factor("Rejeito")),
                    data.frame(Valor = redundancia_plantio$redu, Categoria = factor("Plantio")))

ggplot(df_boxplot, aes(x = Categoria, y = Valor, fill = Categoria)) +
  geom_boxplot(show.legend = FALSE) +
  geom_errorbar(stat="boxplot",width=0.2) +
  scale_fill_manual(values = c('#a1ac88','#9d4e0f','#464d70')) +
  labs(title = "Comparação entre áreas",
       x = "Área",
       y = "Redundância") +
       theme(legend.position = "top",
          legend.text = element_text(size = 10),
          axis.title = element_text(size = 12, face = ),
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 15, hjust = 0.5),)

summary(aov(Valor ~ Categoria, data = df_boxplot)) #testar por ano (falar com caio)
TukeyHSD(aov(Valor ~ Categoria, data = df_boxplot))

#BOXPLOT DIVERSIDADE TAXONOMICA

df_boxplot_taxonomico <- rbind(data.frame(Valor = dados_natural$Simpson, Categoria = factor("Natural")),
                               data.frame(Valor = dados_rejeito$Simpson, Categoria = factor("Rejeito")),
                               data.frame(Valor = dados_plantio$Simpson, Categoria = factor("Plantio")))

ggplot(df_boxplot_taxonomico, aes(x = Categoria, y = Valor, fill = Categoria)) +
  geom_boxplot(show.legend = FALSE) +
  geom_errorbar(stat="boxplot",width=0.2) +
  scale_fill_manual(values = c('#a1ac88','#9d4e0f','#464d70')) +
  labs(title = "Comparação entre áreas",
       x = "Área",
       y = "Diversidade Taxonomica") +
  theme(legend.position = "top",
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = ),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 15, hjust = 0.5),)

TukeyHSD(aov(Valor ~ Categoria, data = df_boxplot_taxonomico))


#BOXPLOT DIVERSIDADE FUNCIONAL

df_boxplot_funcional <- rbind(data.frame(Valor = dados_natural$FunRao, Categoria = factor("Natural")),
                               data.frame(Valor = dados_rejeito$FunRao, Categoria = factor("Rejeito")),
                               data.frame(Valor = dados_plantio$FunRao, Categoria = factor("Plantio")))

ggplot(df_boxplot_funcional, aes(x = Categoria, y = Valor, fill = Categoria)) +
  geom_boxplot(show.legend = FALSE) +
  geom_errorbar(stat="boxplot",width=0.2) +
  scale_fill_manual(values = c('#a1ac88','#9d4e0f','#464d70')) +
  labs(title = "Comparação entre áreas",
       x = "Área",
       y = "Diversidade funcional") +
  theme(legend.position = "top",
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = ),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 15, hjust = 0.5),)

TukeyHSD(aov(Valor ~ Categoria, data = df_boxplot_funcional))
#entender rao, simpsom(difrença com shannon) e adicionar reprodução. Analisar especies removidas, atualizar dados de abundancia. Dados historicos de cheia para avaliar

#peixes 