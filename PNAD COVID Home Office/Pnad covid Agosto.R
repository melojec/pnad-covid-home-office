# PNAD COVID

####Importando Dados####

# carregar os arquivos de maio e junho
# carregar as bibliotecas
library(tidyverse)
library(srvyr)
library(readxl)
library(viridis)
library(dplyr)
library(srvyr) 
library(readr)
library(ggplot2)
library(Cairo)
# importar o dicionário

estados <- read_excel("Dicionario_PNAD_COVID.xls",
                      sheet = "dicionário pnad covid",
                      skip = 4, n_max = 27
) %>%
  select(UF = ...5, estado = ...6)

#PNAD Agosto

pnad_agosto <- read.csv("PNAD_COVID_082020.csv")

####Trabalhando com os Dados####
####Agosto####
#Criando os pesos e filtrando São luís

pnad_pesos08 <- pnad_agosto %>% as_survey_design(
  ids = UPA, 
  strata = Estrato, 
  weights = V1032, 
  nest = TRUE) %>% filter(CAPITAL == "21")


# Criando colunas com Variáveis 
pnad_pesos08 <- pnad_pesos08 %>% 
  mutate(one = 1,
         sexo = ifelse(A003 == 1, "Homem", "Mulher"), 
         Idade = case_when(
           A002 %in% 15:24 ~ "15-24",
           A002 %in% 25:34 ~ "25-34", 
           A002 %in% 35:49 ~ "35-49", 
           A002 %in% 50:64 ~ "50-64", 
           A002 > 64 ~ "65+"),
         cor = case_when(
           A004 == 1 ~ "Branca", 
           A004 == 2 ~ "Preta", 
           A004 == 4 ~ "Parda"),
         Escolaridade = factor(case_when( 
           A005 %in% 1:2 ~ "Sem Instrução ou Fundamental Incompleto", 
           A005 %in% 3:4 ~ "Fundamental completo ou Médio Incompleto", 
           A005 %in% 5:6 ~ "Médio completo ou Superior Incompleto", 
           A005 == 7 ~ "Superior completo", 
           A005 == 8 ~ "Pós-graduação"), 
           levels = c( "Sem Instrução ou Fundamental Incompleto",
                       "Fundamental completo ou Médio Incompleto", 
                       "Médio completo ou Superior Incompleto",
                       "Superior completo",
                       "Pós-graduação")), 
         Tipo_emprego = factor(case_when(
           C007 == 1 ~ "Trabalhador doméstico (empregado doméstico, cuidados, babá)",
           C007 == 2 ~ "Militar",
           C007 == 3 ~ "Policial ou Bombeiro",
           C007 == 4 ~ "Setor privado",
           C007 == 5 ~ "Setor público",
           C007 == 6 ~ "Empregador",
           C007 == 7 ~ "Autônomo (Conta própria)"),
           levels = c( "Trabalhador doméstico (empregado doméstico, cuidados, babá)",
                       "Militar", 
                       "Policial ou Bombeiro",
                       "Setor privado",
                       "Setor público",
                       "Empregador",
                       "Autônomo (Conta própria)")), 
         Faixa_salario = factor(case_when(
           C01012 <= 1044 ~ "Menos de um salário mínimo",
           C01012 %in% c(1045:2090) ~ "Entre 1 e 2",
           C01012 %in% c(2091:3135) ~ "Entre 2 e 3",
           C01012 %in% c(3136:4180) ~ "Entre 3 e 4",
           C01012 %in% c(4181:5225) ~ "Entre 4 e 5",
           C01012 >= 5226 ~ "Mais de 5"),
           levels = c("Menos de um salário mínimo",
                      "Entre 1 e 2",
                      "Entre 2 e 3",
                      "Entre 3 e 4",
                      "Entre 4 e 5",
                      "Mais de 5")),
         domicilio_situacao = factor(case_when(
           F001 == 1 ~ "Próprio - já pago",
           F001 == 2 ~ "Próprio - ainda pagando" ,                                  
           F001 == 3 ~ "Alugado",
           F001 %in% 4:6 ~ "Cedido (Por empregador, Familiar ou outro)"),
           levels = c("Próprio - já pago",
                      "Próprio - ainda pagando",
                      "Alugado", 
                      "Cedido (Por empregador, Familiar ou outro)")),
         home_office = ifelse(C013 == 1, "Home Office", "Presencial"),
         auxilio_emergencial = ifelse(D0051 == 1, "Auxílio", "Sem auxílio")
  )


####Gerando Gráficos####
####Agosto####
###Home Office por Cor e Sexo

# Criando dataset para conferir pessoas em Home Office
home_sexo_cor08 <- pnad_pesos08 %>%
  group_by(sexo, cor) %>%
  summarise(
    home_office = survey_total(C013 == 1, na.rm = TRUE),
    mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
  select_if(colSums(!is.na(.)) > 0) %>%
  drop_na()

# Home office - Por sexo e cor #
#Criando dataset para conferir pessoas em Home Office
# gráfico
grafhomsexcor08 <- ggplot(home_sexo_cor08, 
                          aes(
                            fill = cor, 
                            y = trab_home_office, 
                            x = sexo)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  geom_text(aes(
    label=sprintf("%1.2f%%",trab_home_office)),
    size = 3,
    position =position_dodge(width=0.9),
    vjust=-0.5,
    color = 'black',
    fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", 
                                   color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black",
                                 size = 1, 
                                 linetype = "solid"),
        axis.text=element_text(size=6, 
                               face="bold"),
        axis.text.x = element_text(face="bold", 
                                   color="#000000", 
                                   size=10),
        plot.title = element_text(colour = "black",
                                  size = 17, 
                                  hjust=0.5),
        legend.position = "bottom", 
        legend.background = element_rect(fill="ghostwhite",
                                         size=0.7, 
                                         linetype="blank")) +
  labs(x = "Sexo", 
       fill = "Cor/Raça: ", 
       caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Agosto 2020.",
       title = "Pessoas em home office, por cor/raça e sexo - São Luís/MA") +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7")) +
  scale_y_discrete(limits = factor(0:100), 
                   breaks = c(0,10,20,30,40,50,60,70,80,90,100), 
                   name = "Percentual (%)")
ggsave(plot = grafhomsexcor08, 
       "Por Cor e Sexo.png",
       path = "C:/Users/Melo/Desktop/RJoao/Repositório 1/PNAD COVID/Gráfico/Home Office",
       width = 10, 
       height = 5, 
       dpi = 120, 
       units = "in",
       type = "cairo")

#Home office - Por Cor e Escolaridade #
home_edu_cor08 <- pnad_pesos08 %>%
  group_by(Escolaridade, cor) %>%
  summarise(
    home_office = survey_total(C013 == 1, na.rm = TRUE),
    mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)
  ) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
  select_if(colSums(!is.na(.)) > 0) %>%
  drop_na()

# gráfico
grafeducor08 <- ggplot(home_edu_cor08[which(home_edu_cor08$home_office>0),], 
                       aes(fill = Escolaridade, 
                           y = trab_home_office, 
                           x = cor)) +
  geom_bar(position = "dodge", 
           stat = "identity") +
  geom_text(aes(
    label=sprintf("%1.2f%%",trab_home_office)),
    size = 3,
    position =position_dodge(width=0.9),
    vjust=-0.5, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold",
                                   color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black",
                                 size = 1,
                                 linetype = "solid"),
        axis.text=element_text(size=6, 
                               face="bold"),
        axis.text.x = element_text(face="bold", 
                                   color="#000000",
                                   size=10),
        plot.title = element_text(colour = "black",
                                  size = 17, 
                                  hjust=0.5),
        legend.position = "bottom",
        legend.background = element_rect(fill="ghostwhite",
                                         size=0.7, 
                                         linetype="blank")) +
  labs(x = "Cor/Raça", 
       fill = "Escolaridade: ", 
       caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Agosto 2020.",
       title = "Pessoas em home office, por cor/raça e escolaridade - São Luís/MA ") +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7","#fdcb6e")) +
  scale_y_discrete(limits=factor(0:100), 
                   breaks = c(0,10,20,30,40,50,60,70,80,90,100),
                   name = "Percentual (%)")

ggsave(plot = grafeducor08, 
       "Por Educação e Sexo.png",
       path = "C:/Users/Melo/Desktop/RJoao/Repositório 1/PNAD COVID/Gráfico/Home Office",
       width = 10, 
       height = 5, 
       dpi = 120, 
       units = "in",
       type = "cairo")

# Home office - Por Sexo e Idade #
home_sexo_idade08 <- pnad_pesos08 %>%
  group_by(sexo, Idade) %>%
  summarise(
    home_office = survey_total(C013 == 1, na.rm = TRUE),
    mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)
  ) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
  select_if(colSums(!is.na(.)) > 0) %>%
  drop_na()

# gráfico
grafsexidade08 <- ggplot(home_sexo_idade08[which(home_sexo_idade08$home_office>0),], 
                         aes(fill = Idade, 
                             y = trab_home_office, 
                             x = sexo)) +
  geom_bar(position = "dodge", 
           stat = "identity") +
  geom_text(aes(
    label=sprintf("%1.2f%%",trab_home_office)),
    size = 3, 
    position =position_dodge(width=0.9),
    vjust=-0.5, 
    color = 'black',
    fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", 
                                   color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1,
                                 linetype = "solid"),
        axis.text=element_text(size=6,
                               face="bold"),
        axis.text.x = element_text(face="bold", 
                                   color="#000000",
                                   size=10),
        plot.title = element_text(colour = "black", 
                                  size = 17,
                                  hjust=0.5),
        legend.position = "bottom", 
        legend.background = element_rect(fill="ghostwhite", 
                                         size=0.7, 
                                         linetype="blank")) +
  labs(x = "Sexo", 
       fill = "Faixa Etária: ", 
       caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Agosto 2020.",
       title = "Pessoas em home office, por sexo e faixa etária - São Luís /MA") +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7","#fdcb6e")) +
  scale_y_discrete(limits=factor(0:100),
                   breaks = c(0,10,20,30,40,50,60,70,80,90,100), 
                   name = "Percentual (%)")
ggsave(plot = grafsexidade08, 
       "Por Idade e Sexo.png",
       path = "C:/Users/Melo/Desktop/RJoao/Repositório 1/PNAD COVID/Gráfico/Home Office",
       width = 10, 
       height = 5, 
       dpi = 120, 
       units = "in",
       type = "cairo")

# Home office - Por trabalho #
home_emprego08 <- pnad_pesos08 %>%
  group_by(Tipo_emprego) %>%
  summarise(
    home_office = survey_total(C013 == 1, na.rm = TRUE),
    mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
  select_if(colSums(!is.na(.)) > 0) %>%
  drop_na()

# ordenando eixo X
legenda_trabalhos <- c("Setor privado",
                       "Setor público",
                       "Empregador",
                       "Autônomo\n (Conta própria)")

# Gráfico
graftipemp08 <- ggplot(home_emprego08[which(home_emprego08$home_office>0),], 
                       aes(fill = Tipo_emprego, 
                           y = trab_home_office, 
                           x = Tipo_emprego)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",trab_home_office)),
            size = 3,
            position =position_dodge(width=0.9),
            vjust=-0.5,
            color = 'black',
            fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", 
                                   color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1,
                                 linetype = "solid"),
        axis.text=element_text(size=6,
                               face="bold"),
        axis.text.x = element_text(face="bold", 
                                   color="#000000", 
                                   size=8),
        plot.title = element_text(colour = "black",
                                  size = 17, 
                                  hjust=0.5),
        legend.position = "none") +
  labs(x = "Tipo de Ocupação",
       caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Agosto 2020.",
       title = "Pessoas em home office, por tipo de ocupação - São Luís/ MA") +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7","#fdcb6e","#636e72", "#55efc4")) +
  scale_x_discrete(labels = legenda_trabalhos) +
  scale_y_discrete(limits=factor(0:100), 
                   breaks = c(0,10,20,30,40,50,60,70,80,90,100), 
                   name = "Percentual (%)")

ggsave(plot = graftipemp08, 
       "Por Tipo de Emprego.png",
       path = "C:/Users/Melo/Desktop/RJoao/Repositório 1/PNAD COVID/Gráfico/Home Office",
       width = 10, 
       height = 5, 
       dpi = 120, 
       units = "in",
       type = "cairo")
