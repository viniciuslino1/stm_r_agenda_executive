#### Carregando pacotes necessários para rodar o STM e demais gráficos ------------------------

if(require(tidyverse) == F) {install.packages("tidyverse"); require(tidyverse)}
if(require(dplyr) == F) {install.packages("dplyr"); require(dplyr)}
if(require(readxl) == F) {install.packages("readxl"); require(readxl)}
if(require(abjutils) == F) {install.packages("abjutils"); require(abjutils)}
if(require(tidytext) == F) {install.packages("tidytext"); require(tidytext)}
if(require(writexl) == F) {install.packages("writexl"); require(writexl)}
if(require(stringi) == F) {install.packages("stringi"); require(stringi)}
if(require(quanteda) == F) install.packages('quanteda'); require(quanteda)
if(require(stm) == F) install.packages('stm'); require(stm)
if(require(RecordLinkage) == F) install.packages('RecordLinkage'); require(RecordLinkage)
if(require(gtools) == F) install.packages('gtools'); require(gtools)
if(require(here) == F) {install.packages("here"); require(here)}; require(here)
if(require(conflicted) == F) {install.packages("conflicted"); require(conflicted)}; require(conflicted)
if(require(ggplot2) == F) {install.packages("ggplot2"); require(ggplot2)}

conflict_prefer("here", "here")
conflict_prefer("filter", "dplyr")

### Carregar o banco de dados -------------------------------------------------

decretos <- read_xlsx(here("/decretos_presidenciais.xlsx"))
proposicoes_semcred <- read_xlsx(here("/proposicoes_legislativas.xlsx"))

### STM 

## Pré tratamento do texto ----------------------------------------------------

# Proposições ----------------------------------------------------

proposicoes_semcred_stm <- proposicoes_semcred%>%
  mutate(text = stri_trans_general(ementa, "Latin-ASCII")) %>%
  mutate(text = str_remove_all(ementa, "[[:digit:]]")) %>%
  corpus(docid_field = "doc_id", text_field = "ementa") %>% 
  tokens(remove_punct = TRUE) %>%
  tokens_tolower() %>%
  tokens(remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE) %>%
  tokens_remove(stopwords(source = "stopwords-iso", language = "pt"), 
                min_nchar = 2) %>%
  tokens_wordstem(language = "pt") %>%
  dfm() %>%
  dfm_trim(min_docfreq = 0.005, docfreq_type = "prop") %>%  # DM: removi stems que nao aparecem em ao menos 0.5% dos documentos 
  dfm_select(pattern = c("sr", "srs", "sra", "sras", "dr", "v.ex", "v.exa", "ser", 
                         "orador", "ate", "ja", "srs", "altera", "firm", "dispositiv", 
                         "esta", "so", "president", "s.ex", "deputado",
                         "acrescent", "art", "-a", "lei", "dispõ", "institui", "exercíci", 
                         "nº", "julh", "dispor", "alter", "public", "providenc",
                         "estabelec", "acrescent", "codig", "nacional",
                         "setembr", "incis", "decreto-l", "decretos-l", "cri", "artig",
                         "determin", "marc", "janeir", "º-a", "direit", "garant",
                         "decret", "ement", "recurs", "outubr", "destin", 
                         "federal", "dezembr", "fevereir", "mai", "decl",
                         "constituica", "junh", "agost", "alteraca", 
                         "_alteraca", "declaraca", "redaç", "revog",
                         "novembr", "març", "fevereir", "dezembr", "agost", "abril", 
                         "maio", "provident", "promulg", "anex", "º", "leis", "ns", 
                         "institu", "praz", "ambit", "autoriz", "fund", "program", "exting",
                         "republ", "prorrog", "brasil", "abre", "outorg", "revogaça", "revog",
                         "especif", "fins", "prorrogaça", "vigenc", "caput", "decorrent", "explicaça",
                         "brasileir", "med", "provisor", "nucl", "entes", "autor", "ministeri", "institut",
                         "institucional", "instituiço", "disposit", "analis", "facilitaça", "realizaça",
                         "diretriz", "proced"),  
             selection = "remove") # retirei arbitrariamente stems muito frequentes que nao possuem significado substantivo para identificacao de temas


proposicoes_semcred_stm <-  proposicoes_semcred_stm %>% convert("stm")

proposicoes_semcred_stm <- prepDocuments(proposicoes_semcred_stm$documents, 
                                         proposicoes_semcred_stm$vocab, 
                                         proposicoes_semcred_stm$meta)

# Decretos ----------------------------------------------------
decretos_stm <- decretos%>%
  mutate(text = stri_trans_general(ementa, "Latin-ASCII")) %>%
  mutate(text = str_remove_all(ementa, "[[:digit:]]")) %>%
  corpus(docid_field = "doc_id", text_field = "ementa") %>% 
  tokens(remove_punct = TRUE) %>%
  tokens_tolower() %>%
  tokens(remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE) %>%
  tokens_remove(stopwords(source = "stopwords-iso", language = "pt"), 
                min_nchar = 2) %>%
  tokens_wordstem(language = "pt") %>%
  dfm() %>%
  dfm_trim(min_docfreq = 0.005, docfreq_type = "prop") %>%  # DM: removi stems que nao aparecem em ao menos 0.5% dos documentos 
  dfm_select(pattern = c("sr", "srs", "sra", "sras", "dr", "v.ex", "v.exa", "ser", 
                         "orador", "ate", "ja", "srs", "altera", "firm", "dispositiv", 
                         "esta", "so", "president", "s.ex", "deputado",
                         "acrescent", "art", "-a", "lei", "dispõ", "institui", "exercíci", 
                         "nº", "julh", "dispor", "alter", "public", "providenc",
                         "estabelec", "acrescent", "codig", "nacional",
                         "setembr", "incis", "decreto-l", "cri", "artig",
                         "determin", "marc", "janeir", "º-a", "direit", "garant",
                         "decret", "ement", "recurs", "outubr", "destin", 
                         "federal", "dezembr", "fevereir", "mai", "decl",
                         "constituica", "junh", "agost", "alteraca", 
                         "_alteraca", "declaraca", "redaç", "revog",
                         "novembr", "març", "fevereir", "dezembr", "agost", "abril", 
                         "maio", "provident", "promulg", "anex", "º", "lei", "leis", "ns",
                         "institu", "praz", "ambit", "autoriz", "fund", "program", "exting",
                         "republ", "prorrog", "nomeaça", "aprov", "regimental", "remanej",
                         "regulament" , "outorg", "revogaça", "revog", "brasileir", "brasil",
                         "autor", "decorrent", "caput"),  
             selection = "remove") 

decretos_stm <-  decretos_stm %>% convert("stm")

decretos_stm <- prepDocuments(decretos_stm$documents, 
                              decretos_stm$vocab, 
                              decretos_stm$meta)

## Rodar STM e estimar tópicos ----------------------------------------------------

# Proposições  (já retirados os pedidos de abertura de crédito) ----------------------------------------------------

#Busca melhor numero de tópicos dentro do intervalo discreto dado a ele (demora!)
searchk_proposicoes_semcred <- searchK(proposicoes_semcred_stm$documents, proposicoes_semcred_stm$vocab, 
                                       K = c(10, 20, 30, 40,50), 
                                       prevalence =~ tipo + s(data_apres) + pandemia 
                                       + ministerio_economia + casa_civil, 
                                       data = proposicoes_semcred_stm$meta)

#Gera gráfico de coerência semântica, proabilidade de retenção, resíduos e limite inferior
plot.searchK(searchk_proposicoes_semcred)


# Criação do STM para esse número de tópicos escolhido com as variáveis escolhidas
aux_semcred <- stm(documents = proposicoes_semcred_stm$documents, 
                   vocab = proposicoes_semcred_stm$vocab,
                   K = 30, #aqui vc coloca a quantidade de tópicos
                   prevalence =~  tipo + s(data_apres) + pandemia 
                   + ministerio_economia + casa_civil , #aqui são as variáveis
                   max.em.its = 75, 
                   data = proposicoes_semcred_stm$meta,
                   init.type = "Spectral", 
                   seed = 123)

#Gera lista de principais palavras de cada tópico
labelTopics(aux_semcred)

#Gera gráfico de tópicos mais presentes no modelo
plot.STM(aux_semcred)

# Rodar modelo do STM para 30 tópicos 
aux_semcred_30 <- tidy(aux_semcred)

# Gráfico de palavras com os termos mais frequentes do modelo
aux_semcred_30%>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta))


# Cria rotulo dos tópicos para na ordem que eles foram apresentados pelo modelo

nomes_topicos_proposicoes <- c("Lei e Crime1", 
                               "Comércio Interno1", 
                               "Burocracia", 
                               "Pandemia - Infraestrutura", 
                               "Pandemia - Rel Intergov", 
                               "Suporte Setores Econômicos1", 
                               "Administração Pública1", 
                               "Regulação e Serviços", 
                               "Defesa - Infra policia e exercito", 
                               "Suporte Setores Econômicos2", 
                               "Defesa", 
                               "Pandemia - Enfrentamento", 
                               "Macroeconomia1", 
                               "Direitos Civis", 
                               "Administração Pública2", 
                               "Saúde", 
                               "Macroeconomia2", 
                               "Pandemia", 
                               "Trabalho e Emprego1", 
                               "Industria e Comunicações", 
                               "Transporte", 
                               "Lei e Crime2", 
                               "Trabalho e Emprego2", 
                               "Tributação2", 
                               "Tributação3", 
                               "Trabalho e Emprego3", 
                               "Administração Pública3", 
                               "Tributação1", 
                               "Comércio Interno2", 
                               "Cultura")

# Tranformar rótulos em lista 
mapeamento <- setNames(nomes_topicos_proposicoes, 1:30)

# Substituir no data frame os tópicos (1,2,3...) pelos rótulos (Lei e crime1, ....)
aux_semcred_30$topic <- mapeamento[aux_semcred_30$topic]

# Gráfico de palavras com os termos mais frequentes do modelo (agora com os rótulos)
aux_semcred_30%>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta))

# Decretos ----------------------------------------------------

#Busca melhor numero de tópicos dentro do intervalo discreto dado a ele (demora!)
searchk_decretos <- searchK(decretos_stm$documents, decretos_stm$vocab, 
                                       K = c(10, 20, 30, 40,50), 
                                       prevalence =~ s(data) + pandemia + ministerio_economia 
                                      + casa_civil + primeiro_ano + ultimo_ano + ano, 
                                       data = decretos_stm$meta)

#Gera gráfico de coerência semântica, proabilidade de retenção, resíduos e limite inferior
plot.searchK(searchk_decreto)

# Rodar modelo do STM para 30 tópicos 
aux_dec <- stm(documents = decretos_stm$documents, 
               vocab = decretos_stm$vocab,
               K = 30, #aqui vc coloca a quantidade de tópicos
               prevalence =~ s(data) + pandemia + ministerio_economia 
               + casa_civil + primeiro_ano + ultimo_ano + ano, #aqui são as variáveis
               max.em.its = 75, 
               data = decretos_stm$meta,
               init.type = "Spectral", 
               seed = 123)

## grafico de palavras 
aux30_dec <- tidy(aux_dec)

aux30_dec%>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta))

# Rótulo dos tópicos
nomes_topicos_decretos <- c("Transporte1", 
                            "Administração Pública1", 
                            "Comércio Exterior1", 
                            "Educação", 
                            "Administração Pública2", 
                            "Lei e Crime", 
                            "Defesa e Exército - Regulação", 
                            "Administração Pública3", 
                            "Tecnologia e Comunicações1", 
                            "Transporte2", 
                            "Burocracia1", 
                            "Pandemia", 
                            "Burocracia do Exército", 
                            "Orçamento", 
                            "Tributação", 
                            "Energia", 
                            "Cultura", 
                            "Desestatização e Setor Privado", 
                            "Administração Pública4", 
                            "Contratações Públicas", 
                            "Tecnologia e Comunicações2", 
                            "Relações Intergovernamentais", 
                            "Órgãos da Presidência", 
                            "Tributação e Benefícios Sociais", 
                            "Comércio Exterior2", 
                            "Burocracia2", 
                            "Defesa e Exército", 
                            "Fontes de Energia", 
                            "Questões Jurídicas", 
                            "Política Externa")

## Tranformar rótulos em lista 
mapeamento_decretos <- setNames(nomes_topicos_decretos, 1:30)

## Substituir no data frame os tópicos pelos rótulos
aux30_dec$topic <- mapeamento_decretos[aux30_dec$topic]

## Gerar gráfico de termos por tópico com os rótulos
aux30_dec%>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta)) + theme(plot.title = element_text(hjust = 0.5))


## Testar efeito das covariáveis no modelo ----------------------------------------------------

# Proposições  ----------------------------------------------------

#efeito de uma covariável dummy (0 ou 1) nas proposicoes
efeito_VARX_proposicoes<-estimateEffect(formula = 1:30 ~ inserir_variavel, 
                                                stmobj = aux_semcred, 
                                                metadata = proposicoes_semcred_stm$meta, 
                                                uncertainty = "Global",
                                                prior = 1e-5)

#plota gráfico demonstrando o efeito sobre os tópicos
plot(efeito_VARX_proposicoes, covariate = "inserir_variavel", topics = c(1,2,3,4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
                                                                             18, 19, 20, 21, 22, 23, 24, 25, 26, 27,28,29,30),
     model = aux_semcred, method = "difference", #para variáveis dummy usamos difference, para variaveis contíua existe o comando continuous
     cov.value1 = "1", cov.value2 = "0",
     xlab = "ROTULO EIXO X",
     main = "TITULO",
     xlim = c(-.1, .1), labeltype = "custom",
     custom.labels = c('Tópico 1','Tópico 2','Tópico 3','Tópico 4','Tópico 5','Tópico 6'
     ,'Tópico 7','Tópico 8','Tópico 9','Tópico 10','Tópico 11','Tópico 12',
     'Tópico 13','Tópico 14','Tópico 15','Tópico 16','Tópico 17','Tópico 18',
     'Tópico 19','Tópico 20','Tópico 21','Tópico 22','Tópico 23','Tópico 24',
     'Tópico 25','Tópico 26','Tópico 27','Tópico 28','Tópico 29','Tópico 30')) #Pode ser substituido por uma lista de termos feita que nem quando foi criada a lista de rotulos dos tópicos, só se atentar que tem que ser na ordem dos tópcios

# Decretos ----------------------------------------------------

#mensura o efeito de uma covariável dummy (0 ou 1) nos decretos 
efeito_VARX_decretos<-estimateEffect(formula = 1:30 ~ VARX, 
                                        stmobj = aux_dec, 
                                        metadata = decretos_stm$meta, 
                                        uncertainty = "Global",
                                        prior = 1e-5)

#plota gráfico demonstrando o efeito sobre os tópicos
plot(efeito_VARX_decretos, covariate = "inserir_variavel", topics = c(1,2,3,4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
                                                                         18, 19, 20, 21, 22, 23, 24, 25, 26, 27,28,29,30),
     model = aux_dec, method = "difference", #para variáveis dummy usamos difference, para variaveis contíua existe o comando continuous
     cov.value1 = "1", cov.value2 = "0",
     xlab = "ROTULO EIXO X",
     main = "TITULO",
     xlim = c(-.1, .1), labeltype = "custom",
     custom.labels = c('Tópico 1','Tópico 2','Tópico 3','Tópico 4','Tópico 5','Tópico 6'
                       ,'Tópico 7','Tópico 8','Tópico 9','Tópico 10','Tópico 11','Tópico 12',
                       'Tópico 13','Tópico 14','Tópico 15','Tópico 16','Tópico 17','Tópico 18',
                       'Tópico 19','Tópico 20','Tópico 21','Tópico 22','Tópico 23','Tópico 24',
                       'Tópico 25','Tópico 26','Tópico 27','Tópico 28','Tópico 29','Tópico 30'))

## Clssificar documentos com maior presença de cada tópico ----------------------------------------------------

# Proposições  ----------------------------------------------------

#calcular o gamma ou probabilidade daquele documento pertencer aquele tópico
prob_top_prop_semcred <- tidy(aux_semcred, matrix = "gamma") 

#filtrar a base somente por o documento e seu tópico de maior gamma
documento_topico_prop_unique <- prob_top_prop_semcred %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) %>%
  ungroup()

#contagem de quantos tópicos existem por documento
contagem_topicos_prop <- documento_topico_prop_unique %>%
  count(topic)

#Gráfico de Barras com a contagem por tópico
ggplot(contagem_topicos_prop, aes(x = factor(topic), y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +  # Adicionar os valores acima das barras
  labs(
    x = "Tópico",
    y = "Número de Proposições",
    title = "Número de Proposições por Tópico"
  ) +
  theme_minimal()

# Transformar lista limpa do stm em table de volta
tabela_proposicoes <- as.data.frame(proposicoes_semcred_stm$meta) 

## Substituir no data frame os tópicos pelos rótulos
prop_topicos$topico <- mapeamento[prop_topicos$topico]

# acrescentar esses dados no dataframe principal em formato de tabela
prop_topicos <- tabela_proposicoes %>% 
  mutate(topico = documento_topico_prop_unique$topic)

# Salvar base de dados
write_xlsx(prop_topicos_semcred, "prop_topicos_semcred.xlsx")


# Decretos ----------------------------------------------------

#calcular o gamma ou probabilidade daquele documento pertencer aquele tópico
documento_topico_dec <- tidy(aux_dec, matrix = "gamma") 

#filtrar a base somente por o documento e seu tópico de maior gamma
documento_topico_dec_unique <- documento_topico_dec %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) %>%
  ungroup()

#contagem de quantos tópicos existem por documento
contagem_topicos_dec <- documento_topico_dec_unique %>%
  count(topic)

#Gráfico de Barras com a contagem por tópico
ggplot(contagem_topicos_dec, aes(x = factor(topic), y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +  # Adicionar os valores acima das barras
  labs(
    x = "Tópico",
    y = "Número de Decretos",
    title = "Número de Decretos por Tópico"
  ) +
  theme_minimal()

# Transformar lista limpa do stm em table de volta
tabela_decretos <- as.data.frame(decretos_stm$meta) 

# acrescentar esses dados no dataframe principal
decretos_topicos <- tabela_decretos %>% 
  mutate(topico = documento_topico_dec_unique$topic)

## Substituir no data frame os tópicos pelos rótulos
decretos_topicos$topico <- mapeamento_decretos[decretos_topicos$topico]

## Salvar base de dados
write_xlsx(decretos_topicos, "decretos_topicos.xlsx")


## Gerar gráficos ----------------------------------------------------
# Proposições  ----------------------------------------------------

#gráfico de quantidade de proposições por tipo ------------------------
proposicoes <- proposicoes %>%
  mutate(tipo = case_when(
    tipo == "Medida Provisória" ~ "MP",
    tipo == "Projeto de Lei" ~ "PL",
    tipo == "Projeto de Lei Complementar" ~ "PLP",
    tipo == "Proposta de Emenda à Constituição" ~ "PEC",
    TRUE ~ tipo  
  ))

#Contagem de proposições por tipo
contagem_tipo_proposicoes <- proposicoes %>%
  count(tipo)

#Porcentagem dos tipos
porcentagens_tipo_proposicoes <- contagem_tipo_proposicoes %>%
  mutate(porcentagem = (n / sum(n)) * 100)

#Gráfico de Porcentagem dos tipos
grafico_tipo_proposicoes <- ggplot(porcentagens_tipo_proposicoes, aes(x = reorder(tipo, desc(-n)), y = porcentagem)) +
  geom_bar(stat = "identity", fill = "#189AB4") +
  theme_minimal() +
  labs(x =  NULL, y = NULL, caption = "Fonte: elaboração própria com os dados do Portal da Câmara dos Deputados") +
  geom_text(aes(label = paste0(round(porcentagem), "%")), 
            vjust = -0.5, 
            size = 4, color = "#05445E") +
  theme(axis.text.y = element_text(size = 10, color = "#05445E"),
        axis.text.x = element_text(size = 10, color = "#05445E"))

#Printar gráfico
grafico_tipo_proposicoes

# ------------------------

#Agregação de tópicos das proposições classificar os 30 tópicos nas 13 enfeases
condicoes_proposicoes <- case_when(
  proposicoes_semcred_topico$topico %in% c("Administração Pública1", "Administração Pública2", "Administração Pública3") ~ "Administração Pública e Governo",
  proposicoes_semcred_topico$topico %in% c("Transporte") ~ "Transporte",
  proposicoes_semcred_topico$topico %in% c("Direitos Civis") ~ "Direitos Civis",
  proposicoes_semcred_topico$topico %in% c("Burocracia") ~ "Burocracia",
  proposicoes_semcred_topico$topico %in% c("Regulação e Serviços", "Comércio Interno1", "Comércio Interno2", "Industria e Comunicações") ~ "Regulação e Serviços",
  proposicoes_semcred_topico$topico %in% c("Defesa", "Defesa - Infra policia e exercito") ~ "Defesa",
  proposicoes_semcred_topico$topico %in% c("Saúde") ~ "Saúde",
  proposicoes_semcred_topico$topico %in% c("Tributação1", "Tributação2","Tributação3") ~ "Tributação",
  proposicoes_semcred_topico$topico %in% c("Pandemia", "Pandemia - Enfrentamento", "Pandemia - Infraestrutura", "Pandemia - Rel Intergov") ~ "Pandemia",
  proposicoes_semcred_topico$topico %in% c("Macroeconomia1", "Macroeconomia2", "Suporte Setores Econômicos1", "Suporte Setores Econômicos2") ~ "Macroeconomia",
  proposicoes_semcred_topico$topico %in% c("Trabalho e Emprego1", "Trabalho e Emprego2", "Trabalho e Emprego3") ~ "Trabalho e Emprego",
  proposicoes_semcred_topico$topico %in% c("Lei e Crime1", "Lei e Crime2") ~ "Lei e Crime",
  proposicoes_semcred_topico$topico %in% c("Cultura") ~ "Cultura",
  TRUE ~ "Outro" # Caso não corresponda a nenhuma das condições anteriores, será atribuído "Outro"
)


#Acrescentar coluna na planilha com o novo tópico agregado
proposicoes_semcred_topico <- proposicoes_semcred_topico %>%
  mutate(topico_agregado = condicoes_proposicoes)

#Contagem de proposicoes
contagem_proposicoes_agregados <- proposicoes_semcred_topico %>%
  count(topico_agregado)

#Porcentagem de proposicoes
porcentagens_proposicoes_agregados <- contagem_proposicoes_agregados %>%
  mutate(porcentagem = (n / sum(n)) * 100)

#Gráfico de Porcentagem das proposicoes
porcentagens_proposicoes_agregados%>%
  ggplot( aes(x = reorder(topico_agregado, desc(-n)), y = porcentagem)) +
  geom_bar(stat = "identity", fill = "#189AB4") +
  theme_minimal() +
  coord_flip() +
  labs(x =  NULL, y = NULL, caption = "Fonte: elaboração própria") +
  geom_text(aes(label = paste0(round(porcentagem), "%")), 
            vjust = 0.5, hjust =  -0.1,
            size = 3, color = "#05445E") +
  theme(axis.text.y = element_text(size = 10, color = "#05445E"),
        axis.text.x = element_text(size = 10, color = "#05445E"))

porcentagens_proposicoes_agregados


#exemplo de como fazer o mesmo gráfico mas filtrado por tipo ou ano (tem que crescentar a coluna ano nas proposições)
contagem_proposicoes_agregados <- proposicoes_semcred_topico %>%
  group_by(tipo) %>%
  count(topico_agregado)

porcentagens_proposicoes_agregados <- contagem_proposicoes_agregados %>%
  group_by(tipo) %>%
  mutate(porcentagem = (n / sum(n)) * 100)

porcentagens_proposicoes_agregados%>%
  group_by(tipo) %>%
  ggplot( aes(x = reorder(topico_agregado, desc(-n)), y = porcentagem)) +
  geom_bar(stat = "identity", fill = "#189AB4") +
  theme_minimal() +
  coord_flip() +
  labs(x =  NULL, y = NULL, caption = "Fonte: elaboração própria") +
  geom_text(aes(label = paste0(round(porcentagem), "%")), 
            vjust = 0.5, hjust =  -0.1,
            size = 3, color = "#05445E") +
  facet_wrap(~ tipo) +
  theme(axis.text.y = element_text(size = 10, color = "#05445E"),
        axis.text.x = element_text(size = 10, color = "#05445E"))




# Decretos ----------------------------------------------------


#Agregação de tópicos dos decretos
condicoes_decretos <- case_when(
  decretos_topicos$topico %in% c("Administração Pública1", "Administração Pública2", "Administração Pública3", 
                                 "Administração Pública4", "Órgãos da Presidência", 
                                 "Relações Intergovernamentais", "Contratações Públicas", "Questões Jurídicas") ~ "Administração Pública e Governo",
  decretos_topicos$topico %in% c("Transporte1", "Transporte2") ~ "Transporte",
  decretos_topicos$topico %in% c("Burocracia1", "Burocracia2") ~ "Burocracia",
  decretos_topicos$topico %in% c("Fontes de Energia", "Energia") ~ "Energia",
  decretos_topicos$topico %in% c("Burocracia do Exército", "Defesa e Exército", "Defesa e Exército - Regulação") ~ "Defesa",
  decretos_topicos$topico %in% c("Comércio Exterior1", "Comércio Exterior2", "Política Externa") ~ "Política e Comércio Exterior",
  decretos_topicos$topico %in% c("Tributação", "Tributação e Benefícios Sociais") ~ "Tributação",
  decretos_topicos$topico %in% c("Pandemia") ~ "Pandemia",
  decretos_topicos$topico %in% c("Macroeconomia") ~ "Orçamento",
  decretos_topicos$topico %in% c("Tecnologia e Comunicações2", "Tecnologia e Comunicações1") ~ "Ciência e Tecnologia",
  decretos_topicos$topico %in% c("Desestatização e Setor Privado") ~ "Desestatização",
  decretos_topicos$topico %in% c("Lei e Crime") ~ "Lei e Crime",
  decretos_topicos$topico %in% c("Cultura", "Educação") ~ "Políticas Sociais",
  TRUE ~ "Outro" # Caso não corresponda a nenhuma das condições anteriores, será atribuído "Outro"
)

#Acrescentar coluna na planilha com o novo tópico agregado
decretos_topicos <- decretos_topicos %>%
  mutate(topico_agregado = condicoes_decretos)

#Contagem de decretos
contagem_decretos_agregados <- decretos_topicos %>%
  count(topico_agregado)

#Porcentagem de decretos
porcentagens_decretos_agregados <- contagem_decretos_agregados %>%
  mutate(porcentagem = (n / sum(n)) * 100)

#Gráfico de Porcentagem dos decretos
porcentagens_decretos_agregados %>%
  ggplot(aes(x = reorder(topico_agregado, desc(-n)), y = porcentagem)) +
  geom_bar(stat = "identity", fill = "#189AB4") +
  theme_minimal() +
  coord_flip() +
  labs(x =  NULL, y = NULL, caption = "Fonte: elaboração própria") +
  geom_text(aes(label = paste0(round(porcentagem), "%")), 
            vjust = 0.5, hjust =  -0.1,
            size = 4, color = "#05445E") +
  theme(axis.text.y = element_text(size = 10, color = "#05445E"),
        axis.text.x = element_text(size = 10, color = "#05445E"))

# por ano

#Contagem de decretos
contagem_decretos_agregados <- decretos_topicos %>%
  group_by(ano) %>%
  count(topico_agregado)

#Porcentagem de decretos
porcentagens_decretos_agregados <- contagem_decretos_agregados %>%
  group_by(ano) %>%
  mutate(porcentagem = (n / sum(n)) * 100)

#Gráfico de Porcentagem dos decretos
porcentagens_decretos_agregados %>%
  group_by(ano) %>%
  ggplot(aes(x = reorder(topico_agregado, desc(-n)), y = porcentagem)) +
  geom_bar(stat = "identity", fill = "#189AB4") +
  theme_minimal() +
  coord_flip() +
  labs(x =  NULL, y = NULL, caption = "Fonte: elaboração própria") +
  geom_text(aes(label = paste0(round(porcentagem), "%")), 
            vjust = 0.5, hjust =  -0.1,
            size = 4, color = "#05445E") +
  facet_wrap(~ ano) +
  theme(axis.text.y = element_text(size = 10, color = "#05445E"),
        axis.text.x = element_text(size = 10, color = "#05445E"))
### FIM ---------------------------------------------------------------------


