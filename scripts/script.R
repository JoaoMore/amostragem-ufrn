source('libraries.R')

load('imagem.RData')
# definindo um vetor com os nomes dos centros (somente esses serão estudados)
centros <- c('CENTRO DE TECNOLOGIA',
             'CENTRO DE CIÊNCIAS EXATAS E DA TERRA',
             'CENTRO DE BIOCIÊNCIAS',
             'CENTRO DE CIÊNCIAS DA SAÚDE',
             'CENTRO DE CIÊNCIAS HUMANAS, LETRAS E ARTES',
             'CENTRO DE EDUCAÇÃO',
             'CENTRO DE ENSINO SUPERIOR DO SERIDÓ',
             'CENTRO DE CIÊNCIAS SOCIAIS APLICADAS')

# lendo o banco de dados original
gastos <- read_delim("gastos-por-unidade.csv", 
                     ";", escape_double = FALSE, trim_ws = TRUE)

#fitrando apenas as observações dos centros da ufrn
cts <- gastos %>% 
  filter(unidade %in% centros) %>% 
  mutate(unidade = sapply(unidade, centro, USE.NAMES = FALSE)) %>% 
  arrange(unidade) 

#dataset com os valores reais dos parâmetros e tamanhos dos estratos
(size <- cts %>% 
    group_by(unidade) %>% 
    summarise(NH = n(), media = mean(valor), variancia = var(valor), desvio = sd(valor)) %>% 
    mutate(N = sum(NH), razao = NH/N) %>%
    select(unidade, NH, N, razao, media, variancia, desvio) %>% 
    arrange(unidade))

# amostragem estratificada ------------------------------------------------

# alocação de neyman
attach(size)

# calculando o tamanho da amostra
N <- 1009
e <- 50000
z <- qnorm(0.975)
V <- (e/qnorm(.975))^2


(n <- ceiling(((N^-2)*(sum(NH*desvio)^2))/(V+(N^-2)*sum(NH*variancia))))

# calculando o tamanho da amostra por estrato
(tam_amost_estr <- round((NH*desvio/sum(NH*desvio))*n))
names(tam_amost_estr) <- size$unidade


# amostragem estratificada
(amost_estr <- strata(data = cts, stratanames = 'unidade', size = tam_amost_estr,
                     method = 'srswor', description = TRUE))

(amost_estr <- data.frame(amost_estr,
                      valor = cts[amost_estr$ID_unit,'valor'],
                      fpc = rep(size$NH, tam_amost_estr)))

# definição do plano amostral para calcular estimativas

design_str <- svydesign(ids = ~ID_unit, strata = ~unidade, data = amost_estr, fpc = ~fpc)

summary(design_str)

# calculo das estimativas

svymean(~valor, design_str, deff = TRUE)
confint(svymean(~valor, design_str))

svytotal(~valor, design_str, deff = TRUE)
confint(svytotal(~valor, design_str))


# amostragem aleatoria simples --------------------------------------------

index <- as.logical(srswor(739,N))

(amostr_assr <- data.frame(id = 1:739, cts[index,], fpc = rep(N,739))) %>% head()

design_aas <- svydesign(~ID_unit, data = amost_estr, fpc = rep(1009, 739))

summary(design_aas)

svymean(~valor, design_aas)
confint(svymean(~valor, design_aas))

svytotal(~valor, design_aas)
confint(svytotal(~valor, design_aas))

save.image(file = 'imagem.RData')


svyby(~valor, by = ~unidade, design = design_str, FUN = svymean)
svyby(~valor, by = ~unidade, design = design_str, FUN = svytotal)
 

medias <- data.frame(li = c(603017.6, 573854.2), 
                     ls = c(722394.4, 672307.1),
                     estimativa = c(662706, 623081),
                     amostra = c('aassr', 'aae'))

totais <- data.frame(li = c(608444802, 579018907), 
                     ls = c(728895907, 678357849),
                     estimativa = c(668670355, 628688378),
                     amostra = c('aassr', 'aae'))

ggplot(medias, aes(x = amostra)) +
  geom_linerange(aes(amostra, ymin = li, ymax = ls)) +
  geom_point(aes(y = li), shape = '_') +
  geom_point(aes(y = ls), shape = '_') +
  geom_point(aes(y = estimativa)) +
  geom_hline(yintercept = mean(cts$valor), col = 'red', lty = 2) +
  geom_text(aes(x = 0.5, y = mean(cts$valor), label = 'Valor real\nda média'),
            vjust = -1, hjust = -.3, size = 3) +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(labels = scales::comma,
                     limits = c(min(medias$li) - 5e4, max(medias$ls) + 5e4),
                     breaks = seq(500000, 750000, by = 50000)) +
  scale_x_discrete(labels = c('Amostragem Estratificada','Amostragem aleatória')) +
  theme(text = element_text(size = 10))


ggplot(totais, aes(x = amostra)) +
  geom_linerange(aes(amostra, ymin = li, ymax = ls)) +
  geom_point(aes(y = li), shape = '_') +
  geom_point(aes(y = ls), shape = '_') +
  geom_point(aes(y = estimativa)) +
  geom_hline(yintercept = sum(cts$valor), col = 'red', lty = 2) +
  geom_text(aes(x = 0.5, y = sum(cts$valor), label = 'Valor real\n do total'),
            vjust = -1, hjust = -.3, size = 3) +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(labels = scales::unit_format(scale = 1e-6,unit = 'M', prefix = 'R$'),
                     limits = c(sum(cts$valor)- 1e8, sum(cts$valor) + 1e8)) +
  scale_x_discrete(labels = c('Amostragem Estratificada','Amostragem aleatória')) +
  theme(text = element_text(size = 10))
