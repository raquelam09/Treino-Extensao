# script roteiro do BDAM - no repositório Treino_Extensao
# Antes de começar a fazer qualquer coisa:
# a) commit este roteiro com a mensagem "script roteiro BDAM" e envie para o repositório Treino_Extensao
# b) salve o script com outro nome (script_BDAM.R) e commit com a mensagem "script BDAM" e envie para o repositório Treino_Extensao

# Ao inserir os comandos em cada Tarefa de cada Etapa, mantenha as linhas de comentários e orientações colocadas pela professora


##### ETAPA 1 - banco 1 - equivalente ao SIM ######
##### Você deve criar e estar na branch banco-1 antes de inserir os comandos #####
##### NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho ###

# Tarefa 1: Leitura do banco de dados banco 1 = SIM.csv com o nome de dados_bd1
# Ler o arquivo, verificar estrutura dos dados e dar uma olhada nos dados
dados_bd1 = read.csv("banco 1 = SIM.csv", header = TRUE, sep=";")
str(dados_bd1)
summary(dados_bd1)
View(dados_bd1)

# Ao terminar a Tarefa 1 commit com a mensagem " script - tarefa 1" e envie para o repositório Treino_Extensao


# Tarefa 2: Manipulação dos dados
# Padronizar as categorias  para Carro e Moto e indicar que branco é NA
# Atribuir legendas para a variável SEXO_CONDUTOR_CAUSADOR, sendo 1: Masculino e 2: Feminino
# Criar uma nova variável em dados_bd1 F_IDADE categorizando as idades em: 22 a 34, 35 a 45
dados_bd1$VEICULO_CAUSADOR[dados_bd1$VEICULO_CAUSADOR == ""] = NA

dados_bd1$VEICULO_CAUSADOR[dados_bd1$VEICULO_CAUSADOR %in% c("carro", "CARRO")] = "Carro"
dados_bd1$VEICULO_CAUSADOR[dados_bd1$VEICULO_CAUSADOR %in% c("moto", "MOTO")] = "Moto"

dados_bd1$SEXO_CONDUTOR_CAUSADOR = factor(dados_bd1$SEXO_CONDUTOR_CAUSADOR, levels = c(1,2), labels = c("Masculino", "Feminino"))

dados_bd1$F_IDADE = ifelse(dados_bd1$IDADE_CONDUTOR_CAUSADOR < 35, "22 a 34", "35 a 45")


# Ao terminar a Tarefa 2 commit com a mensagem " script - tarefa 1 a 2" e envie para o repositório Treino_Extensao


# Tarefa 3: Criar o banco de dados BANCO1_RJ, POR MUNICÍPIO, com as seguintes variáveis listadas abaixo. 
# Variáveis que se referem a medidas de posição e de dispersão devem ser calculadas sem considerar NAs

# Atenção: a 1a linha do banco deve ser da UF 33
# ANO: 2025
# NIVEL: UF ou MUNICIPIO
# CODIGO: código do municipio (ou da UF)
# TV: total de veículos causadores de acidentes
# TC: total de carros causadores do acidente
# TM: total de motos causadoras do acidente
# TVCF: total de veículos causadores de acidentes com condutor mulher
# TVCM: total de veículos causadores de acidentes com condutor homem
# TC_22_34: total de condutores causadores de acidentes na faixa etária de 22 a 34 anos
# TC_35_45: total de condutores causadores de acidentes na faixa etária de 35 a 45 anos
# NMF: número médio de feridos
# DPF: desvio-padrão de feridos
# F_P25: percentil 25 do número de feridos
# F_P50: percentil 50 do número de feridos
# F_P75: percentil 75 do número de feridos
# TAFA: total de acidentes cuja causa foi falta de atenção
# TADS: total de acidentes cuja causa foi desrespeito à sinalização
# TADA: total de acidentes cuja causa foi o uso de drogas ou álcool
# TACO: total de acidentes cuja causa foi outros

names(dados_bd1)[names(dados_bd1) == "MUNICIPIO"] = "CODIGO"

base = data.frame(CODIGO = sort(unique(dados_bd1$CODIGO)))

# TV: total de veículos causadores de acidentes
TV = as.data.frame(table(factor(dados_bd1$CODIGO)))
names(TV) = c("CODIGO","TV")

base = merge(base, TV, by = "CODIGO", all.x = TRUE)


# TC: total de carros causadores do acidente e TM: total de motos causadoras do acidente
tab = table(dados_bd1$CODIGO, factor(dados_bd1$VEICULO_CAUSADOR, levels = c("Carro","Moto")))
df = as.data.frame.matrix(tab)
names(df) = c("TC","TM")
df$CODIGO = rownames(df)

base = merge(base, df, by = "CODIGO", all.x = TRUE)

# TVCF: total de veículos causadores de acidentes com condutor mulher e TVCM: total de veículos causadores de acidentes com condutor homem
tab = table(dados_bd1$CODIGO, factor(dados_bd1$SEXO_CONDUTOR_CAUSADOR, levels = c("Feminino","Masculino")))
df = as.data.frame.matrix(tab)
names(df) = c("TVCF","TVCM")
df$CODIGO = rownames(df)

base = merge(base, df, by = "CODIGO", all.x = TRUE)

# TC_22_34: total de condutores causadores de acidentes na faixa etária de 22 a 34 anos e TC_35_45: total de condutores causadores de acidentes na faixa etária de 35 a 45 anos
tab = table(dados_bd1$CODIGO, factor(dados_bd1$F_IDADE, levels = c("22 a 34","35 a 45")))
df = as.data.frame.matrix(tab)
names(df) = c("TC_22_34","TC_35_45")
df$CODIGO = rownames(df)

base = merge(base, df, by = "CODIGO", all.x = TRUE)

# NMF: número médio de feridos e DPF: desvio-padrão de feridos
media_feridos = aggregate(NUM_FERIDOS_GRAVES ~ CODIGO, dados_bd1, mean, na.rm = TRUE)
media_feridos$NUM_FERIDOS_GRAVES = round(media_feridos$NUM_FERIDOS_GRAVES, 2)
names(media_feridos)[2] = "NMF"


dp_feridos = aggregate(NUM_FERIDOS_GRAVES ~ CODIGO, dados_bd1, sd, na.rm = TRUE)
dp_feridos$NUM_FERIDOS_GRAVES = round(dp_feridos$NUM_FERIDOS_GRAVES, 2)
names(dp_feridos)[2] = "DPF"

temp = merge(media_feridos, dp_feridos, by = "CODIGO")
base = merge(base, temp, by = "CODIGO", all.x = TRUE)

# F_P25: percentil 25 do número de feridos F_P50: percentil 50 do número de feridos e F_P75: percentil 75 do número de feridos
p_feridos = aggregate(NUM_FERIDOS_GRAVES ~ CODIGO,dados_bd1, function(x) quantile(x, probs = c(0.25,0.5,0.75), na.rm = TRUE))
p_feridos = do.call(data.frame, p_feridos)
names(p_feridos) = c("CODIGO","F_P25","F_P50","F_P75")
p_feridos[, c("F_P25","F_P50","F_P75")] = round(p_feridos[, c("F_P25","F_P50","F_P75")], 2)

base = merge(base, p_feridos, by="CODIGO", all.x=TRUE)

# TAFA: total de acidentes cuja causa foi falta de atenção  TADS: total de acidentes cuja causa foi desrespeito à sinalização
# TADA: total de acidentes cuja causa foi o uso de drogas ou álcool  TACO: total de acidentes cuja causa foi outros
tab = table(dados_bd1$CODIGO, factor(dados_bd1$CAUSA_ACIDENTE, levels = c("falta de atencao","desrespeito a sinalizacao", "alcool ou drogas", "outros")))
df = as.data.frame.matrix(tab)
names(df) = c("TAFA","TADS", "TADA", "TACO")
df$CODIGO = rownames(df)

base = merge(base, df, by = "CODIGO", all.x = TRUE)

# Linha da UF
linha_estado = base[1, ]
linha_estado[,] = NA

# Colunas de contagem: indicar as variáveis contínuas, que por exclusão não terão valores somados
cols_contagem = setdiff(names(base), c("CODIGO","NMF","DPF","F_P25","F_P50","F_P75"))

linha_estado[cols_contagem] = colSums(base[cols_contagem], na.rm = TRUE)

# Colunas de medidas para variáveis quantitativas 
linha_estado$NMF = round(mean(dados_bd1$NUM_FERIDOS_GRAVES, na.rm = TRUE), 2)
linha_estado$DPF = round(sd(dados_bd1$NUM_FERIDOS_GRAVES, na.rm = TRUE), 2)

q = round(quantile(dados_bd1$NUM_FERIDOS_GRAVES, probs = c(0.25,0.5,0.75), na.rm = TRUE), 2)
linha_estado$F_P25 = q[1]
linha_estado$F_P50 = q[2]
linha_estado$F_P75 = q[3]

# Código da UF e ordem das colunas
linha_estado$CODIGO = 33

# Banco de dados final para o Rio de Janeiro
BANCO1_RJ = rbind(linha_estado, base)

BANCO1_RJ$NIVEL = c("UF", rep("MUNICIPIO", nrow(BANCO1_RJ)-1))
BANCO1_RJ$ANO = 2025

BANCO1_RJ = BANCO1_RJ[, c("ANO","NIVEL","CODIGO", names(BANCO1_RJ)[!names(BANCO1_RJ) %in% c("ANO","NIVEL","CODIGO")])]
BANCO1_RJ$CODIGO = as.character(BANCO1_RJ$CODIGO)

# Verificando o banco final
str(BANCO1_RJ)
head(BANCO1_RJ)
dim(BANCO1_RJ)

# Ao terminar a Tarefa 3 commit com a mensagem " script - tarefa 1 a 3" e envie para o repositório Treino_Extensao


# Tarefa 4: Exportar o banco de dados BANCO1_RJ com o nome BANCO1_RJ.csv
write.csv(BANCO1_RJ, "BANCO1_RJ.csv",  row.names = FALSE)

# Ao terminar a Tarefa 4 commit com a mensagem "dados e script - Etapa 1"



##### ETAPA 2 - banco 2 - equivalente ao SINASC ######
##### Você deve criar e estar na branch banco-2 antes de inserir os comandos #####
##### NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho ###

# Tarefa 1: Leitura do banco de dados banco 2 = SINASC.csv com o nome de dados_bd2
# Ler o arquivo, verificar estrutura dos dados e dar uma olhada nos dados

# Ao terminar a Tarefa 1 commit com a mensagem " script - tarefa 1" e envie para o repositório Treino_Extensao


# Tarefa 2: Manipulação dos dados
# Padronizar as categorias SEXO_PROPRIETARIO para Masculino e Feminino
# Atribuir legendas para a variável TIPO_VEICULO, sendo 1: Carro e 2: Moto
# Criar uma nova variável em dados_bd2 F_IDADE categorizando as idades em: 22 a 34, 35 a 45

# Ao terminar a Tarefa 2 commit com a mensagem " script - tarefa 1 a 2" e envie para o repositório Treino_Extensao


# Tarefa 3: Criar o banco de dados BANCO2_RJ, POR MUNICÍPIO, com as seguintes variáveis listadas abaixo. 
# Variáveis que se referem a medidas de posição e de dispersão devem ser calculadas sem considerar NAs

# Atenção: a 1a linha do banco deve ser da UF 33
# ANO: 2025
# NIVEL: UF ou MUNICIPIO
# CODIGO: código do municipio (ou da UF)
# TVV: total de veiculos vendidos
# TCV: total de carros vendidos
# TMV: total de motos vendidas
# TVVF: total de veículos vendidos para mulher
# TVVM: total de veículos vendidos para homem
# TVC_22_34: total de veiculos vendidos para pessoas na faixa etária de 22 a 34 anos
# TVC_35_45: total de veiculos vendidos para pessoas na faixa etária de 35 a 45 anos
# VMV: valor médio dos veículos vendidos
# DPV: desvio-padrão do valor dos veículos vendidos
# V_P25: percentil 25 do valor dos veículos vendidos
# V_P50: percentil 50 do valor dos veículos vendidos
# V_P75: percentil 75 do valor dos veículos vendidos

# Ao terminar a Tarefa 3 commit com a mensagem " script - tarefa 1 a 3" e envie para o repositório Treino_Extensao


# Tarefa 4: Exportar o banco de dados BANCO2_RJ com o nome BANCO2_RJ.csv

# Ao terminar a Tarefa 4 commit com a mensagem "dados e script - Etapa 2" e envie para o repositório Treino_Extensao



##### ETAPA 3 - banco 3 - equivalente ao SIDRA ######
##### Você deve criar e estar na branch banco-3 antes de inserir os comandos #####
##### NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho ###

# Tarefa 1: Leitura do banco de dados banco 3 = SIDRA.csv com o nome de dados_bd3
# Ler o arquivo, verificar estrutura dos dados e dar uma olhada nos dados

# Ao terminar a Tarefa 1 commit com a mensagem " script - tarefa 1" e envie para o repositório Treino_Extensao


# Tarefa 2: Manipulação dos dados
# Criar a variável MUNICIPIOS = MUNICIPIO em dados_bd3, sendo que agora com 6 dígitos (em vez de 7 dígitos), desprezando o último dígito verificador

# Ao terminar a Tarefa 2 commit com a mensagem " script - tarefa 1 a 2" e envie para o repositório Treino_Extensao


# Tarefa 3: Criar o banco de dados BANCO3_RJ, POR MUNICÍPIO, com as seguintes variáveis listadas abaixo. 
# Atenção: a 1a linha do banco deve ser da UF 33
# ANO: 2025
# NIVEL: UF ou MUNICIPIO
# CODIGO: código do municipio (ou da UF)
# POPH: população total de habilitados
# POPHF: população total feminina de habilitadas
# POPHM: população total masculina de habilitadas

# Ao terminar a Tarefa 3 commit com a mensagem " script - tarefa 1 a 3" e envie para o repositório Treino_Extensao


# Tarefa 4: Exportar o banco de dados BANCO3_RJ com o nome BANCO3_RJ.csv

# Ao terminar a Tarefa 4 commit com a mensagem "dados e script - Etapa 3" e envie para o repositório Treino_Extensao


##### ETAPA 4 - banco 4 - equivalente ao ATLAS ######
##### Você deve criar e estar na branch banco-4 antes de inserir os comandos #####
##### NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho ###

# Tarefa 1: Leitura do banco de dados banco 4 = ATLAS.csv com o nome de dados_bd4 e do arquivo com tabela de códigos do IBGE
# códigos dos municípios - 2010.csv" com os códigos do IBGE para os municípios do Brasil
# Ler os arquivos, verificar estruturas dos dados e dar uma olhada nos dados

# Ao terminar a Tarefa 1 commit com a mensagem " script - tarefa 1" e envie para o repositório Treino_Extensao


# Tarefa 2: Manipulação dos dados
# Criar uma nova variável em dados_bd4 MUNICIPIOS atribuindo os códigos dos municípios, de forma a ficar
# coerente com os nomes dos municipios e códigos IBGE

# Ao terminar a Tarefa 2 commit com a mensagem " script - tarefa 1 a 2" e envie para o repositório Treino_Extensao


# Tarefa 3: Criar o banco de dados BANCO4_RJ, POR MUNICÍPIO, com as seguintes variáveis listadas abaixo. 
# Atenção: a 1a linha do banco deve ser da UF 33
# ANO: 2025
# NIVEL: UF ou MUNICIPIO
# CODIGO: código do municipio (ou da UF)
# QR_CA: qualidade da rodovia em 2020
# QRU: qualidade das rodovias urbanas
# QRR: qualidade das rodovias rurais


# Ao terminar a Tarefa 3 commit com a mensagem " script - tarefa 1 a 3" e envie para o repositório Treino_Extensao


# Tarefa 4: Exportar o banco de dados BANCO4_RJ com o nome BANCO4_RJ.csv

# Ao terminar a Tarefa 4 commit com a mensagem "dados e script - Etapa 4" e envie para o repositório Treino_Extensao



##### ETAPA 5 - banco 5 - equivalente ao SINISA ######
##### Você deve criar e estar na branch banco-5 antes de inserir os comandos #####
##### NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho ###

# Tarefa 1: Leitura do banco de dados banco 5 = SINISA.csv com o nome de dados_bd5 
# Ler os arquivos, verificar estruturas dos dados e dar uma olhada nos dados

# Ao terminar a Tarefa 1 commit com a mensagem " script - tarefa 1" e envie para o repositório Treino_Extensao


# Tarefa 2: Manipulação dos dados
# Observe que os números estão com ponto indicando milhar. Estes pontos devem ser extraídos para não confundir com decimal

# Ao terminar a Tarefa 2 commit com a mensagem " script - tarefa 1 a 2" e envie para o repositório Treino_Extensao


# Tarefa 3: Criar o banco de dados BANCO5_RJ, POR MUNICÍPIO, com as seguintes variáveis listadas abaixo. 
# Atenção: a 1a linha do banco deve ser da UF 33
# ANO: 2025
# NIVEL: UF ou MUNICIPIO
# CODIGO: código do municipio (ou da UF)
# NCR: número de carros registrados
# NMR: número de motos registradas

# Ao terminar a Tarefa 3 commit com a mensagem " script - tarefa 1 a 3" e envie para o repositório Treino_Extensao


# Tarefa 4: Exportar o banco de dados BANCO5_RJ com o nome BANCO5_RJ.csv

# Ao terminar a Tarefa 4 commit com a mensagem "dados e script - Etapa 5" e envie para o repositório Treino_Extensao



##### Merge para a branch main ######
##### Após terminar todas as 5 etapas acima você deve ir para a branch main e fazer merge de cada branch para o Git ajustar tudo


##### ETAPA 6 - criação do BDAM - equivalente ao BDEM ######
##### Você deve estar na branch main #####

# Tarefa 1: Concatenar (merge) os 5 arquivos gerados em cada uma das 5 etapas num único arquivo chamado BDAM_RJ, de modo que
# as variáveis fiquem dispostas da seguinte descrita abaixo

# Atenção: a 1a linha do banco deve ser da UF 33
# ANO: 2025
# NIVEL: UF ou MUNICIPIO
# CODIGO: código do municipio (ou da UF)
# POPH: população total de habilitados
# POPHF: população total feminina de habilitadas
# POPHM: população total masculina de habilitadas
# QR_CA: qualidade da rodovia em 2020
# QRU: qualidade das rodovias urbanas
# QRR: qualidade das rodovias rurais
# TVV: total de veiculos vendidos
# TCV: total de carros vendidos
# TMV: total de motos vendidas
# TVVF: total de veículos vendidos para mulher
# TVVM: total de veículos vendidos para homem
# TVC_22_34: total de veiculos vendidos para pessoas na faixa etária de 22 a 34 anos
# TVC_35_45: total de veiculos vendidos para pessoas na faixa etária de 35 a 45 anos
# VMV: valor médio dos veículos vendidos
# DPV: desvio-padrão do valor dos veículos vendidos
# V_P25: percentil 25 do valor dos veículos vendidos
# V_P50: percentil 50 do valor dos veículos vendidos
# V_P75: percentil 75 do valor dos veículos vendidos
# TV: total de veículos causadores de acidentes
# TC: total de carros causadores do acidente
# TM: total de motos causadoras do acidente
# TVCF: total de veículos causadores de acidentes com condutor mulher
# TVCM: total de veículos causadores de acidentes com condutor homem
# TC_22_34: total de condutores causadores de acidentes na faixa etária de 22 a 34 anos
# TC_35_45: total de condutores causadores de acidentes na faixa etária de 35 a 45 anos
# NMF: número médio de feridos
# DPF: desvio-padrão de feridos
# F_P25: percentil 25 do número de feridos
# F_P50: percentil 50 do número de feridos
# F_P75: percentil 75 do número de feridos
# TAFA: total de acidentes cuja causa foi falta de atenção
# TADS: total de acidentes cuja causa foi desrespeito à sinalização
# TADA: total de acidentes cuja causa foi o uso de drogas ou álcool
# TACO: total de acidentes cuja causa foi outros
# NCR: número de carros registrados
# NMR: número de motos registradas

# Ao terminar a Tarefa 1 commit com a mensagem " script - tarefa 1" e envie para o repositório Treino_Extensao


# Tarefa 2: Exportar o banco de dados BDAM_RJ com o nome BDAM_RJ.csv

# Ao terminar a Tarefa 2 commit com a mensagem "dados e script - Etapa 6" e envie para o repositório Treino_Extensao


