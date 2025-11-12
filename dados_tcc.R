################################################################################
#                                   PACOTES                                    #
################################################################################

install.packages("remotes")
install.packages("devtools")
install.packages("sidrar")
install.packages("arrow")
install.packages("ggplot2")
install.packages("geobr")
install.packages("sf")
devtools::install_github("danicat/read.dbc")
remotes::install_github("rfsaldanha/microdatasus")

library(microdatasus)
library(sidrar)
library(dplyr)
library(readxl)
library(writexl)
library(arrow)
library(ggplot2)
library(geobr)
library(sf)
library(tidyr)
library(grid)
library(stringr)
library(purrr)


install.packages("cowplot")
library(cowplot)

################################################################################
#                         CÓDIGOS E MAPAS DE MUNICÍPIOS                        #
################################################################################

# Divisão territorial brasileira de 2023:
munic_br <- read_excel("./data_read/municipios_BR.xlsx") 
munic_br <- subset(munic_br, select = -`...10`)

# Compatibiliza os códigos IBGE e os códigos TSE:
munic_br_tse_ibge <- read_excel("./data_read/municipios_BR_tse_ibge.xlsx")
munic_br_tse_ibge$codigo_ibge <- as.character(munic_br_tse_ibge$codigo_ibge)

munic_br <- munic_br |>
  left_join(munic_br_tse_ibge %>% select(codigo_ibge, codigo_tse),
            by = c("cod_munic7D" = "codigo_ibge"))

munic_br$codigo_tse <- as.character(munic_br$codigo_tse)

# Compara as divisões territoriais de 2010 e 2022:
munic_2010 <- read_excel("./data_read/dtb_2010.xls")
munic_2022 <- read_excel("./data_read/dtb_2022.xls")

novos_2022 <- munic_2022 %>%
  anti_join(munic_2010, by = c("Código Município Completo" = "Município"))

# Gera a base de dados do tcc:
dados_tcc <- munic_br |>
  select(uf, nome_uf, cod_munic7D, nome_munic, codigo_tse) |>
  mutate(cod_munic6D = substr(cod_munic7D, 1, 6))

dados_tcc <- dados_tcc[,c("uf", "nome_uf", "cod_munic6D", "cod_munic7D", "codigo_tse", "nome_munic")]

# Extrai as formas dos municípios:
municipios <- st_read("./data_read/BR_Municipios_2022.shp")

# Acrescenta as geometrias na base do tcc:
dados_tcc <- dados_tcc |>
  left_join(municipios |> select(CD_MUN, geometry), by = c("cod_munic7D" = "CD_MUN"))

# Exclui a tabela auxiliar:
rm("municipios")

################################################################################
#                                ÓBITOS MATERNOS                               #
################################################################################

#-------------------------------2009, 2010 e 2011-------------------------------

# Extrai e processa os dados do Datasus:
obtMat9a11_bruto <- fetch_datasus(year_start = 2009, 
                                  year_end = 2011, 
                                  uf = "all", 
                                  information_system = "SIM-DOMAT")

obtMat9a11 <- process_sim(obtMat9a11_bruto)

rm("obtMat9a11_bruto")

# Óbitos maternos entre 0 e 42 dias após o parto por municípío:
obtMat9a11_munic <- obtMat9a11 |> 
  filter(OBITOPUERP == "De 0 a 42 dias") |>
  group_by(CODMUNRES) |>
  summarise(obtMat9a11_aux = n())

# Acrescenta óbitos maternos por municípios em 2010 na base do tcc:
dados_tcc <- dados_tcc |>
  left_join(obtMat9a11_munic,
            by = c("cod_munic6D" = "CODMUNRES"))

# Substitui valores NA por 0:
dados_tcc$obtMat9a11 <- ifelse(is.na(dados_tcc$obtMat9a11_aux), 0, dados_tcc$obtMat9a11_aux)

# Exclui a coluna auxiliar:
dados_tcc <- dados_tcc[,setdiff(names(dados_tcc), 
                                c("obtMat9a11_aux"))]

#-------------------------------2021, 2022 e 2023-------------------------------

# Extrai e processa os dados do Datasus:
obtMat21a23_bruto <- fetch_datasus(year_start = 2021, 
                                  year_end = 2023, 
                                  uf = "all", 
                                  information_system = "SIM-DOMAT")

obtMat21a23 <- process_sim(obtMat21a23_bruto)

rm("obtMat21a23_bruto")

# Óbitos maternos entre 0 e 42 dias após o parto por municípío:
obtMat21a23_munic <- obtMat21a23 |> 
  filter(OBITOPUERP == "De 0 a 42 dias") |>
  group_by(CODMUNRES) |>
  summarise(obtMat21a23_aux = n())

# Acrescenta óbitos maternos por municípios em 2010 na base do tcc:
dados_tcc <- dados_tcc |>
  left_join(obtMat21a23_munic,
            by = c("cod_munic6D" = "CODMUNRES"))

# Substitui valores NA por 0:
dados_tcc$obtMat21a23 <- ifelse(is.na(dados_tcc$obtMat21a23_aux), 0, dados_tcc$obtMat21a23_aux)

# Exclui a coluna auxiliar:
dados_tcc <- dados_tcc[,setdiff(names(dados_tcc), 
                                c("obtMat21a23_aux"))]

#-------------------------------------------------------------------------------

# Exclui as tabelas auxiliares:
rm(list = c("obtMat9a11", "obtMat9a11_munic", "obtMat21a23", "obtMat21a23_munic"))

################################################################################
#                                  NASCIDOS VIVOS                              #
################################################################################

#-------------------------------------2010--------------------------------------

# Extrai e processa os dados do Datasus:
nascVivos2010_bruto <- fetch_datasus(year_start = 2010, 
                                  year_end = 2010, 
                                  uf = "all", 
                                  information_system = "SINASC")

nascVivos2010 <- process_sinasc(nascVivos2010_bruto)

rm("nascVivos2010_bruto")

# Nascidos vivos por município:
nascVivos2010_munic <- nascVivos2010 |>
  group_by(CODMUNRES) |>
  summarise(nascVivos2010_aux = n())

# Acrescenta nascidos vivos por municípios em 2010 na base do tcc:
dados_tcc <- dados_tcc |>
  left_join(nascVivos2010_munic,
            by = c("cod_munic6D" = "CODMUNRES"))

# Substitui valores NA por 0:
dados_tcc$nascVivos2010 <- ifelse(is.na(dados_tcc$nascVivos2010_aux), 0, dados_tcc$nascVivos2010_aux)

#-------------------------------------2022--------------------------------------

# Extrai e processa os dados do Datasus:
nascVivos2022_bruto <- fetch_datasus(year_start = 2022, 
                                     year_end = 2022, 
                                     uf = "all", 
                                     information_system = "SINASC")

nascVivos2022 <- process_sinasc(nascVivos2022_bruto)

rm("nascVivos2022_bruto")

# Nascidos vivos por município:
nascVivos2022_munic <- nascVivos2022 |>
  group_by(CODMUNRES) |>
  summarise(nascVivos2022_aux = n())

# Acrescenta nascidos vivos por municípios em 2022 na base do tcc:
dados_tcc <- dados_tcc |>
  left_join(nascVivos2022_munic,
            by = c("cod_munic6D" = "CODMUNRES"))

# Substitui valores NA por 0:
dados_tcc$nascVivos2022 <- ifelse(is.na(dados_tcc$nascVivos2022_aux), 0, dados_tcc$nascVivos2022_aux)

#-------------------------------------------------------------------------------

# Exclui as colunas auxiliares:
dados_tcc <- dados_tcc[,setdiff(names(dados_tcc), 
                                c("nascVivos2010_aux", "nascVivos2022_aux"))]

# Exclui as tabelas auxiliares:
rm(list = c("nascVivos2010", "nascVivos2010_munic", "nascVivos2022", "nascVivos2022_munic"))

################################################################################
#                         TAXA DE MORTALIDADE MATERNA                          #
################################################################################

#-------------------------------------2010--------------------------------------

# Calcula o indicador:
dados_tcc$mortMat2010_aux <- (dados_tcc$obtMat9a11/dados_tcc$nascVivos2010)*100000

# Imputa valores para os municípios que ainda não existiam em 2010:
dados_tcc <- dados_tcc |>
  mutate(mortMat2010_aux = case_when(
    cod_munic7D == "1504752" ~ 29.94909,  # Mojuí dos Campos (PA): dados de Santarém
    cod_munic7D == "4220000" ~ 240.9639,  # Balneário Rincão (SC): dados de Içara
    cod_munic7D == "4212650" ~ 0,         # Pescaria Brava (SC): dados de Laguna
    cod_munic7D == "4314548" ~ 0,         # Pinto Bandeira (RS): dados de Bento Gonçalves
    cod_munic7D == "5006275" ~ 0,         # Paraíso das Águas (MS): dados de Águas Claras
    TRUE ~ mortMat2010_aux))

# Ajusta os dados para a metodologia do PNUD:
dados_tcc$mortMat2010 <- ifelse(dados_tcc$mortMat2010_aux < 10, 10, dados_tcc$mortMat2010_aux)
dados_tcc$mortMat2010 <- ifelse(dados_tcc$mortMat2010_aux > 1000, 1000, dados_tcc$mortMat2010)

#-------------------------------------2022--------------------------------------

# Calcula o indicador:
dados_tcc$mortMat2022_aux <- (dados_tcc$obtMat21a23/dados_tcc$nascVivos2022)*100000

# Ajusta os dados para a metodologia do PNUD:
dados_tcc$mortMat2022 <- ifelse(dados_tcc$mortMat2022_aux < 10, 10, dados_tcc$mortMat2022_aux)
dados_tcc$mortMat2022 <- ifelse(dados_tcc$mortMat2022_aux > 1000, 1000, dados_tcc$mortMat2022)

#-------------------------------------------------------------------------------

# Exclui as colunas auxiliares:
dados_tcc <- dados_tcc[,setdiff(names(dados_tcc), 
                                c("mortMat2010_aux", "mortMat2022_aux"))]

################################################################################
#                             MULHERES ADOLESCENTES                            #
################################################################################

#-------------------------------------2010--------------------------------------

# Extrai os dados do SIDRA:
demogIBGE2010 <- get_sidra(api="/t/1378/n6/all/v/allxp/p/all/c1/0/c2/5/c287/0,107453,111286/c455/0") 

# Mulheres entre 15 e 19 anos por município:
mulheres15a19_2010 <- demogIBGE2010 |>
  filter(Idade == "15 a 17 anos" | Idade == "18 ou 19 anos") |>
  group_by(`Município (Código)`) |>
  summarise(qtdeAdolesc2010 = sum(Valor))

# Acrescenta mulheres entre 15 e 19 anos por município em 2010 na base do tcc:
dados_tcc <- dados_tcc |>
  left_join(mulheres15a19_2010,
            by = c("cod_munic7D" = "Município (Código)"))

#-------------------------------------2022--------------------------------------

# Extrai os dados do SIDRA:
demogIBGE2022 <- get_sidra(api="/t/9514/n6/all/v/allxp/p/all/c2/5/c287/93086,100362/c286/113635")

# Mulheres entre 15 e 19 anos por município:
mulheres15a19_2022 <- demogIBGE2022 |>
  filter(Idade == "15 a 19 anos") |>
  group_by(`Município (Código)`) |>
  summarise(qtdeAdolesc2022 = sum(Valor))

# Acrescenta mulheres entre 15 e 19 anos por município em 2022 na base do tcc:
dados_tcc <- dados_tcc |>
  left_join(mulheres15a19_2022,
            by = c("cod_munic7D" = "Município (Código)"))

#-------------------------------------------------------------------------------

# Exclui as tabelas auxiliares:
rm(list = c("demogIBGE2010", "mulheres15a19_2010", "demogIBGE2022", "mulheres15a19_2022"))

################################################################################
#                     NASCIDOS VIVOS DE MÃES ADOLESCENTES                      #
################################################################################

#-------------------------------------2010--------------------------------------

# Nascidos vivos de mães adolescentes por município:
nascVivosMaeAdol2010 <- nascVivos2010 |>
  filter(IDADEMAE == "15" |
         IDADEMAE == "16" |
         IDADEMAE == "17" |
         IDADEMAE == "18" |
         IDADEMAE == "19") |>
  group_by(CODMUNRES) |>
  summarise(nascVivosMaeAdol2010_aux = n())

# Acrescenta nascidos vivos de mães adolescentes por município em 2010 na base do tcc:
dados_tcc <- dados_tcc |>
  left_join(nascVivosMaeAdol2010,
            by = c("cod_munic6D" = "CODMUNRES"))

# Substitui valores NA por 0:
dados_tcc$nascVivosMaeAdol2010 <- ifelse(is.na(dados_tcc$nascVivosMaeAdol2010_aux), 0, dados_tcc$nascVivosMaeAdol2010_aux)

#-------------------------------------2022--------------------------------------

# Nascidos vivos de mães adolescentes por município:
nascVivosMaeAdol2022 <- nascVivos2022 |>
  filter(IDADEMAE == "15" |
         IDADEMAE == "16" |
         IDADEMAE == "17" |
         IDADEMAE == "18" |
         IDADEMAE == "19") |>
  group_by(CODMUNRES) |>
  summarise(nascVivosMaeAdol2022_aux = n())

# Acrescenta nascidos vivos de mães adolescentes por município em 2010 na base do tcc:
dados_tcc <- dados_tcc |>
  left_join(nascVivosMaeAdol2022,
            by = c("cod_munic6D" = "CODMUNRES"))

# Substitui valores NA por 0:
dados_tcc$nascVivosMaeAdol2022 <- ifelse(is.na(dados_tcc$nascVivosMaeAdol2022_aux), 0, dados_tcc$nascVivosMaeAdol2022_aux)

#-------------------------------------------------------------------------------

# Exclui as colunas auxiliares:
dados_tcc <- dados_tcc[,setdiff(names(dados_tcc), 
                                c("nascVivosMaeAdol2010_aux", "nascVivosMaeAdol2022_aux"))]

# Exclui as tabelas auxiliares:
rm(list = c("demogIBGE2010", "mulheres15a19_2010", "demogIBGE2022", "mulheres15a19_2022"))

################################################################################
#                      TAXA DE NATALIDADE NA ADOLESCÊNCIA                      #
################################################################################

#-------------------------------------2010--------------------------------------

# Calcula o indicador:
dados_tcc$natAdolesc2010_aux <- (dados_tcc$nascVivosMaeAdol2010/dados_tcc$qtdeAdolesc2010)*1000

# Imputa valores para os municípios que ainda não existiam em 2010:
dados_tcc <- dados_tcc |>
  mutate(natAdolesc2010_aux = case_when(
    cod_munic7D == "1504752" ~ 86.36392,  # Mojuí dos Campos (PA): dados de Santarém
    cod_munic7D == "4220000" ~ 58.68989,  # Balneário Rincão (SC): dados de Içara
    cod_munic7D == "4212650" ~ 54.8893,   # Pescaria Brava (SC): dados de Laguna
    cod_munic7D == "4314548" ~ 33.7218,   # Pinto Bandeira (RS): dados de Bento Gonçalves
    cod_munic7D == "5006275" ~ 98.38473,  # Paraíso das Águas (MS): dados de Águas Claras
    TRUE ~ natAdolesc2010_aux))

# Ajusta os dados para a metodologia do PNUD (ajusta para o valor mínimo):
dados_tcc$natAdolesc2010 <- ifelse(dados_tcc$natAdolesc2010_aux == 0, 1, dados_tcc$natAdolesc2010_aux)

#-------------------------------------2022--------------------------------------

# Calcula o indicador:
dados_tcc$natAdolesc2022_aux <- (dados_tcc$nascVivosMaeAdol2022/dados_tcc$qtdeAdolesc2022)*1000

# Ajusta os dados para a metodologia do PNUD (ajusta para o valor mínimo):
dados_tcc$natAdolesc2022 <- ifelse(dados_tcc$natAdolesc2022_aux == 0, 1, dados_tcc$natAdolesc2022_aux)

#-------------------------------------------------------------------------------

# Exclui as colunas auxiliares:
dados_tcc <- dados_tcc[,setdiff(names(dados_tcc), 
                                c("natAdolesc2010_aux", "natAdolesc2022_aux"))]

# Exclui as tabelas auxiliares:
rm(list = c("nascVivosMaeAdol2010", "nascVivosMaeAdol2022"))

################################################################################
#                          PARTICIPAÇÃO NO PARLAMENTO                          #
################################################################################

#-------------------------------------2008--------------------------------------

# Dados eleitorais do TSE:
eleicoes2008 <- read_excel("./data_read/eleicoes2008.xlsx")
candidatos2008 <- read_excel("./data_read/candidatos2008.xlsx")

# Dados eleitorais do TSE (outra fonte, com apenas eleitos):
resultado2008 <- read_excel("./data_read/resultado2008.xlsx")

# Gênero dos candidatos eleitos:
parlamento2008 <- resultado2008 |>
  select(sg_uf, nm_municipio, sq_candidato, nm_candidato) |>
  left_join(candidatos2008 |> select(SQ_CANDIDATO, NM_CANDIDATO, DS_GENERO),
            by = c("sq_candidato" = "SQ_CANDIDATO",
              "nm_candidato" = "NM_CANDIDATO"))

# Confere se há candidatos eleitos que ficaram sem gênero:
semGenero2008 <- parlamento2008[which(is.na(parlamento2008$DS_GENERO), arr.ind = TRUE), 3]

# Imputa o gênero para os candidatos que faltam:
parlamento2008 <- parlamento2008 |>
  mutate(DS_GENERO = case_when(
          nm_candidato == "ARMANDO D'AINES DE FREITAS" ~ "MASCULINO",
          nm_candidato == "ROMARIO TAVARES D'AVILA" ~ "MASCULINO",
          nm_candidato == "MAÉCIO FERREIRA DE ANDRADE'" ~ "MASCULINO",
          nm_candidato == "PEDRO D'ANGELO DE SOUZA" ~ "MASCULINO",
          nm_candidato == "ELIVAR COSTA TRINDADE'" ~ "MASCULINO",
          nm_candidato == "MARIA DA CONCEIÇÃO REIS DE ALMEIDA" ~ "FEMININO",
          nm_candidato == "ANTONIO CARLOS D'OLIVEIRA" ~ "MASCULINO",
          nm_candidato == "JOAO NERICE DE OLIVEIRA'" ~ "MASCULINO",
          nm_candidato == "KALINE MARIELE SANT' ANA MONTEIRO" ~ "FEMININO",
          nm_candidato == "MARLENE SANT'ANA VASQUES NOVAES" ~ "FEMININO",
          nm_candidato == "MANOEL INÁCIO D'ABADIA AQUINO DE SÁ FILHO" ~ "MASCULINO",
          nm_candidato == "MAURICIO CARDSO E SILVA" ~ "MASCULINO",
          nm_candidato == "D'AVID DA SILVA INÁCIO" ~ "MASCULINO",
          nm_candidato == "LENILDA MARIA DA SILVA DAMASCENO" ~ "FEMININO",
          nm_candidato == "GENILSON ANDRA KEZOMAE" ~ "MASCULINO",
          nm_candidato == "ELSA LAIRE DALL'AQUA" ~ "FEMININO",
          nm_candidato == "MARCONDES BALTAZAR DE MENDOÇA" ~ "MASCULINO",
          nm_candidato == "ALLAN D'LLON CANDEIA DE MACEDO" ~ "MASCULINO",
          nm_candidato == "ARI ERMINIO DALL'AGNOL" ~ "MASCULINO",
          nm_candidato == "EDUARDO DE SANT'ANA MARIOTTI" ~ "MASCULINO",
          nm_candidato == "WALDEMIR SANT'ANA GUIMARÃES" ~ "MASCULINO",
          nm_candidato == "AMELIA LOPES MARIA DO NASCIMENTO" ~ "FEMININO",
          nm_candidato == "DIEGO D'AVILA CHRISTOFF" ~ "MASCULINO",
          nm_candidato == "CARMEM GUNTHEIL ZACARIAS" ~ "FEMININO",
          nm_candidato == "JOÃO BATISTA BARCELLOS PINHEIRO," ~ "MASCULINO",
          nm_candidato == "JOSE ARTUR D'AVILA DIAS" ~ "MASCULINO",
          nm_candidato == "ZENAIDE GEMA D'AGOSTINI BAZZOTTI" ~ "FEMININO",
          nm_candidato == "ERICO D'AMORIM" ~ "MASCULINO",
          nm_candidato == "CARLOS ALBERTO DALL'OGLIO" ~ "MASCULINO",
          nm_candidato == "NILVO DALL'ORSOLETTA" ~ "MASCULINO",
          nm_candidato == "SANT'CLAIR ANTONIO MARINHO FILHO" ~ "MASCULINO",
          nm_candidato == "ROSIMEIRE PAN D'ARCO DE ALMEIDA SERPA" ~ "FEMININO",
          nm_candidato == "LEE JEFFERSON ROBERTO B, G, DE B, V, B, DE OLIVEIRA LEITE" ~ "MASCULINO",
          nm_candidato == "JORGE LUIZ D'ANDREA" ~ "MASCULINO",
          nm_candidato == "LUIZ ANTONIO PEREIRA DE SANT'ANA" ~ "MASCULINO",
          nm_candidato == "ELIETY LOURENÇO DE SAOUZA" ~ "FEMININO",
          nm_candidato == "MARIA HILMA O, MASCARENHAS" ~ "FEMININO",
          TRUE ~ DS_GENERO),
         estado = case_when(
           sg_uf == "AC" ~ "Acre",
           sg_uf == "AL" ~ "Alagoas",
           sg_uf == "AP" ~ "Amapá",
           sg_uf == "AM" ~ "Amazonas",
           sg_uf == "BA" ~ "Bahia",
           sg_uf == "CE" ~ "Ceará",
           sg_uf == "DF" ~ "Distrito Federal",
           sg_uf == "ES" ~ "Espírito Santo",
           sg_uf == "GO" ~ "Goiás",
           sg_uf == "MA" ~ "Maranhão",
           sg_uf == "MT" ~ "Mato Grosso",
           sg_uf == "MS" ~ "Mato Grosso do Sul",
           sg_uf == "MG" ~ "Minas Gerais",
           sg_uf == "PA" ~ "Pará",
           sg_uf == "PB" ~ "Paraíba",
           sg_uf == "PR" ~ "Paraná",
           sg_uf == "PE" ~ "Pernambuco",
           sg_uf == "PI" ~ "Piauí",
           sg_uf == "RJ" ~ "Rio de Janeiro",
           sg_uf == "RN" ~ "Rio Grande do Norte",
           sg_uf == "RS" ~ "Rio Grande do Sul",
           sg_uf == "RO" ~ "Rondônia",
           sg_uf == "RR" ~ "Roraima",
           sg_uf == "SC" ~ "Santa Catarina",
           sg_uf == "SP" ~ "São Paulo",
           sg_uf == "SE" ~ "Sergipe",
           sg_uf == "TO" ~ "Tocantins",
           TRUE ~ NA_character_))

# Inclui os códigos dos municípios
parlamento2008 <- parlamento2008 |>
  left_join(munic_2010 |> select(Nome_UF, Município, Nome_Munic),
            by = c("estado" = "Nome_UF",
                   "nm_municipio" = "Nome_Munic"))

# Confere se há municípios que ficaram sem código:
semMunic2008 <- parlamento2008[which(is.na(parlamento2008$Município), arr.ind = TRUE), c(2,6)]

# Imputa o código para os municípios que faltam:
parlamento2008 <- parlamento2008 |>
  mutate(Município = case_when(
    nm_municipio == "ÁGUA BRANCA DO AMAPARI" & estado == "Amapá" ~ "1600055", #Serra do Navio
    nm_municipio == "ARAÇÁS" & estado == "Bahia" ~ "2902054", #Araças
    nm_municipio == "CAEM" & estado == "Bahia" ~ "2905107", #Caém
    nm_municipio == "GOVERNADOR LOMANTO JÚNIOR" & estado == "Bahia" ~ "2903300", #Barro Preto
    nm_municipio == "IUIU" & estado == "Bahia" ~ "2917334", #Iuiú
    nm_municipio == "MUQUÉM DO SÃO FRANCISCO" & estado == "Bahia" ~ "2922250", #Muquém de São Francisco
    nm_municipio == "QUINJINGUE" & estado == "Bahia" ~ "2925907", #Quijingue
    nm_municipio == "SANTA TEREZINHA" & estado == "Bahia" ~ "2928505", #Santa Teresinha
    nm_municipio == "SANTO ESTEVÃO" & estado == "Bahia" ~ "2928802", #Santo Estêvão
    nm_municipio == "ATÍLIO VIVÁCQUA" & estado == "Espírito Santo" ~ "3200706", #Atilio Vivacqua
    nm_municipio == "ANHANGÜERA" & estado == "Goiás" ~ "5201207", #Anhanguera
    nm_municipio == "SÃO LUIZ DO NORTE" & estado == "Goiás" ~ "5220157", #São Luíz do Norte
    nm_municipio == "OLHOS D'ÁGUA" & estado == "Minas Gerais" ~ "3145455", #Olhos-d'Água
    nm_municipio == "PASSA VINTE" & estado == "Minas Gerais" ~ "3147808", #Passa-Vinte
    nm_municipio == "PINGO D'ÁGUA" & estado == "Minas Gerais" ~ "3150539", #Pingo-d'Água
    nm_municipio == "SEM PEIXE" & estado == "Minas Gerais" ~ "3165560", #Sem-Peixe
    nm_municipio == "POXORÉU" & estado == "Mato Grosso" ~ "5107008", #Poxoréo
    nm_municipio == "CAMPO DE SANTANA" & estado == "Paraíba" ~ "2516409", #Tacima
    nm_municipio == "QUIXABA" & estado == "Paraíba" ~ "2512606", #Quixabá
    nm_municipio == "SÃO DOMINGOS DE POMBAL" & estado == "Paraíba" ~ "2513968", #São Domingos
    nm_municipio == "SÃO VICENTE DO SERIDÓ" & estado == "Paraíba" ~ "2515401", #Seridó
    nm_municipio == "BELÉM DE SÃO FRANCISCO" & estado == "Pernambuco" ~ "2601607", #Belém do São Francisco
    nm_municipio == "ITAMARACÁ" & estado == "Pernambuco" ~ "2607604", #Ilha de Itamaracá
    nm_municipio == "LUIS CORREIA" & estado == "Piauí" ~ "2205706", #Luís Correia
    nm_municipio == "ANTONIO OLINTO" & estado == "Paraná" ~ "4101309", #Antônio Olinto
    nm_municipio == "SANTO ANTONIO DO CAIUÁ" & estado == "Paraná" ~ "4124202", #Santo Antônio do Caiuá
    nm_municipio == "PARATI" & estado == "Rio de Janeiro" ~ "3303807", #Paraty
    nm_municipio == "TRAJANO DE MORAIS" & estado == "Rio de Janeiro" ~ "3305901", #Trajano de Moraes
    nm_municipio == "ASSÚ" & estado == "Rio Grande do Norte" ~ "2400208", #Açu
    nm_municipio == "BOA SAÚDE" & estado == "Rio Grande do Norte" ~ "2405306", #Januário Cicco
    nm_municipio == "CAMPO GRANDE" & estado == "Rio Grande do Norte" ~ "2401305", #Augusto Severo
    nm_municipio == "OLHO D'ÁGUA DO BORGES" & estado == "Rio Grande do Norte" ~ "2408409",#Olho-d'Água do Borges
    nm_municipio == "SERRA CAIADA" & estado == "Rio Grande do Norte" ~ "2410306", #Presidente Juscelino
    nm_municipio == "ALTA FLORESTA DO OESTE" & estado == "Rondônia" ~ "1100015", #Alta Floresta D'Oeste
    nm_municipio == "ALVORADA DO OESTE" & estado == "Rondônia" ~ "1100346", #Alvorada D'Oeste
    nm_municipio == "ESPIGÃO DO OESTE" & estado == "Rondônia" ~ "1100098", #Espigão D'Oeste
    nm_municipio == "MACHADINHO DO OESTE" & estado == "Rondônia" ~ "1100130", #Machadinho D'Oeste
    nm_municipio == "NOVA BRASILÂNDIA DO OESTE" & estado == "Rondônia" ~ "1100148", #Nova Brasilândia D'Oeste
    nm_municipio == "SANTA LUZIA DO OESTE" & estado == "Rondônia" ~ "1100296", #Santa Luzia D'Oeste
    nm_municipio == "SÃO FELIPE DO OESTE" & estado == "Rondônia" ~ "1101484", #São Felipe D'Oeste
    nm_municipio == "MAÇAMBARA" & estado == "Rio Grande do Sul" ~ "4311718", #Maçambará
    nm_municipio == "RESTINGA SÊCA" & estado == "Rio Grande do Sul" ~ "4315503", #Restinga Seca
    nm_municipio == "SANTANA DO LIVRAMENTO" & estado == "Rio Grande do Sul" ~ "4317103", #Sant'Ana do Livramento
    nm_municipio == "VESPASIANO CORRÊA" & estado == "Rio Grande do Sul" ~ "4322855", #Vespasiano Correa
    nm_municipio == "GRÃO-PARÁ" & estado == "Santa Catarina" ~ "4206108", #Grão Pará
    nm_municipio == "LAURO MÜLLER" & estado == "Santa Catarina" ~ "4209607", #Lauro Muller
    nm_municipio == "LUÍS ALVES" & estado == "Santa Catarina" ~ "4210001", #Luiz Alves
    nm_municipio == "PRESIDENTE CASTELO BRANCO" & estado == "Santa Catarina" ~ "4213906", #Presidente Castello Branco
    nm_municipio == "SÃO CRISTÓVÃO DO SUL" & estado == "Santa Catarina" ~ "4216057", #São Cristovão do Sul
    nm_municipio == "GRACCHO CARDOSO" & estado == "Sergipe" ~ "2802601", #Gracho Cardoso
    nm_municipio == "BIRITIBA MIRIM" & estado == "São Paulo" ~ "3506607", #Biritiba-Mirim
    nm_municipio == "FLORÍNEA" & estado == "São Paulo" ~ "3516101", #Florínia
    nm_municipio == "MOGI MIRIM" & estado == "São Paulo" ~ "3530805", #Moji Mirim
    nm_municipio == "COUTO DE MAGALHÃES" & estado == "Tocantins" ~ "1706001", #Couto Magalhães
    nm_municipio == "SÃO VALÉRIO DO TOCANTINS" & estado == "Tocantins" ~ "1720499", #São Valério
    TRUE ~ Município
  ))

# Calcula o total de vereadores por município:
parlaMunic2008 <- parlamento2008 |>
  group_by(Município) |>
  mutate(total = n()) |>
  select(Município, total) |>
  unique()

# Confere quais municípios não obtiveram resultado nas eleições de 2008:
foraEleicoes2008 <- munic_2010 |>
  anti_join(parlaMunic2008, by = "Município")

# Resgata, da primeira base de dados, os municípios faltantes na segunda:
aux_2008 <- eleicoes2008 |>
  filter((NM_MUNICIPIO == "NOVO AIRÃO" & SG_UF == "AM") |
           (NM_MUNICIPIO == "CAPITÃO POÇO" & SG_UF == "PA") |
           (NM_MUNICIPIO == "CURRALINHO" & SG_UF == "PA") |
           (NM_MUNICIPIO == "ÁGUA BRANCA DO AMAPARI" & SG_UF == "AP") |
           (NM_MUNICIPIO == "CURURUPU" & SG_UF == "MA") |
           (NM_MUNICIPIO == "SÃO LUÍS DO QUITUNDE" & SG_UF == "AL") |
           (NM_MUNICIPIO == "APARECIDA" & SG_UF == "SP") |
           (NM_MUNICIPIO == "POTIM" & SG_UF == "SP") |
           (NM_MUNICIPIO == "ROSEIRA" & SG_UF == "SP") |
           (NM_MUNICIPIO == "RANCHO ALEGRE" & SG_UF == "PR") |
           (NM_MUNICIPIO == "IBIRUBÁ" & SG_UF == "RS") |
           (NM_MUNICIPIO == "CAMPINÁPOLIS" & SG_UF == "MT") |
           (NM_MUNICIPIO == "GUAPÓ" & SG_UF == "GO") |
           (NM_MUNICIPIO == "PIRES DO RIO" & SG_UF == "GO"),
         DS_CARGO == "Vereador",
         DS_SIT_TOT_TURNO == "ELEITO") |>
  select(SG_UF, NM_MUNICIPIO, SQ_CANDIDATO, NM_CANDIDATO) |>
  left_join(candidatos2008 |> select(SQ_CANDIDATO, NM_CANDIDATO, DS_GENERO),
            by = c("SQ_CANDIDATO", "NM_CANDIDATO")) |>
  mutate(DS_GENERO = case_when(
    NM_CANDIDATO == "ELIANA LUCIA BARBOZA DE SOUZA" ~ "FEMININO",
    NM_CANDIDATO == "FERNANDO FERNANDES DE LIMA" ~ "MASCULINO",
    NM_CANDIDATO == "JOSIMAR COUTINHO DE AGUIAR" ~ "MASCULINO",
    NM_CANDIDATO == "EDINEY CARDOSO DE AGUIAR" ~ "MASCULINO",
    NM_CANDIDATO == "FRANCISCO IZONILDO PIRES DE SOUZA" ~ "MASCULINO",
    NM_CANDIDATO == "PAULA REGINA PINTO DOS SANTOS" ~ "FEMININO",
    NM_CANDIDATO == "RAIMUNDO MORAES MOTA SOBRINHO" ~ "MASCULINO",
    NM_CANDIDATO == "SIMÃO TEIXEIRA MOTA" ~ "MASCULINO",
    NM_CANDIDATO == "JAIR DO SOCORRO PINHEIRO REIS" ~ "MASCULINO",
    NM_CANDIDATO == "JANIL MACEDO MARTINS" ~ "MASCULINO",
    NM_CANDIDATO == "MARCOS BARATINHA OLIVEIRA" ~ "MASCULINO",
    NM_CANDIDATO == "PAULO RONALDO GONÇALVES CARDOSO" ~ "MASCULINO",
    NM_CANDIDATO == "MANOEL NOGUEIRA DE ANDRADE" ~ "MASCULINO",
    NM_CANDIDATO == "EDNALDO SILVA RIBEIRO" ~ "MASCULINO",
    NM_CANDIDATO == "HELOY MARCOS DE MATOS AZEVEDO" ~ "MASCULINO",
    NM_CANDIDATO == "ANTONIO MARCIO DE SIQUEIRA" ~ "MASCULINO",
    NM_CANDIDATO == "ADILSON JOSE DE LIMA CASTRO" ~ "MASCULINO",
    NM_CANDIDATO == "ADVAL BENEDITO COELHO" ~ "MASCULINO",
    NM_CANDIDATO == "MARIA APARECIDA CASTRO ROCHA" ~ "FEMININO",
    NM_CANDIDATO == "ELCIO RIBEIRO PINTO" ~ "MASCULINO",
    NM_CANDIDATO == "HARLEI DINIZ DE CARVALHO" ~ "MASCULINO",
    NM_CANDIDATO == "PAULO BENEDITO DOS SANTOS" ~ "MASCULINO",
    NM_CANDIDATO == "WALDEMIR JOSE PEDROSO" ~ "MASCULINO",
    NM_CANDIDATO == "BENITO CARLOS THOMAZ" ~ "MASCULINO",
    NM_CANDIDATO == "CLAUDINEI RICARDO DA PAIXÃO" ~ "MASCULINO",
    NM_CANDIDATO == "JOÃO GUILHERME SANTOS ANGELIERI" ~ "MASCULINO",
    NM_CANDIDATO == "ANDRÉ LUIZ BERTULINO" ~ "MASCULINO",
    NM_CANDIDATO == "ROBERTO RIVELINO FELIX DE ABREU" ~ "MASCULINO",
    NM_CANDIDATO == "EMERSON KIOGI TANAKA" ~ "MASCULINO",
    NM_CANDIDATO == "MARCOS DE OLIVEIRA GALVAO" ~ "MASCULINO",
    NM_CANDIDATO == "FRACISCO DE ASSIS MOURA VIEIRA" ~ "MASCULINO",
    NM_CANDIDATO == "JOSE AUGUSTO COELHO PEREIRA" ~ "MASCULINO",
    NM_CANDIDATO == "JOEL POLYDORO" ~ "MASCULINO",
    NM_CANDIDATO == "NELSON TIXEIRA MADRUGA" ~ "MASCULINO",
    NM_CANDIDATO == "JOSE ROBERTO DA PALMA" ~ "MASCULINO",
    NM_CANDIDATO == "JOAO VILACA GUIMARAES" ~ "MASCULINO",
    NM_CANDIDATO == "EVANDRO VALADARES DE SOUZA" ~ "MASCULINO",
    NM_CANDIDATO == "MARIA DA PENHA FORTUNA" ~ "FEMININO",
    NM_CANDIDATO == "ZULENE DOS SANTOS SILVA DE OLIVEIRA" ~ "FEMININO",
    NM_CANDIDATO == "RAIMUNDO NONATO DIAS MOTA DA SILVA" ~ "MASCULINO")) |>
  mutate(estado = case_when(
    SG_UF == "AC" ~ "Acre",
    SG_UF == "AL" ~ "Alagoas",
    SG_UF == "AP" ~ "Amapá",
    SG_UF == "AM" ~ "Amazonas",
    SG_UF == "BA" ~ "Bahia",
    SG_UF == "CE" ~ "Ceará",
    SG_UF == "DF" ~ "Distrito Federal",
    SG_UF == "ES" ~ "Espírito Santo",
    SG_UF == "GO" ~ "Goiás",
    SG_UF == "MA" ~ "Maranhão",
    SG_UF == "MT" ~ "Mato Grosso",
    SG_UF == "MS" ~ "Mato Grosso do Sul",
    SG_UF == "MG" ~ "Minas Gerais",
    SG_UF == "PA" ~ "Pará",
    SG_UF == "PB" ~ "Paraíba",
    SG_UF == "PR" ~ "Paraná",
    SG_UF == "PE" ~ "Pernambuco",
    SG_UF == "PI" ~ "Piauí",
    SG_UF == "RJ" ~ "Rio de Janeiro",
    SG_UF == "RN" ~ "Rio Grande do Norte",
    SG_UF == "RS" ~ "Rio Grande do Sul",
    SG_UF == "RO" ~ "Rondônia",
    SG_UF == "RR" ~ "Roraima",
    SG_UF == "SC" ~ "Santa Catarina",
    SG_UF == "SP" ~ "São Paulo",
    SG_UF == "SE" ~ "Sergipe",
    SG_UF == "TO" ~ "Tocantins",
    TRUE ~ NA_character_))
    
# Inclui novamente os códigos dos municípios
aux_2008 <- aux_2008 |>
  mutate(NM_MUNICIPIO = case_when(
    NM_MUNICIPIO == "ÁGUA BRANCA DO AMAPARI" ~ "PEDRA BRANCA DO AMAPARI",
    TRUE ~ NM_MUNICIPIO)) |>
  left_join(munic_2010 |> select(Nome_UF, Município, Nome_Munic),
            by = c("estado" = "Nome_UF",
                   "NM_MUNICIPIO" = "Nome_Munic"))

# Empilhando linhas
names(aux_2008) <- names(parlamento2008)
parlamento2008 <- bind_rows(parlamento2008, aux_2008)

# Confere novamente se há municípios que ficaram sem código:
semMunic2008 <- parlamento2008[which(is.na(parlamento2008$Município), arr.ind = TRUE), c(2,6)]

# Calcula novamente o total de vereadores por município:
parlaMunic2008 <- parlamento2008 |>
  group_by(Município) |>
  mutate(total = n()) |>
  select(Município, total) |>
  unique()

names(parlaMunic2008)[1] <- "cod_munic"

# Calcula o total de homens eleitos por município:
parlaMunic2008_2 <- parlamento2008 |>
  filter(DS_GENERO == "MASCULINO") |>
  group_by(Município) |>
  mutate(homem = n()) |>
  select(Município, homem) |>
  unique()

# Junta o total de homens ao total de vereadores:
parlaMunic2008 <- parlaMunic2008 |>
  left_join(parlaMunic2008_2, by = c("cod_munic" = "Município"))

# Substitui valores NA por 0:
parlaMunic2008$homem <- ifelse(is.na(parlaMunic2008$homem), 0, parlaMunic2008$homem)

# Calcula o total de mulheres eleitas por município:
parlaMunic2008_3 <- parlamento2008 |>
  filter(DS_GENERO == "FEMININO") |>
  group_by(Município) |>
  mutate(mulher = n()) |>
  select(Município, mulher) |>
  unique()

# Junta o total de mulheres ao total de vereadores:
parlaMunic2008 <- parlaMunic2008 |>
  left_join(parlaMunic2008_3, by = c("cod_munic" = "Município"))

# Substitui valores NA por 0:
parlaMunic2008$mulher <- ifelse(is.na(parlaMunic2008$mulher), 0, parlaMunic2008$mulher)

# Confere novamente quais municípios não obtiveram resultado nas eleições de 2008:
foraEleicoes2008 <- munic_2010 |>
  anti_join(parlaMunic2008, by = c("Município" = "cod_munic"))

# Refaz os passos anteriores para os municípios faltantes e para os municípios 
# com menos de 9 vereadores eleitos utilizando os dados das eleições de 2012:
resultado2012 <- read_excel("./data_read/resultado2012.xlsx")
candidatos2012 <- read_excel("./data_read/candidatos2012.xlsx")

refazer_2008 <- data.frame(cod_munic = foraEleicoes2008$cod_munic)
refazer_2008 <- bind_rows(refazer_2008, parlaMunic2008 |> filter(total<9) |> select(cod_munic))
names(novos_2022)[12] <- "cod_munic"
refazer_2008 <- bind_rows(refazer_2008, novos_2022 |> select(cod_munic))
refazer_2008 <- refazer_2008 |>
  left_join(munic_2022 |> select(Nome_UF, `Código Município Completo`, Nome_Município),
            by = c("cod_munic" = "Código Município Completo"))
refazer_2008 <- refazer_2008 |>
  filter(!(Nome_Município %in% c("FERNANDO DE NORONHA", "BRASÍLIA")))

parlamento2012 <- resultado2012 |>
  semi_join(refazer_2008, by = c("sg_uf" = "SG_UF",
                                 "nm_municipio" = "Nome_Município")) |>
  select(sg_uf, nm_municipio, sq_candidato, nm_candidato) |>
  left_join(candidatos2012 |> select(SQ_CANDIDATO, NM_CANDIDATO, DS_GENERO),
            by = c("sq_candidato" = "SQ_CANDIDATO",
                   "nm_candidato" = "NM_CANDIDATO")) |>
  mutate(estado = case_when(
    sg_uf == "AC" ~ "Acre",
    sg_uf == "AL" ~ "Alagoas",
    sg_uf == "AP" ~ "Amapá",
    sg_uf == "AM" ~ "Amazonas",
    sg_uf == "BA" ~ "Bahia",
    sg_uf == "CE" ~ "Ceará",
    sg_uf == "DF" ~ "Distrito Federal",
    sg_uf == "ES" ~ "Espírito Santo",
    sg_uf == "GO" ~ "Goiás",
    sg_uf == "MA" ~ "Maranhão",
    sg_uf == "MT" ~ "Mato Grosso",
    sg_uf == "MS" ~ "Mato Grosso do Sul",
    sg_uf == "MG" ~ "Minas Gerais",
    sg_uf == "PA" ~ "Pará",
    sg_uf == "PB" ~ "Paraíba",
    sg_uf == "PR" ~ "Paraná",
    sg_uf == "PE" ~ "Pernambuco",
    sg_uf == "PI" ~ "Piauí",
    sg_uf == "RJ" ~ "Rio de Janeiro",
    sg_uf == "RN" ~ "Rio Grande do Norte",
    sg_uf == "RS" ~ "Rio Grande do Sul",
    sg_uf == "RO" ~ "Rondônia",
    sg_uf == "RR" ~ "Roraima",
    sg_uf == "SC" ~ "Santa Catarina",
    sg_uf == "SP" ~ "São Paulo",
    sg_uf == "SE" ~ "Sergipe",
    sg_uf == "TO" ~ "Tocantins",
    TRUE ~ NA_character_))

semGenero2012 <- parlamento2012[which(is.na(parlamento2012$DS_GENERO), arr.ind = TRUE), 4]

parlamento2012 <- parlamento2012 |>
  left_join(munic_2022 |> select(Nome_UF, `Código Município Completo`, Nome_Município),
            by = c("estado" = "Nome_UF",
                   "nm_municipio" = "Nome_Município"))

semMunic2012 <- parlamento2012[which(is.na(parlamento2012$`Código Município Completo`), arr.ind = TRUE), c(2,6)]

names(parlamento2012)[7] <- "cod_munic"

parlaMunic2012 <- parlamento2012 |>
  group_by(cod_munic) |>
  mutate(total = n()) |>
  select(cod_munic, total) |>
  unique()

parlaMunic2012_2 <- parlamento2012 |>
  filter(DS_GENERO == "MASCULINO") |>
  group_by(cod_munic) |>
  mutate(homem = n()) |>
  select(cod_munic, homem) |>
  unique()

parlaMunic2012 <- parlaMunic2012 |>
  left_join(parlaMunic2012_2, by = "cod_munic")

parlaMunic2012$homem <- ifelse(is.na(parlaMunic2012$homem), 0, parlaMunic2012$homem)

parlaMunic2012_3 <- parlamento2012 |>
  filter(DS_GENERO == "FEMININO") |>
  group_by(cod_munic) |>
  mutate(mulher = n()) |>
  select(cod_munic, mulher) |>
  unique()

parlaMunic2012 <- parlaMunic2012 |>
  left_join(parlaMunic2012_3, by = "cod_munic")

parlaMunic2012$mulher <- ifelse(is.na(parlaMunic2012$mulher), 0, parlaMunic2012$mulher)

parlaMunic2008 <- parlaMunic2008 |>
  filter(!(cod_munic %in% parlaMunic2012$cod_munic)) |>
  rbind(parlaMunic2012)

# Imputa dados para Brasília e Fernando de Noronha com base nas eleições estaduais
# de 2010 (https://sig.tse.jus.br/ords/dwapr/r/seai/sig-eleicao/home e 
# https://www.tre-pe.jus.br/eleicoes/eleicoes-anteriores/eleicoes-2010-1/resultado-1o-turno-em-pernambuco)
parlaMunic2008 <- parlaMunic2008 |>
  bind_rows(data.frame(cod_munic = "5300108", #Brasília
                     total = 24,
                     homem = 20,
                     mulher = 4)) |>
  bind_rows(data.frame(cod_munic = "2605459", #Fernando de Noronha
                       total = 49,
                       homem = 45,
                       mulher = 4))

# Calcula as proporções:
parlaMunic2008 <- parlaMunic2008 |>
  mutate(p_homem2010 = homem/total,
         p_mulher2010 = mulher/total)

# Ajusta os dados para a metodologia do PNUD (ajusta para o valor mínimo):
parlaMunic2008$p_homem2010 <- ifelse(parlaMunic2008$p_homem2010 == 0, 0.001, parlaMunic2008$p_homem2010)
parlaMunic2008$p_mulher2010 <- ifelse(parlaMunic2008$p_mulher2010 == 0, 0.001, parlaMunic2008$p_mulher2010)

# Acrescenta participação no parlamento em 2010 na base do tcc:
dados_tcc <- dados_tcc |>
  left_join(parlaMunic2008 |> select(cod_munic, p_homem2010, p_mulher2010),
            by = c("cod_munic7D" = "cod_munic"))

#-------------------------------------2020--------------------------------------

# Dados eleitorais do TSE:
eleicoes2020 <- read_excel("./data_read/eleicoes2020.xlsx")
candidatos2020 <- read_excel("./data_read/candidatos2020.xlsx")

# Dados eleitorais do TSE (outra fonte, com apenas eleitos):
resultado2020 <- read_excel("./data_read/resultado2020.xlsx")

# Gênero dos candidatos eleitos:
parlamento2020 <- resultado2020 |>
  select(sg_uf, nm_municipio, sq_candidato, nm_candidato) |>
  left_join(candidatos2020 |> 
              filter(DS_CARGO == "VEREADOR") |> select(SQ_CANDIDATO, NM_CANDIDATO, DS_GENERO),
            by = c("sq_candidato" = "SQ_CANDIDATO",
                   "nm_candidato" = "NM_CANDIDATO"))

# Confere se há candidatos eleitos que ficaram sem gênero:
semGenero2020 <- parlamento2020[which(is.na(parlamento2020$DS_GENERO), arr.ind = TRUE), 4]

# Imputa o gênero para os candidatos que faltam:
parlamento2020 <- parlamento2020 |>
  mutate(DS_GENERO = case_when(
    nm_candidato == "VALMIR D'ELREI DA SILVA" ~ "MASCULINO",
    nm_candidato == "PEDRO D'ANGELO DE SOUZA" ~ "MASCULINO",
    nm_candidato == "EDSON PIRES DA SILVA" ~ "MASCULINO",
    nm_candidato == "ANTONIO CARLOS D'OLIVEIRA" ~ "MASCULINO",
    nm_candidato == "CLEUSIVAM PAULO DE ARAUJO" ~ "MASCULINO",
    nm_candidato == "LUCIANO DINIZ SANT'ANNA" ~ "MASCULINO",
    nm_candidato == "WENDEL SANT'ANA LIMA" ~ "MASCULINO",
    nm_candidato == "D'ORLEANS TRINDADE VAZ" ~ "MASCULINO",
    nm_candidato == "GUILHERME DALL'AGNOL" ~ "MASCULINO",
    nm_candidato == "SESE KA'APOR" ~ "MASCULINO",
    nm_candidato == "D'AVID DA SILVA INACIO" ~ "MASCULINO",
    nm_candidato == "JOSÉ D'AVILA BITENCOURT FILHO" ~ "MASCULINO",
    nm_candidato == "THIAGO INÁCIO D'PAULA FURTADO" ~ "MASCULINO",
    nm_candidato == "ANDREA FERNADES FIM MORAIS" ~ "FEMININO",
    nm_candidato == "GININHO TSEREDZAPRIWÊ TSIBO''OOPRÉ''" ~ "MASCULINO",
    nm_candidato == "JOSE MARIA DE SOUSA MOREIRA" ~ "MASCULINO",
    nm_candidato == "EDSON SANTOS FURTADO DA SILVA" ~ "MASCULINO",
    nm_candidato == "DORICO BUSS JUNIOR" ~ "MASCULINO",
    nm_candidato == "ROGERIO BUSS" ~ "MASCULINO",
    nm_candidato == "ANTÕNIO NANÔ DE FREITAS" ~ "MASCULINO",
    nm_candidato == "JOSE NELSON ZORTEA" ~ "MASCULINO",
    nm_candidato == "FABRICIO FERREIRA MEIRELES" ~ "MASCULINO",
    nm_candidato == "EDSON DE DEUS VEIRA" ~ "MASCULINO",
    nm_candidato == "ANTONIO MARCIO FARIAS GONÇALVES" ~ "MASCULINO",
    nm_candidato == "ANTONIO FERREIRA DE ARAUJO" ~ "MASCULINO",
    nm_candidato == "ALECIO STRINGARI" ~ "MASCULINO",
    nm_candidato == "CARLOS PAREIRA ALVES" ~ "MASCULINO",
    nm_candidato == "MARIO JOSE ALBINO QUEIROZ" ~ "MASCULINO",
    nm_candidato == "MARLOS ALAND'LON GOMES D'AVILA" ~ "MASCULINO",
    nm_candidato == "JAIFRAN RODRIGUES XIMENDES" ~ "MASCULINO",
    nm_candidato == "FLAVIO MAGALAHES" ~ "MASCULINO",
    nm_candidato == "GILVAN LIMA DA SILVA" ~ "MASCULINO",
    nm_candidato == "FRANCISCO WAGNER SAMPAIO SOARES" ~ "MASCULINO",
    nm_candidato == "ANTÔNIO MARCOS DA ROCHA" ~ "MASCULINO",
    nm_candidato == "LUIZ ALBERTO DIB CANÔNICO" ~ "MASCULINO",
    nm_candidato == "FLAVIA ADRIANE SANT'ANA CABRAL" ~ "FEMININO",
    nm_candidato == "MATHEUS HENRIQUE NEVES" ~ "MASCULINO",
    nm_candidato == "LUCIANE MUNHOS D'ALECIO" ~ "FEMININO",
    nm_candidato == "CHARLES LINDBERG NEVES" ~ "MASCULINO",
    nm_candidato == "DELZA OLIVEIRA SANT'ANNA DE ALMEIDA" ~ "FEMININO",
    nm_candidato == "EDIO ANTONIO ROGRIGUES NETO" ~ "MASCULINO",
    nm_candidato == "IMBERÊ MOREIRA ALVES'" ~ "MASCULINO",
    nm_candidato == "EDUARDO DE SANT'ANA MARIOTTI" ~ "MASCULINO",
    nm_candidato == "RAQUEL DE CARVALHO OLIVEIRA SANT'ANA" ~ "FEMININO",
    nm_candidato == "JORGE BEZERRA MORAES" ~ "MASCULINO",
    nm_candidato == "FRANCISCO HELIO FEITOSA ROGDRIGUES" ~ "MASCULINO",
    nm_candidato == "CÍNTIA JARDIM D'AVILA DANI" ~ "FEMININO",
    nm_candidato == "FERDINEI DALL' AGNOL" ~ "MASCULINO",
    nm_candidato == "ELISABETE DALL'ACQUA ALBAN" ~ "FEMININO",
    nm_candidato == "CARLOS HENRIQUE BASTOS D'AVILA" ~ "MASCULINO",
    nm_candidato == "ALDORI DALL' AGNOL" ~ "MASCULINO",
    nm_candidato == "MOACIR ELVIS D'AGOSTINI" ~ "MASCULINO",
    nm_candidato == "JEAN KELLY DALL'AGNOL" ~ "MASCULINO",
    nm_candidato == "ERICO D'AMORIM" ~ "MASCULINO",
    nm_candidato == "ZAURI D'AVILA DA FONSECA" ~ "MASCULINO",
    nm_candidato == "CLEONICE BEATRIZ ZART DALL'AGNOL" ~ "",
    nm_candidato == "LUCAS DOS SANTOS RODRIGUES" ~ "MASCULINO",
    nm_candidato == "SILVIA DANIELA DOMINGOS D'AVILA ALVES" ~ "FEMININO",
    nm_candidato == "WEMERSON LUIS SANT'ANA" ~ "MASCULINO",
    nm_candidato == "DEBORA RODRIGUES DE SANT'ANNA LIMA" ~ "FEMININO",
    nm_candidato == "MARCELO DE COSTA OLIVEIRA" ~ "MASCULINO",
    nm_candidato == "EDIVALNILDO DE SOUZA MOREIRA" ~ "MASCULINO",
    nm_candidato == "LUIZ ARAUJO DE JESUS'" ~ "MASCULINO",
    TRUE ~ DS_GENERO),
    
    estado = case_when(
      sg_uf == "AC" ~ "Acre",
      sg_uf == "AL" ~ "Alagoas",
      sg_uf == "AP" ~ "Amapá",
      sg_uf == "AM" ~ "Amazonas",
      sg_uf == "BA" ~ "Bahia",
      sg_uf == "CE" ~ "Ceará",
      sg_uf == "DF" ~ "Distrito Federal",
      sg_uf == "ES" ~ "Espírito Santo",
      sg_uf == "GO" ~ "Goiás",
      sg_uf == "MA" ~ "Maranhão",
      sg_uf == "MT" ~ "Mato Grosso",
      sg_uf == "MS" ~ "Mato Grosso do Sul",
      sg_uf == "MG" ~ "Minas Gerais",
      sg_uf == "PA" ~ "Pará",
      sg_uf == "PB" ~ "Paraíba",
      sg_uf == "PR" ~ "Paraná",
      sg_uf == "PE" ~ "Pernambuco",
      sg_uf == "PI" ~ "Piauí",
      sg_uf == "RJ" ~ "Rio de Janeiro",
      sg_uf == "RN" ~ "Rio Grande do Norte",
      sg_uf == "RS" ~ "Rio Grande do Sul",
      sg_uf == "RO" ~ "Rondônia",
      sg_uf == "RR" ~ "Roraima",
      sg_uf == "SC" ~ "Santa Catarina",
      sg_uf == "SP" ~ "São Paulo",
      sg_uf == "SE" ~ "Sergipe",
      sg_uf == "TO" ~ "Tocantins",
      TRUE ~ NA_character_))

# Inclui os códigos dos municípios
parlamento2020 <- parlamento2020 |>
  left_join(munic_2022 |> select(Nome_UF, `Código Município Completo`, Nome_Município),
            by = c("estado" = "Nome_UF",
                   "nm_municipio" = "Nome_Município"))

# Confere se há municípios que ficaram sem código:
semMunic2020 <- unique(parlamento2020[which(is.na(parlamento2020$`Código Município Completo`), arr.ind = TRUE), c(2,6)])

# Imputa o código para os municípios que faltam:
parlamento2020 <- parlamento2020 |>
  mutate(`Código Município Completo` = case_when(
    nm_municipio == "CAEM" & estado == "Bahia" ~ "2905107", #Caém
    nm_municipio == "CAMACÃ" & estado == "Bahia" ~ "2905602", #Camacan
    nm_municipio == "SANTO ESTEVÃO" & estado == "Bahia" ~ "2928802", #Santo Estêvão
    nm_municipio == "ANHANGÜERA" & estado == "Goiás" ~ "5201207", #Anhanguera
    nm_municipio == "DONA EUSÉBIA" & estado == "Minas Gerais" ~ "3122900", #Dona Euzébia
    nm_municipio == "OLHOS D'ÁGUA" & estado == "Minas Gerais" ~ "3145455", #Olhos-d'Água
    nm_municipio == "PINGO D'ÁGUA" & estado == "Minas Gerais" ~ "3150539", #Pingo-d'Água
    nm_municipio == "SEM PEIXE" & estado == "Minas Gerais" ~ "3165560", #Sem-Peixe3
    nm_municipio == "SÃO THOMÉ DAS LETRAS" & estado == "Minas Gerais" ~ "3165206", #São Tomé das Letras
    nm_municipio == "SANTO ANTÔNIO DO LEVERGER" & estado == "Mato Grosso" ~ "5107800", #Santo Antônio de Leverger
    nm_municipio == "ELDORADO DOS CARAJÁS" & estado == "Pará" ~ "1502954", #Eldorado do Carajás
    nm_municipio == "SANTA ISABEL DO PARÁ" & estado == "Pará" ~ "1506500", #Santa Izabel do Pará
    nm_municipio == "LUIS CORREIA" & estado == "Piauí" ~ "2205706", #Luís Correia
    nm_municipio == "ANTONIO OLINTO" & estado == "Paraná" ~ "4101309", #Antônio Olinto
    nm_municipio == "SANTO ANTONIO DO CAIUÁ" & estado == "Paraná" ~ "4124202", #Santo Antônio do Caiuá
    nm_municipio == "AREZ" & estado == "Rio Grande do Norte" ~ "2401206", #Arês
    nm_municipio == "ASSÚ" & estado == "Rio Grande do Norte" ~ "2400208", #Açu
    nm_municipio == "BOA SAÚDE" & estado == "Rio Grande do Norte" ~ "2405306", #Januário Cicco
    nm_municipio == "ALVORADA DO OESTE" & estado == "Rondônia" ~ "1100346", #Alvorada D'Oeste
    nm_municipio == "ESPIGÃO DO OESTE" & estado == "Rondônia" ~ "1100098", #Espigão D'Oeste
    nm_municipio == "WESTFALIA" & estado == "Rio Grande do Sul" ~ "4323770", #Westfália
    nm_municipio == "AMPARO DE SÃO FRANCISCO" & estado == "Sergipe" ~ "2800100", #Amparo do São Francisco
    nm_municipio == "GRACCHO CARDOSO" & estado == "Sergipe" ~ "2802601", #Gracho Cardoso
    nm_municipio == "SÃO LUÍS DO PARAITINGA" & estado == "São Paulo" ~ "3550001", #São Luiz do Paraitinga
    TRUE ~ `Código Município Completo`
  ))

# Calcula o total de vereadores por município:
parlaMunic2020 <- parlamento2020 |>
  group_by(`Código Município Completo`) |>
  mutate(total = n()) |>
  select(`Código Município Completo`, total) |>
  unique()

# Confere quais municípios não obtiveram resultado nas eleições de 2020:
foraEleicoes2020 <- munic_2022 |>
  anti_join(parlaMunic2020, by = "Código Município Completo")

# Calcula o total de homens eleitos por município:
parlaMunic2020_2 <- parlamento2020 |>
  filter(DS_GENERO == "MASCULINO") |>
  group_by(`Código Município Completo`) |>
  mutate(homem = n()) |>
  select(`Código Município Completo`, homem) |>
  unique()

# Junta o total de homens ao total de vereadores:
parlaMunic2020 <- parlaMunic2020 |>
  left_join(parlaMunic2020_2, by = "Código Município Completo")

# Substitui valores NA por 0:
parlaMunic2020$homem <- ifelse(is.na(parlaMunic2020$homem), 0, parlaMunic2020$homem)

# Calcula o total de mulheres eleitas por município:
parlaMunic2020_3 <- parlamento2020 |>
  filter(DS_GENERO == "FEMININO") |>
  group_by(`Código Município Completo`) |>
  mutate(mulher = n()) |>
  select(`Código Município Completo`, mulher) |>
  unique()

# Junta o total de mulheres ao total de vereadores:
parlaMunic2020 <- parlaMunic2020 |>
  left_join(parlaMunic2020_3, by = "Código Município Completo")

# Substitui valores NA por 0:
parlaMunic2020$mulher <- ifelse(is.na(parlaMunic2020$mulher), 0, parlaMunic2020$mulher)

# Imputa valores para o municípío faltante (Itiruçu) com base neste site: 
# https://noticias.uol.com.br/eleicoes/2020/apuracao/1turno/ba/itirucu/
names(parlaMunic2020)[1] <- "cod_munic"
parlaMunic2020 <- parlaMunic2020 |>
  bind_rows(data.frame(cod_munic = "2916906",
                       total = 9,
                       homem = 9,
                       mulher = 0))

# Imputa dados para Brasília e Fernando de Noronha com base nas eleições estaduais
# de 2022 (https://sig.tse.jus.br/ords/dwapr/r/seai/sig-eleicao/home e 
# https://www1.folha.uol.com.br/poder/eleicoes/2022/apuracao/1turno/pe/deputado-estadual.shtml)
parlaMunic2020 <- parlaMunic2020 |>
  bind_rows(data.frame(cod_munic = "5300108", #Brasília
                       total = 24,
                       homem = 20,
                       mulher = 4)) |>
  bind_rows(data.frame(cod_munic = "2605459", #Fernando de Noronha
                       total = 49,
                       homem = 44,
                       mulher = 5))

# Calcula as proporções
parlaMunic2020 <- parlaMunic2020 |>
  mutate(p_homem2022 = homem/total,
         p_mulher2022 = mulher/total)

# Ajusta os dados para a metodologia do PNUD (ajusta para o valor mínimo):
parlaMunic2020$p_homem2022 <- ifelse(parlaMunic2020$p_homem2022 == 0, 0.001, parlaMunic2020$p_homem2022)
parlaMunic2020$p_mulher2022 <- ifelse(parlaMunic2020$p_mulher2022 == 0, 0.001, parlaMunic2020$p_mulher2022)

# Acrescenta participação no parlamento em 2020 na base do tcc:
dados_tcc <- dados_tcc |>
  left_join(parlaMunic2020 |> select(cod_munic, p_homem2022, p_mulher2022),
            by = c("cod_munic7D" = "cod_munic"))

#-------------------------------------------------------------------------------

# Exclui as tabelas auxiliares:
rm(list = c("eleicoes2008", "candidatos2008", "resultado2008", "parlamento2008", 
  "semGenero2008", "semMunic2008", "parlaMunic2008", "foraEleicoes2008", 
  "aux_2008", "parlaMunic2008_2", "parlaMunic2008_3", "resultado2012", 
  "candidatos2012", "refazer_2008", "parlamento2012", "semGenero2012", 
  "semMunic2012", "parlaMunic2012", "parlaMunic2012_2", "parlaMunic2012_3", 
  "eleicoes2020", "candidatos2020", "resultado2020", "parlamento2020", 
  "semGenero2020", "semMunic2020", "parlaMunic2020", "foraEleicoes2020", 
  "parlaMunic2020_2", "parlaMunic2020_3"))

################################################################################
#                              NÍVEL DE INSTRUÇÃO                               
################################################################################

#-------------------------------------2010--------------------------------------

# Extrai os dados de educação do SIDRA e manipula:
educacao2010 <- get_sidra(api="/t/3547/n6/all/v/allxp/p/all/c2/allxt/c1568/9495,99713/d/v1643%200")

educacao2010 <- educacao2010 |>
  select(Valor, `Município (Código)`, Município, Sexo, `Nível de instrução`)

names(educacao2010) <- c("valor", "cod_mun", "nome_mun", "sexo", "nivel")

# Substitui valores NA por 0:
educacao2010$valor <- ifelse(is.na(educacao2010$valor), 0, educacao2010$valor)

# Cria a tabela resumida:
eduMunic2010 <- as.data.frame(unique(educacao2010$cod_mun))
names(eduMunic2010) <- "cod_mun"

# Calcula o total de homens com 25 anos ou mais com, no mínimo, educação 
# secundária por município:
eduMunic2010_2 <- educacao2010 |>
  filter(sexo == "Homens") |>
  group_by(cod_mun) |>
  mutate(homem = sum(valor)) |>
  select(cod_mun, homem) |>
  unique()

# Inclui o total de homens com 25 anos ou mais com, pelo menos, educação 
# secundária na tabela resumida:
eduMunic2010 <- eduMunic2010 |>
  left_join(eduMunic2010_2, by = "cod_mun")

# Calcula o total de mulheres com 25 anos ou mais com, no mínimo, educação 
# secundária por município:
eduMunic2010_3 <- educacao2010 |>
  filter(sexo == "Mulheres") |>
  group_by(cod_mun) |>
  mutate(mulher = sum(valor)) |>
  select(cod_mun, mulher) |>
  unique()

# Inclui o total de mulheres com 25 anos ou mais com, pelo menos, educação 
# secundária na tabela resumida:
eduMunic2010 <- eduMunic2010 |>
  left_join(eduMunic2010_3, by = "cod_mun")

# Calcula as proporções
eduMunic2010 <- eduMunic2010 |>
  mutate(e_homem2010 = homem/(homem + mulher),
         e_mulher2010 = mulher/(homem + mulher))

# Imputa valores para os municípios que ainda não existiam em 2010:
eduMunic2010 <- eduMunic2010 |>
  bind_rows(data.frame(cod_mun = "1504752", # Mojuí dos Campos (PA): dados de Santarém
                       homem = NA,
                       mulher= NA,
                       e_homem2010 = 0.2816494,
                       e_mulher2010 = 0.3733143)) |>
  bind_rows(data.frame(cod_mun = "4220000", # Balneário Rincão (SC): dados de Içara
                       homem = NA,
                       mulher= NA,
                       e_homem2010 = 0.2655255,
                       e_mulher2010 = 0.2323277)) |>
  bind_rows(data.frame(cod_mun = "4212650", # Pescaria Brava (SC): dados de Laguna
                       homem = NA,
                       mulher= NA,
                       e_homem2010 = 0.3224438,
                       e_mulher2010 = 0.3257355)) |>
  bind_rows(data.frame(cod_mun = "4314548", # Pinto Bandeira (RS): dados de Bento Gonçalves
                       homem = NA,
                       mulher= NA,
                       e_homem2010 = 0.3789438,
                       e_mulher2010 = 0.3938691)) |>
  bind_rows(data.frame(cod_mun = "5006275", # Paraíso das Águas (MS): dados de Águas Claras
                       homem = NA,
                       mulher= NA,
                       e_homem2010 = 0.1483764,
                       e_mulher2010 = 0.2279147))

# Ajusta os dados para a metodologia do PNUD (ajusta para o valor mínimo):
eduMunic2010$e_homem2010 <- ifelse(eduMunic2010$e_homem2010 == 0, 0.001, eduMunic2010$e_homem2010)
eduMunic2010$e_mulher2010 <- ifelse(eduMunic2010$e_mulher2010 == 0, 0.001, eduMunic2010$e_mulher2010)

# Acrescenta nível de instrução em 2010 na base do tcc:
dados_tcc <- dados_tcc |>
  left_join(eduMunic2010 |> select(cod_mun, e_homem2010, e_mulher2010),
            by = c("cod_munic7D" = "cod_mun"))

#-------------------------------------2022--------------------------------------

# Extrai os dados de educação do SIDRA e manipula:
educacao2022 <- get_sidra(api="/t/10061/n6/all/v/allxp/p/all/c1568/9495%2099713/c58/108866/c2/allxt/c86/95251")

educacao2022 <- educacao2022 |>
  select(Valor, `Município (Código)`, Município, Sexo)

names(educacao2022) <- c("valor", "cod_mun", "nome_mun", "sexo")

# Cria a tabela resumida:
eduMunic2022 <- as.data.frame(unique(educacao2022$cod_mun))
names(eduMunic2022) <- "cod_mun"

# Calcula o total de homens com 25 anos ou mais com, no mínimo, educação 
# secundária por município:
eduMunic2022_2 <- educacao2022 |>
  filter(sexo == "Homens") |>
  group_by(cod_mun) |>
  mutate(homem = sum(valor)) |>
  select(cod_mun, homem) |>
  unique()

# Inclui o total de homens com 25 anos ou mais com, pelo menos, educação 
# secundária na tabela resumida:
eduMunic2022 <- eduMunic2022 |>
  left_join(eduMunic2022_2, by = "cod_mun")

# Calcula o total de mulheres com 25 anos ou mais com, no mínimo, educação 
# secundária por município:
eduMunic2022_3 <- educacao2022 |>
  filter(sexo == "Mulheres") |>
  group_by(cod_mun) |>
  mutate(mulher = sum(valor)) |>
  select(cod_mun, mulher) |>
  unique()

# Inclui o total de mulheres com 25 anos ou mais com, pelo menos, educação 
# secundária na tabela resumida:
eduMunic2022 <- eduMunic2022 |>
  left_join(eduMunic2022_3, by = "cod_mun")

# Calcula as proporções
eduMunic2022 <- eduMunic2022 |>
  mutate(e_homem2022 = homem/(homem + mulher),
         e_mulher2022 = mulher/(homem + mulher))

# Ajusta os dados para a metodologia do PNUD (ajusta para o valor mínimo):
eduMunic2022$e_homem2022 <- ifelse(eduMunic2022$e_homem2022 == 0, 0.001, eduMunic2022$e_homem2022)
eduMunic2022$e_mulher2022 <- ifelse(eduMunic2022$e_mulher2022 == 0, 0.001, eduMunic2022$e_mulher2022)

# Acrescenta nível de instrução em 2020 na base do tcc:
dados_tcc <- dados_tcc |>
  left_join(eduMunic2022 |> select(cod_mun, e_homem2022, e_mulher2022),
            by = c("cod_munic7D" = "cod_mun"))

#-------------------------------------------------------------------------------

# Exclui as tabelas auxiliares:
rm(list = c(
  "educacao2010", "eduMunic2010", "eduMunic2010_2", "eduMunic2010_3",
  "educacao2022", "eduMunic2022", "eduMunic2022_2", "eduMunic2022_3"))

################################################################################
#                             MERCADO DE TRABALHO                              #
################################################################################

#-------------------------------------2010--------------------------------------

# Lê os dados retirados do RAIS e manipula:
rais2010 <- read_excel("./data_read/rais2010.xlsx")

rais2010 <- rais2010 |>
  separate(cod_mun, into = c("cod_munic", "resto"), sep = ":") |>
  separate(resto, into = c("sg_uf", "nome_munic"), sep = "-")

# # Imputa valores para os municípios que ainda não existiam em 2010 ou que 
# não estavam na base de dados de 2010:
rais2010 <- rais2010 |>
  bind_rows(data.frame(cod_munic = "220455", # Guaribas (PI): dados de 2011
                       sg_uf = "PI",
                       nome_munic = "GUARIBAS",
                       homem = 47,
                       mulher = 56,
                       total = 103)) |>
  bind_rows(data.frame(cod_munic = "150475", # Mojuí dos Campos (PA): dados de 2013
                       sg_uf = "PA",
                       nome_munic = "MOJUÍ DOS CAMPOS",
                       homem = 19,
                       mulher = 6,
                       total = 25)) |>
  bind_rows(data.frame(cod_munic = "422000", # Balneário Rincão (SC): dados de 2013
                       sg_uf = "SC",
                       nome_munic = "BALNEÁRIO RINCÃO",
                       homem = 200,
                       mulher = 190,
                       total = 390)) |>
  bind_rows(data.frame(cod_munic = "421265", # Pescaria Brava (SC): dados de 2013
                       sg_uf = "SC",
                       nome_munic = "PESCARIA BRAVA",
                       homem = 130,
                       mulher = 113,
                       total = 243)) |>
  bind_rows(data.frame(cod_munic = "431454", # Pinto Bandeira (RS): dados de 2013
                       sg_uf = "RS",
                       nome_munic = "PINTO BANDEIRA",
                       homem = 100,
                       mulher = 78,
                       total = 178)) |>
  bind_rows(data.frame(cod_munic = "500627", # Paraíso das Águas (MS): dados de 2013
                       sg_uf = "MS",
                       nome_munic = "PARAÍSO DAS AGUAS",
                       homem = 114,
                       mulher = 120,
                       total = 234))

# Calcula as proporções
rais2010 <- rais2010 |>
  mutate(t_homem2010 = homem/total,
         t_mulher2010 = mulher/total)

# Ajusta os dados para a metodologia do PNUD (ajusta para o valor mínimo):
rais2010$t_mulher2010 <- ifelse(rais2010$t_mulher2010 == 0, 0.001, rais2010$t_mulher2010)

# Acrescenta vínculos de trabalho em 2010 na base do tcc:
dados_tcc <- dados_tcc |>
  left_join(rais2010 |> select(cod_munic, t_homem2010, t_mulher2010),
            by = c("cod_munic6D" = "cod_munic"))

#-------------------------------------2022--------------------------------------

# Lê os dados retirados do RAIS e manipula:
rais2022 <- read_excel("./data_read/rais2022.xlsx")

rais2022 <- rais2022 |>
  separate(cod_mun, into = c("cod_munic", "resto"), sep = ":") |>
  separate(resto, into = c("sg_uf", "nome_munic"), sep = "-")

# Calcula as proporções
rais2022 <- rais2022 |>
  mutate(t_homem2022 = homem/total,
         t_mulher2022 = mulher/total)

# Acrescenta vínculos de trabalho em 2022 na base do tcc:
dados_tcc <- dados_tcc |>
  left_join(rais2022 |> select(cod_munic, t_homem2022, t_mulher2022),
            by = c("cod_munic6D" = "cod_munic"))

#-------------------------------------------------------------------------------

# Exclui as tabelas auxiliares:
rm(list = c("rais2010", "rais2022"))

################################################################################
#                           GENDER INEQUALITY INDEX                            #
################################################################################

#-------------------------------------2010--------------------------------------

# Dimensão de saúde:
dados_tcc$saudeMulher2010 <- ((10/dados_tcc$mortMat2010)*(1/dados_tcc$natAdolesc2010))^(1/2)

# Dimensão de empoderamento:
dados_tcc$empodHomem2010 <- ((dados_tcc$p_homem2010)*(dados_tcc$e_homem2010))^(1/2)
dados_tcc$empodMulher2010 <- ((dados_tcc$p_mulher2010)*(dados_tcc$e_mulher2010))^(1/2)

# Dimensão de mercado de trabalho:
dados_tcc$trabHomem2010 <- dados_tcc$t_homem2010
dados_tcc$trabMulher2010 <- dados_tcc$t_mulher2010

# Médias geométricas por gênero:
dados_tcc$geomHomem2010 <- (1*(dados_tcc$empodHomem2010)*(dados_tcc$trabHomem2010))^(1/3)
dados_tcc$geomMulher2010 <- ((dados_tcc$saudeMulher2010)*(dados_tcc$empodMulher2010)*(dados_tcc$trabMulher2010))^(1/3)

# Média harmônica entre as médias geométricas:
dados_tcc$harmHM2010 <- 2/(1/dados_tcc$geomHomem2010+1/dados_tcc$geomMulher2010)

# Média geométrica de referência:
dados_tcc$geomHM2010 <- (((1+dados_tcc$saudeMulher2010)/2)*((dados_tcc$empodHomem2010+dados_tcc$empodMulher2010)/2)*((dados_tcc$trabHomem2010+dados_tcc$trabMulher2010)/2))^(1/3)

# Gender Inequality Index:
dados_tcc$gii2010 <- 1-(dados_tcc$harmHM2010/dados_tcc$geomHM2010)

#-------------------------------------2022--------------------------------------

# Dimensão de saúde:
dados_tcc$saudeMulher2022 <- ((10/dados_tcc$mortMat2022)*(1/dados_tcc$natAdolesc2022))^(1/2)

# Dimensão de empoderamento:
dados_tcc$empodHomem2022 <- ((dados_tcc$p_homem2022)*(dados_tcc$e_homem2022))^(1/2)
dados_tcc$empodMulher2022 <- ((dados_tcc$p_mulher2022)*(dados_tcc$e_mulher2022))^(1/2)

# Dimensão de mercado de trabalho:
dados_tcc$trabHomem2022 <- dados_tcc$t_homem2022
dados_tcc$trabMulher2022 <- dados_tcc$t_mulher2022

# Médias geométricas por gênero:
dados_tcc$geomHomem2022 <- (1*(dados_tcc$empodHomem2022)*(dados_tcc$trabHomem2022))^(1/3)
dados_tcc$geomMulher2022 <- ((dados_tcc$saudeMulher2022)*(dados_tcc$empodMulher2022)*(dados_tcc$trabMulher2022))^(1/3)

# Média harmônica entre as médias geométricas:
dados_tcc$harmHM2022 <- 2/(1/dados_tcc$geomHomem2022+1/dados_tcc$geomMulher2022)

# Média geométrica de referência:
dados_tcc$geomHM2022 <- (((1+dados_tcc$saudeMulher2022)/2)*((dados_tcc$empodHomem2022+dados_tcc$empodMulher2022)/2)*((dados_tcc$trabHomem2022+dados_tcc$trabMulher2022)/2))^(1/3)

# Gender Inequality Index:
dados_tcc$gii2022 <- 1-(dados_tcc$harmHM2022/dados_tcc$geomHM2022)

################################################################################
#                                     MAPAS                                    #
################################################################################

#-------------------------Taxa de Mortalidade Materna---------------------------

# 2010
tmm2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = mortMat2010),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkblue",
                      limits = c(10,1000),
                      breaks = c(1,0,250,500,750,1000),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("01_tmm2010.png", tmm2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# 2022
tmm2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = mortMat2022),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkblue",
                      limits = c(10,1000),
                      breaks = c(1,0,250,500,750,1000),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("01_tmm2022.png", tmm2022,
       width = 16, height = 12, units = "in", dpi = 1200)

rm(tmm2010, tmm2022)

#-----------------------Taxa de Gravidez na Adolescência------------------------

# 2010
tga2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = natAdolesc2010),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkred",
                      limits = c(1,250),
                      breaks = c(1,75,175,250),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("02_tga2010.png", tga2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# 2022
tga2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = natAdolesc2022),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkred",
                      limits = c(1,250),
                      breaks = c(1,75,175,250),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("02_tga2022.png", tga2022,
       width = 16, height = 12, units = "in", dpi = 1200)

rm(tga2010, tga2022)

#--------------------------Participação no parlamento---------------------------

# Homem 2010
parlaHomem2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = p_homem2010*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#034402",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010 - Homens") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("03_parlaHomem2010.png", parlaHomem2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2010
parlaMulher2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = p_mulher2010*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#034402",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010 - Mulheres ") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("03_parlaMulher2010.png", parlaMulher2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Homem 2022
parlaHomem2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = p_homem2022*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#034402",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022 - Homens") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("03_parlaHomem2022.png", parlaHomem2022,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2022
parlaMulher2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = p_mulher2022*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#034402",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022 - Mulheres") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("03_parlaMulher2022.png", parlaMulher2022,
       width = 16, height = 12, units = "in", dpi = 1200)

rm(parlaHomem2010, parlaMulher2010, parlaHomem2022, parlaMulher2022)

#-------------------------Educação secundária/superior--------------------------

# Homem 2010
educHomem2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = e_homem2010*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#D76C38",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010 - Homens") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("04_educHomem2010.png", educHomem2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2010
educMulher2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = e_mulher2010*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#D76C38",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010 - Mulheres") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("04_educMulher2010.png", educMulher2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Homem 2022
educHomem2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = e_homem2022*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#D76C38",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022 - Homens") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("04_educHomem2022.png", educHomem2022,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2022
educMulher2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = e_mulher2022*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#D76C38",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022 - Mulheres") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("04_educMulher2022.png", educMulher2022,
       width = 16, height = 12, units = "in", dpi = 1200)

rm(educHomem2010, educMulher2010, educHomem2022, educMulher2022)

#-----------------------------Mercado de trabalho-------------------------------

# Homem 2010
trabHomem2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = t_homem2010*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#013A63",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010 - Homens") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("05_trabHomem2010.png", trabHomem2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2010
trabMulher2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = t_mulher2010*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#013A63",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010 - Mulheres") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("05_trabMulher2010.png", trabMulher2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Homem 2022
trabHomem2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = t_homem2022*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#013A63",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022 - Homens") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("05_trabHomem2022.png", trabHomem2022,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2022
trabMulher2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = t_mulher2022*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#013A63",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022 - Mulheres") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("05_trabMulher2022.png", trabMulher2022,
       width = 16, height = 12, units = "in", dpi = 1200)

rm(trabHomem2010, trabMulher2010, trabHomem2022, trabMulher2022)

#--------------------------------Dimensão Saúde---------------------------------

# Mulher 2010
saudeMulher2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = saudeMulher2010),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkblue",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      labels = scales::number_format(accuracy = 0.01),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("06_saudeMulher2010.png", saudeMulher2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2022
saudeMulher2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = saudeMulher2022),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkblue",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      labels = scales::number_format(accuracy = 0.01),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("06_saudeMulher2022.png", saudeMulher2022,
       width = 16, height = 12, units = "in", dpi = 1200)

rm(saudeHomem2010, saudeMulher2010, saudeHomem2022, saudeMulher2022)

#----------------------------Dimensão Empoderamento-----------------------------

# Homem 2010
empodHomem2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = empodHomem2010),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#034402",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      labels = scales::number_format(accuracy = 0.01),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010 - Homens") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("07_empodHomem2010.png", empodHomem2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2010
empodMulher2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = empodMulher2010),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#034402",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      labels = scales::number_format(accuracy = 0.01),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010 - Mulheres") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("07_empodMulher2010.png", empodMulher2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Homem 2022
empodHomem2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = empodHomem2022),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#034402",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      labels = scales::number_format(accuracy = 0.01),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022 - Homens") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("07_empodHomem2022.png", empodHomem2022,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2022
empodMulher2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = empodMulher2022),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#034402",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      labels = scales::number_format(accuracy = 0.01),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022 - Mulheres") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("07_empodMulher2022.png", empodMulher2022,
       width = 16, height = 12, units = "in", dpi = 1200)

rm(empodHomem2010, empodMulher2010, empodHomem2022, empodMulher2022)

#-------------------------Médias geométricas por gênero-------------------------

# Homem 2010
geomHomem2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = geomHomem2010),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "black",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010 - Homens") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("08_geomHomem2010.png", geomHomem2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2010
geomMulher2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = geomMulher2010),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "black",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010 - Mulheres") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("08_geomMulher2010.png", geomMulher2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Homem 2022
geomHomem2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = geomHomem2022),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "black",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022 - Homens") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("08_geomHomem2022.png", geomHomem2022,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2022
geomMulher2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = geomMulher2022),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "black",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022 - Mulheres") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("08_geomMulher2022.png", geomMulher2022,
       width = 16, height = 12, units = "in", dpi = 1200)

rm(geomHomem2010, geomMulher2010, geomHomem2022, geomMulher2022)

#---------------------------Gender Inequality Index-----------------------------

# 2010
gii2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = gii2010),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkred",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("09_gii2010.png", gii2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# 2022
gii2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = gii2022),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkred",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("09_gii2022.png", gii2022,
       width = 16, height = 12, units = "in", dpi = 1200)

rm(gii2010, gii2022)

#---------------------------------Delta Saúde-----------------------------------

min <- round(min(range(dados_tcc$delta_saude, na.rm = TRUE)), 2)
max <- round(max(range(dados_tcc$delta_saude, na.rm = TRUE)), 2)

deltaSaude <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = delta_saude),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient2(
    low = "#C5392F", mid = "white", high = "#2B6CB0",
    midpoint = 0,
    limits = c(min, max),
    breaks  = c(min, pretty(c(min, max), n = 5), max),
    oob = scales::squish,
    na.value = "grey85",
    name = NULL,
    guide = guide_colorbar(ticks.linewidth = 1, ticks.colour = "white")
  ) +
  labs(title = "Variação da dimensão de saúde") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"),
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("deltaSaude.png", deltaSaude,
       width = 16, height = 12, units = "in", dpi = 1200)

rm(min, max, deltaSaude)

#---------------------------Delta Empoderamento Homem---------------------------

min <- round(min(range(dados_tcc$delta_empodH, na.rm = TRUE)), 2)
max <- round(max(range(dados_tcc$delta_empodH, na.rm = TRUE)), 2)

deltaEmpod <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = delta_empodH),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient2(
    low = "#C5392F", mid = "white", high = "#2B6CB0",
    midpoint = 0,
    limits = c(min, max),
    breaks  = pretty(c(min, max), n = 5),
    oob = scales::squish,
    na.value = "grey85",
    name = NULL,
    guide = guide_colorbar(ticks.linewidth = 1, ticks.colour = "white")
  ) +
  labs(title = "Homens") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"),
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("deltaEmpodHomens.png", deltaEmpod,
       width = 16, height = 12, units = "in", dpi = 1200)

rm(min, max, deltaEmpod)

#-------------------------Delta Empoderamento Mulheres--------------------------

min <- round(min(range(dados_tcc$delta_empodM, na.rm = TRUE)), 2)
max <- round(max(range(dados_tcc$delta_empodM, na.rm = TRUE)), 2)

deltaEmpod <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = delta_empodM),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient2(
    low = "#C5392F", mid = "white", high = "#2B6CB0",
    midpoint = 0,
    limits = c(min, max),
    breaks  = pretty(c(min, max), n = 5),
    oob = scales::squish,
    na.value = "grey85",
    name = NULL,
    guide = guide_colorbar(ticks.linewidth = 1, ticks.colour = "white")
  ) +
  labs(title = "Mulheres") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"),
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("deltaEmpodMulheres.png", deltaEmpod,
       width = 16, height = 12, units = "in", dpi = 1200)

rm(min, max, deltaEmpod)

#---------------------------Delta de Trabalho Homens----------------------------

min <- round(min(range(dados_tcc$delta_tHomem, na.rm = TRUE)), 2)
max <- round(max(range(dados_tcc$delta_tHomem, na.rm = TRUE)), 2)

delta_tHomem <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = delta_tHomem),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient2(
    low = "#C5392F", mid = "white", high = "#2B6CB0",
    midpoint = 0,
    limits = c(min, max),
    breaks  = pretty(c(min, max), n = 5),
    oob = scales::squish,
    na.value = "grey85",
    name = NULL,
    guide = guide_colorbar(ticks.linewidth = 1, ticks.colour = "white")
  ) +
  labs(title = "Homens") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"),
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("delta_tHomem.png", delta_tHomem,
       width = 16, height = 12, units = "in", dpi = 1200)

rm(min, max, delta_tHomem)

#--------------------------Delta de Trabalho Mulheres---------------------------

min <- round(min(range(dados_tcc$delta_tMulher, na.rm = TRUE)), 2)
max <- round(max(range(dados_tcc$delta_tMulher, na.rm = TRUE)), 2)

delta_tMulher <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = delta_tMulher),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient2(
    low = "#C5392F", mid = "white", high = "#2B6CB0",
    midpoint = 0,
    limits = c(min, max),
    breaks  = pretty(c(min, max), n = 5),
    oob = scales::squish,
    na.value = "grey85",
    name = NULL,
    guide = guide_colorbar(ticks.linewidth = 1, ticks.colour = "white")
  ) +
  labs(title = "Mulheres") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"),
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("delta_tMulher.png", delta_tMulher,
       width = 16, height = 12, units = "in", dpi = 1200)

rm(min, max, delta_tMulher)

#---------------------------Gender Inequality Index-----------------------------

min <- round(min(range(dados_tcc$delta_gii, na.rm = TRUE)), 2)
max <- round(max(range(dados_tcc$delta_gii, na.rm = TRUE)), 2)

delta_gii <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = delta_gii),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient2(
    low = "#C5392F", mid = "white", high = "#2B6CB0",
    midpoint = 0,
    limits = c(min, max),
    breaks  = pretty(c(min, max), n = 5),
    oob = scales::squish,
    na.value = "grey85",
    name = NULL,
    guide = guide_colorbar(ticks.linewidth = 1, ticks.colour = "white")
  ) +
  labs(title = "Variação do GII") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"),
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("delta_gii.png", delta_gii,
       width = 16, height = 12, units = "in", dpi = 1200)

rm(min, max, delta_gii)

################################################################################
#                                   BOXPLOTS                                   #
################################################################################

#-------------------------Taxa de Mortalidade Materna---------------------------

tmm_long <- dados_tcc |>
  select(cod_munic7D, nome_regiao, mortMat2010, mortMat2022)

names(tmm_long) <- c("cod_munic7D", "nome_regiao", "2010", "2022")

tmm_long <- tmm_long |>
  pivot_longer(cols = c(`2010`, `2022`),
               names_to = "ano", values_to = "mortMat")

tmm_bp <- ggplot(tmm_long, aes(x = nome_regiao, y = mortMat, fill = ano)) +
  geom_boxplot(position = position_dodge2(width = 0.8, preserve = "single"),
               color = "black",
               outlier.colour = "#F05D5E",  
               outlier.shape = 16,
               outlier.size = 2,
               staplewidth = 0.5) +
  scale_fill_manual(values = c("2010" = "bisque", "2022" = "#C3E8F2")) +
  labs(title = NULL, x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.box.spacing = grid::unit(-5, "pt"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    text = element_text(size = 30))

ggsave("01_tmm.png", tmm_bp,
       width = 15, height = 8, units = "in", dpi = 300)

rm(tmm_long, tmm_bp)

#-----------------------Taxa de Gravidez na Adolescência------------------------

tga_long <- dados_tcc |>
  select(cod_munic7D, nome_regiao, natAdolesc2010, natAdolesc2022)

names(tga_long) <- c("cod_munic7D", "nome_regiao", "2010", "2022")

tga_long <- tga_long |>
  pivot_longer(cols = c(`2010`, `2022`),
               names_to = "ano", values_to = "natAdolesc")

tga_bp <- ggplot(tga_long, aes(x = nome_regiao, y = natAdolesc, fill = ano)) +
  geom_boxplot(position = position_dodge2(width = 0.8, preserve = "single"),
               color = "black",
               outlier.colour = "#F05D5E",  
               outlier.shape = 16,
               outlier.size = 2,
               staplewidth = 0.5) +
  scale_fill_manual(values = c("2010" = "bisque", "2022" = "#C3E8F2")) +
  labs(title = NULL, x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.box.spacing = grid::unit(-5, "pt"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    text = element_text(size = 30))

ggsave("02_tga.png", tga_bp,
       width = 15, height = 8, units = "in", dpi = 300)

rm(tga_long, tga_bp)

#--------------------------Participação no parlamento---------------------------

parla_long <- dados_tcc |>
  select(cod_munic7D, nome_regiao, p_mulher2010, p_mulher2022)

names(parla_long) <- c("cod_munic7D", "nome_regiao", "2010", "2022")

parla_long <- parla_long |>
  pivot_longer(cols = c(`2010`, `2022`),
               names_to = "ano", values_to = "p_mulher")

parla_bp <- ggplot(parla_long, aes(x = nome_regiao, y = p_mulher*100, fill = ano)) +
  geom_boxplot(position = position_dodge2(width = 0.8, preserve = "single"),
               color = "black",
               outlier.colour = "#F05D5E",  
               outlier.shape = 16,
               outlier.size = 2,
               staplewidth = 0.5) +
  scale_fill_manual(values = c("2010" = "bisque", "2022" = "#C3E8F2")) +
  labs(title = NULL, x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.box.spacing = grid::unit(-5, "pt"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    text = element_text(size = 30))

ggsave("03_parla.png", parla_bp,
       width = 15, height = 8, units = "in", dpi = 300)

rm(parla_long, parla_bp)

# ---------- 2ª opção

parla_long <- dados_tcc |>
  select(cod_munic7D, nome_regiao, p_homem2010, p_homem2022, p_mulher2010, p_mulher2022)

names(parla_long) <- c("cod_munic7D", "nome_regiao", "2010 - Homens", "2022 - Homens", "2010 - Mulheres", "2022 - Mulheres")

parla_long <- parla_long |>
  pivot_longer(cols = c(`2010 - Homens`, `2022 - Homens`, `2010 - Mulheres`, `2022 - Mulheres`),
               names_to = "indicador", values_to = "valor")

parla_long$indicador <- factor(
  parla_long$indicador,
  levels = c("2010 - Homens", "2022 - Homens", "2010 - Mulheres", "2022 - Mulheres"))

parla_bp <- ggplot(parla_long, aes(x = nome_regiao, y = valor*100, fill = indicador)) +
  geom_boxplot(position = position_dodge2(width = 0.8, preserve = "single"),
               color = "black",
               outlier.colour = "#F05D5E",  
               outlier.shape = 16,
               outlier.size = 2,
               staplewidth = 0.5) +
  scale_fill_manual(values = c("2010 - Homens" = "bisque", 
                               "2010 - Mulheres" = "#C3E8F2",
                               "2022 - Homens" = "#D4A574",
                               "2022 - Mulheres" = "#5FB0D9"),
                    breaks = c("2010 - Homens", "2022 - Homens", "2010 - Mulheres", "2022 - Mulheres")) +
  labs(title = NULL, x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.box.spacing = grid::unit(-5, "pt"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    text = element_text(size = 30))

ggsave("03_parla2.png", parla_bp,
       width = 15, height = 8, units = "in", dpi = 300)

rm(parla_long, parla_bp)

#-------------------------Educação secundária/superior--------------------------

educ_long <- dados_tcc |>
  select(cod_munic7D, nome_regiao, e_mulher2010, e_mulher2022)

names(educ_long) <- c("cod_munic7D", "nome_regiao", "2010", "2022")

educ_long <- educ_long |>
  pivot_longer(cols = c(`2010`, `2022`),
               names_to = "ano", values_to = "e_mulher")

educ_bp <- ggplot(educ_long, aes(x = nome_regiao, y = e_mulher*100, fill = ano)) +
  geom_boxplot(position = position_dodge2(width = 0.8, preserve = "single"),
               color = "black",
               outlier.colour = "#F05D5E",  
               outlier.shape = 16,
               outlier.size = 2,
               staplewidth = 0.5) +
  scale_fill_manual(values = c("2010" = "bisque", "2022" = "#C3E8F2")) +
  labs(title = NULL, x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.box.spacing = grid::unit(-5, "pt"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    text = element_text(size = 30))

ggsave("04_educ.png",educ_bp,
       width = 15, height = 8, units = "in", dpi = 300)

rm(educ_long, educ_bp)

# ---------- 2ª opção

educ_long <- dados_tcc |>
  select(cod_munic7D, nome_regiao, e_homem2010, e_homem2022, e_mulher2010, e_mulher2022)

names(educ_long) <- c("cod_munic7D", "nome_regiao", "2010 - Homens", "2022 - Homens", "2010 - Mulheres", "2022 - Mulheres")

educ_long <- educ_long |>
  pivot_longer(cols = c(`2010 - Homens`, `2022 - Homens`, `2010 - Mulheres`, `2022 - Mulheres`),
               names_to = "indicador", values_to = "valor")

educ_long$indicador <- factor(
  educ_long$indicador,
  levels = c("2010 - Homens", "2022 - Homens", "2010 - Mulheres", "2022 - Mulheres"))

educ_bp <- ggplot(educ_long, aes(x = nome_regiao, y = valor*100, fill = indicador)) +
  geom_boxplot(position = position_dodge2(width = 0.8, preserve = "single"),
               color = "black",
               outlier.colour = "#F05D5E",  
               outlier.shape = 16,
               outlier.size = 2,
               staplewidth = 0.5) +
  scale_fill_manual(values = c("2010 - Homens" = "bisque", 
                               "2010 - Mulheres" = "#C3E8F2",
                               "2022 - Homens" = "#D4A574",
                               "2022 - Mulheres" = "#5FB0D9"),
                    breaks = c("2010 - Homens", "2022 - Homens", "2010 - Mulheres", "2022 - Mulheres")) +
  labs(title = NULL, x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.box.spacing = grid::unit(-5, "pt"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    text = element_text(size = 30))

ggsave("04_educ2.png", educ_bp,
       width = 15, height = 8, units = "in", dpi = 300)

rm(educ_long, educ_bp)

#-----------------------------Mercado de trabalho-------------------------------

trab_long <- dados_tcc |>
  select(cod_munic7D, nome_regiao, t_mulher2010, t_mulher2022)

names(trab_long) <- c("cod_munic7D", "nome_regiao", "2010", "2022")

trab_long <- trab_long |>
  pivot_longer(cols = c(`2010`, `2022`),
               names_to = "ano", values_to = "t_mulher")

trab_bp <- ggplot(trab_long, aes(x = nome_regiao, y = t_mulher*100, fill = ano)) +
  geom_boxplot(position = position_dodge2(width = 0.8, preserve = "single"),
               color = "black",
               outlier.colour = "#F05D5E",  
               outlier.shape = 16,
               outlier.size = 2,
               staplewidth = 0.5) +
  scale_fill_manual(values = c("2010" = "bisque", "2022" = "#C3E8F2")) +
  labs(title = NULL, x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.box.spacing = grid::unit(-5, "pt"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    text = element_text(size = 30))

ggsave("05_trab.png",trab_bp,
       width = 15, height = 8, units = "in", dpi = 300)

rm(trab_long, trab_bp)

# ---------- 2ª opção

trab_long <- dados_tcc |>
  select(cod_munic7D, nome_regiao, t_homem2010, t_homem2022, t_mulher2010, t_mulher2022)

names(trab_long) <- c("cod_munic7D", "nome_regiao", "2010 - Homens", "2022 - Homens", "2010 - Mulheres", "2022 - Mulheres")

trab_long <- trab_long |>
  pivot_longer(cols = c(`2010 - Homens`, `2022 - Homens`, `2010 - Mulheres`, `2022 - Mulheres`),
               names_to = "indicador", values_to = "valor")

trab_long$indicador <- factor(
  trab_long$indicador,
  levels = c("2010 - Homens", "2022 - Homens", "2010 - Mulheres", "2022 - Mulheres"))

trab_bp <- ggplot(trab_long, aes(x = nome_regiao, y = valor*100, fill = indicador)) +
  geom_boxplot(position = position_dodge2(width = 0.8, preserve = "single"),
               color = "black",
               outlier.colour = "#F05D5E",  
               outlier.shape = 16,
               outlier.size = 2,
               staplewidth = 0.5) +
  scale_fill_manual(values = c("2010 - Homens" = "bisque", 
                               "2010 - Mulheres" = "#C3E8F2",
                               "2022 - Homens" = "#D4A574",
                               "2022 - Mulheres" = "#5FB0D9"),
                    breaks = c("2010 - Homens", "2022 - Homens", "2010 - Mulheres", "2022 - Mulheres")) +
  labs(title = NULL, x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.box.spacing = grid::unit(-5, "pt"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    text = element_text(size = 30))

ggsave("05_trab2.png",trab_bp,
       width = 15, height = 8, units = "in", dpi = 300)

rm(trab_long, trab_bp)

#--------------------------------Dimensão Saúde---------------------------------

dSaude_long <- dados_tcc |>
  select(cod_munic7D, nome_regiao, saudeMulher2010, saudeMulher2022)

names(dSaude_long) <- c("cod_munic7D", "nome_regiao", "2010", "2022")

dSaude_long <- dSaude_long |>
  pivot_longer(cols = c(`2010`, `2022`),
               names_to = "ano", values_to = "saude")

dSaude_bp <- ggplot(dSaude_long, aes(x = nome_regiao, y = saude, fill = ano)) +
  geom_boxplot(position = position_dodge2(width = 0.8, preserve = "single"),
               color = "black",
               outlier.colour = "#F05D5E",  
               outlier.shape = 16,
               outlier.size = 2,
               staplewidth = 0.5) +
  scale_fill_manual(values = c("2010" = "bisque", "2022" = "#C3E8F2")) +
  labs(title = NULL, x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.box.spacing = grid::unit(-5, "pt"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    text = element_text(size = 30))

ggsave("06_dSaude.png",dSaude_bp,
       width = 15, height = 8, units = "in", dpi = 300)

rm(dSaude_long, dSaude_bp)

#----------------------------Dimensão Empoderamento-----------------------------

dEmpod_long <- dados_tcc |>
  select(cod_munic7D, nome_regiao, empodMulher2010, empodMulher2022)

names(dEmpod_long) <- c("cod_munic7D", "nome_regiao", "2010", "2022")

dEmpod_long <- dEmpod_long |>
  pivot_longer(cols = c(`2010`, `2022`),
               names_to = "ano", values_to = "empod")

dEmpod_bp <- ggplot(dEmpod_long, aes(x = nome_regiao, y = empod, fill = ano)) +
  geom_boxplot(position = position_dodge2(width = 0.8, preserve = "single"),
               color = "black",
               outlier.colour = "#F05D5E",  
               outlier.shape = 16,
               outlier.size = 2,
               staplewidth = 0.5) +
  scale_fill_manual(values = c("2010" = "bisque", "2022" = "#C3E8F2")) +
  labs(title = NULL, x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.box.spacing = grid::unit(-5, "pt"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    text = element_text(size = 30))

ggsave("07_dEmpod.png",dEmpod_bp,
       width = 15, height = 8, units = "in", dpi = 300)

rm(dEmpod_long, dEmpod_bp)

# ---------- 2ª opção

dEmpod_long <- dados_tcc |>
  select(cod_munic7D, nome_regiao, empodHomem2010, empodHomem2022, empodMulher2010, empodMulher2022)

names(dEmpod_long) <- c("cod_munic7D", "nome_regiao", "2010 - Homens", "2022 - Homens", "2010 - Mulheres", "2022 - Mulheres")

dEmpod_long <- dEmpod_long |>
  pivot_longer(cols = c(`2010 - Homens`, `2022 - Homens`, `2010 - Mulheres`, `2022 - Mulheres`),
               names_to = "indicador", values_to = "valor")

dEmpod_long$indicador <- factor(
  dEmpod_long$indicador,
  levels = c("2010 - Homens", "2022 - Homens", "2010 - Mulheres", "2022 - Mulheres"))

dEmpod_bp <- ggplot(dEmpod_long, aes(x = nome_regiao, y = valor*100, fill = indicador)) +
  geom_boxplot(position = position_dodge2(width = 0.8, preserve = "single"),
               color = "black",
               outlier.colour = "#F05D5E",  
               outlier.shape = 16,
               outlier.size = 2,
               staplewidth = 0.5) +
  scale_fill_manual(values = c("2010 - Homens" = "bisque", 
                               "2010 - Mulheres" = "#C3E8F2",
                               "2022 - Homens" = "#D4A574",
                               "2022 - Mulheres" = "#5FB0D9"),
                    breaks = c("2010 - Homens", "2022 - Homens", "2010 - Mulheres", "2022 - Mulheres")) +
  labs(title = NULL, x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.box.spacing = grid::unit(-5, "pt"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    text = element_text(size = 30))

ggsave("07_dEmpod2.png",dEmpod_bp,
       width = 15, height = 8, units = "in", dpi = 300)

rm(dEmpod_long, dEmpod_bp)

#-------------------------Médias geométricas por gênero-------------------------

geom_long <- dados_tcc |>
  select(cod_munic7D, nome_regiao, geomMulher2010, geomMulher2022)
  
names(geom_long_long) <- c("cod_munic7D", "nome_regiao", "2010", "2022")

geom_long <- geom_long |>
  pivot_longer(cols = c(`2010`, `2022`),
               names_to = "ano", values_to = "geom")

geom_bp <- ggplot(geom_long, aes(x = nome_regiao, y = geom, fill = ano)) +
  geom_boxplot(position = position_dodge2(width = 0.8, preserve = "single"),
               color = "black",
               outlier.colour = "#F05D5E",  
               outlier.shape = 16,
               outlier.size = 2,
               staplewidth = 0.5) +
  scale_fill_manual(values = c("2010" = "bisque", "2022" = "#C3E8F2")) +
  labs(title = NULL, x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.box.spacing = grid::unit(-5, "pt"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    text = element_text(size = 30))

ggsave("08_geom.png", geom_bp,
       width = 15, height = 8, units = "in", dpi = 300)

rm(geom_long, geom_bp)

#-------------------------------Razão harm/geom---------------------------------

# 2010
razao2010_long <- dados_tcc |>
  select(cod_munic7D, nome_regiao, harmHM2010, geomHM2010)

names(razao2010_long) <- c("cod_munic7D", "nome_regiao", "harm", "geom")

razao2010_long <- razao2010_long |>
  pivot_longer(cols = c("harm", "geom"),
               names_to = "media", values_to = "razao")
               
razao2010_bp <- ggplot(razao2010_long, aes(x = nome_regiao, y = razao, fill = media)) +
  geom_boxplot(position = position_dodge2(width = 0.8, preserve = "single"),
               color = "black",
               outlier.colour = "#F05D5E",  
               outlier.shape = 16,
               outlier.size = 2,
               staplewidth = 0.5) +
  scale_fill_manual(values = c("harm" = "bisque", "geom" = "#C3E8F2")) +
  labs(title = NULL, x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.box.spacing = grid::unit(-5, "pt"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    text = element_text(size = 30))

ggsave("09_razao2010.png", razao2010_bp,
       width = 15, height = 8, units = "in", dpi = 300)

rm(razao2010_long, razao2010_bp)

# 2022
razao2022_long <- dados_tcc |>
  select(cod_munic7D, nome_regiao, harmHM2022, geomHM2022)

names(razao2022_long) <- c("cod_munic7D", "nome_regiao", "harm", "geom")

razao2022_long <- razao2022_long |>
  pivot_longer(cols = c("harm", "geom"),
               names_to = "media", values_to = "razao")

razao2022_bp <- ggplot(razao2022_long, aes(x = nome_regiao, y = razao, fill = media)) +
  geom_boxplot(position = position_dodge2(width = 0.8, preserve = "single"),
               color = "black",
               outlier.colour = "#F05D5E",  
               outlier.shape = 16,
               outlier.size = 2,
               staplewidth = 0.5) +
  scale_fill_manual(values = c("harm" = "bisque", "geom" = "#C3E8F2")) +
  labs(title = NULL, x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.box.spacing = grid::unit(-5, "pt"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    text = element_text(size = 30))

ggsave("10_razao2022.png", razao2022_bp,
       width = 15, height = 8, units = "in", dpi = 300)

rm(razao2022_long, razao2022_bp)

#---------------------------Gender Inequality Index-----------------------------

gii_long <- dados_tcc |>
  select(cod_munic7D, nome_regiao, gii2010, gii2022)

names(gii_long) <- c("cod_munic7D", "nome_regiao", "2010", "2022")

gii_long <- gii_long |>
  pivot_longer(cols = c(`2010`, `2022`),
               names_to = "ano", values_to = "gii")

gii_bp <- ggplot(gii_long, aes(x = nome_regiao, y = gii, fill = ano)) +
  geom_boxplot(position = position_dodge2(width = 0.8, preserve = "single"),
               color = "black",
               outlier.colour = "#F05D5E",  
               outlier.shape = 16,
               outlier.size = 2,
               staplewidth = 0.5) +
  scale_fill_manual(values = c("2010" = "bisque", "2022" = "#C3E8F2")) +
  labs(title = NULL, x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.box.spacing = grid::unit(-5, "pt"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    text = element_text(size = 30))

ggsave("11_gii.png",gii_bp,
       width = 15, height = 8, units = "in", dpi = 300)

rm(gii_long, gii_bp)

################################################################################
#                                 HISTOGRAMAS                                  #
################################################################################

#------------------------------------Brasil-------------------------------------

hst_BR2010 <- ggplot(dados_tcc, aes(gii2010)) +
  geom_histogram(
    binwidth = 0.1,
    boundary = 0,
    fill = "bisque",        
    color = "black",          
    linewidth = 0.4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 400)) +
  labs(x = "GII", y = "Frequência", title = "2010") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 0)),
    plot.background   = element_rect(fill = "white", color = "white"),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "white", color = NA))

ggsave("01_brasil2010.png", hst_BR2010,
       width = 4, height = 2.5, units = "in", dpi = 300)

hst_BR2022 <- ggplot(dados_tcc, aes(gii2022)) +
  geom_histogram(
    binwidth = 0.1,
    boundary = 0,
    fill = "#C3E8F2",        
    color = "black",          
    linewidth = 0.4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 400)) +
  labs(x = "GII", y = "Frequência", title = "2022") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 0)),
    plot.background   = element_rect(fill = "white", color = "white"),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "white", color = NA))

ggsave("01_brasil2022.png", hst_BR2022,
       width = 4, height = 2.5, units = "in", dpi = 300)

#---------------------------------Centro-Oeste----------------------------------

hst_CO2010 <- ggplot(dados_tcc |> filter(nome_regiao == "Centro-Oeste"), aes(gii2010)) +
  geom_histogram(
    binwidth = 0.1,
    boundary = 0,
    fill = "bisque",        
    color = "black",          
    linewidth = 0.4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 40)) +
  labs(x = "GII", y = "Frequência", title = "2010") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 0)),
    plot.background   = element_rect(fill = "white", color = "white"),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "white", color = NA))

ggsave("02_CO2010.png", hst_CO2010,
       width = 4, height = 2.5, units = "in", dpi = 300)

hst_CO2022 <- ggplot(dados_tcc |> filter(nome_regiao == "Centro-Oeste"), aes(gii2022)) +
  geom_histogram(
    binwidth = 0.1,
    boundary = 0,
    fill = "#C3E8F2",        
    color = "black",          
    linewidth = 0.4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 40)) +
  labs(x = "GII", y = "Frequência", title = "2022") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 0)),
    plot.background   = element_rect(fill = "white", color = "white"),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "white", color = NA))

ggsave("02_CO2022.png", hst_CO2022,
       width = 4, height = 2.5, units = "in", dpi = 300)

#-----------------------------------Nordeste------------------------------------

hst_NE2010 <- ggplot(dados_tcc |> filter(nome_regiao == "Nordeste"), aes(gii2010)) +
  geom_histogram(
    binwidth = 0.1,
    boundary = 0,
    fill = "bisque",        
    color = "black",          
    linewidth = 0.4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(limits = c(0, 700), breaks = seq(0, 700, 140)) +
  labs(x = "GII", y = "Frequência", title = "2010") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 0)),
    plot.background   = element_rect(fill = "white", color = "white"),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "white", color = NA))

ggsave("03_NE2010.png", hst_NE2010,
       width = 4, height = 2.5, units = "in", dpi = 300)

hst_NE2022 <- ggplot(dados_tcc |> filter(nome_regiao == "Nordeste"), aes(gii2022)) +
  geom_histogram(
    binwidth = 0.1,
    boundary = 0,
    fill = "#C3E8F2",        
    color = "black",          
    linewidth = 0.4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(limits = c(0, 700), breaks = seq(0, 700, 140)) +
  labs(x = "GII", y = "Frequência", title = "2022") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 0)),
    plot.background   = element_rect(fill = "white", color = "white"),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "white", color = NA))

ggsave("03_NE2022.png", hst_NE2022,
       width = 4, height = 2.5, units = "in", dpi = 300)

#-------------------------------------Norte-------------------------------------

hst_N2010 <- ggplot(dados_tcc |> filter(nome_regiao == "Norte"), aes(gii2010)) +
  geom_histogram(
    binwidth = 0.1,
    boundary = 0,
    fill = "bisque",        
    color = "black",          
    linewidth = 0.4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, 40)) +
  labs(x = "GII", y = "Frequência", title = "2010") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 0)),
    plot.background   = element_rect(fill = "white", color = "white"),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "white", color = NA))

ggsave("04_N2010.png", hst_N2010,
       width = 4, height = 2.5, units = "in", dpi = 300)

hst_N2022 <- ggplot(dados_tcc |> filter(nome_regiao == "Norte"), aes(gii2022)) +
  geom_histogram(
    binwidth = 0.1,
    boundary = 0,
    fill = "#C3E8F2",        
    color = "black",          
    linewidth = 0.4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, 40)) +
  labs(x = "GII", y = "Frequência", title = "2022") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 0)),
    plot.background   = element_rect(fill = "white", color = "white"),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "white", color = NA))

ggsave("04_N2022.png", hst_N2022,
       width = 4, height = 2.5, units = "in", dpi = 300)

#-----------------------------------Sudeste-------------------------------------

hst_SE2010 <- ggplot(dados_tcc |> filter(nome_regiao == "Sudeste"), aes(gii2010)) +
  geom_histogram(
    binwidth = 0.1,
    boundary = 0,
    fill = "bisque",        
    color = "black",          
    linewidth = 0.4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(limits = c(0, 610), breaks = seq(0, 600, 120)) +
  labs(x = "GII", y = "Frequência", title = "2010") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 0)),
    plot.background   = element_rect(fill = "white", color = "white"),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "white", color = NA))

ggsave("05_SE2010.png", hst_SE2010,
       width = 4, height = 2.5, units = "in", dpi = 300)

hst_SE2022 <- ggplot(dados_tcc |> filter(nome_regiao == "Sudeste"), aes(gii2022)) +
  geom_histogram(
    binwidth = 0.1,
    boundary = 0,
    fill = "#C3E8F2",        
    color = "black",          
    linewidth = 0.4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(limits = c(0, 610), breaks = seq(0, 600, 120)) +
  labs(x = "GII", y = "Frequência", title = "2022") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 0)),
    plot.background   = element_rect(fill = "white", color = "white"),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "white", color = NA))

ggsave("05_SE2022.png", hst_SE2022,
       width = 4, height = 2.5, units = "in", dpi = 300)

#-------------------------------------Sul---------------------------------------

hst_S2010 <- ggplot(dados_tcc |> filter(nome_regiao == "Sul"), aes(gii2010)) +
  geom_histogram(
    binwidth = 0.1,
    boundary = 0,
    fill = "bisque",        
    color = "black",          
    linewidth = 0.4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(limits = c(0, 450), breaks = seq(0, 450, 90)) +
  labs(x = "GII", y = "Frequência", title = "2010") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 0)),
    plot.background   = element_rect(fill = "white", color = "white"),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "white", color = NA))

ggsave("06_S2010.png", hst_S2010,
       width = 4, height = 2.5, units = "in", dpi = 300)

hst_S2022 <- ggplot(dados_tcc |> filter(nome_regiao == "Sul"), aes(gii2022)) +
  geom_histogram(
    binwidth = 0.1,
    boundary = 0,
    fill = "#C3E8F2",        
    color = "black",          
    linewidth = 0.4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(limits = c(0, 450), breaks = seq(0, 450, 90)) +
  labs(x = "GII", y = "Frequência", title = "2022") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 0)),
    plot.background   = element_rect(fill = "white", color = "white"),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "white", color = NA))

ggsave("06_S2022.png", hst_S2022,
       width = 4, height = 2.5, units = "in", dpi = 300)

#-------------------------------------------------------------------------------

teste <- dados_tcc |>
  filter(nome_regiao == "Sul")

h <- hist(teste$gii2022, breaks = seq(0, 1, 0.1), plot = FALSE)
h$count

rm(h, teste, hst_BR2010, hst_BR2022, hst_CO2010, hst_CO2022, 
   hst_NE2010, hst_NE2022, hst_N2010, hst_N2022, hst_SE2010, hst_SE2022,
   hst_S2010, hst_S2022)

################################################################################
#                               OUTROS GRÁFICOS                                #
################################################################################

#---------------------------Gender Inequality Index-----------------------------

gii <- read_excel("./data_read/gii.xlsx")

gii$value <- as.numeric(gii$value)

gii <- gii |>
  select(country, year, value) |>
  mutate(nivel = case_when(
    country == "Brazil" ~ "Brasil",
    country == "Latin America And The Caribbean" ~ "América Latina e Caribe",
    country == "World" ~ "Mundo")) |>
  select(-country) |>
  pivot_wider(names_from = nivel,
              values_from = value)

graf_gii <- ggplot(gii, aes(x = year)) +
  geom_line(aes(y = Brasil, color = "Brasil"), linewidth = 1.2) +
  geom_point(aes(y = Brasil, color = "Brasil"), size = 2) +
  geom_line(aes(y = `América Latina e Caribe`, color = "América Latina e Caribe"), linewidth = 1.2) +
  geom_point(aes(y = `América Latina e Caribe`, color = "América Latina e Caribe"), size = 2) +
  geom_line(aes(y = Mundo, color = "Mundo"), linewidth = 1.2) +
  geom_point(aes(y = Mundo, color = "Mundo"), size = 2) +
  scale_x_continuous(breaks = sort(unique(gii$year))) +
  scale_y_continuous(
    limits = c(0.37, 0.6),
    breaks = seq(0.4, 0.6, 0.05)) +
  scale_color_manual(
    values = c(
      "Brasil" = "#2A9D8F",
      "América Latina e Caribe" = "#B56576",
      "Mundo" = "#E0A458"
    ),
    breaks = c("Brasil", "América Latina e Caribe", "Mundo"),
    name = ""
  ) +
  labs(x = "", y = "GII") +
  theme_minimal(11) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9),
    legend.box = "vertical",
    legend.key.width  = unit(10, "pt"),
    legend.key.height = unit(10, "pt"),
    legend.margin = margin(t = 1, r = 2, b = 2, l = 2),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 12)),
    plot.background   = element_rect(fill = "white", color = "white"),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "white", color = NA))

ggsave("graf_gii2.png", graf_gii,
       width = 6, height = 4, units = "in", dpi = 300)

rm(gii, graf_gii)

#------------------------------Nível de instrução-------------------------------

nivel_instrucao_homens <- read_excel("./data_read/nivel_instrucao_R.xlsx", sheet = "homens")

nivel_instrucao_mulheres <- read_excel("./data_read/nivel_instrucao_R.xlsx", sheet = "mulheres")

nivel_instrucao_homens <- nivel_instrucao_homens |>
  pivot_longer(cols = c(`2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`, `2024`),
             names_to = "ano", values_to = "total_h")

nivel_instrucao_mulheres <- nivel_instrucao_mulheres |>
  pivot_longer(cols = c(`2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`, `2024`),
               names_to = "ano", values_to = "total_m")

nivel_instrucao <- nivel_instrucao_homens |>
  left_join(nivel_instrucao_mulheres, by = c("regiao", "nivel", "ano"))

graf_nivel_instrucao <- ggplot(nivel_instrucao |> filter(regiao == "Brasil"), aes(x = ano, y = round(total_m*100/(total_m + total_h),2), fill = nivel)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, 10)) + 
  scale_fill_manual(
    values = c(
      "#2F3E46", "#A0CBE8", "#6C757D",
      "#2A9D8F", "#8D99AE", "#B56576", "#E0A458"
    )
  ) +
  labs(title = "", x = "", y = "Proporção de mulheres (%)", fill = "") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9),
    legend.box = "vertical",
    legend.key.width  = unit(10, "pt"),
    legend.key.height = unit(10, "pt"),
    legend.margin = margin(t = 2, r = 2, b = 2, l = 2),
    axis.title.y = element_text(margin = margin(r = 12)),
    plot.background   = element_rect(fill = "white", color = "white"),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "white", color = NA),
  ) +
  guides(fill = guide_legend(nrow = 4, byrow = TRUE))

ggsave("graf_nivel_instrucao.png", graf_nivel_instrucao,
       width = 8, height = 4, units = "in", dpi = 300)

rm(nivel_instrucao, nivel_instrucao_homens, nivel_instrucao_mulheres, graf_nivel_instrucao)

#-----------------------Participação na força de trabalho-----------------------

p_forca_trabalho <- read_excel("./data_read/trabalho_censos.xlsx")

graf_forca_trabalho <- ggplot(p_forca_trabalho, aes(x = ano)) +
  geom_line(aes(y = homens, color = "Homens"), linewidth = 1.2) +
  geom_point(aes(y = homens, color = "Homens"), size = 2) +
  geom_line(aes(y = mulheres, color = "Mulheres"), linewidth = 1.2) +
  geom_point(aes(y = mulheres, color = "Mulheres"), size = 2) +
  geom_text(aes(y = homens, label = sprintf("%.1f", homens)),
            vjust = -1, size = 3, show.legend = FALSE) +
  geom_text(aes(y = mulheres, label = sprintf("%.1f", mulheres)),
            vjust = -1, size = 3, show.legend = FALSE) +
  scale_x_continuous(breaks = sort(unique(p_forca_trabalho$ano))) +
  scale_y_continuous(limits = c(0, 90), breaks = seq(0, 90, 10)) +
  scale_color_manual(
    values = c("Homens" = "#2A9D8F", "Mulheres" = "#B56576"),
    breaks = c("Homens", "Mulheres"),
    name = ""
  ) +
  labs(x = "", y = "") +
  theme_minimal(11) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9),
    legend.box = "vertical",
    legend.key.width  = unit(10, "pt"),
    legend.key.height = unit(10, "pt"),
    legend.margin = margin(t = 0, r = 2, b = 2, l = 2),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 8)),
    plot.background   = element_rect(fill = "white", color = "white"),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "white", color = NA))

ggsave("graf_forca_trabalho.png", graf_forca_trabalho,
       width = 6, height = 4, units = "in", dpi = 300)

rm(p_forca_trabalho, graf_forca_trabalho)

#-------------------------------Rendimento médio--------------------------------

rendimento_homens <- read_excel("./data_read/trabalho_rendimento.xlsx", sheet = "homens")

rendimento_mulheres <- read_excel("./data_read/trabalho_rendimento.xlsx", sheet = "mulheres")

rendimento_homens <- rendimento_homens |>
  pivot_longer(cols = c(`2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`, `2024`),
               names_to = "ano", values_to = "rend_h")

rendimento_homens$ano <- as.numeric(rendimento_homens$ano)

rendimento_mulheres <- rendimento_mulheres |>
  pivot_longer(cols = c(`2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`, `2024`),
               names_to = "ano", values_to = "rend_m")

rendimento_mulheres$ano <- as.numeric(rendimento_mulheres$ano)

graf_rendimento_homens <- ggplot(rendimento_homens, aes(x = ano, y = rend_h, color = regiao)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = sort(unique(rendimento_homens$ano))) +
  scale_y_continuous(limits = c(1700, 4400), breaks = seq(1700, 4400, 300)) +
  scale_color_manual(
    values = c("Centro-Oeste" = "#2F3E46",
               "Nordeste" = "#E0A458",
               "Norte" = "#2A9D8F",
               "Sudeste" = "#B56576",
               "Sul" = "#6C757D"),
    breaks = c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul"),
    name = "") +
  labs(x = "", y = "") +
  theme_minimal(11) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9),
    legend.box = "vertical",
    legend.key.width  = unit(10, "pt"),
    legend.key.height = unit(10, "pt"),
    legend.box.margin = margin(t = -6, r = 0, b = 0, l = 0),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(0, "pt"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 0)),
    plot.background   = element_rect(fill = "white", color = "white"),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "white", color = NA)) + 
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

ggsave("graf_rendimento_homens.png", graf_rendimento_homens,
       width = 4, height = 4, units = "in", dpi = 400)

graf_rendimento_mulheres <- ggplot(rendimento_mulheres, aes(x = ano, y = rend_m, color = regiao)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = sort(unique(rendimento_homens$ano))) +
  scale_y_continuous(limits = c(1700, 4400), breaks = seq(1700, 4400, 300)) +
  scale_color_manual(
    values = c("Centro-Oeste" = "#2F3E46",
               "Nordeste" = "#E0A458",
               "Norte" = "#2A9D8F",
               "Sudeste" = "#B56576",
               "Sul" = "#6C757D"),
    breaks = c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul"),
    name = "") +
  labs(x = "", y = "") +
  theme_minimal(11) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9),
    legend.box = "vertical",
    legend.key.width  = unit(10, "pt"),
    legend.key.height = unit(10, "pt"),
    legend.box.margin = margin(t = -6, r = 0, b = 0, l = 0),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(0, "pt"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 0)),
    plot.background   = element_rect(fill = "white", color = "white"),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "white", color = NA)) + 
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

ggsave("graf_rendimento_mulheres.png", graf_rendimento_mulheres,
       width = 4, height = 4, units = "in", dpi = 400)

rm(rendimento_homens, rendimento_mulheres, graf_rendimento_homens, graf_rendimento_mulheres)

################################################################################
#                             SEÇÃO DE RESULTADOS                              #
################################################################################

dados_tcc <- dados_tcc |>
  mutate(
    delta_RMM = mortMat2022 - mortMat2010,
    delta_TGA = natAdolesc2022 - natAdolesc2010,
    delta_pHomem = p_homem2022 - p_homem2010,
    delta_pMulher = p_mulher2022 - p_mulher2010,
    delta_eHomem = e_homem2022 - e_homem2010,
    delta_eMulher = e_mulher2022 - e_mulher2010,
    delta_tHomem = t_homem2022 - t_homem2010,
    delta_tMulher = t_mulher2022 - t_mulher2010,
    delta_saude = saudeMulher2022 - saudeMulher2010,
    delta_empodH = empodHomem2022 - empodHomem2010,
    delta_empodM = empodMulher2022 - empodMulher2010,
    delta_gii = gii2022 - gii2010)

# ============================================================
# 2) Tabela ÚNICA com N, média, mediana, dp, min, max, P10, P90
#    (Brasil e Regiões) para colunas largo (2010/2022)
# ============================================================

cols_resumir <- c(
  "nascVivos2010", "nascVivos2022",
  "mortMat2010", "mortMat2022", "qtdeAdolesc2010",
  "qtdeAdolesc2022", "nascVivosMaeAdol2010", "nascVivosMaeAdol2022",
  "natAdolesc2010", "natAdolesc2022", "p_homem2010",
  "p_homem2022", "p_mulher2010", "p_mulher2022",
  "e_homem2010", "e_homem2022", "e_mulher2010",
  "e_mulher2022", "t_homem2010", "t_homem2022",
  "t_mulher2010", "t_mulher2022", "saudeMulher2010",
  "saudeMulher2022", "empodHomem2010", "empodHomem2022",
  "empodMulher2010", "empodMulher2022", "trabHomem2010",
  "trabHomem2022", "trabMulher2010", "trabMulher2022",
  "geomHomem2010", "geomHomem2022", "geomMulher2010",
  "geomMulher2022", "harmHM2010", "harmHM2022",
  "geomHM2010", "geomHM2022", "gii2010",
  "gii2022")

# Tornar o df em formato longo:
df_long <- dados_tcc %>%
  select(nome_regiao, all_of(cols_resumir)) %>%
  pivot_longer(
    cols = -nome_regiao,
    names_to = c("indicador","ano"),
    names_pattern = "^(.*?)(\\d{4})$",
    values_to = "valor") %>%
  mutate(ano = as.integer(ano))

# Brasil
tab_brasil <- df_long %>%
  group_by(indicador, ano) %>%
  summarise(resumir_vetor(valor), .groups = "drop") %>%
  mutate(escopo = "Brasil")

# Regiões
tab_regioes <- df_long %>%
  group_by(nome_regiao, indicador, ano) %>%
  summarise(resumir_vetor(valor), .groups = "drop") %>%
  rename(escopo = nome_regiao)

# Tabela final (Brasil + Regiões)
resultados_final <- bind_rows(tab_brasil, tab_regioes) %>%
  mutate(
    escopo = factor(escopo, levels = c("Brasil","Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))
  ) %>%
  arrange(escopo, ano, indicador)

write_xlsx(resultados_final, "./data_write/resultados_final.xlsx")

rm(df_long, tab_brasil, tab_regioes)

# ============================================================
# 3) Média da VARIAÇÃO PERCENTUAL por região (2010→2022)
#    para os mesmos indicadores escolhidos
# ============================================================

indicadores_base <- c("nascVivos", "mortMat", "nascVivosMaeAdol", "natAdolesc", 
                      "p_homem", "p_mulher", "e_homem", "e_mulher", "t_homem", 
                      "t_mulher", "gii", "saudeMulher", "empodHomem",
                      "empodMulher","trabHomem","trabMulher")

pares_anos <- tibble(
  indicador = indicadores_base,
  col_2010  = paste0(indicador, "2010"),
  col_2022  = paste0(indicador, "2022")
) %>%
  filter(col_2010 %in% names(dados_tcc), col_2022 %in% names(dados_tcc))

med_varpct_regiao <- purrr::pmap_dfr(
  list(pares_anos$indicador, pares_anos$col_2010, pares_anos$col_2022),
  function(ind, c2010, c2022) {
    dados_tcc %>%
      select(nome_regiao, all_of(c2010), all_of(c2022)) %>%
      transmute(
        nome_regiao,
        indicador = ind,
        var_pct = varpct(.data[[c2010]], .data[[c2022]])
      )
  }
) %>%
  group_by(nome_regiao, indicador) %>%
  summarise(
    media_var_pct   = mean(var_pct, na.rm = TRUE),
    mediana_var_pct = median(var_pct, na.rm = TRUE),
    n               = sum(!is.na(var_pct)),
    .groups = "drop"
  ) %>%
  arrange(indicador, nome_regiao)

write_xlsx(med_varpct_regiao, "./data_write/med_varpct_regiao.xlsx")

rm(indicadores_base, pares_anos)

#-------------------------------------GII---------------------------------------

dados_aux <- dados_tcc |>
  mutate(faixa_gii2010 = case_when(
      gii2010 <= 0.2 ~ "Baixo",
      gii2010 <= 0.5 ~ "Médio",
      gii2010 <= 0.7 ~ "Alto",
      TRUE ~ "Muito Alto"),
    faixa_gii2022 = case_when(
      gii2022 <= 0.2 ~ "Baixo",
      gii2022 <= 0.5 ~ "Médio",
      gii2022 <= 0.7 ~ "Alto",
      TRUE ~ "Muito Alto"))

dados_aux2 <- dados_aux |>
  group_by(nome_regiao) |>
  mutate(n_regiao = n()) |>
  ungroup() |>
  group_by(nome_regiao, faixa_gii2010) |>
  summarise(total = n()*100/max(n_regiao)) |>
  pivot_wider(names_from = faixa_gii2010, values_from = total) |>
  select(nome_regiao, Baixo, `Médio`, Alto, `Muito Alto`)

dados_aux3 <- dados_aux |>
  group_by(faixa_gii2010) |>
  summarise(total = n()*100/5570) |>
  mutate(nome_regiao = "Brasil") |>
  pivot_wider(names_from = faixa_gii2010, values_from = total) |>
  select(nome_regiao, Baixo, `Médio`, Alto, `Muito Alto`)

dados_aux2 <- dados_aux2 |>
  bind_rows(dados_aux3) |>
  arrange(nome_regiao)

write_xlsx(dados_aux2, "./data_write/gii2010_faixas.xlsx")

dados_aux2 <- dados_aux |>
  group_by(nome_regiao) |>
  mutate(n_regiao = n()) |>
  ungroup() |>
  group_by(nome_regiao, faixa_gii2022) |>
  summarise(total = n()*100/max(n_regiao)) |>
  pivot_wider(names_from = faixa_gii2022, values_from = total) |>
  select(nome_regiao, Baixo, `Médio`, Alto, `Muito Alto`)

dados_aux3 <- dados_aux |>
  group_by(faixa_gii2022) |>
  summarise(total = n()*100/5570) |>
  mutate(nome_regiao = "Brasil") |>
  pivot_wider(names_from = faixa_gii2022, values_from = total) |>
  select(nome_regiao, Baixo, `Médio`, Alto, `Muito Alto`)

dados_aux2 <- dados_aux2 |>
  bind_rows(dados_aux3) |>
  arrange(nome_regiao)

write_xlsx(dados_aux2, "./data_write/gii2022_faixas.xlsx")

rm(dados_aux, dados_aux2, dados_aux3)

#---------------------------------Correlações-----------------------------------

# 2010:

alvo <- "gii2010"

dados_aux <- dados_tcc |>
  select(gii2010, mortMat2010, natAdolesc2010, e_mulher2010, p_mulher2010, t_mulher2010)

num <- sapply(dados_aux, is.numeric)
vars <- setdiff(names(dados_aux)[num], alvo)

res <- tibble(var = vars) |>
  mutate(
    r_pearson  = map_dbl(var, ~ cor(dados_aux[[.x]], dados_aux[[alvo]],
                                    use = "pairwise.complete.obs",
                                    method = "pearson")),
    r_spearman = map_dbl(var, ~ cor(dados_aux[[.x]], dados_aux[[alvo]],
                                    use = "pairwise.complete.obs",
                                    method = "spearman")))

write_xlsx(res, "./data_write/correlacao2010.xlsx")

rm(alvo, dados_aux, num, vars, res)

# 2022:

alvo <- "gii2022"

dados_aux <- dados_tcc |>
  select(gii2022, mortMat2022, natAdolesc2022, e_mulher2022, p_mulher2022, t_mulher2022)

num <- sapply(dados_aux, is.numeric)
vars <- setdiff(names(dados_aux)[num], alvo)

res <- tibble(var = vars) |>
  mutate(
    r_pearson  = map_dbl(var, ~ cor(dados_aux[[.x]], dados_aux[[alvo]],
                                    use = "pairwise.complete.obs",
                                    method = "pearson")),
    r_spearman = map_dbl(var, ~ cor(dados_aux[[.x]], dados_aux[[alvo]],
                                    use = "pairwise.complete.obs",
                                    method = "spearman")))

write_xlsx(res, "./data_write/correlacao2022.xlsx")

rm(alvo, dados_aux, num, vars, res)

################################################################################
#                            DIAGRAMAS DE DISPERSÃO                            #
################################################################################

#--------------------------------IDHM x GII2010---------------------------------

idhm <- read_excel("./data_read/idhm.xlsx") |>
  select(Territorialidades, codigo_municipio, `IDHM 2010`)

idhm$codigo_municipio <- as.character(idhm$codigo_municipio)
names(idhm) <- c("Territorialidades", "codigo_municipio", "idhm2010")

# Imputa valores para os municípios que ainda não existiam em 2010:
idhm <- idhm |>
  bind_rows(data.frame(Territorialidades = "Mojuí dos Campos (PA)", # dados de Santarém
                       codigo_municipio = "1504752",
                       idhm2010 = 0.691)) |>
  bind_rows(data.frame(Territorialidades = "Balneário Rincão (SC)", # dados de Içara
                       codigo_municipio = "4220000",
                       idhm2010 = 0.741)) |>
  bind_rows(data.frame(Territorialidades = "Pescaria Brava (SC)", # dados de Laguna
                       codigo_municipio = "4212650",
                       idhm2010 = 0.752)) |>
  bind_rows(data.frame(Territorialidades = "Pinto Bandeira (RS)", # dados de Bento Gonçalves
                       codigo_municipio = "4314548",
                       idhm2010 = 0.778)) |>
  bind_rows(data.frame(Territorialidades = "Paraíso das Águas (MS)", # dados de Água Clara
                       codigo_municipio = "5006275",
                       idhm2010 = 0.67))

idhm <- idhm |>
  left_join(dados_tcc |> select(cod_munic7D, gii2010, nome_regiao), c("codigo_municipio" = "cod_munic7D"))

disp_idhm_BR <- ggplot(idhm, aes(x = gii2010, y = idhm2010, color = nome_regiao)) +
  geom_point(alpha = 0.5, size = 1.6) +
  # reta de regressão por região (uma por cor)
  # geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  # eixos (0 a 1) e mesma escala nos dois
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  # geom_vline(xintercept = 0.4, linetype = "dashed", color = "black", linewidth = 0.6) +
  # geom_hline(yintercept = 0.37, linetype = "dashed", color = "black", linewidth = 0.6) +
  # geom_abline(slope = 1, intercept = 0, linetype = 2, color = "gray60") +
  # paleta e ordem da legenda
  scale_color_manual(
    values = c("Centro-Oeste" = "#2F3E46",
               "Nordeste"     = "#E0A458",
               "Norte"        = "#2A9D8F",
               "Sudeste"      = "#B56576",
               "Sul"          = "#6C757D"),
    breaks = c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul"),
    name = ""
  ) +
  labs(x = "GII 2010", y = "IDHM 2010", title = "") +
  theme_minimal(11) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9),
    legend.box = "vertical",
    legend.key.width  = unit(10, "pt"),
    legend.key.height = unit(10, "pt"),
    legend.box.margin = margin(t = -6, r = 0, b = 0, l = 0),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(0, "pt"),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 0)),
    plot.background   = element_rect(fill = "white", color = "white"),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "white", color = NA)
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

ggsave("02_disp_idhm_BR.png", disp_idhm_BR,
       width = 6, height = 6, units = "in", dpi = 300)

rm(idhm)

#-------------------------------GII2022 x GII2010-------------------------------

disp_gii_BR <- ggplot(dados_tcc, aes(x = gii2010, y = gii2022, color = nome_regiao)) +
  geom_point(alpha = 0.5, size = 1.6) +
  # reta de regressão por região (uma por cor)
  # geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  # eixos (0 a 1) e mesma escala nos dois
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  # geom_vline(xintercept = 0.4, linetype = "dashed", color = "black", linewidth = 0.6) +
  # geom_hline(yintercept = 0.37, linetype = "dashed", color = "black", linewidth = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "gray60") +
  # paleta e ordem da legenda
  scale_color_manual(
    values = c("Centro-Oeste" = "#2F3E46",
               "Nordeste"     = "#E0A458",
               "Norte"        = "#2A9D8F",
               "Sudeste"      = "#B56576",
               "Sul"          = "#6C757D"),
    breaks = c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul"),
    name = ""
  ) +
  labs(x = "GII 2010", y = "GII 2022", title = "") +
  theme_minimal(11) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9),
    legend.box = "vertical",
    legend.key.width  = unit(10, "pt"),
    legend.key.height = unit(10, "pt"),
    legend.box.margin = margin(t = -6, r = 0, b = 0, l = 0),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(0, "pt"),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 0)),
    plot.background   = element_rect(fill = "white", color = "white"),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "white", color = NA)
  ) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

ggsave("01_disp_gii_BR.png", disp_gii_BR,
       width = 6, height = 6, units = "in", dpi = 300)

rm(disp_gii_BR)

################################################################################
#                                   FUNÇÕES                                    #
################################################################################

# Linha de divisão das seções
divider_line <- function(texto, digito) {
  qtde_character <- nchar(texto)
  metade1 <- round((80-qtde_character)/2,0)
  i = 1
  resultado = "#"
  while (i<metade1) {
    resultado = paste0(resultado,digito)
    i = i + 1
  }
  resultado = paste0(resultado,texto)
  j = nchar(resultado)
  while (j<80) {
    resultado = paste0(resultado,digito)
    j = j + 1
  }
  return(resultado)
}

# Função de resumo
resumir_vetor <- function(x) {
  tibble(
    N       = sum(!is.na(x)),
    media   = mean(x, na.rm = TRUE),
    mediana = median(x, na.rm = TRUE),
    dp      = sd(x, na.rm = TRUE),
    minimo  = min(x, na.rm = TRUE),
    maximo  = max(x, na.rm = TRUE),
    P10     = quantile(x, 0.10, na.rm = TRUE, type = 7),
    P90     = quantile(x, 0.90, na.rm = TRUE, type = 7)
  )
}

# Variação percentual
varpct <- function(v2010, v2022) {
  (v2022 - v2010) / ifelse(v2010 == 0 | is.na(v2010), NA_real_, v2010) * 100
}
