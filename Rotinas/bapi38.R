library(tidyverse)
library(janitor)

#Importando Base
load("C:/Users/gabli/Desktop/r/SIM/Bapi36/sim_doext_12_22.RData")

# Tabela - contribuição de cada região no total ------------------------------------
sim_doext |> filter(intencao_homic == "Homicídio") |>
  tabyl(ano,reg_resd) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "row") |> adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front",format_func = function(x) format(x, big.mark = ".",decimal.mark = ",") )  |>
  rio::export(x = _, "regiões.xlsx" )




# Tabela UFs da região nordeste ---------------------------------------
sim_doext |> filter(intencao_homic == "Homicídio" & reg_resd == "Nordeste") |> droplevels() |>
  tabyl(ano,uf_resd) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "row") |> adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front",format_func = function(x) format(x, big.mark = ".",decimal.mark = ",") )  |>
  rio::export(x = _, "uf_nordeste_resd.xlsx" )


# Série temporal taxa por região ------------------------------------------
#Importando Base
library(tidyverse)
library(janitor)

#Importando a base
load("C:/Users/gabli/Desktop/r/SIM/Bapi36/sim_doext_12_22.RData")


#Contagem de homicídios por UF
sim_doext |> filter(intencao_homic == "Homicídio") |>
  count(ano,reg_resd, name = "homic") -> homic

#Importando população - Geral
data_list <- rio::import_list("D:/Dropbox/Ipea/Atlas/Pop-PNADc.xlsx", 
                              which = seq(from = 1, to = 22, by = 2), 
                              #Indicar as sheets desejadas. Aqui estou pegando a população Geral
                              #Com o passar dos anos precisa alterar o from, indicando a sheet com a primeira pnadc jovem desejada
                              #Com o passar dos anos precisa alterar o by, indicando a sheet com a última pnadc jovem
                              setclass = "tbl")
# Loop para extrair dataframe da list data_list e criar dataframe com o nome do  dataframe na data_list
for(i in seq_along(data_list)) {
  assign(names(data_list)[i], data_list[[i]])
}
rm(data_list,i)

#Aplicar bind_rows a todos os dataframes no ambiente global e juntar as pnadcs em tabela única
pop_pnadc <- do.call(bind_rows, 
                     mget(setdiff(ls(pattern = "^[^.]+$"),c("homic","sim_doext") ) ) ) |> 
  #Selecionando população geral
  select(UF,ano,pop = Pop)

#Removendo pnadcs, exceto pop_pnadc_homem_jovem
rm(list = setdiff(ls(), c("pop_pnadc","homic","sim_doext") ) )

#População PNADc das regiões.
pop_pnadc |> rename(uf = UF) |>
  #Mantém as regiões.
  filter(uf %in% c("Norte","Centro Oeste","Nordeste","Sudeste","Sul") ) |>
  mutate(ano = ano |> as_factor(),
         uf = uf |> as_factor() ) |>
  rename(reg_resd = uf) -> pop_pnadc

#Join homicídios e população.  
left_join(x = homic, y = pop_pnadc, by = join_by("ano","reg_resd") ) -> base
rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(reg_resd="Brasil" |> as.factor(),
                        pop = sum(pop),
                        homic = sum(homic), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round( (homic/pop)*100000,digits = 1) ) |> as.double() ) -> base


#Gráfico das Taxas, por Região
base |>
  #Transforma ano_not em date. Será utilizado no eixo
  mutate(ano = ano |> fct_drop() |> as.character() |> as.numeric(),
         ano = make_date(ano) ) |> 
  ggplot() +
  geom_line(aes(x = ano, y = tx_homic, color = reg_resd, group = reg_resd) ) +
  geom_point(aes(x = ano, y = tx_homic, color = reg_resd) ) +
  ggrepel::geom_text_repel(aes(x = ano, y = tx_homic,label = tx_homic, color = reg_resd), size = 3, show.legend = FALSE) +
  scale_x_date(breaks = scales::breaks_width("1 year"), labels = scales::label_date("'%y") ) +
  scale_y_continuous(breaks = seq(0,50,5) ) +
  guides(color = guide_legend(position = "inside",nrow = 2) ) +
  theme(legend.position.inside = c(0.8, 0.90),
        axis.text.x=element_text(size=8),axis.text.y=element_text(size=8),
        axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
        plot.title = element_text(size = 9), legend.title=element_text(size=8),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(color = "", x = "Ano", y = "Taxa. de Homicídio")
ggsave(filename ="tx_regiões.bmp",width = 8,height = 5,device='bmp', dpi=150)
ggsave(filename ="fig1.eps",width = 8,height = 5, dpi=150)


#Tabela das taxas de homicídio, por região.
base |>
  select(ano, reg_resd, tx_homic) |>
  pivot_wider(names_from = ano, values_from = tx_homic) |>
  #Exportando
  rio::export(x = _, "tx_homic_reg.xlsx")

rm(base, sim_doext)

# Taxas de homicídio nas UFs Nordeste ----------------------------------------------
library(tidyverse)
library(janitor)

#Importando a base
load("C:/Users/gabli/Desktop/r/SIM/Bapi36/sim_doext_12_22.RData")


#Contagem de homicídios por UF
sim_doext |> filter(intencao_homic == "Homicídio" & reg_resd == "Nordeste") |>
  count(ano,uf_resd, name = "homic") -> homic

#Importando população - Geral
data_list <- rio::import_list("D:/Dropbox/Ipea/Atlas/Pop-PNADc.xlsx", 
                              which = seq(from = 1, to = 22, by = 2), 
                              setclass = "tbl")
# Loop para extrair dataframe da list data_list e criar dataframe com o nome do  dataframe na data_list
for(i in seq_along(data_list)) {
  assign(names(data_list)[i], data_list[[i]])
}
rm(data_list,i)

#Aplicar bind_rows a todos os dataframes no ambiente global e juntar as pnadcs em tabela única
pop_pnadc <- do.call(bind_rows, 
                     mget(setdiff(ls(pattern = "^[^.]+$"),c("homic","sim_doext") ) ) ) |> 
  #Selecionando população geral
  select(UF,ano,pop = Pop)

#Removendo pnadcs, exceto pop_pnadc
rm(list = setdiff(ls(), c("pop_pnadc","homic","sim_doext") ) )

#População PNADc das UFs no nordeste
pop_pnadc |> rename(uf_resd = UF) |>
  #Mantém UFs de interese
  filter(uf_resd %in% c("Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Pernambuco",
                        "Piauí", "Rio Grande do Norte", "Sergipe") ) |>
  mutate(ano = ano |> as_factor(),
         uf_resd = uf_resd |> as_factor() ) -> pop_pnadc

#Join homicídios e população.  
left_join(x = homic, y = pop_pnadc, by = join_by("ano","uf_resd") ) -> base
rm(homic,pop_pnadc)

#Acrescentando Total centro oeste e criando taxas.
base %>%
  bind_rows(.|>
              summarise(uf_resd="Nordeste" |> as.factor(),
                        pop = sum(pop),
                        homic = sum(homic), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round( (homic/pop)*100000,digits = 1) ) |> as.double() ) -> base


#Acrescentar taxa Brasil. Fiz na mão, a ideia veio depois de tudo pronto.
base |> 
  select(ano,uf_resd,tx_homic) |> 
  bind_rows(
    #Bind da taxa homic Brasil. Peguei do PDf do Atlas
    tibble(ano = 2012:2022 |> as_factor(),
           uf_resd = "Brasil" |> as_factor(),
           tx_homic = c(28.9, 28.8, 30.1, 29.1, 30.6, 31.8, 27.9, 21.7, 23.6, 22.5, 21.7) ) ) -> base

base |>
  #Transforma ano_not em date. Será utilizado no eixo
  mutate(ano = ano |> fct_drop() |> as.character() |> as.numeric(),
         ano = make_date(ano) ) |> 
  ggplot() + 
  geom_line(aes(x = ano, y = tx_homic, color = uf_resd, group = uf_resd), show.legend = FALSE )+
  geom_point(aes(x = ano, y = tx_homic, color = uf_resd), show.legend = FALSE ) +
  facet_wrap(vars(uf_resd), scales = "free") +
  #ggrepel::geom_text_repel(aes(x = ano, y = tx_homic,label = tx_homic, color = reg_resd), size = 3, show.legend = FALSE) +
  scale_x_date(breaks = scales::breaks_width("1 year"), labels = scales::label_date("'%y") ) +
  #scale_y_continuous(breaks = seq(0,50,5) ) +
  #guides(color = guide_legend(position = "inside",nrow = 2) ) +
  theme(
        axis.text.x=element_text(size=8),axis.text.y=element_text(size=8),
        axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
        plot.title = element_text(size = 9), legend.title=element_text(size=8),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(color = "", x = "Ano", y = "Taxa. de Homicídio.")
ggsave(filename ="tx_ufs.bmp",width = 10,height = 5,device='bmp', dpi=150)
ggsave(filename ="fig2.eps",width = 8,height = 5, dpi=150)



#Contagem de homicídios por UF
sim_doext |> filter(intencao_homic == "Homicídio" & reg_resd == "Nordeste") |>
  count(ano,uf_resd, name = "homic") -> homic

#Importando população - Geral
data_list <- rio::import_list("D:/Dropbox/Ipea/Atlas/Pop-PNADc.xlsx", 
                              which = seq(from = 1, to = 22, by = 2), 
                              setclass = "tbl")
# Loop para extrair dataframe da list data_list e criar dataframe com o nome do  dataframe na data_list
for(i in seq_along(data_list)) {
  assign(names(data_list)[i], data_list[[i]])
}
rm(data_list,i)

#Aplicar bind_rows a todos os dataframes no ambiente global e juntar as pnadcs em tabela única
pop_pnadc <- do.call(bind_rows, 
                     mget(setdiff(ls(pattern = "^[^.]+$"),c("homic","sim_doext") ) ) ) |> 
  #Selecionando população geral
  select(UF,ano,pop = Pop)

#Removendo pnadcs, exceto pop_pnadc
rm(list = setdiff(ls(), c("pop_pnadc","homic","sim_doext") ) )

#População PNADc das UFs no nordeste
pop_pnadc |> rename(uf_resd = UF) |>
  #Mantém UFs de interese
  filter(uf_resd %in% c("Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Pernambuco",
                        "Piauí", "Rio Grande do Norte", "Sergipe") ) |>
  mutate(ano = ano |> as_factor(),
         uf_resd = uf_resd |> as_factor() ) -> pop_pnadc

#Join homicídios e população.  
left_join(x = homic, y = pop_pnadc, by = join_by("ano","uf_resd") ) -> base
rm(homic,pop_pnadc)

#Acrescentando Total centro oeste e criando taxas.
base %>%
  bind_rows(.|>
              summarise(uf_resd="Nordeste" |> as.factor(),
                        pop = sum(pop),
                        homic = sum(homic), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round( (homic/pop)*100000,digits = 1) ) |> as.double() ) -> base


#Acrescentar taxa Brasil. Fiz na mão, a ideia veio depois de tudo pronto.
base |> 
  select(ano,uf_resd,tx_homic) |> 
  bind_rows(
    #Bind da taxa homic Brasil. Peguei do PDf do Atlas
    tibble(ano = 2012:2022 |> as_factor(),
           uf_resd = "Brasil" |> as_factor(),
           tx_homic = c(28.9, 28.8, 30.1, 29.1, 30.6, 31.8, 27.9, 21.7, 23.6, 22.5, 21.7) ) ) -> base

#Gráfico taxa de homicídios nas UFs
base |> 
  filter(!uf_resd %in% c("Brasil", "Nordeste") ) |> 
  #Transforma ano_not em date. Será utilizado no eixo
  mutate(ano = ano |> fct_drop() |> as.character() |> as.numeric(),
         ano = make_date(ano) ) |> 
  ggplot() + 
  geom_line(aes(x = ano, y = tx_homic, color = uf_resd, group = uf_resd) )+
  geom_point(aes(x = ano, y = tx_homic, color = uf_resd), show.legend = FALSE ) +
  #facet_wrap(vars(uf_resd), scales = "free") +
  #ggrepel::geom_text_repel(aes(x = ano, y = tx_homic,label = tx_homic, color = reg_resd), size = 3, show.legend = FALSE) +
  scale_x_date(breaks = scales::breaks_width("1 year"), labels = scales::label_date("'%y") ) +
  guides(color = guide_legend(position = "inside",nrow = 3) ) +
  theme(legend.position.inside = c(0.8,0.9),
    axis.text.x=element_text(size=8),axis.text.y=element_text(size=8),
    axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
    plot.title = element_text(size = 9), legend.title=element_text(size=8),
    legend.key = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(color = "", x = "Ano", y = "Taxa. de Homicídio.")
ggsave(filename ="tx_ufs.bmp",width = 10,height = 5,device='bmp', dpi=150)
ggsave(filename ="fig2.eps",width = 10,height = 5,device='bmp', dpi=150)



#Tabela taxas por UF
base |>
  #Exclusão de Brasil e Região.
  dplyr::filter(!uf_resd %in% c("Brasil","Nordeste") ) |>
  pivot_wider(names_from = ano, values_from = tx_homic) |>
  #Exportando
  rio::export(x = _, "tx_homic_ufs.xlsx")
  


rm(base,sim_doext)

# Taxas estaduais e ranking -----------------------------------------------
library(tidyverse)
library(janitor)

#Importando Base
load("C:/Users/gabli/Desktop/r/SIM/Bapi36/sim_doext_12_22.RData")

#Contagem de homicídios por UF
sim_doext |> filter(intencao_homic == "Homicídio") |>
  count(ano,uf_resd, name = "homic") -> homic

#Importando população - Geral
data_list <- rio::import_list("D:/Dropbox/Ipea/Atlas/Pop-PNADc.xlsx", 
                              which = seq(from = 1, to = 22, by = 2), 
                              setclass = "tbl")
# Loop para extrair dataframe da list data_list e criar dataframe com o nome do  dataframe na data_list
for(i in seq_along(data_list)) {
  assign(names(data_list)[i], data_list[[i]])
}
rm(data_list,i)

#Aplicar bind_rows a todos os dataframes no ambiente global e juntar as pnadcs em tabela única
pop_pnadc <- do.call(bind_rows, 
                     mget(setdiff(ls(pattern = "^[^.]+$"),c("homic","sim_doext") ) ) ) |> 
  #Selecionando população geral
  select(UF,ano,pop = Pop)

#Removendo pnadcs, exceto pop_pnadc_homem_jovem
rm(list = setdiff(ls(), c("pop_pnadc","homic","sim_doext") ) )

#População PNADc das UFs no centro oeste
pop_pnadc |> rename(uf_resd = UF) |>
  mutate(ano = ano |> as_factor(),
         uf_resd = uf_resd |> as_factor() ) -> pop_pnadc

#Join homicídios e população.  
left_join(x = homic, y = pop_pnadc, by = join_by("ano","uf_resd") ) |>
  mutate(tx_homic = format(round( (homic/pop)*100000,digits = 1) ) |> as.double() ) -> base
rm(homic,pop_pnadc)


#Criando ranking
base |>
  #Ranking de município mais violênto no país
  mutate(rank_geral = dense_rank( desc(tx_homic) ), .by = ano ) |>
  filter(ano == 2022) |> view()




# Municípios --------------------------------------------------------------
library(tidyverse)
library(janitor)

#Importando base utilizada no Atlas 2024
readxl::read_excel("D:/Dropbox/Ipea/Atlas/Atlas Munic 2024/bases/homic_munic.xlsx") |>
  #Exclusão mortes no exterior
  drop_na() |>
  #Média móvel de três meses da taxa de homicídio
  mutate(tx_homic_reg = slider::slide_dbl(tx_homic_reg, .f= mean, .before = 2) ) |>
  #Mantém somente 2022 e municípios com po superior a 10k.
  filter(ano == 2022 & pop >= 10000) |> 
  #Variáveis de interesse
  select(uf_resd,pop, munic_resd, tx_homic_reg) |>
  #Criando a região de residência
  mutate(
    reg_resd = as.factor(
      case_when(
        #Região Norte
        uf_resd %in% c("Acre","Amapá","Amazonas","Pará","Rondônia","Roraima", "Tocantins") ~ "Norte",
        #Região Nordeste
        uf_resd %in% c("Alagoas","Bahia","Ceará","Maranhão","Paraíba","Pernambuco","Piauí","Rio Grande do Norte","Sergipe") ~ "Nordeste",
        #Região Centro-Oeste
        uf_resd %in% c("Goiás","Mato Grosso", "Mato Grosso do Sul","Distrito Federal") ~ "Centro Oeste",
        #Região Sudeste
        uf_resd %in% c("Rio de Janeiro","São Paulo","Espírito Santo","Minas Gerais") ~ "Sudeste", TRUE ~ "Sul") ) ) |>
  #Ranking de município mais violênto no país
  mutate(rank_geral = dense_rank( desc(tx_homic_reg) ) ) |> 
  #Agrupa por UF.
  group_by(uf_resd) |>
  #Ranking na UF de municípios violentos.
  mutate(rank_uf = dense_rank(desc(tx_homic_reg) ) ) |> 
  #selecionando os três municípios mais violêntos na UF
  slice_max(order_by = tx_homic_reg, n = 3) |> ungroup() |>
  #Ranking na região
  mutate(rank_reg = dense_rank(desc(tx_homic_reg)), .by = reg_resd) |> 
  #Mantém somente centro oeste
  filter(reg_resd == "Centro Oeste") |> view()




# Taxas municipais, por UF. -----------------------------------------------
library(tidyverse)
library(janitor)

#Importando base utilizada no Atlas 2024
readxl::read_excel("C:/Users/gabli/Dropbox/Ipea/Atlas/Atlas Munic 2024/bases/homic_munic.xlsx") |>
  #Exclusão mortes no exterior
  drop_na() |>
  #Média móvel de três meses da taxa de homicídio
  mutate(tx_homic_reg_mean = slider::slide_dbl(tx_homic_reg, .f= mean, .before = 2) ) |>
  #Mantém somente 2022 e municípios com po superior a 10k.
  filter(pop >= 10000) |> 
  #Variáveis de interesse
  select(uf_resd,ano, munic_resd, tx_homic_reg,tx_homic_reg_mean) |>
  #Criando a região de residência
  mutate(
    reg_resd = as.factor(
      case_when(
        #Região Norte
        uf_resd %in% c("Acre","Amapá","Amazonas","Pará","Rondônia","Roraima", "Tocantins") ~ "Norte",
        #Região Nordeste
        uf_resd %in% c("Alagoas","Bahia","Ceará","Maranhão","Paraíba","Pernambuco","Piauí","Rio Grande do Norte","Sergipe") ~ "Nordeste",
        #Região Centro-Oeste
        uf_resd %in% c("Goiás","Mato Grosso", "Mato Grosso do Sul","Distrito Federal") ~ "Centro Oeste",
        #Região Sudeste
        uf_resd %in% c("Rio de Janeiro","São Paulo","Espírito Santo","Minas Gerais") ~ "Sudeste", TRUE ~ "Sul") ) ) |>
  #Mantém somente centro oeste
  filter(reg_resd == "Centro Oeste") |>  
  #Gráfico
  ggplot() +
  geom_line( aes(x = ano, y = tx_homic_reg, color = uf_resd, group = munic_resd) ) +
  facet_wrap(vars(uf_resd), scales = "free" )





# Variações nas taxas municipais de homicídio -----------------------------
#Importando base utilizada no Atlas 2024
readxl::read_excel("D:/Dropbox/Ipea/Atlas/Atlas Munic 2024/bases/homic_munic.xlsx") |> 
  #Exclusão mortes no exterior
  drop_na() |>
  #Média móvel de três meses da taxa de homicídio
  mutate(tx_homic_reg = slider::slide_dbl(tx_homic_reg, .f= mean, .before = 2) ) |>
  #Mantém municípios da região centro oeste
  filter(stringr::str_extract(codmunres, "^.{2}") %in% c(50,51,52,53) ) |>
  #Mantém somente 2014 e 2022 e municípios com po superior a 10k.
  filter(ano %in% c(2014,2022) ) |> 
  #Variáveis de interesse
  select(uf_resd,ano, munic_resd, tx_homic_reg) |>
  #Variações. Tabela no formato wider. Facilitar criar variação
  pivot_wider(names_from = ano, values_from = tx_homic_reg, names_prefix = "tx_") |>
  mutate(g_var = round(  ( (tx_2022/tx_2014)*100),1 ) ) |> 
#Gráfico
ggplot() +
  geom_point(aes(x = tx_2014, y = g_var, color = uf_resd) )






# Tabela estatísticas descritivas --------------------------------------------------
library(tidyverse)
library(janitor)
library(gtsummary)
library(kableExtra)

#Importando Base
load("C:/Users/gabli/Desktop/r/SIM/Bapi36/sim_doext_12_22.RData") 

theme_gtsummary_language(language  = "pt", big.mark = ".",decimal.mark=",")
sim_doext |> filter(intencao_homic == "Homicídio" & reg_resd == "Nordeste") |>
  #Acrescentando raçacor Negra nos microdado so SIM 
  mutate(racacor = case_match(racacor,
                              c("Parda","Preta") ~ "Negra", .default = racacor)  ) |> droplevels() |>
  select(c(uf_resd, racacor,idade,esc,estciv,local_obito, instrumento, sexo) ) |>
  #Declarando a tabela
  tbl_summary(
    by = uf_resd, 
    
    percent = "column",
    
    #missing_text = "(Missing)",
    missing = "no",
    
    #Alterando o label das variáveis 
    label = list(idade ~ "Idade", racacor ~ "Raça\\cor", sexo ~ "Sexo", estciv ~ "Estado Civil", esc ~ "Escolaridade",
                 local_obito ~ "Local do incidente",instrumento ~ "Instrumento"),
    
    type = all_continuous() ~ "continuous2",
    #Alterando estatísticas utilizadas
    statistic = all_continuous() ~ c(
      "{N_nonmiss} ({p_nonmiss}%)",
      "{N_miss} ({p_miss}%)",
      "{mean} ({sd})",
      "{median} ({p25}, {p75})",
      "{min} - {max}"),
    
    #Alterando número de dígitos em cada linha das estatísticas de idade.
    digits = list(idade ~ c(0,1,0,1,1,1,1,1,0,0) ) ) |>
  
  #Adicionando coluna com valores totais 
  add_overall(col_label = "**Centro Oeste**  \nN = {style_number(N)}",last = TRUE) |>
  
  #Alterando label das estatísticas de idade.
  add_stat_label(label = idade ~ c("N(%)", "N faltante (%)", "Média (SD)", "Mediana (IQR)", "Min - Max")) |>
  
  modify_header(#label = "**Características**",   # update the column header
    #Alterando as estatísticas dos totais apresentados no topo da tabela.
    all_stat_cols() ~ "**{level}** \n N = {scales::number(n)} ({style_percent(p,digits = 1)}%)") |> bold_labels() |>
  as_hux_xlsx("example_gtsummary2.xlsx")





# Ocupação ----------------------------------------------------------------
library(tidyverse)
library(janitor)
#Importando Base
load("C:/Users/gabli/Desktop/r/SIM/Bapi36/sim_doext_12_22.RData")

#Total das ocupações conhecidas
sim_doext |>
  filter(intencao_homic == "Homicídio" & reg_resd == "Nordeste") |>
  tabyl(ocupa) |> arrange(desc(n)) |> head()
1-0.47704467 #Então, 52,2% das ocupações são conhecidas.


#Aqui 80% das ocupações conhecidas.
sim_doext |>
  filter(intencao_homic == "Homicídio" & ocupa!= "Missing" & reg_resd == "Nordeste") |>
  droplevels() |>
  tabyl(ocupa) |> arrange(desc(n)) |> head(17) |>  adorn_totals() |> adorn_pct_formatting() |> 
  rio::export(x=_,"ocupa.xlsx")




# Histograma Idade x Instrumento ------------------------------------------
library(tidyverse)
library(janitor)
#Importando Base
load("C:/Users/gabli/Desktop/r/SIM/Bapi36/sim_doext_12_22.RData")

#Histograma de idade por instrumento
sim_doext |> 
  filter(!is.na(idade) & reg_resd == "Centro Oeste" & intencao_homic == "Homicídio") |>
  ggplot() +
  geom_histogram(aes(x=idade,fill = instrumento), binwidth  = 3,color = "white") +
  #Idade Média, por UF de residência
  geom_vline(data = sim_doext |>
               filter(!is.na(idade) & reg_resd == "Centro Oeste" & intencao_homic == "Homicídio") |> 
               summarise(idade = mean(idade), .by = c(uf_resd)),
             aes(xintercept = mean(idade) ),color = "red",linewidth = 0.5, linetype="dashed")  +
  facet_wrap(vars(uf_resd), scales = "free") +
  labs(x="", y = "Frequência",fill = "") + 
  theme(legend.position = "bottom") + guides(fill = guide_legend(nrow = 2))
ggsave(filename ="idade_instr.bmp",width = 8,height = 5,device='bmp', dpi=150)





# Gráfico de Barra - Faixa Etária e instrumento ---------------------------
library(tidyverse)
library(janitor)
library(ggstats)
#Importando Base
load("C:/Users/gabli/Desktop/r/SIM/Bapi36/sim_doext_12_22.RData")

#Contagem de homicídios na faixa etária
sim_doext |> 
  filter(intencao_homic == "Homicídio" & !is.na(idade) & reg_resd == "Centro Oeste") |>
  mutate(
    #Idade até 90 anos 
    idade = case_when(idade >= 90 ~ 90, .default = idade),    
    #Faixa etária
    fxet = (idade %/% 5) * 5 ) |> 
  #Contagem
  count(uf_resd, fxet, name = "n_fext") |> 
  add_count(uf_resd,  wt = n_fext, name = "homic") |> 
  mutate(p_fxet = round( (n_fext/homic)*100,1 ) ) -> counts

#Gráfico de faixa etária e instrumento
sim_doext |> 
  filter(intencao_homic == "Homicídio" & !is.na(idade) & reg_resd == "Centro Oeste") |>
  mutate(
    #Idade até 90 anos 
    idade = case_when(idade >= 90 ~ 90, .default = idade),    
    #Faixa etária
    fxet = (idade %/% 5) * 5 ) |> 
  #Contagem de notificações, por sexo e tipo de violência.
  count(uf_resd,fxet,instrumento) |> 
  #Precisa converter em factor para aparecer no eixo x, ao fazer o facet
  mutate(fxet = fxet |> as_factor() ) |>
  
  #Gráfico
  ggplot( aes(x = fxet, fill = instrumento, 
              weight = n, by = fxet ) ) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(0.5), size = 2.5 ) +
  #Por UF de residência
  facet_wrap(vars(uf_resd), scales = "free") +
  
  scale_x_discrete( 
    
    labels = paste(counts$fxet, "\n n = ", counts$n_fext, "\n", paste(counts$p_fxet,"%") ) ) + 
  
  scale_y_continuous(labels =  scales::percent) +
  theme(legend.position = "bottom") +
  labs(x = "Faixa Etária",
       y = "Proporção (%)",
       fill = "") 
ggsave(filename ="fig5.eps",width = 15,height = 10,device=cairo_ps, dpi=150)

rm(counts)  



# Gráfico de Barras - Idade e Instrumento, por UF -------------------------
#Fazendo por UF
library(tidyverse)
library(janitor)
library(ggstats)
#Importando Base
load("C:/Users/gabli/Desktop/r/SIM/Bapi36/sim_doext_12_22.RData")

#Goiás
#Contagem de homicídios por faixa etária e UF
sim_doext |> 
  filter(intencao_homic == "Homicídio" & !is.na(idade) & uf_resd == "Goiás") |>
  mutate(
    #Idade até 90 anos 
    idade = case_when(idade >= 90 ~ 90, .default = idade),    
    #Faixa etária
    fxet = (idade %/% 5) * 5 ) |> 
  #Contagem
  count(uf_resd, fxet, name = "n_fext") |> 
  add_count(uf_resd,  wt = n_fext, name = "homic") |> 
  mutate(p_fxet = round( (n_fext/homic)*100,1 ) ) -> counts

#Gráfico de faixa etária e instrumento
sim_doext |> 
  filter(intencao_homic == "Homicídio" & !is.na(idade) & uf_resd == "Goiás") |>
  mutate(
    #Idade até 90 anos 
    idade = case_when(idade >= 90 ~ 90, .default = idade),    
    #Faixa etária
    fxet = (idade %/% 5) * 5 ) |> 
  #Contagem de notificações, por sexo e tipo de violência.
  count(uf_resd,fxet,instrumento) |> 
  #Precisa converter em factor para aparecer no eixo x, ao fazer o facet
  mutate(fxet = fxet |> as_factor() ) |>
  
  #Gráfico
  ggplot( aes(x = fxet, fill = instrumento, 
              weight = n, by = fxet ) ) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(0.5), size = 2.5 ) +
  
  scale_x_discrete( 
    labels = paste(counts$fxet, "\n n = ", counts$n_fext, "\n", paste(counts$p_fxet,"%") ) ) + 
  
  scale_y_continuous(labels =  scales::percent) +
  
  guides(fill = guide_legend(nrow = 1)) +
  
  theme(legend.position = "bottom",
        axis.text.x=element_text(size=6.75),axis.text.y=element_text(size=8),
        axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
        plot.title = element_text(size = 9,face="bold"),
        legend.text=element_text(size=8), 
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(title = "Goiás", x = "", y = "Proporção (%)", fill = "") -> g1
rm(counts)  

#Distrito Federal
#Contagem de homicídios por faixa etária e UF
sim_doext |> 
  filter(intencao_homic == "Homicídio" & !is.na(idade) & uf_resd == "Distrito Federal") |>
  mutate(
    #Idade até 90 anos 
    idade = case_when(idade >= 90 ~ 90, .default = idade),    
    #Faixa etária
    fxet = (idade %/% 5) * 5 ) |> 
  #Contagem
  count(uf_resd, fxet, name = "n_fext") |> 
  add_count(uf_resd,  wt = n_fext, name = "homic") |> 
  mutate(p_fxet = round( (n_fext/homic)*100,1 ) ) -> counts

#Gráfico de faixa etária e instrumento
sim_doext |> 
  filter(intencao_homic == "Homicídio" & !is.na(idade) & uf_resd == "Distrito Federal") |>
  mutate(
    #Idade até 90 anos 
    idade = case_when(idade >= 90 ~ 90, .default = idade),    
    #Faixa etária
    fxet = (idade %/% 5) * 5 ) |> 
  #Contagem de notificações, por sexo e tipo de violência.
  count(uf_resd,fxet,instrumento) |> 
  #Precisa converter em factor para aparecer no eixo x, ao fazer o facet
  mutate(fxet = fxet |> as_factor() ) |>
  
  #Gráfico
  ggplot( aes(x = fxet, fill = instrumento, 
              weight = n, by = fxet ) ) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(0.5), size = 2.5 ) +
  
  scale_x_discrete( 
    labels = paste(counts$fxet, "\n n = ", counts$n_fext, "\n", paste(counts$p_fxet,"%") ) ) + 
  
  scale_y_continuous(labels =  scales::percent) +
  
  guides(fill = guide_legend(nrow = 1)) +
  
  theme(legend.position = "bottom",
        axis.text.x=element_text(size=6.75),axis.text.y=element_text(size=8),
        axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
        plot.title = element_text(size = 9,face="bold"),
        legend.text=element_text(size=8), 
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(title = "Distrito Federal", x = "", y = "Proporção (%)", fill = "")  -> g2
rm(counts)  


#Mato Grosso
#Contagem de homicídios por faixa etária e UF
sim_doext |> 
  filter(intencao_homic == "Homicídio" & !is.na(idade) & uf_resd == "Mato Grosso") |>
  mutate(
    #Idade até 90 anos 
    idade = case_when(idade >= 90 ~ 90, .default = idade),    
    #Faixa etária
    fxet = (idade %/% 5) * 5 ) |> 
  #Contagem
  count(uf_resd, fxet, name = "n_fext") |> 
  add_count(uf_resd,  wt = n_fext, name = "homic") |> 
  mutate(p_fxet = round( (n_fext/homic)*100,1 ) ) -> counts

#Gráfico de faixa etária e instrumento
sim_doext |> 
  filter(intencao_homic == "Homicídio" & !is.na(idade) & uf_resd == "Mato Grosso") |>
  mutate(
    #Idade até 90 anos 
    idade = case_when(idade >= 90 ~ 90, .default = idade),    
    #Faixa etária
    fxet = (idade %/% 5) * 5 ) |> 
  #Contagem de notificações, por sexo e tipo de violência.
  count(uf_resd,fxet,instrumento) |> 
  #Precisa converter em factor para aparecer no eixo x, ao fazer o facet
  mutate(fxet = fxet |> as_factor() ) |>
  
  #Gráfico
  ggplot( aes(x = fxet, fill = instrumento, 
              weight = n, by = fxet ) ) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(0.5), size = 2.5 ) +
  
  scale_x_discrete( 
    labels = paste(counts$fxet, "\n n = ", counts$n_fext, "\n", paste(counts$p_fxet,"%") ) ) + 
  
  scale_y_continuous(labels =  scales::percent) +
  
  guides(fill = guide_legend(nrow = 1)) +
  
  theme(legend.position = "bottom",
        axis.text.x=element_text(size=6.75),axis.text.y=element_text(size=8),
        axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
        plot.title = element_text(size = 9,face="bold"),
        legend.text=element_text(size=8), 
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(title = "Mato Grosso", x = "", y = "Proporção (%)", fill = "")  -> g3
rm(counts)


#Mato Grosso do Sul
#Contagem de homicídios por faixa etária e UF
sim_doext |> 
  filter(intencao_homic == "Homicídio" & !is.na(idade) & uf_resd == "Mato Grosso do Sul") |>
  mutate(
    #Idade até 90 anos 
    idade = case_when(idade >= 90 ~ 90, .default = idade),    
    #Faixa etária
    fxet = (idade %/% 5) * 5 ) |> 
  #Contagem
  count(uf_resd, fxet, name = "n_fext") |> 
  add_count(uf_resd,  wt = n_fext, name = "homic") |> 
  mutate(p_fxet = round( (n_fext/homic)*100,1 ) ) -> counts

#Gráfico de faixa etária e instrumento
sim_doext |> 
  filter(intencao_homic == "Homicídio" & !is.na(idade) & uf_resd == "Mato Grosso do Sul") |>
  mutate(
    #Idade até 90 anos 
    idade = case_when(idade >= 90 ~ 90, .default = idade),    
    #Faixa etária
    fxet = (idade %/% 5) * 5 ) |> 
  #Contagem de notificações, por sexo e tipo de violência.
  count(uf_resd,fxet,instrumento) |> 
  #Precisa converter em factor para aparecer no eixo x, ao fazer o facet
  mutate(fxet = fxet |> as_factor() ) |>
  
  #Gráfico
  ggplot( aes(x = fxet, fill = instrumento, 
              weight = n, by = fxet ) ) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(0.5), size = 2.5 ) +
  
  scale_x_discrete( 
    labels = paste(counts$fxet, "\n n = ", counts$n_fext, "\n", paste(counts$p_fxet,"%") ) ) + 
  
  scale_y_continuous(labels =  scales::percent) +
  
  guides(fill = guide_legend(nrow = 1)) +
  
  theme(legend.position = "bottom",
        axis.text.x=element_text(size=6.75),axis.text.y=element_text(size=8),
        axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
        plot.title = element_text(size = 9,face="bold"),
        legend.text=element_text(size=8), 
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(title = "Mato Grosso do Sul", x = "", y = "Proporção (%)", fill = "")  -> g4
rm(counts)



library(ggpubr)
ggarrange(g1,g2,g3,g4, ncol=2,nrow = 2, common.legend = TRUE, legend="bottom")
ggsave(filename ="idade_instrumento2.bmp",width = 20,height = 10,device='bmp', dpi=200)
rm(g1,g2,g3,g4)



# Proporção de jovens homicídio PAF ---------------------------------------
#Fazendo por UF
library(tidyverse)
library(janitor)
#Importando Base
load("C:/Users/gabli/Desktop/r/SIM/Bapi36/sim_doext_12_22.RData")


sim_doext |> 
  filter(intencao_homic == "Homicídio" & !is.na(idade) & 
           reg_resd == "Centro Oeste") |>
  mutate(
    #Idade até 90 anos 
    idade = case_when(idade >= 90 ~ 90, .default = idade),    
    #Faixa etária
    fxet = (idade %/% 5) * 5 ) |> 
  filter(fxet %in% c(15,20,25) & instrumento == "PAF" ) |>
  count(intencao_homic)
#São 20.740 homcídios de jovens por PAF

sim_doext |> 
  filter(intencao_homic == "Homicídio" & between(idade,15,29) &
           reg_resd == "Centro Oeste") |>
  count(intencao_homic)
#São 26.701 homicídios de jovens
20740/26701



# Concentração de homicídios nos municípios -------------------------------
library(tidyverse)
library(janitor)
#Importando Base
load("C:/Users/gabli/Desktop/r/SIM/Bapi36/sim_doext_12_22.RData")
year <- c(2012:2022)


#Municípios com registro de homicídio
sim_doext |> 
  filter(intencao_homic == "Homicídio" & reg_resd == "Nordeste") |>
  count(codmunres) |>
  #Exclusão de município Ignorado
  filter(!codmunres %in% c("500000","510000","520000") ) |> arrange(desc(n) ) |> view()


#Homicídio por ano município de residência
sim_doext |> 
  filter(intencao_homic == "Homicídio" & reg_resd == "Nordeste") |>
  count(ano,codmunres) -> homic

#Fonte: https://www.ibge.gov.br/explica/codigos-dos-municipios.php
base_munics <- readxl::read_excel("D:/Dropbox/Ipea/Atlas/municipios_br.xls",sheet = "munics") |>
  select("Nome_UF", "Código Município Completo","Nome_Município") |> clean_names() |>
  rename(cod_ibge = "codigo_municipio_completo", munic_resd = nome_municipio,uf_resd = nome_uf) |>
  #No microdado do SIM. A partir de 2006 o código do município aparece com 6. Vou deixar todos os municípios em todos os anos com 6 dígitos.
  mutate(cod_ibge = substr(cod_ibge,1,6)) 


#Adicionando município Ignorado ou exterior. 
#A saúde utiliza código de município ingorado. Esses municípios não aparecem em outras bases.
munics_ign <- tribble(~cod_ibge,~munic_resd, ~uf_resd,
                      "000000", "Ignorado ou exterior", "Ignorado ou exterior",
                      "110000", "Município ignorado - RO", "Rondônia",
                      "130000", "Município ignorado - AM", "Amazonas", 
                      "150000", "Município ignorado - PA", "Pará", 
                      "210000", "Município ignorado - MA", "Maranhão",
                      "170000", "Município ignorado - TO", "Tocantins",
                      "240000", "Município ignorado - RN", "Rio Grande do Norte",
                      "260000" ,"Município ignorado - PE", "Pernambuco",
                      "280000", "Município ignorado - SE", "Sergipe",
                      "310000", "Município ignorado - MG", "Minas Gerais",
                      "330000", "Município ignorado - RJ", "Rio de Janeiro",
                      "410000", "Município ignorado - PR", "Paraná",
                      "430000", "Município ignorado - RS", "Rio Grande do Sul",
                      "510000", "Município ignorado - MT", "Mato Grosso",
                      "520000", "Município ignorado - GO", "Goiás",
                      "120000", "Município ignorado - AC", "Acre",        
                      "140000", "Município ignorado - RR", "Roraima",
                      "160000", "Município ignorado - AP", "Amapá",  
                      "220000", "Município ignorado - PI", "Piauí",
                      "230000", "Município ignorado - CE", "Ceará",  
                      "250000", "Município ignorado - PB", "Paraíba",
                      "270000", "Município ignorado - AL", "Alagoas",
                      "290000", "Município ignorado - BA", "Bahia",
                      "320000", "Município ignorado - ES", "Espírito Santo",
                      "350000", "Município ignorado - SP", "São Paulo",
                      "420000", "Município ignorado - SC", "Santa Catarina",
                      "500000", "Município ignorado - MS",  "Mato Grosso do Sul")

#Não estão incluídios os municípios que são bairros presentes no início da série.
#Por exemplo, Rio de Janeiro.

#Bind de municípios conhecidos e ignorados.
bind_rows(base_munics,munics_ign) |>
  #Mantém somente municípios da região centro oeste
  filter(uf_resd %in% c("Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Pernambuco",
                        "Piauí", "Rio Grande do Norte", "Sergipe") ) -> base_munics
rm(munics_ign)

#Painel de dados com os municípios do centro oeste
base_munics |>
  crossing(year) |>
  #Transforma year em factor. Por causa do joint
  mutate(year = year |> as_factor() ) -> base_munics

#Join homicídios e nome dos municípios
left_join(x = homic, y = base_munics, by = join_by("codmunres" == "cod_ibge", "ano" == "year") ) |>
  #Ordem das colunas
  select(uf_resd,codmunres,munic_resd,ano,n) |>
  
  #Exclusão de município Ignorado
  filter(!codmunres %in% c("290000","270000", "250000", "230000", "220000",
                           "280000", "260000", "240000", "210000") ) |> 
  
  #Adicionar o total de homicídios, por ano
  add_count(ano, wt = n, name = "n_homic") |> 
  
  #group por ano
  group_by(ano) |>
  
  #Ordem descrecente dos homicídio, por ano
  arrange(desc(n), .by_group = TRUE) |> 
  
  
  #Acumulado de homicídios
  mutate(sum_acumulada = cumsum(n),
         p_acum =  round( (sum_acumulada/n_homic)*100,1),
         #Criando id, por ano
         id = row_number()) -> base
rm(base_munics,homic)  


base |> 
  select(ano,p_acum,id) |>
  filter(between(p_acum,79.9,80.1) )
#Em 2012 44 municípios acumulam 80% dos homicídios
#Em 2022 98 municípios acumulam 80% dos homicídios


base |> 
  select(ano,p_acum,id) |>
  filter(between(p_acum,49.1,50.1) )


#Gráfico acumulado de homicídios, por acumulado de municípios.
base |>
  #Número de municípios, por ano
  mutate(idd = length(id),
         p_idd = round( (id/idd)*100,1) ) |>
  filter(ano %in% c(2012,2014,2016,2018,2020,2022)) |>
  ggplot() +
  
  geom_hline(yintercept = 50, linetype = "dashed", alpha = 0.5) +
  
  geom_point(aes(x =p_idd, y = p_acum, color = ano) ) +  
  
  scale_x_continuous(breaks = seq(0,100,5), labels = scales::label_percent(scale = 1)  ) +
  
  scale_y_continuous(breaks = seq(0,100,5), labels = scales::label_percent(scale = 1)) + 
  
  guides(color = guide_legend(position = "inside", nrow = 3)  ) +  
  
  theme(legend.position.inside = c(0.8, 0.2),
        axis.text.x=element_text(size=8),axis.text.y=element_text(size=8),
        axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
        legend.text=element_text(size=8), 
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x = "%Acumulado de Municípios", y = "%Acumulado de Homicídios", color = "")



#Gráfico acumulado de homicídios, por acumulado de municípios.
#Filtro no acumulado de homicídios
base |>
  #Número de municípios, por ano
  mutate(idd = length(id),
         p_idd = round( (id/idd)*100,1) ) |>
  filter(ano %in% c(2012,2014,2016,2018,2020,2022) & p_acum <= 90.1 ) |> 
  ggplot() +
  
  geom_hline(yintercept = 50, linetype = "dashed", alpha = 0.5) +
  
  geom_point(aes(x =p_idd, y = p_acum, color = ano) ) +  
  facet_wrap(vars(uf_resd), scales = "free") +
  
  scale_x_continuous(breaks = seq(0,100,2.5), labels = scales::label_percent(scale = 1)  ) +
  
  scale_y_continuous(breaks = seq(0,100,5), labels = scales::label_percent(scale = 1)) + 
  
  guides(color = guide_legend(position = "inside", nrow = 3)  ) +  
  
  theme(legend.position.inside = c(0.8, 0.2),
        axis.text.x=element_text(size=6),axis.text.y=element_text(size=6),
        axis.title.x=element_text(size=7),axis.title.y=element_text(size=7),
        legend.text=element_text(size=10), 
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x = "%Acumulado de Municípios", y = "%Acumulado de Homicídios", color = "")
ggsave(filename ="acum_homic_munic.bmp",width = 8,height = 5,device='bmp', dpi=150)
ggsave(filename ="fig3.eps",width = 8,height = 5,device=cairo_ps, dpi=150)



# Municípios com maiores taxas --------------------------------------------
library(tidyverse)
library(janitor)
#Importando base
#load("C:/Users/gabli/Desktop/r/SIM/Bapi36/sim_doext_12_22.RData")

#Importando taxas
readxl::read_excel("D:/Dropbox/Ipea/Atlas/Atlas Munic 2024/bases/homic_munic.xlsx") |>
  clean_names() |> 
  #Mantém UFs do Centro Oeste
  filter(uf_resd %in% c("Distrito Federal","Mato Grosso", "Mato Grosso do Sul", "Goiás") ) |>
  #Variáveis utilizadas
  select(uf_resd,codmunres,munic_resd,ano,tx_homic_reg) |>
  #Drop de NAs na taxa de homicídio. São os municípios ignorados
  drop_na(tx_homic_reg) |>
  #Ano como factor. 
  mutate(rol_tx_homic = slider::slide_dbl(tx_homic_reg, .f= mean, .before = 1, .complete = TRUE),
         ano = ano |> as_factor(),
         munic_resd = munic_resd |> as_factor(), .by = munic_resd) |>
  filter(uf_resd == "Goiás" & ano!= 2012) |> 
  #Gráfico
  ggplot() +
  geom_line(aes(x = ano, y = rol_tx_homic, group = munic_resd, color = munic_resd), show.legend = FALSE ) +
  geom_point(aes(x = ano, y = rol_tx_homic, group = munic_resd, color = munic_resd), show.legend = FALSE ) +
  facet_wrap(vars(ano), scales = "free")


# Mapa das Taxas ----------------------------------------------------------
library(tidyverse)
library(janitor)
library(geobr)
library(sf)
#Importando base
load("C:/Users/gabli/Desktop/r/SIM/Bapi36/sim_doext_12_22.RData")

#Importa coordenadas para o mapa de municípios.
sf_munics <- read_municipality(year = 2022) |>
  #Somente região centro oeste 
  filter(name_region == "Centro Oeste") |>
  #Redução do cod_ibge para seis dígitos.
  mutate(code_muni = code_muni |> as.character() |> str_sub(1,6) ) |>
  #Adicionando ano e transformar em painel
  expand_grid(ano = 2012:2022 |> as_factor() ) |> st_as_sf()


#Importando taxas
homic <- readxl::read_excel("D:/Dropbox/Ipea/Atlas/Atlas Munic 2024/bases/homic_munic.xlsx") |>
  clean_names() |> 
  #Mantém UFs do Centro Oeste
  filter(uf_resd %in% c("Distrito Federal","Mato Grosso", "Mato Grosso do Sul", "Goiás") ) |>
  #Variáveis utilizadas
  select(codmunres,ano,tx_homic_reg) |>
  #Drop de NAs na taxa de homicídio. São os municípios ignorados
  drop_na(tx_homic_reg) |>
  #Ano como factor. Para o join
  mutate(ano = ano |> as_factor() )


#Join de homicídios e sf_munics.
left_join(x = sf_munics,  #Tenho todos os municípios conhecidos
          y = homic, #Municípios conhecidos  
          #Os municípios ignorados serão excluídos.
          by = join_by("code_muni" == "codmunres", "ano") ) -> base

#Elimina base não utilizadas.
rm(homic,sf_munics)


### Gráfico para UFs e Períodos seelcioandos
# Definir os estados e anos de interesse
states <- c("Goiás", "Mato Grosso")
years <- c(2012, 2016, 2018, 2022)
# Criar todas as combinações de estados e anos
combinations <- expand_grid(name_state = states, ano = years)

# Aplicar o loop sobre as combinações de 'name_state' e 'ano'
plotlist <- combinations %>%
  pmap(~ {
    base %>%
      filter(name_state == ..1 & ano == ..2) %>%
      ggplot() +
      geom_sf(aes(fill = tx_homic_reg)) +
      scale_fill_gradient(low = "lightgray", high = "#BD262D") +
      theme(
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        plot.title = element_text(size = 6, face = "bold"),
        legend.text = element_text(size = 5), 
        legend.key.size = unit(0.5, 'cm'),
        legend.key.width = unit(0.25, 'cm'),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = 'transparent')
      ) +
      labs(title = paste(..1, "- Ano:", ..2), fill = "")
  })
library(cowplot)
output <- plot_grid(plotlist = plotlist, ncol = 2, nrow = 4, rel_widths = c(1, 0.95, 0.72))
output
ggsave(filename ="mapa_tx_homic_ufs.bmp",width = 8,height = 5,device='bmp', dpi=150)
rm(combinations,states,years,output)


### Gráfico para cada UF selecionada
#Goiás
plotlist <- base %>%
  #Mantém anos desejados
  #filter(ano %in% c(2012,2014,2019, 2022) ) |>
  
  distinct(ano) %>%             # Extrair os anos únicos
  
  pull(ano) %>%                 # Pegar os anos como vetor
  
  map(~ {
    base %>%
      filter(name_state == "Goiás" & ano == .x) %>%
      ggplot() +
      geom_sf(aes(fill = tx_homic_reg)) +
      #scale_fill_gradient(low = "lightgray", high = "#BD262D") +
      scale_fill_viridis_c(option = "plasma",  direction = -1) +
      theme(
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        plot.title = element_text(size = 6, face = "bold"),
        legend.text=element_text(size=5), 
        legend.key.size = unit(0.5, 'cm'),legend.key.width= unit(0.25, 'cm'),
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = 'transparent')
      ) +
      labs(title = paste("Goiás - Ano:", .x), fill = "")
  } )

library(cowplot)
output <- plot_grid(plotlist = plotlist, ncol = 2, nrow = 5, rel_widths = c(1, 0.95, 0.72))
output
ggsave(filename ="goias.bmp",width = 8,height = 10,device='bmp', dpi=150)
rm(plotlist,output)

#Mato Grosso
base |> 
  filter(name_state == "Mato Grosso" & ano %in% c(2012,2022 ) ) |>
  ggplot() +
  geom_sf( aes(fill = tx_homic_reg ) ) +
  #scale_fill_gradient(low = "lightgray", high = "darkblue") +
  scale_fill_viridis_c(option = "plasma", name = "Taxa de Homicídios",direction = -1) +
  facet_wrap(vars(ano) ) +
  theme(
    legend.key = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    plot.title = element_text(size = 6, face = "bold"),
    legend.text=element_text(size=5), 
    legend.key.size = unit(0.5, 'cm'),legend.key.width= unit(0.25, 'cm'),
    axis.title = element_blank(), axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = 'transparent') ) +
  labs( fill = "") 



#Mato Grosso do sul
base |> 
  filter(name_state == "Mato Grosso do Sul" & ano %in% c(2012,2022 ) ) |>
  ggplot() +
  geom_sf( aes(fill = tx_homic_reg ) ) +
  scale_fill_gradient(low = "lightgray", high = "darkblue") +
  facet_wrap(vars(name_state, ano) ) +
  labs(fill = "") +
  theme(
    legend.key = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    axis.title=element_blank(), axis.text=element_blank(),
    axis.ticks=element_blank() ) 

#Gráficos juntos
library(patchwork)
(g1 + g2) / (g3 + g4)
ggsave(filename ="Tx-homic.bmp",width = 9,height = 5,device='bmp', dpi=250)
rm(g1,g2,g3,g4,base,homic)


#Homicídio no início do período e no final do período
#Join de homicídios e sf_munics
left_join(x = sf_munics,
          y = homic, by = join_by("code_muni" == "codmunocor", "ano") )  |>
  filter(ano %in% c(2010, 2022)) |>
  ggplot() + 
  geom_sf(aes(fill = desc(n) ) ) +
  facet_wrap(vars(name_state,ano) ) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank() )




base |>
  ggplot() + geom_sf(aes(fill = n) )
rm(base,homic)


#Homicídios média de 3 anos
sim_doext |>
  #Somente região Centro Oeste
  filter(reg_ocor == "Centro Oeste") |>
  #Contagem de homicídios, por codmunocor
  count(ano, codmunocor,intencao, .drop = FALSE) |> 
  #Mantém somente Homicídios
  filter(intencao == "Homicídio") -> homic
# #Média de três anos de homicídios no município
# mutate(n_rol = slider::slide_dbl(n,.f=mean,.before = 2,.after = 0), .by = codmunocor) |>
# arrange(codmunocor) 

#Join de homicídios e sf_munics
left_join(x = sf_munics, y = homic, by = join_by("code_muni" == "codmunocor", "ano") ) -> base
rm(homic)

base |>
  #Mantém anos de interesse
  filter(ano %in% c(2010,2014,2018,2022) & name_state == "Distrito Federal") |> 
  ggplot() +
  geom_sf(aes(fill = n) ) + 
  facet_wrap(vars(name_state,ano)) + 
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank() )

rm(base,homic)


# Análise espacial --------------------------------------------------------
library(tidyverse)
library(janitor)
library(geobr)
library(sf)
#Importando base
load("C:/Users/gabli/Desktop/r/SIM/Bapi36/sim_doext_12_22.RData")

#Importa coordenadas para o mapa de municípios.
sf_munics <- read_municipality(year = 2022) |>
  #Somente região centro oeste 
  filter(name_region == "Centro Oeste") |>
  #Redução do cod_ibge para seis dígitos.
  mutate(code_muni = code_muni |> as.character() |> str_sub(1,6) ) |>
  #Adicionando ano e transformar em painel
  expand_grid(ano = 2012:2022 |> as_factor() ) |> st_as_sf()


#Importando taxas
homic <- readxl::read_excel("C:/Users/gabli/Dropbox/Ipea/Atlas/Atlas Munic 2024/bases/homic_munic.xlsx") |>
  clean_names() |> 
  #Mantém UFs do Centro Oeste
  filter(uf_resd %in% c("Distrito Federal","Mato Grosso", "Mato Grosso do Sul", "Goiás") ) |>
  #Variáveis utilizadas
  select(codmunres,ano,tx_homic_reg) |>
  #Drop de NAs na taxa de homicídio. São os municípios ignorados
  drop_na(tx_homic_reg) |>
  #Ano como factor. Para o join
  mutate(ano = ano |> as_factor() )


#Join de homicídios e sf_munics.
left_join(x = sf_munics,  #Tenho todos os municípios conhecidos
          y = homic, #Municípios conhecidos  
          #Os municípios ignorados serão excluídos.
          by = join_by("code_muni" == "codmunres", "ano") ) -> base

#Elimina base não utilizadas.
rm(homic,sf_munics)


#Correlação entre a distância de cada município as capitais e a taxa de homicídios.

# Suponha que 'base' seja sua base de dados com geometrias de municípios
# Filtro para os centros urbanos definidos
centros_sf <- base %>% 
  filter(name_muni == "Campo Grande")

# Calculando a distância de cada município ao centro urbano mais próximo
dist <- base %>% filter(name_state == "Mato Grosso do Sul" & name_muni != "Campo Grande" ) |>
  mutate(dist_centro_urbano = st_distance(geom, st_union(centros_sf$geom)) %>% 
           units::set_units("km") %>% as.numeric() )  

# Visualizar as distâncias calculadas
head(dist$dist_centro_urbano)

# Calcular a correlação entre distância ao centro urbano e a taxa de homicídios
correlacao <- cor(dist$dist_centro_urbano, dist$tx_homic_reg, use = "complete.obs")
print(paste("Correlação entre distância ao centro urbano e taxa de homicídios: ", correlacao))
rm(centros_sf,correlacao)


# Gráfico de dispersão entre distância e taxa de homicídios
dist |> 
  ggplot( aes(x = dist_centro_urbano, y = tx_homic_reg) ) +
  geom_point(aes(color = ano), show.legend = FALSE ) +
  geom_smooth(method = "lm", color = "blue", se = FALSE, linetype = "solid", lineend = "round") +  
  facet_wrap(vars(ano), scales = "free") +
  theme(
    axis.text.x=element_text(size=6),axis.text.y=element_text(size=6),
    axis.title.x=element_text(size=7),axis.title.y=element_text(size=7)  ) +
  labs(x = "Distância a Capital (km)",
       y = "Taxa de Homicídios por 100 mil habitantes")
ggsave(filename ="dist-MS.bmp",width = 8,height = 6.5,device='bmp', dpi=150)


rm(dist,centros_sf,correlacao)

# Ajustar um modelo de regressão linear
modelo <- summary(lm(tx_homic_reg ~ dist_centro_urbano, data = dist) )

# Resumo do modelo
summary(modelo)




# Análise das regiões geográficas intermediáras ----------------------------------------------------
library(tidyverse)
library(janitor)

#Importa planilha de municípios do IBGE
#Fonte: https://www.ibge.gov.br/explica/codigos-dos-municipios.php
base_munics <- readxl::read_excel("D:/Dropbox/Ipea/Atlas/municipios_br.xls",sheet = "munics") |>
  clean_names() |>
  select(uf_resd = nome_uf, nome_reg_int = nome_regiao_geografica_intermediaria,
         cod_ibge = codigo_municipio_completo, munic_resd = nome_municipio) |> 
  #Mantém UFs da região Centro Oeste
  filter(uf_resd %in% c("Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Pernambuco",
                        "Piauí", "Rio Grande do Norte", "Sergipe") ) |>
  #No microdado do SIM. A partir de 2006 o código do município aparece com 6. Vou deixar todos os municípios em todos os anos com 6 dígitos.
  mutate(cod_ibge = substr(cod_ibge,1,6)) 


#Importando municípios de interesse.
homic <- readxl::read_excel("D:/Dropbox/Ipea/Atlas/Atlas Munic 2024/bases/homic_munic.xlsx") |>
  clean_names() |> 
  #Mantém UFs do Centro Oeste
  filter(uf_resd %in% c("Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Pernambuco",
                        "Piauí", "Rio Grande do Norte", "Sergipe") ) |>
  #Variáveis utilizadas. Seleciona valor absoluto para fazer a taxa por região intermediária.
  select(uf_resd,ano,codmunres,munic_resd,homic_reg,pop) |>
  #Drop de municípios ignorados.
  drop_na(pop) 


#Join de Municípios e microrregiões
left_join(x = base_munics, y = homic, by = join_by(uf_resd,munic_resd, "cod_ibge" == "codmunres") ) -> base
rm(base_munics,homic)


#Gráfico da Taxa de Homicídios, por microrregião
base |>
  #Criando a taxa de homicídio
  summarise(tx_homic = (sum(homic_reg)/sum(pop))*100000, .by = c(ano,uf_resd,nome_reg_int) ) |>
  #Gráfico
  ggplot(aes(x=ano, y = tx_homic,  color = nome_reg_int)) +
  geom_line() + geom_point(show.legend = FALSE) +
  facet_wrap(vars(uf_resd), scales = "free") +
  scale_x_continuous(breaks = seq(2012,2022,1) ) +
  labs(x = "", y = "Taxa de Homicídio")
#A legenda fica horrível.

#Alagoas
base |>
  #Mantém somente a UF desejada
  filter(uf_resd == "Alagoas") |>
  #Criando a taxa de homicídio
  summarise(tx_homic = (sum(homic_reg)/sum(pop))*100000, .by = c(ano,uf_resd,nome_reg_int) ) |>
  #Transforma ano_not em date. Será utilizado no eixo
  mutate(ano = make_date(ano) ) |> 
  #Gráfico
  ggplot(aes(x=ano, y = tx_homic, color = nome_reg_int)) +
  geom_line() + geom_point(show.legend = FALSE) + 
  scale_x_date(breaks = scales::breaks_width("1 year"), labels = scales::label_date("'%y") ) +
  guides(color = guide_legend(position = "inside", nrow = 1)) +
  theme(plot.title = element_text(size = 7, face = "bold"),
        legend.position.inside = c(0.725,0.8),
        axis.text.x=element_text(size=6),axis.text.y=element_text(size=6),
        axis.title.x=element_text(size=5.75),axis.title.y=element_text(size=7),
        legend.text = element_text(size = 6.5,face="bold"),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(title = "Alagoas", x = "", y = "",color = "") -> AL

#Bahia
base |>
  #Mantém somente a UF desejada
  filter(uf_resd == "Bahia") |>
  #Criando a taxa de homicídio
  summarise(tx_homic = (sum(homic_reg)/sum(pop))*100000, .by = c(ano,uf_resd,nome_reg_int) ) |>
  #Transforma ano_not em date. Será utilizado no eixo
  mutate(ano = make_date(ano) ) |> 
  #Gráfico
  ggplot(aes(x=ano, y = tx_homic, color = nome_reg_int)) +
  geom_line() + geom_point(show.legend = FALSE) + 
  scale_x_date(breaks = scales::breaks_width("1 year"), labels = scales::label_date("'%y") ) +
  guides(color = guide_legend(position = "inside", nrow = 2)) +
  theme(plot.title = element_text(size = 7, face = "bold"),
        legend.position.inside = c(0.70,0.175),
        axis.text.x=element_text(size=6),axis.text.y=element_text(size=6),
        axis.title.x=element_text(size=5.75),axis.title.y=element_text(size=7),
        legend.text = element_text(size = 6.5,face="bold"),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(title = "Bahia", x = "", y = "",color = "") -> BA


#Ceará
base |>
  #Mantém somente a UF desejada
  filter(uf_resd == "Ceará") |>
  #Criando a taxa de homicídio
  summarise(tx_homic = (sum(homic_reg)/sum(pop))*100000, .by = c(ano,uf_resd,nome_reg_int) ) |>
  #Transforma ano_not em date. Será utilizado no eixo
  mutate(ano = make_date(ano) ) |> 
  #Gráfico
  ggplot(aes(x=ano, y = tx_homic, color = nome_reg_int)) +
  geom_line() + geom_point(show.legend = FALSE) + 
  scale_x_date(breaks = scales::breaks_width("1 year"), labels = scales::label_date("'%y") ) +
  guides(color = guide_legend(position = "inside", nrow = 3)) +
  theme(plot.title = element_text(size = 7, face = "bold"),
        legend.position.inside = c(0.8,0.9),
        axis.text.x=element_text(size=6),axis.text.y=element_text(size=6),
        axis.title.x=element_text(size=5.75),axis.title.y=element_text(size=7),
        legend.text = element_text(size = 6.5,face="bold"),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(title = "Ceará", x = "", y = "",color = "") -> CE

#Maranhão
base |>
  #Mantém somente a UF desejada
  filter(uf_resd == "Maranhão") |>
  #Criando a taxa de homicídio
  summarise(tx_homic = (sum(homic_reg)/sum(pop))*100000, .by = c(ano,uf_resd,nome_reg_int) ) |>
  #Transforma ano_not em date. Será utilizado no eixo
  mutate(ano = make_date(ano) ) |> 
  #Gráfico
  ggplot(aes(x=ano, y = tx_homic, color = nome_reg_int)) +
  geom_line() + geom_point(show.legend = FALSE) + 
  scale_x_date(breaks = scales::breaks_width("1 year"), labels = scales::label_date("'%y") ) +
  guides(color = guide_legend(position = "inside", nrow = 3)) +
  theme(plot.title = element_text(size = 7, face = "bold"),
        legend.position.inside = c(0.76,0.9),
        axis.text.x=element_text(size=6),axis.text.y=element_text(size=6),
        axis.title.x=element_text(size=5.75),axis.title.y=element_text(size=7),
        legend.text = element_text(size = 6.5,face="bold"),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(title = "Maranhão", x = "", y = "Taxa de Homicídio",color = "") -> MA


#Paraíba
base |>
  #Mantém somente a UF desejada
  filter(uf_resd == "Paraíba") |>
  #Criando a taxa de homicídio
  summarise(tx_homic = (sum(homic_reg)/sum(pop))*100000, .by = c(ano,uf_resd,nome_reg_int) ) |>
  #Transforma ano_not em date. Será utilizado no eixo
  mutate(ano = make_date(ano) ) |> 
  #Gráfico
  ggplot(aes(x=ano, y = tx_homic, color = nome_reg_int)) +
  geom_line() + geom_point(show.legend = FALSE) + 
  scale_x_date(breaks = scales::breaks_width("1 year"), labels = scales::label_date("'%y") ) +
  guides(color = guide_legend(position = "inside", nrow = 2)) +
  theme(plot.title = element_text(size = 7, face = "bold"),
        legend.position.inside = c(0.678,0.9),
        axis.text.x=element_text(size=6),axis.text.y=element_text(size=6),
        axis.title.x=element_text(size=5.75),axis.title.y=element_text(size=7),
        legend.text = element_text(size = 6.5,face="bold"),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(title = "Paraíba", x = "", y = "",color = "") -> PB


#Pernambuco
base |>
  #Mantém somente a UF desejada
  filter(uf_resd == "Pernambuco") |>
  #Criando a taxa de homicídio
  summarise(tx_homic = (sum(homic_reg)/sum(pop))*100000, .by = c(ano,uf_resd,nome_reg_int) ) |>
  #Transforma ano_not em date. Será utilizado no eixo
  mutate(ano = make_date(ano) ) |> 
  #Gráfico
  ggplot(aes(x=ano, y = tx_homic, color = nome_reg_int)) +
  geom_line() + geom_point(show.legend = FALSE) + 
  scale_x_date(breaks = scales::breaks_width("1 year"), labels = scales::label_date("'%y") ) +
  guides(color = guide_legend(position = "inside", nrow = 2)) +
  theme(plot.title = element_text(size = 7, face = "bold"),
        legend.position.inside = c(0.8,0.9),
        axis.text.x=element_text(size=6),axis.text.y=element_text(size=6),
        axis.title.x=element_text(size=5.75),axis.title.y=element_text(size=7),
        legend.text = element_text(size = 6.5,face="bold"),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(title = "Pernambuco", x = "", y = "",color = "") -> PE


#Piauí
base |>
  #Mantém somente a UF desejada
  filter(uf_resd == "Piauí") |>
  #Criando a taxa de homicídio
  summarise(tx_homic = (sum(homic_reg)/sum(pop))*100000, .by = c(ano,uf_resd,nome_reg_int) ) |>
  #Transforma ano_not em date. Será utilizado no eixo
  mutate(ano = make_date(ano) ) |> 
  #Gráfico
  ggplot(aes(x=ano, y = tx_homic, color = nome_reg_int)) +
  geom_line() + geom_point(show.legend = FALSE) + 
  scale_x_date(breaks = scales::breaks_width("1 year"), labels = scales::label_date("'%y") ) +
  guides(color = guide_legend(position = "inside", nrow = 2)) +
  theme(plot.title = element_text(size = 7, face = "bold"),
        legend.position.inside = c(0.7,0.9),
        axis.text.x=element_text(size=6),axis.text.y=element_text(size=6),
        axis.title.x=element_text(size=5.75),axis.title.y=element_text(size=7),
        legend.text = element_text(size = 6.5,face="bold"),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(title = "Piauí", x = "", y = "",color = "") -> PI

#Rio Grande do Norte
base |>
  #Mantém somente a UF desejada
  filter(uf_resd == "Rio Grande do Norte") |>
  #Criando a taxa de homicídio
  summarise(tx_homic = (sum(homic_reg)/sum(pop))*100000, .by = c(ano,uf_resd,nome_reg_int) ) |>
  #Transforma ano_not em date. Será utilizado no eixo
  mutate(ano = make_date(ano) ) |> 
  #Gráfico
  ggplot(aes(x=ano, y = tx_homic, color = nome_reg_int)) +
  geom_line() + geom_point(show.legend = FALSE) + 
  scale_x_date(breaks = scales::breaks_width("1 year"), labels = scales::label_date("'%y") ) +
  guides(color = guide_legend(position = "inside", nrow = 2)) +
  theme(plot.title = element_text(size = 7, face = "bold"),
        legend.position.inside = c(0.78,0.9),
        axis.text.x=element_text(size=6),axis.text.y=element_text(size=6),
        axis.title.x=element_text(size=5.75),axis.title.y=element_text(size=7),
        legend.text = element_text(size = 6.5,face="bold"),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(title = "Rio Grande do Norte", x = "", y = "",color = "") -> RN


#Sergipe
base |>
  #Mantém somente a UF desejada
  filter(uf_resd == "Sergipe") |>
  #Criando a taxa de homicídio
  summarise(tx_homic = (sum(homic_reg)/sum(pop))*100000, .by = c(ano,uf_resd,nome_reg_int) ) |>
  #Transforma ano_not em date. Será utilizado no eixo
  mutate(ano = make_date(ano) ) |> 
  #Gráfico
  ggplot(aes(x=ano, y = tx_homic, color = nome_reg_int)) +
  geom_line() + geom_point(show.legend = FALSE) + 
  scale_x_date(breaks = scales::breaks_width("1 year"), labels = scales::label_date("'%y") ) +
  guides(color = guide_legend(position = "inside", nrow = 1)) +
  theme(plot.title = element_text(size = 7, face = "bold"),
        legend.position.inside = c(0.7,0.9),
        axis.text.x=element_text(size=6),axis.text.y=element_text(size=6),
        axis.title.x=element_text(size=5.75),axis.title.y=element_text(size=7),
        legend.text = element_text(size = 6.5,face="bold"),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(title = "Sergipe", x = "", y = "",color = "") -> SE


library(patchwork)
#(BA + CE) / (MA + PB) / (PE + PI) / (RN + SE)
wrap_plots(AL, BA, CE, MA, PB, PE, PI, RN, SE)

ggsave(filename ="tx_homic_reg.bmp",width = 15,height = 10,device='bmp', dpi=180)
ggsave(filename ="figA4.eps",width = 8,height = 5,device=cairo_ps, dpi=150)

