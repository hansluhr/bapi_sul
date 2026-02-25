library(tidyverse)
library(janitor)


#Pasta raiz
here::i_am("Rotinas/bapi_sul.R")

#Importando base SIM-DOEXT
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.RData") ) 



# Tabela - contribuição de cada região no ano ------------------------------------
sim_doext |> filter(intencao_homic == "Homicídio") |>
  tabyl(ano,def_def_reg_resd) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "row") |> adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front",format_func = function(x) format(x, big.mark = ".",decimal.mark = ",") )  |>
  rio::export(x = _, "Tabelas/regiões.xlsx" )




# Tabela - Contribuição de cada região no total ---------------------------
sim_doext |> filter(intencao_homic == "Homicídio") |>
  tabyl(ano,def_def_reg_resd) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "col") |> adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front",format_func = function(x) format(x, big.mark = ".",decimal.mark = ",") )  |>
  rio::export(x = _, "Tabelas/regiões.xlsx" )



# Tabela UFs da região SUL - contribuição no ano---------------------------------------
sim_doext |> filter(intencao_homic == "Homicídio" & def_def_reg_resd == "Sul") |> droplevels() |>
  tabyl(ano,def_uf_resd) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "row") |> adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front",format_func = function(x) format(x, big.mark = ".",decimal.mark = ",") )  |>
  rio::export(x = _, "Tabelas/uf_sul_resd.xlsx" )


# Tabela UFs da região SUL - contribuição no total---------------------------------------
sim_doext |> filter(intencao_homic == "Homicídio" & def_def_reg_resd == "Sul") |> droplevels() |>
  tabyl(ano,def_uf_resd) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "col") |> adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front",format_func = function(x) format(x, big.mark = ".",decimal.mark = ",") )  |>
  rio::export(x = _, "Tabelas/uf_sul_resd.xlsx" )



# Série temporal taxa por região ------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/bapi_sul.R")
#Importando base SIM-DOEXT
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.RData") ) 

#Contagem de homicídios por região
sim_doext |> filter(intencao_homic == "Homicídio") |>
  count(ano,def_reg_resd, name = "homic") -> homic

##Importando população.
#Caminho do excel com pnadc
excel_pnadc <- "C:/Users/gabli/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx"

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
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
  rename(def_reg_resd = uf) -> pop_pnadc

#Join homicídios e população.  
left_join(x = homic, y = pop_pnadc, by = join_by("ano","def_reg_resd") ) -> base
rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(def_reg_resd="Brasil" |> as.factor(),
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
  geom_line(aes(x = ano, y = tx_homic, color = def_reg_resd, group = def_reg_resd) ) +
  geom_point(aes(x = ano, y = tx_homic, color = def_reg_resd) ) +
  ggrepel::geom_text_repel(aes(x = ano, y = tx_homic,label = tx_homic, color = def_reg_resd), size = 3, show.legend = FALSE) +
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
ggsave(filename ="Figuras/tx_regiões.bmp",width = 8,height = 5,device='bmp', dpi=150)
ggsave(filename ="Figuras/fig1.eps",width = 8,height = 5, dpi=150)


#Tabela das taxas de homicídio, por região.
base |>
  select(ano, def_reg_resd, tx_homic) |>
  pivot_wider(names_from = ano, values_from = tx_homic) |>
  #Exportando
  rio::export(x = _, "Tabelas/tx_homic_reg.xlsx")

rm(base, sim_doext)



# Taxas de homicídio nas UFs Sul ----------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/bapi_sul.R")
#Importando base SIM-DOEXT
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.RData") ) 


#Contagem de homicídios por UF
sim_doext |> filter(intencao_homic == "Homicídio" & def_reg_resd == "Sul") |>
  count(ano,def_uf_resd, name = "homic") -> homic

##Importando população.
#Caminho do excel com pnadc
excel_pnadc <- "C:/Users/gabli/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx"

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  #Selecionando população geral
  select(UF,ano,pop = Pop)
#Removendo pnadcs, exceto pop_pnadc
rm(list = setdiff(ls(), c("pop_pnadc","homic","sim_doext") ) )

#População PNADc das UFs no nordeste
pop_pnadc |> rename(def_uf_resd = UF) |>
  #Mantém UFs de interese
  filter(def_uf_resd %in% c("Paraná", "Rio Grande do Sul", "Santa Catarina") ) |>
  mutate(ano = ano |> as_factor(),
         def_uf_resd = def_uf_resd |> as_factor() ) -> pop_pnadc

#Join homicídios e população.  
left_join(x = homic, y = pop_pnadc, by = join_by("ano","def_uf_resd") ) -> base
rm(homic,pop_pnadc)

#Acrescentando Total centro oeste e criando taxas.
base %>%
  bind_rows(.|>
              summarise(def_uf_resd="Sul" |> as.factor(),
                        pop = sum(pop),
                        homic = sum(homic), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round( (homic/pop)*100000,digits = 1) ) |> as.double() ) -> base


#Acrescentar taxa Brasil. Fiz na mão, a ideia veio depois de tudo pronto.
base |> 
  select(ano,def_uf_resd,tx_homic) |> 
  bind_rows(
    #Bind da taxa homic Brasil. Peguei do PDf do Atlas
    tibble(ano = 2014:2024 |> as_factor(),
           def_uf_resd = "Brasil" |> as_factor(),
           tx_homic = c(30.2, 29.3, 30.8, 32.1, 28.2, 22, 23.9, 22.8, 22.1, 21.7, 20.1) ) ) -> base

base |>
  #Transforma ano_not em date. Será utilizado no eixo
  mutate(ano = ano |> fct_drop() |> as.character() |> as.numeric(),
         ano = make_date(ano) ) |> 
  ggplot() + 
  geom_line(aes(x = ano, y = tx_homic, color = def_uf_resd, group = def_uf_resd), show.legend = FALSE )+
  geom_point(aes(x = ano, y = tx_homic, color = def_uf_resd), show.legend = FALSE ) +
  facet_wrap(vars(def_uf_resd), scales = "free") +
  ggrepel::geom_text_repel(aes(x = ano, y = tx_homic,label = tx_homic, color = def_uf_resd), size = 3, show.legend = FALSE) +
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
ggsave(filename ="Figuras/tx_ufs.bmp",width = 10,height = 5,device='bmp', dpi=150)
ggsave(filename ="Figuras/tx_ufs.eps",width = 8,height = 5, dpi=150)


#Gráfico taxa de homicídios nas UFs
base |>
  #Transforma ano_not em date. Será utilizado no eixo
  mutate(ano = ano |> fct_drop() |> as.character() |> as.numeric(),
         ano = make_date(ano) ) |> 
  ggplot() + 
  geom_line(aes(x = ano, y = tx_homic, color = def_uf_resd, group = def_uf_resd) )+
  geom_point(aes(x = ano, y = tx_homic, color = def_uf_resd) ) +
  #facet_wrap(vars(def_uf_resd), scales = "free") +
  ggrepel::geom_text_repel(aes(x = ano, y = tx_homic,label = tx_homic, color = def_uf_resd), size = 3, show.legend = FALSE) +
  scale_x_date(breaks = scales::breaks_width("1 year"), labels = scales::label_date("'%y") ) +
  #scale_y_continuous(breaks = seq(0,50,5) ) +
  guides(color = guide_legend(position = "inside",nrow = 2) ) +
  theme(legend.position.inside =  c(0.8, 0.90),
    axis.text.x=element_text(size=8),axis.text.y=element_text(size=8),
    axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
    plot.title = element_text(size = 9), legend.title=element_text(size=8),
    legend.key = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(color = "", x = "Ano", y = "Taxa. de Homicídio.")
ggsave(filename ="Figuras/tx_ufs2.bmp",width = 10,height = 5,device='bmp', dpi=150)
ggsave(filename ="Figuras/tx_ufs2.eps",width = 10,height = 5,device='bmp', dpi=150)

#Tabela taxas por UF
base |>
  #Exclusão de Brasil e Região.
  #dplyr::filter(!def_uf_resd %in% c("Brasil","Sul") ) |>
  pivot_wider(names_from = ano, values_from = tx_homic) |>
  #Exportando
  rio::export(x = _, "Tabelas/tx_homic_ufs.xlsx")

rm(base,sim_doext)


# Taxas estaduais e ranking -----------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/bapi_sul.R")
#Importando base SIM-DOEXT
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.RData") ) 


#Contagem de homicídios por UF
sim_doext |> filter(intencao_homic == "Homicídio") |>
  count(ano,def_uf_resd, name = "homic") -> homic

##Importando população.
#Caminho do excel com pnadc
excel_pnadc <- "C:/Users/gabli/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx"

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  #Selecionando população geral
  select(UF,ano,pop = Pop)
#Removendo pnadcs, exceto pop_pnadc
rm(list = setdiff(ls(), c("pop_pnadc","homic","sim_doext") ) )

#População PNADc das UFs no centro oeste
pop_pnadc |> rename(def_uf_resd = UF) |>
  mutate(ano = ano |> as_factor(),
         def_uf_resd = def_uf_resd |> as_factor() ) -> pop_pnadc

#Join homicídios e população.  
left_join(x = homic, y = pop_pnadc, by = join_by("ano","def_uf_resd") ) |>
  mutate(tx_homic = format(round( (homic/pop)*100000,digits = 1) ) |> as.double() ) -> base
rm(homic,pop_pnadc)


#Criando ranking
base |>
  #Ranking de UFs mais violênto no país
  mutate(rank_geral = dense_rank( desc(tx_homic) ), .by = ano ) |> 
  rio::export(x = _, "Tabelas/ranking.xlsx")




# MUNICÍPIOS --------------------------------------------------------------


# Taxas municipais, por UF. -----------------------------------------------

# Variações nas taxas municipais de homicídio -----------------------------


# Tabela estatísticas descritivas --------------------------------------------------
library(tidyverse)
library(janitor)
library(gtsummary)
library(kableExtra)
#Pasta raiz
here::i_am("Rotinas/bapi_sul.R")
#Importando base SIM-DOEXT
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.RData") ) 



theme_gtsummary_language(language  = "pt", big.mark = ".",decimal.mark=",")
sim_doext |> filter(intencao_homic == "Homicídio" & def_reg_resd == "Sul") |>
  #Acrescentando raçacor Negra nos microdado so SIM 
  mutate(def_racacor = case_match(def_racacor,
                              c("Parda","Preta") ~ "Negra", .default = def_racacor),
         idade = idade |> as.integer() ) |> droplevels() |>
  select(c(def_uf_resd, def_racacor, idade, def_esc, def_estciv, local_incd, instrumento, def_sexo) ) |>
  #Declarando a tabela
  tbl_summary(
    by = def_uf_resd, 
    
    percent = "column",
    
    #missing_text = "(Missing)",
    missing = "no",
    
    #Alterando o label das variáveis 
    label = list(idade ~ "Idade", def_racacor ~ "Raça\\cor", def_sexo ~ "Sexo", def_estciv ~ "Estado Civil", def_esc ~ "Escolaridade",
                 local_incd ~ "Local do incidente",instrumento ~ "Instrumento"),
    
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
  as_hux_xlsx("Tabelas/descritivas.xlsx")




# Ocupação ----------------------------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/bapi_sul.R")
#Importando base SIM-DOEXT
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.RData") ) 

#Ocupações conhecidas.
sim_doext |>
  filter(intencao_homic == "Homicídio" & def_ocup!= "Missing" & def_reg_resd == "Sul") |>
  droplevels() |>
  tabyl(def_ocup) |> arrange(desc(n)) |> #head(17) |>  
  adorn_totals() |> adorn_pct_formatting() |>
  rio::export(x=_,"Tabelas/ocupa.xlsx")


# Histograma Idade x Instrumento ------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/bapi_sul.R")
#Importando base SIM-DOEXT
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.RData") ) 

#Histograma de idade por instrumento
sim_doext |> 
  
  mutate(idade = idade |> as.integer() ) |>
  
  filter(!is.na(idade) & def_reg_resd == "Sul" & intencao_homic == "Homicídio") |>
  
  ggplot() +
  
  geom_histogram(aes(x = idade, fill = instrumento), binwidth  = 3,color = "white")  +
  
  #Idade Média, por UF de residência
  geom_vline(data = sim_doext |>
               
               mutate(idade = idade |> as.integer() ) |>
               
               filter(!is.na(idade) & def_reg_resd == "Sul" & intencao_homic == "Homicídio") |> 
               summarise(idade = mean(idade), .by = c(def_reg_resd)),
             aes(xintercept = mean(idade) ),color = "red",linewidth = 0.5, linetype="dashed")  +
  
  facet_wrap(vars(def_uf_resd), scales = "free") +
  
  labs(x="", y = "Frequência",fill = "") + 
  
  theme(legend.position = "bottom") + guides(fill = guide_legend(nrow = 2))
ggsave(filename ="Figuras/idade_instr.bmp",width = 8,height = 5,device='bmp', dpi=150)



# Gráfico de Barra - Faixa Etária e instrumento ---------------------------
library(tidyverse)
library(janitor)
library(ggstats)
#Pasta raiz
here::i_am("Rotinas/bapi_sul.R")
#Importando base SIM-DOEXT
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.RData") ) 

#Contagem de homicídios na faixa etária
sim_doext |> 
  filter(intencao_homic == "Homicídio" & !is.na(idade) & def_reg_resd == "Sul") |>
  mutate(
    idade = idade |> as.integer(),
    #Idade até 90 anos 
    idade = case_when(idade >= 90 ~ 90, .default = idade),    
    #Faixa etária
    fxet = (idade %/% 5) * 5 ) |> 
  #Contagem
  count(def_uf_resd, fxet, name = "n_fext") |> 
  add_count(def_uf_resd,  wt = n_fext, name = "homic") |> 
  mutate(p_fxet = round( (n_fext/homic)*100,1 ) ) -> counts

#Gráfico de faixa etária e instrumento
sim_doext |> 
  filter(intencao_homic == "Homicídio" & !is.na(idade) & def_reg_resd == "Sul") |>
  mutate(
    idade = idade |> as.integer(),
    #Idade até 90 anos 
    idade = case_when(idade >= 90 ~ 90, .default = idade),    
    #Faixa etária
    fxet = (idade %/% 5) * 5 ) |> 
  #Contagem de notificações, por sexo e tipo de violência.
  count(def_uf_resd,fxet,instrumento) |> 
  #Precisa converter em factor para aparecer no eixo x, ao fazer o facet
  mutate(fxet = fxet |> as_factor() ) |>
  
  #Gráfico
  ggplot( aes(x = fxet, fill = instrumento, 
              weight = n, by = fxet ) ) +
  
  geom_bar(position = "fill") + 
  
  geom_text(stat = "prop", position = position_fill(0.5), size = 2.5) +
  #Por UF de residência
  facet_wrap(vars(def_uf_resd), scales = "free", ncol = 2) +
  
    scale_x_discrete( 
    
    labels = paste(counts$fxet, "\n n = ", counts$n_fext, "\n", paste(counts$p_fxet,"%") ) ) + 
  
  scale_y_continuous(labels =  scales::percent) +
  
  #Excluir títulos das legendas.
  guides(
    fill = guide_legend(title = NULL, nrow = 2, position = "inside"),
    color = guide_legend(title = NULL) ) +
  
  theme(
    strip.text = element_text(face = "bold"),
    #Título do eixo
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    #Eixo no gráfico
    axis.text.x = element_text(size = 6.5),
    axis.text.y = element_text(size = 6.5),
    #Legenda
    legend.position.inside = c(0.7, 0.2),
    #legend.position = "bottom",
    legend.text = element_text(size = 12, face = "bold"),
    legend.key.size = unit(0.3, "cm") ) +
  
  labs(x = "Faixa Etária",
       y = "(%) Instrumento do óbito", fill = "") 
ggsave(filename ="Figuras/uf_idade_instrumento.bmp",width = 17,height = 11,device='bmp', dpi=250)
ggsave(filename ="Figuras/uf_idade_instrumento.eps",width = 17,height = 11,device=cairo_ps, dpi=250)



#Exportação tabela % de instrumento na idade, por UF
sim_doext |> 
  filter(intencao_homic == "Homicídio" & !is.na(idade) & def_reg_resd == "Sul") |>
  mutate(
    idade = idade |> as.integer(),
    #Idade até 90 anos 
    idade = case_when(idade >= 90 ~ 90, .default = idade),    
    #Faixa etária
    fxet = (idade %/% 5) * 5 ) |> 
  #Contagem
  count(def_uf_resd, instrumento, fxet, name = "n_fext") |> 
  
  add_count(def_uf_resd,  wt = n_fext, name = "homic") |> 
  
  mutate(p_fxet = round( (n_fext/homic)*100,2 ) ) |>
  rio::export(x = _, "Tabelas/uf_idade_instrumento.xlsx")

rm(list = ls() ); gc()


# Gráfico de Barras - Idade e Instrumento, por UF -------------------------
# #Fazendo por UF
# library(tidyverse)
# library(janitor)
# library(ggstats)
# 
# #Pasta raiz
# here::i_am("Rotinas/bapi_sul.R")
# #Importando base SIM-DOEXT
# load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.RData") ) 
# 
# #Paraná
# #Contagem de homicídios por faixa etária e UF
# sim_doext |> 
#   filter(intencao_homic == "Homicídio" & !is.na(idade) & def_uf_resd == "Paraná") |>
#   mutate(
#     idade = idade |> as.integer(),
#     #Idade até 90 anos 
#     idade = case_when(idade >= 90 ~ 90, .default = idade),    
#     #Faixa etária
#     fxet = (idade %/% 5) * 5 ) |> 
#   #Contagem
#   count(def_uf_resd, fxet, name = "n_fext") |> 
#   add_count(def_uf_resd,  wt = n_fext, name = "homic") |> 
#   mutate(p_fxet = round( (n_fext/homic)*100,1 ) ) -> counts
# 
# #Gráfico de faixa etária e instrumento
# sim_doext |> 
#   filter(intencao_homic == "Homicídio" & !is.na(idade) & def_uf_resd == "Paraná") |>
#   mutate(
#     idade = idade |> as.integer(),
#     #Idade até 90 anos 
#     idade = case_when(idade >= 90 ~ 90, .default = idade),    
#     #Faixa etária
#     fxet = (idade %/% 5) * 5 ) |> 
#   #Contagem de notificações, por sexo e tipo de violência.
#   count(def_uf_resd,fxet,instrumento) |> 
#   #Precisa converter em factor para aparecer no eixo x, ao fazer o facet
#   mutate(fxet = fxet |> as_factor() ) |>
#   
#   #Gráfico
#   ggplot( aes(x = fxet, fill = instrumento, 
#               weight = n, by = fxet ) ) +
#   geom_bar(position = "fill") +
#   geom_text(stat = "prop", position = position_fill(0.5), size = 2.5 ) +
#   
#   scale_x_discrete( 
#     labels = paste(counts$fxet, "\n n = ", counts$n_fext, "\n", paste(counts$p_fxet,"%") ) ) + 
#   
#   scale_y_continuous(labels =  scales::percent) +
#   
#   guides(fill = guide_legend(nrow = 1)) +
#   
#   theme(legend.position = "bottom",
#         axis.text.x=element_text(size=6.75),axis.text.y=element_text(size=8),
#         axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
#         plot.title = element_text(size = 9,face="bold"),
#         legend.text=element_text(size=8), 
#         legend.key = element_rect(fill = "transparent", colour = NA),
#         legend.background = element_rect(fill = "transparent", colour = NA) ) +
#   labs(title = "Paraná", x = "", y = "Proporção (%)", fill = "") -> g1
# rm(counts)  
# 
# #Rio Grande do Sul
# #Contagem de homicídios por faixa etária e UF
# sim_doext |> 
#   filter(intencao_homic == "Homicídio" & !is.na(idade) & def_uf_resd == "Rio Grande do Sul") |>
#   mutate(
#     idade = idade |> as.integer(),
#     #Idade até 90 anos 
#     idade = case_when(idade >= 90 ~ 90, .default = idade),    
#     #Faixa etária
#     fxet = (idade %/% 5) * 5 ) |> 
#   #Contagem
#   count(def_uf_resd, fxet, name = "n_fext") |> 
#   add_count(def_uf_resd,  wt = n_fext, name = "homic") |> 
#   mutate(p_fxet = round( (n_fext/homic)*100,1 ) ) -> counts
# 
# #Gráfico de faixa etária e instrumento
# sim_doext |> 
#   filter(intencao_homic == "Homicídio" & !is.na(idade) & def_uf_resd == "Rio Grande do Sul") |>
#   mutate(
#     idade = idade |> as.integer(),
#     #Idade até 90 anos 
#     idade = case_when(idade >= 90 ~ 90, .default = idade),    
#     #Faixa etária
#     fxet = (idade %/% 5) * 5 ) |> 
#   #Contagem de notificações, por sexo e tipo de violência.
#   count(def_uf_resd,fxet,instrumento) |> 
#   #Precisa converter em factor para aparecer no eixo x, ao fazer o facet
#   mutate(fxet = fxet |> as_factor() ) |>
#   
#   #Gráfico
#   ggplot( aes(x = fxet, fill = instrumento, 
#               weight = n, by = fxet ) ) +
#   geom_bar(position = "fill") +
#   geom_text(stat = "prop", position = position_fill(0.5), size = 2.5 ) +
#   
#   scale_x_discrete( 
#     labels = paste(counts$fxet, "\n n = ", counts$n_fext, "\n", paste(counts$p_fxet,"%") ) ) + 
#   
#   scale_y_continuous(labels =  scales::percent) +
#   
#   guides(fill = guide_legend(nrow = 1)) +
#   
#   theme(legend.position = "bottom",
#         axis.text.x=element_text(size=6.75),axis.text.y=element_text(size=8),
#         axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
#         plot.title = element_text(size = 9,face="bold"),
#         legend.text=element_text(size=8), 
#         legend.key = element_rect(fill = "transparent", colour = NA),
#         legend.background = element_rect(fill = "transparent", colour = NA) ) +
#   labs(title = "Rio Grande do Sul", x = "", y = "Proporção (%)", fill = "")  -> g2
# rm(counts)  
# 
# 
# #Mato Grosso
# #Contagem de homicídios por faixa etária e UF
# sim_doext |> 
#   filter(intencao_homic == "Homicídio" & !is.na(idade) & def_uf_resd == "Santa Catarina") |>
#   mutate(
#     idade = idade |> as.integer(),
#     #Idade até 90 anos 
#     idade = case_when(idade >= 90 ~ 90, .default = idade),    
#     #Faixa etária
#     fxet = (idade %/% 5) * 5 ) |> 
#   #Contagem
#   count(def_uf_resd, fxet, name = "n_fext") |> 
#   add_count(def_uf_resd,  wt = n_fext, name = "homic") |> 
#   mutate(p_fxet = round( (n_fext/homic)*100,1 ) ) -> counts
# 
# #Gráfico de faixa etária e instrumento
# sim_doext |> 
#   filter(intencao_homic == "Homicídio" & !is.na(idade) & def_uf_resd == "Santa Catarina") |>
#   mutate(
#     idade = idade |> as.integer(),
#     #Idade até 90 anos 
#     idade = case_when(idade >= 90 ~ 90, .default = idade),    
#     #Faixa etária
#     fxet = (idade %/% 5) * 5 ) |> 
#   #Contagem de notificações, por sexo e tipo de violência.
#   count(def_uf_resd,fxet,instrumento) |> 
#   #Precisa converter em factor para aparecer no eixo x, ao fazer o facet
#   mutate(fxet = fxet |> as_factor() ) |>
#   
#   #Gráfico
#   ggplot( aes(x = fxet, fill = instrumento, 
#               weight = n, by = fxet ) ) +
#   geom_bar(position = "fill") +
#   geom_text(stat = "prop", position = position_fill(0.5), size = 2.5 ) +
#   
#   scale_x_discrete( 
#     labels = paste(counts$fxet, "\n n = ", counts$n_fext, "\n", paste(counts$p_fxet,"%") ) ) + 
#   
#   scale_y_continuous(labels =  scales::percent) +
#   
#   guides(fill = guide_legend(nrow = 1)) +
#   
#   theme(legend.position = "bottom",
#         axis.text.x=element_text(size=6.75),axis.text.y=element_text(size=8),
#         axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
#         plot.title = element_text(size = 9,face="bold"),
#         legend.text=element_text(size=8), 
#         legend.key = element_rect(fill = "transparent", colour = NA),
#         legend.background = element_rect(fill = "transparent", colour = NA) ) +
#   labs(title = "Santa Catarina", x = "", y = "Proporção (%)", fill = "")  -> g3
# rm(counts)
# 
# 
# 
# library(ggpubr)
# ggpubr::ggarrange(g1,g2,g3, ncol=2,nrow = 2, common.legend = TRUE, legend="bottom")
# ggsave(filename ="idade_instrumento2.bmp",width = 20,height = 10,device='bmp', dpi=200)
# rm(g1,g2,g3)


# Concentração de homicídios nos municípios -------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/bapi_sul.R")
#Importando base SIM-DOEXT
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.RData") ) 

year <- 2014:2024 #Utilizado no painel a seguir
#Municípios com registro de homicídio
sim_doext |> 
  filter(intencao_homic == "Homicídio" & def_reg_resd == "Sul") |>
  count(codmunresd) |>  
  #Exclusão de município Ignorado
  filter(!codmunresd %in% c("430000","410000","420000") ) |> arrange(desc(n) ) |> view()


#Homicídio por ano município de residência
sim_doext |> 
  filter(intencao_homic == "Homicídio" & def_reg_resd == "Sul") |>
  count(ano,codmunresd) -> homic

#Fonte: https://www.ibge.gov.br/explica/codigos-dos-municipios.php
base_munics <- readxl::read_excel("C:/Users/gabli/Dropbox/Ipea/Atlas/municipios_br.xls",sheet = "munics") |>
  select("Nome_UF", "Código Município Completo","Nome_Município") |> clean_names() |>
  rename(cod_ibge = "codigo_municipio_completo", def_munic_resd = nome_municipio, def_uf_resd = nome_uf) |>
  #No microdado do SIM. A partir de 2006 o código do município aparece com 6. Vou deixar todos os municípios em todos os anos com 6 dígitos.
  mutate(cod_ibge = substr(cod_ibge,1,6)) 


#Adicionando município Ignorado ou exterior. 
#A saúde utiliza código de município ingorado. Esses municípios não aparecem em outras bases.
munics_ign <- tribble(~cod_ibge,~def_munic_resd, ~def_uf_resd,
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
  filter(def_uf_resd %in% c("Paraná", "Rio Grande do Sul", "Santa Catarina") ) -> base_munics
rm(munics_ign)

#Painel de dados com os municípios do centro oeste
base_munics |>
  crossing(year) |>
  #Transforma year em factor. Por causa do joint
  mutate(year = year |> as_factor() ) -> base_munics

#Join homicídios e nome dos municípios
left_join(x = homic, y = base_munics, by = join_by("codmunresd" == "cod_ibge", "ano" == "year") ) |>
  #Ordem das colunas
  select(def_uf_resd,codmunresd,def_munic_resd,ano,n) |>
  
  #Exclusão de município Ignorado
  filter(!codmunresd %in% c("430000","410000","420000") ) |> 
  
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
  filter(ano %in% c(2014,2016,2018,2020,2022,2024)) |>
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
  filter(ano %in% c(2014,2016,2018,2020,2022,202) & p_acum <= 90.1 ) |> 
  ggplot() +
  
  geom_hline(yintercept = 50, linetype = "dashed", alpha = 0.5) +
  
  geom_point(aes(x =p_idd, y = p_acum, color = ano) ) +  
  facet_wrap(vars(def_uf_resd), scales = "free") +
  
  scale_x_continuous(breaks = seq(0,100,2.5), labels = scales::label_percent(scale = 1)  ) +
  
  scale_y_continuous(breaks = seq(0,100,5), labels = scales::label_percent(scale = 1)) + 
  
  guides(color = guide_legend(position = "inside", nrow = 2)  ) +  
  
  theme(legend.position.inside = c(0.85, 0.2),
        axis.text.x=element_text(size=5, angle = 45), axis.text.y=element_text(size=5),
        axis.title.x=element_text(size=6),axis.title.y=element_text(size=6),
        legend.text=element_text(size=8, face = "bold"), 
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x = "%Acumulado de Municípios", y = "%Acumulado de Homicídios", color = "")



ggsave(filename ="Figuras/acum_homic_munic.bmp",width = 10,height = 5,device='bmp', dpi=150)
ggsave(filename ="Figuras/acum_homic_munic.bmp",width = 10,height = 5,device = cairo_ps, dpi=150)



# Municípios com maiores taxas --------------------------------------------


# Mapa das Taxas ----------------------------------------------------------
