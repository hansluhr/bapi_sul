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
  filter(intencao_homic == "Homicídio" & !is.na(idade) & def_reg_resd == "Centro Oeste") |>
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
  
  #geom_text(stat = "prop", position = position_fill(0.5), size = 2.5 ) 
  #Por UF de residência
  facet_wrap(vars(def_uf_resd), scales = "free") +
  
  scale_x_discrete( 
    
    labels = paste(counts$fxet, "\n n = ", counts$n_fext, "\n", paste(counts$p_fxet,"%") ) ) + 
  
  scale_y_continuous(labels =  scales::percent) +
  theme(legend.position = "bottom") +
  labs(x = "Faixa Etária",
       y = "Proporção (%)",
       fill = "") 
ggsave(filename ="fig5.eps",width = 15,height = 10,device=cairo_ps, dpi=150)

rm(counts)  