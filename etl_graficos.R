library(tidyverse)
library(rtn)
library(sidrar)


pib<- get_sidra(x = 5938,
                variable = 37,
                period = as.character(1991:2022))

pib<- janitor::clean_names(pib)

contas<-
rtn::get_full_account_name()


pib_ibge_ate_1995 <- 
  pib_ibge %>%
  filter(ano<1996)

pib_ibge_apos_1995 <- read_delim("pib_ibge.CSV", delim = ";", 
                       escape_double = FALSE, trim_ws = TRUE)


pib<- bind_rows(pib_ibge_apos_1995, pib_ibge_ate_1995)

# install.packages("devtools")
devtools::install_github("tchiluanda/rtn")


#' Get all the available data about RTN
#'
#' @return tibble with all.
#' @examples
#' get_full_data()
#' @export


get_full_data <- function(){
  
  if (NROW(pkg.env$dados_rtn) != 0){
    
    return(pkg.env$dados_rtn)
  }
  
  tb_ckan<- ckanr::resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="http://www.tesourotransparente.gov.br/ckan")
  URL_add <- tb_ckan$url
  
  tmp <- tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp, extra = "-R", method = "libcurl")
  
  rtn_receita<- readxl::read_xlsx(tmp,sheet = 4,skip = 4,n_max = 57)
  rtn_despesa<- readxl::read_xlsx(tmp,sheet = 4,skip = 62,n_max = 92,col_names = FALSE )
  rtn_geral <- readxl::read_xlsx(tmp,sheet = 2,skip = 64,n_max = 8)
  
  names(rtn_receita)[1]<-"Rubrica"
  names(rtn_despesa)[1]<-"Rubrica"
  names(rtn_geral)[1]<-"Rubrica"
  
  #plano_contas<- tibble::tibble(Rubrica=c(rtn_receita$Rubrica, rtn_despesa$Rubrica, rtn_geral$Rubrica))
  
  
  # plano_contas<-
  #   plano_contas %>%
  #   dplyr::mutate(id=  dplyr::row_number()) %>%
  #   dplyr::select(Rubrica, id)
  
  
  sheet3<- readxl::read_xlsx(tmp,sheet = 3,skip = 4,col_names = FALSE)
  
  deflator_IPCA <- readxl::read_xlsx(tmp,sheet = 3,skip = 76,n_max = 1, col_names = FALSE)
  names(deflator_IPCA)<-names(rtn_receita)
  names(rtn_despesa) <-names(rtn_receita)
  
  rtn_receita$id <- 1:NROW(rtn_receita)
  series_temporais_analise_rec<-tidyr::gather(rtn_receita,Data, Valor,c(-Rubrica, -id))
  series_temporais_analise_rec$Data<-as.Date(as.numeric(series_temporais_analise_rec$Data), origin="1899-12-30")
  series_temporais_analise_rec$Valor <-as.numeric(series_temporais_analise_rec$Valor)
  series_temporais_analise_rec$Valor[is.na(series_temporais_analise_rec$Valor)]<-0
  series_temporais_analise_rec$tipo <- "R"
  
  proximo_id <- NROW(rtn_receita) +1
  ultimo_id <- proximo_id + NROW(rtn_despesa) -1
  
  
  rtn_despesa$id <- proximo_id:ultimo_id
  series_temporais_analise_desp<-tidyr::gather(rtn_despesa,Data, Valor,c(-Rubrica, -id))
  series_temporais_analise_desp$Data<-as.Date(as.numeric(series_temporais_analise_desp$Data), origin="1899-12-30")
  series_temporais_analise_desp$Valor <-as.numeric(series_temporais_analise_desp$Valor)
  series_temporais_analise_desp$Valor[is.na(series_temporais_analise_desp$Valor)]<-0
  series_temporais_analise_desp$tipo <- "D"
  
  
  proximo_id <- ultimo_id +1
  ultimo_id <- proximo_id + NROW(rtn_geral) -1
  rtn_geral$id <- proximo_id:ultimo_id
  
  names(rtn_geral)<-names(rtn_receita)
  names(rtn_geral)[1]<-"Rubrica"
  
  
  series_temporais_analise<-tidyr::gather(rtn_geral,Data, Valor,c(-Rubrica, -id))
  series_temporais_analise$Data<-as.Date(as.numeric(series_temporais_analise$Data), origin="1899-12-30")
  series_temporais_analise$Valor <-as.numeric(series_temporais_analise$Valor)
  series_temporais_analise$Valor[is.na(series_temporais_analise$Valor)]<-0
  
  names(deflator_IPCA)[1]<-"Rubrica"
  series_temporais_analise_IPCA<-tidyr::gather(deflator_IPCA,Data, Valor,c(-Rubrica))
  series_temporais_analise_IPCA$Data<-as.Date(as.numeric(series_temporais_analise_IPCA$Data), origin="1899-12-30")
  series_temporais_analise_IPCA$Valor <-as.numeric(series_temporais_analise_IPCA$Valor)
  
  serie_completa<-
    series_temporais_analise_rec %>%
    dplyr::bind_rows(series_temporais_analise_desp,
                     series_temporais_analise)
  
  serie_completa<-
    serie_completa %>%
    dplyr::mutate(valor_historico = Valor) %>%
    dplyr::inner_join(
      series_temporais_analise_IPCA %>%
        dplyr::mutate(deflator =  Valor) %>%
        dplyr::select(Data, deflator), by = "Data") %>%
    dplyr::mutate(valor_atualizado = deflator * valor_historico ) %>%
    dplyr::arrange(Data, id, Rubrica) %>%
    dplyr::mutate(Rubrica= stringr::str_trim(stringr::str_remove_all(Rubrica, pattern = "[0-9]+/")))%>%
    dplyr::select(Data,Rubrica, id, tipo,valor_historico, valor_atualizado )
  
  
  
  pkg.env$dados_rtn<- serie_completa
  
  
  serie_completa
}

contas<-
rtn::get_full_account_name()

resultados_anuais<-
  rtn::get_year_accumulated_account_data(contas[c(150,157)])

#faz estimativa do PIB para 2023
pib[pib$ano==2023,2] <- pib[pib$ano==2022,2] * (1+0.03)   

posicao<- ifelse(sign(resultados_anuais$valor_historico[resultados_anuais$Rubrica== contas[157]])==1,-0.5,1)

resultados_anuais %>%
  filter(Rubrica == contas[157]) %>%
  rename(ano=Data) %>%
  inner_join(pib) %>%
  mutate(perc_pib = ((valor_historico)/pib)*100) %>%
  mutate(sinal= as.factor(sign(perc_pib))) %>%
  ggplot(aes(x=factor(ano), y=perc_pib)) +
  geom_col(aes(fill=sinal), show.legend = FALSE) +
  geom_text(aes(label= round(perc_pib,1),
                vjust = posicao),
            size=2.8) +
  theme_light() +
  theme(
   panel.grid =  element_blank(),
   axis.text.y  = element_blank(),
   axis.title.x = element_blank(),
   axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  labs(
    title= "Resultado primário do governo central - Abaixo da linha",
    subtitle =  "Brasil 1997-2023 - % do PIB",
    caption = "Fonte STN e IBGE. PIB 2023 estimado"
    
  )


