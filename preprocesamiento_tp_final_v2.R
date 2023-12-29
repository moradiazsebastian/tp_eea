require("data.table")
require("tidyverse")
require("tidymodels")
require("GGally")
require("corrr")
require("MASS")
require("lubridate")
require("broom")
require("robustbase")

#Parametros iniciales
path <- "C:/Users/Vicente Mora/Desktop/Maestria/EEA/trabajo_final"

procesar_posiciones <- c(
  "Centreleft" = "Centreleft",
  "Centreright" = "Centreright",
  "Centre" = "Centre",
  "Rightwing" = "Rightwing",
  "Centre/Centreright" = "Centreright",
  "Leftwing" = "Leftwing",
  "Centre to centreleft" = "Centreleft"
  )

#Funciones y utilidades
visualizar_frecuencia <- function (arr){
  value_counts <- table(arr)
  sorted_value_counts <- sort(value_counts, decreasing = TRUE)
  sorted_value_counts_refunds <- as.data.frame(sorted_value_counts)
  
  print(sorted_value_counts_refunds)
} 

procesar_strings <- function (arr){
  resultado <- iconv(arr, to = "ASCII//TRANSLIT")
  resultado <- toupper(resultado)
  resultado <- trimws(resultado)
  
  return(resultado)
} 

mean_no_outliers <- function(x, threshold = 1.5) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  lower <- q1 - threshold * iqr
  upper <- q3 + threshold * iqr
  
  result <- mean(x[x >= lower & x <= upper], na.rm = TRUE)
  
  return(result)
}

################################################################################
# PREPROCESAMIENTO Y CARGA DE DATOS                                            #

setwd(path)

################################################################################
# REEMBOLSOS POR DIPUTADO                                                      #

# Limpiar deputy_name para poder hacer un JOIN usando los nombres de diputados
refunds_basic <- read.csv("../datasets/deputies_dataset.csv")
refunds_basic$deputy_name <- procesar_strings(refunds_basic$deputy_name)


refunds_enrriched <- read.csv("../datasets/dirty_deputies_v2.csv")
refunds_enrriched$deputy_name <- procesar_strings(refunds_enrriched$deputy_name)

parties <- refunds_enrriched %>%
  group_by(political_party) %>%
  summarise(
    political_party = political_party,
    party_pg = party_pg,
    party_regdate = party_regdate,
    party_nmembers = party_nmembers,
    party_ideology1 = party_ideology1,
    party_ideology2 = party_ideology2, 
    party_ideology3 = party_ideology3, 
    party_ideology4 = party_ideology4,
    party_position = party_position
  ) %>%
  distinct(political_party, .keep_all = TRUE)


refunds <- merge(refunds_basic, parties, by="political_party", all.x=TRUE)
refunds$political_party[refunds$political_party == " S.PART."] <- "SEM PARTIDO ASSOCIADO" 
refunds$deputy_name <- procesar_strings(refunds$deputy_name)
refunds$deputy_name[refunds$deputy_name == "AUREO"] <- "CHRISTINO AUREO"
refunds$receipt_value <- abs(refunds$receipt_value)
refunds <- refunds %>%
  mutate(
    party_position = procesar_posiciones[party_position]
  )

refunds <- refunds[refunds$receipt_value != 0,]
refunds <- refunds[refunds$receipt_date != "",]
refunds <- refunds[complete.cases(refunds$party_position), ]
refunds <- refunds[refunds$party_position != "", ]

################################################################################
# INFORMACION PERSONAL DIPUTADO                                                #

# Limpiar nome para poder hacer un JOIN usando los nombres de diputados
deputies <- read.csv("../datasets/deputados.csv",sep=";")
deputies$nome <- procesar_strings(deputies$nome)
deputies$nome[deputies$nome == "ROMAN"] <- "EVANDRO ROMAN"
deputies$nome[deputies$nome == "FRANKLIN"] <- "FRANKLIN LIMA"
deputies$nome[deputies$nome == "JOSE AIRTON FELIX CIRILO"] <- "JOSE AIRTON CIRILO"
deputies$nome[deputies$nome == "PAULINHO DA FORCA"] <- "PAULO PEREIRA DA SILVA"
deputies <- deputies[deputies$nomeCivil != "José Vicente de Macedo",]
deputies <- deputies[deputies$dataFalecimento == "",]

################################################################################
# LEGISLATURAS O PERIODO DE ACTIVIDAD                                          #

legislation <- read.csv("../datasets/legislaturas.csv",sep=";")

################################################################################
# UNION Y ENRIQUECIMIENTO DEL DATASET                                          #

extended_deputies <- merge(deputies, legislation, by.x = "idLegislaturaInicial", by.y = "idLegislatura", all.x = TRUE) %>%
  rename(
    dataInicio_legislaturaInicial = dataInicio, 
    dataFim_legislaturaInicial = dataFim  
  )

extended_deputies <- merge(extended_deputies, legislation, by.x = "idLegislaturaFinal", by.y = "idLegislatura", all.x = TRUE) %>%
  rename(
    dataInicio_legislaturaFinal = dataInicio, 
    dataFim_legislaturaFinal = dataFim  
  )


df <- merge(refunds, extended_deputies, by.x = "deputy_name", by.y = "nome", all.x = TRUE)
df <- df[complete.cases(df$ufNascimento), ]

################################################################################
# FEATURE ENGINEERING                                                          #

# Procesamiento de fechas: cambio tipo de dato
df <- df %>%
  mutate(
    receipt_date = as.Date(na.omit(df$receipt_date), format = "%Y-%m-%d"),
    dataNascimento = as.Date(na.omit(df$dataNascimento), format = "%Y-%m-%d"),
    dataInicio_legislaturaInicial = as.Date(na.omit(df$dataInicio_legislaturaInicial), format = "%Y-%m-%d"),
    dataFim_legislaturaFinal = as.Date(na.omit(df$dataFim_legislaturaFinal), format = "%Y-%m-%d"),
  )

df <- df[year(df$receipt_date) == "2016" || year(df$receipt_date) == "2015",]

# solo nos quedamos con reembolsos que tengan valores validos de actividad del diputado
df <- df %>%
  filter(receipt_date > dataInicio_legislaturaInicial, receipt_date < dataFim_legislaturaFinal)


# calculo de edad diputados y campo de fecha para agrupar por mes. 
df <- df %>%
  mutate(
    receipt_date_year_month = format(na.omit(receipt_date), "%Y-%m"),
    deputy_age = as.integer(difftime(Sys.Date(), dataNascimento, units = "days") / 365.25),
    service_months = as.integer(difftime(receipt_date,dataInicio_legislaturaInicial, units = "days") / 30.44)
  )


# calculo de variables que permiten conocer los gastos mensuales de los 
# diputados.
df <- df %>%
  arrange(deputy_name, receipt_date_year_month)

df <- df %>%
  group_by(deputy_name,establishment_name,receipt_date_year_month) %>%
  mutate(
    n_refunds_per_company_per_month = n()
  ) 


df <- df %>%
  group_by(deputy_name,receipt_date_year_month) %>%
  mutate(
    avg_refunds_per_month = as.numeric(mean_no_outliers(receipt_value)),
    sum_refunds_per_month = sum(receipt_value),
    n_refunds_per_month = n()
  ) 


df <- df %>%
  group_by(deputy_name,receipt_date_year_month,receipt_description) %>%
  mutate(
    n_refunds_per_description = n()
  ) 

grouped_df <- df %>%
  group_by(deputy_name,receipt_date_year_month) %>%
  summarise(
    deputy_name = deputy_name,
    receipt_date_year_month = receipt_date_year_month,
    sum_refunds_per_month = sum(receipt_value),
    n_refunds_per_month = n(),
    service_months = max(service_months), 
    siglaSexo = siglaSexo, 
    party_nmembers = party_nmembers,
    deputy_age = deputy_age,
    political_party = political_party,
    party_position = party_position
  ) %>%
  distinct(deputy_name, receipt_date_year_month, sum_refunds_per_month, n_refunds_per_month, siglaSexo, deputy_age, .keep_all = TRUE)

file_path <- "C:/Users/Vicente Mora/Desktop/Maestria/EEA/datasets/deputies_extended.csv"
write.csv(df, file = file_path, row.names = FALSE)

file_path <- "C:/Users/Vicente Mora/Desktop/Maestria/EEA/datasets/deputies_grouped.csv"
write.csv(grouped_df, file = file_path, row.names = FALSE)
