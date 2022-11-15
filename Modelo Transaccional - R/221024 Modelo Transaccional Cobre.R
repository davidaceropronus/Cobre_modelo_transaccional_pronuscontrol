rm(list=ls())
T1 = Sys.time()
# Librerias

library(dplyr)
library(readxl)
library(xlsx)
library(DataLoader)
library(writexl)
library(tidyverse)
library(ggplot2)
library(timetk)
library(lubridate)
library(plotly)
library(rgdal)
library(maptools)
library(sp)
library(tmap)
library(viridis)
library(ggsci)
library(zoo)
library(scales)
library(stringi)
library(dbscan)
library(fpc)
library(factoextra)
library(ggrepel)
library(thematic)


mes1 = 9
year1 = 2022

options(scipen=999)


# Dir Base de Datos  -------------------------------------------------------

dir = "G:/.shortcut-targets-by-id/0B1C9Ls9i7WCSOTRBcmpxWWR6bEk/PRONUS/Pronus control/Gestión de riesgos/Cobre/7. Programación R/Base de datos"


# Data Management ---------------------------------------------------------


## Cargar base de datos ---------------------------------------------------

df_egresos = importAllMdf(dir)
df_egresos = Reduce(rbind, df_egresos)

## Variable Creation ------------------------------------------------------

#' Columna (Nit): Hace referencia al id del tercero.
#' Columna (Tipo de pago): El filtro de interés es "Pago de factura".
#' Columna (Importe): Hace referencia al total del gasto.
#' Columna (Nota): Permite conocer la razón del gasto.


# Tipo id

  # Nit = 9 Dígitos
  # CC = 10 o 8 dígitos

df_egresos = df_egresos %>% 
  mutate( tipo_id = ifelse(nchar(Nit)==9, "Persona Juridica",
                           "Persona Natural"))

# Tipo Pago : Se filtra "Pago de Factura"

df_egresos = df_egresos %>% 
  filter( `Tipo de pago` == "Pago de factura") 

# Importe : Se convierte a positivo (-1)

df_egresos$Importe = -as.numeric(df_egresos$Importe)

# Convertir fecha en formato

df_egresos$Fecha = as.Date(df_egresos$Fecha)

# Crear variables categoricas desde notas
df_egresos$Nota = toupper(df_egresos$Nota)

df_egresos = df_egresos %>% 
  mutate(concepto = case_when(
    grepl("PUBLICIDAD",Proveedor)== T ~ "PUBLICIDAD",
    grepl("PUBLICIDAD",Nota)== T ~ "PUBLICIDAD",
    grepl("PAUTA",Nota)== T ~ "PUBLICIDAD",
    grepl("SERVICIO",Nota)== T ~ "SERVICIO",
    grepl("MOVISTAR",Nota)== T ~ "SERVICIO",
    grepl("RESTAURANTE",Nota)== T ~ "RESTAURANTE",
    grepl("BONOS",Nota)== T ~ "BONOS",
    grepl("PASAJE",Nota)== T ~ "PASAJE",
    grepl("TRAVEL",Proveedor)== T ~ "VIAJES/PASAJE",
    grepl("TRAVEL",Proveedor)== T ~ "VIAJES/PASAJE",
    grepl("ABOGADOS",Proveedor)== T ~ "CONSULTORIA",
    grepl("ASESOR",Nota)== T ~ "CONSULTORIA",
    grepl("CONSULTOR",Nota)== T ~ "CONSULTORIA",
    grepl("HONORARIOS",Nota)== T ~ "CONSULTORIA",
    grepl("SUSCRIPCI",Nota)== T ~ "LICENCIAS/SUSCRIPCIONES",
    grepl("AMAZON",Proveedor)== T ~ "LICENCIAS/SUSCRIPCIONES",
    grepl("LICENCIA",Nota)== T ~ "LICENCIAS/SUSCRIPCIONES",
    grepl("NUBE",Nota)== T ~ "LICENCIAS/SUSCRIPCIONES",
    grepl("PLATAF",Nota)== T ~ "LICENCIAS/SUSCRIPCIONES",
    TRUE ~ "OTRO"
  ),
  fecha_1 = Fecha)


frecuencia_fun = function(pasos,id_freq){
  # id_freq = variable a la que le quiere estimar la frecuencia
  # pasos = cuantas veces aparece id_freq en la base de datos
  n = length(pasos)
  frecuencia = sum(id_freq[1:n] == id_freq[n+1])
  return(frecuencia)
}

df_egresos_m = df_egresos %>% 
  group_by(Nit) %>% 
  summarise_by_time(
    .date_var = Fecha,
    .by = "month",
    total_monto = sum(Importe),
    fecha_mean = as.Date(mean(as.numeric(fecha_1)), origin = "1970-01-01"),
    n = n(),
    concepto = concepto,
    tipo_id = tipo_id
  ) %>% 
  unique() %>% 
  ungroup() %>% 
  mutate(frecuencia_tercero = c(0,
                                rollapplyr(Nit,
                                           width = list(-1:-length(Nit)),
                                           partial = T,
                                           FUN = frecuencia_fun,
                                           Nit
                                ))) %>% 
  group_by(Nit) %>% 
  mutate(
    recurrencia = as.numeric(fecha_mean)-as.numeric(lag(fecha_mean)),
    max_freq = max(frecuencia_tercero)
  ) %>% 
  ungroup()

df_egresos_m$recurrencia[is.na(df_egresos_m$recurrencia)] = 0 



# Gráficas Descriptivas ---------------------------------------------------

thematic::thematic_on(bg = "transparent",
                      fg = "black")

theme_pronus = theme(plot.title = element_text(hjust = 0.5, size =14, colour = "#A21824"),
                     text = element_text(colour = "#3C3C3C"),
                     plot.subtitle = element_text(hjust = 0.5),
                     legend.text = element_text(size = 8),
                     legend.title = element_text(hjust = 0.5))

# Egresos mensuales

shiny_egresos_mensuales = df_egresos %>% 
  summarise_by_time(
    .date_var = Fecha,
    .by = "month",
    total = sum(Importe)/1000000
  )%>% 
  ggplot(aes(x=Fecha,y=total,label = round(total,2))) + 
  geom_line(col = "#A21824")+
  geom_label_repel(nudge_y = 300, nudge_x = -5,segment.linetype = 2)+
  theme_pronus+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Total Egresos Históricos - Mensual",
       x = "Fecha",
       y = "COP Millones")
  

# Crecimiento Mensual

shiny_egresos_mensuales_creci = df_egresos %>% 
  summarise_by_time(
    .date_var = Fecha,
    .by = "month",
    total = sum(Importe)/1000000
  ) %>%
  mutate(creci = total/lag(total)-1) %>% 
  ggplot(aes(x=Fecha,y=creci)) + 
  geom_bar(fill = "#A21824", stat = "identity", width = 15)+
  theme_pronus+
  scale_y_continuous(labels = scales::percent)+
  labs(title = "Evolución Egresos - Mensual",
       x = "Fecha",
       y = "Porcentaje")

shiny_egresos_mensuales_creci = ggplotly(shiny_egresos_mensuales_creci)


# Distribucion de egresos por concepto

df_egresos_m_um = df_egresos_m %>%
  filter(month(Fecha) == mes1 & year(Fecha)==year1)

shiny_egresos_distr_concepto_um = plot_ly(data= df_egresos_m_um, type = "pie", labels = ~concepto,
                                       values = ~total_monto) %>%
  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
         autosize = T,
         legend = list(x = 100, y = 0.5),
         title =list(text='Distribución por Concepto - Último Mes',
                     y = 0.95, x = 0.5, xanchor = 'center', yanchor =  'top')) 

shiny_egresos_distr_concepto = plot_ly(data= df_egresos_m, type = "pie", labels = ~concepto,
                                          values = ~total_monto) %>%
  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
         autosize = T,
         legend = list(x = 100, y = 0.5),
         title =list(text='Distribución por Concepto - Histórico',
                     y = 0.95, x = 0.5, xanchor = 'center', yanchor =  'top'))

# Graficos de Area - por tipo de id

shiny_area_tipo_id = df_egresos %>%
  group_by(tipo_id) %>% 
  summarise_by_time(
    .date_var = Fecha,
    .by = "month",
    total = sum(Importe)
  ) %>% 
  ungroup() %>% 
  group_by(Fecha) %>% 
  mutate(total_egreso_mes = sum(total)) %>% 
  ungroup() %>% 
  mutate(porc = total/total_egreso_mes) %>% 
  ggplot(aes(x= Fecha, y = porc, fill =tipo_id))+
  geom_area()+
  theme_pronus+
  scale_y_continuous(labels = scales::percent)+
  labs(title = "Evolución Egresos - Tipo Id",
       x = "Fecha",
       y = "Porcentaje",
       fill = "Tipo Id")

shiny_area_tipo_id = ggplotly(shiny_area_tipo_id)


# Graficas de Frecuencia y Recurrencia

shiny_gph_frecuencia_um = df_egresos_m %>%
  filter(month(Fecha) == mes1 & year(Fecha)==year1) %>%
  group_by(max_freq) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x=as.factor(max_freq),y=n)) + 
  geom_bar(fill = "#A21824", stat = "identity")+
  theme_pronus+
  labs(title = "Frecuencia - Proveedores (Último Mes)",
       x = "Antigüedad Proveedores en Meses",
       y = "Número")

shiny_gph_frecuencia_um = ggplotly(shiny_gph_frecuencia_um)
  
shiny_gph_recurrencia = df_egresos_m %>%  
  ggplot(aes(x = recurrencia, fill=tipo_id, col = tipo_id)) +
  geom_histogram(alpha=0.6, binwidth = 5)+
  theme_pronus+
  labs(title = "Histograma - Recurrencia",
       x = "Días",
       y = "Veces",
       fill = "Tipo Id",
       col = "Tipo Id")

shiny_gph_recurrencia = ggplotly(shiny_gph_recurrencia)



# Análisis Transaccional --------------------------------------------------

## Tipo ID ----------------------------------------------------------------


df_dbs_df_id = df_egresos_m %>% 
  group_by(tipo_id) %>% 
  mutate(
    std_valor = (total_monto -mean(total_monto))/sd(total_monto),
    std_n = (n -mean(n))/sd(n),
    std_frq =scale(max_freq),
    std_recu = scale(recurrencia)
  ) %>% 
  ungroup()

kNNdistplot(df_dbs_df_id[,c(11,12,13)], k = 4)

dbs_cluster_id = fpc::dbscan(df_dbs_df_id[,c(11,12,13)], eps = 1.5, MinPts = 4,
                             method = "hybrid")

cluster_id = dbs_cluster_id$cluster
dbs_df_id_res = cbind(df_dbs_df_id,cluster_id)
colnames(dbs_df_id_res)[15]="cluster"


gph_3d_tipo_id = plot_ly(x = dbs_df_id_res$std_n,
                         y = dbs_df_id_res$std_valor,
                         z = dbs_df_id_res$std_frq,
                         type="scatter3d", mode="markers", color=factor(dbs_df_id_res$cluster),
                         colors = c("red", "#3C3C3C", "#FE4C02"),
                         alpha = 0.7)%>%
  layout(
    scene = list(
      xaxis = list(title = "Numero transacciones"),
      yaxis = list(title = "Total egreso"),
      zaxis = list(title = "Frecuencia")
    ),
    title = list(text='Nube de puntos por id', xanchor = 'center', yanchor =  'top')
  ) 

anomalias_id = dbs_df_id_res %>% 
  filter(cluster == 0) %>%
  mutate(mes = month(fecha_mean),
         ano = year(fecha_mean),
         ano_id = 1) %>% 
  select(Nit,mes,ano,ano_id)

anomalias_id_um = dbs_df_id_res %>% 
  filter(cluster == 0 & month(fecha_mean)==mes1 & year(fecha_mean)==year1) %>% 
  select(Fecha, Nit, total_monto,n, concepto,n,recurrencia)

shiny_gph_ano_id = gph_3d_tipo_id 
shiny_tab_ano_id = anomalias_id_um 


## Concepto ----------------------------------------------------------------

df_dbs_df_concepto = df_egresos_m %>% 
  group_by(concepto) %>% 
  mutate(
    std_valor = (total_monto -mean(total_monto))/sd(total_monto),
    std_n = (n -mean(n))/sd(n),
    std_frq =scale(max_freq),
    std_recu = scale(recurrencia)
  ) %>% 
  ungroup()

df_dbs_df_concepto$std_frq[df_dbs_df_concepto$std_frq == "NaN"] = 0
df_dbs_df_concepto$std_n[is.na(df_dbs_df_concepto$std_n)] = 0
df_dbs_df_concepto$std_valor[is.na(df_dbs_df_concepto$std_valor)] = 0


kNNdistplot(df_dbs_df_concepto[,c(11,12,13)], k = 4)

dbs_cluster_concepto = fpc::dbscan(df_dbs_df_concepto[,c(11,12,13)],
                                   eps = 1.5, MinPts = 4,method = "hybrid")

cluster_concepto = dbs_cluster_concepto$cluster
dbs_df_concepto_res = cbind(df_dbs_df_concepto,cluster_concepto)
colnames(dbs_df_concepto_res)[15]="cluster"


gph_3d_concepto = plot_ly(x = dbs_df_concepto_res$std_n,
                         y = dbs_df_concepto_res$std_valor,
                         z = dbs_df_concepto_res$std_frq,
                         type="scatter3d", mode="markers", color=factor(dbs_df_concepto_res$cluster),
                         colors = c("red", "#3C3C3C", "#FE4C02"),
                         alpha = 0.7)%>%
  layout(
    scene = list(
      xaxis = list(title = "Numero transacciones"),
      yaxis = list(title = "Total egreso"),
      zaxis = list(title = "Frecuencia")
    ),
    title = list(text='Nube de puntos por Concepto', xanchor = 'center', yanchor =  'top')
  ) 

anomalias_concepto = dbs_df_concepto_res %>% 
  filter(cluster == 0) %>%
  mutate(mes = month(fecha_mean),
         ano = year(fecha_mean),
         ano_concepto = 1) %>% 
  select(Nit,mes,ano,ano_concepto)

anomalias_concepto_um = dbs_df_id_res %>% 
  filter(cluster == 0 & month(fecha_mean)==mes1 & year(fecha_mean)==year1) %>% 
  select(Fecha, Nit, total_monto,n, concepto,n,recurrencia)

shiny_gph_ano_concepto = gph_3d_concepto
shiny_tab_ano_concepto = anomalias_concepto_um 


## Anomalias Generales -----------------------------------------------------

ano_generales_list = full_join(anomalias_concepto,anomalias_id,
                               by = c("mes","ano","Nit"))

info_importante = df_egresos_m %>% 
  select(Nit, Fecha,total_monto, concepto,tipo_id) %>% 
  unique() %>% 
  mutate(ano = year(Fecha),
         mes = month(Fecha))

shiny_ano_generales_um = inner_join(ano_generales_list,info_importante,
                             by = c("mes","ano","Nit")) %>% 
  filter(mes==mes1 & ano==year1) %>% 
  select(Fecha,Nit,total_monto, concepto,tipo_id, ano_concepto,ano_id,)


# Guardar lo importante ---------------------------------------------------

T0 = Sys.time()

shiny_diff_time_T = T0 -T1
shiny_diff_time_T

# Time difference of 8.74 mins (EL TIEMPO TOTAL DE CORRER)

rm(list=setdiff(ls(), ls()[grep("shiny_",ls())]))

mes1 = 9
year1 = 2022

dir_resultados = "G:/.shortcut-targets-by-id/0B1C9Ls9i7WCSOTRBcmpxWWR6bEk/PRONUS/Pronus control/Gestión de riesgos/Cobre/7. Programación R/Resultados - Shiny/2022/Octubre"
setwd(dir_resultados)
save(list = ls(all.names = TRUE), file = ".RData", envir = .GlobalEnv)
Sys.time()

