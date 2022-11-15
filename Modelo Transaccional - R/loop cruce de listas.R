#loop para el cruce de listas 

rm(list = ls()) # Limpiar el área de trabajo
pacman::p_load(tidyverse,readxl) #Cargar los paquetes necesarios

#lista de nombres----
lista_directorios=list.files("G:/.shortcut-targets-by-id/0B1C9Ls9i7WCSOTRBcmpxWWR6bEk/PRONUS/Pronus control/Gestión de riesgos/Cobre/6. Cruces de listas/Informe/Particular/2022",recursive = T,
                             pattern = "\\.xlsx",full.names = T,include.dirs = T)

#Cargar las bases----
bases=list()
m=1
for (j in lista_directorios) {
  bases[[j]]=read_excel(j,range = "C14:F100",col_types = rep("text",4)) #La celda F100 se definió por prueba y error
  if ("EN CASO DE COINCIDENCIAS" %in% unlist(bases[[j]][,1])) {
    bases[[paste("Coincidencias",j)]]=bases[[j]][which(bases[[j]][,1]=="TIPO DE TERCERO")[1]:nrow(bases[[j]]),]
    bases[[paste("Coincidencias",j)]]=bases[[paste("Coincidencias",j)]][2:which(is.na(bases[[paste("Coincidencias",j)]][,1]))[1],]
  } # Este if es para extraer las bases de "EN CASO DE COINCIDENCIAS"
  bases[[j]]=bases[[j]][1:which(is.na(bases[[j]][,1]))[1]-1,] # Segmentar la base a interes
  bases[[j]][,ncol(bases[[j]])+1]=read_excel(j,range = "D5:D5",col_names = c("Fecha")) #Extraer la fecha
}
base=bind_rows(bases[!str_detect(names(bases),pattern = "Coincidencias")]) #Base de cruces 
base2=bind_rows(bases[str_detect(names(bases),pattern = "Coincidencias")]) #Base de cruces en caso de coincidencias

# Modificaciones sobre la base de datos----

base$`NOMBRE/APELLIDO`=toupper(base$`NOMBRE/APELLIDO`) # Estandarizar los nombres 
base$Fecha=as.Date(base$Fecha) #Fecha tipo fecha
base$IDENTIFICACIÓN=gsub("\\.","",base$IDENTIFICACIÓN) #Eliminar puntos de identificacion
base$IDENTIFICACIÓN=gsub("-[0-9]$","",base$IDENTIFICACIÓN) #Eliminar numero de confirmacion
base$IDENTIFICACIÓN=gsub("- [0-9]$","",base$IDENTIFICACIÓN) #Eliminar numero de confirmacion
base$tipo_id=case_when(
  nchar(base$IDENTIFICACIÓN)==9 & !is.na(as.numeric(base$IDENTIFICACIÓN))==T  ~ "Persona Juridica",
  grepl("SAS",base$`NOMBRE/APELLIDO`)== T ~ "Persona Juridica",
  grepl("S.A.S.",base$`NOMBRE/APELLIDO`)== T ~ "Persona Juridica",
  grepl("S.A.S",base$`NOMBRE/APELLIDO`)== T ~ "Persona Juridica",
  grepl("S. A. S",base$`NOMBRE/APELLIDO`)== T ~ "Persona Juridica",
  grepl("S.A.",base$`NOMBRE/APELLIDO`)== T ~ "Persona Juridica",
  str_ends(base$`NOMBRE/APELLIDO`,pattern = "LLC$")==T ~ "Persona Juridica",
  str_ends(base$`NOMBRE/APELLIDO`,pattern = "SA$")==T ~ "Persona Juridica",
  str_ends(base$`NOMBRE/APELLIDO`,pattern = "LTDA$")==T ~ "Persona Juridica",
  str_ends(base$`NOMBRE/APELLIDO`,pattern = "LTD$")==T ~ "Persona Juridica",
  base$`NOMBRE/APELLIDO`=="GI INTERNATIONAL S.R.L" ~ "Persona Juridica",
  nchar(base$IDENTIFICACIÓN)>9 & !is.na(as.numeric(base$IDENTIFICACIÓN))==T  ~ "Persona Natural",
  nchar(base$IDENTIFICACIÓN)<9 & !is.na(as.numeric(base$IDENTIFICACIÓN))==T  ~ "Persona Natural",
  TRUE ~ "Otro"
)

