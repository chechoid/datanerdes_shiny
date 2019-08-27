##Librerias####
library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(sf)
library(shinyWidgets)
library(DT)
library(ggthemes)
library(leaflet)
library(openxlsx)
library(lubridate)
library(htmlwidgets)
library(curl)
library(forcats)

#el codigo JS es para activar el widget de guardar el mapa (MapaBA)
jsfile <- "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js"

##1 - Datasets####

#Seguridad y administracion####
#seguridad 
comisarias <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/comisarias-policia-de-la-ciudad/comisarias-policia-de-la-ciudad.csv', encoding = 'UTF-8') %>% 
    select("nombre", "direccion", "barrio", "comuna", "long", "lat") %>% 
    mutate(barrio=case_when(barrio=="ALMAGRO"~"Almagro",
                            barrio=="BALVANERA"~"Balvanera",
                            barrio=="BARRACAS"~"Barracas",
                            barrio=="BELGRANO"~"Belgrano",
                            barrio=="BOCA"~"La Boca",
                            barrio=="BOEDO"~"Boedo",
                            barrio=="CABALLITO"~"Caballito",
                            barrio=="CHACARITA"~"Chacarita",
                            barrio=="CONSTITUCION"~"Constitución",
                            barrio=="FLORES"~"Flores",
                            barrio=="FLORESTA"~"Floresta",
                            barrio=="MATADEROS"~"Mataderos",
                            barrio=="MONSERRAT"~"Monserrat",
                            barrio=="NUEVA POMPEYA"~"Nueva Pompeya",
                            barrio=="NUÑEZ"~"Núñez",
                            barrio=="PALERMO"~"Palermo",
                            barrio=="PARQUE AVELLANEDA"~"Parque Avellaneda",
                            barrio=="PARQUE PATRICIOS"~"Parque Patricios",
                            barrio=="PUERTO MADERO"~"Puerto Madero",
                            barrio=="RECOLETA"~"Recoleta",
                            barrio=="RETIRO"~"Retiro",
                            barrio=="SAAVEDRA"~"Saavedra",
                            barrio=="SAN CRISTOBAL"~"San Cristóbal",
                            barrio=="SAN NICOLAS"~"San Nicolás",
                            barrio=="SAN TELMO"~"San Telmo",
                            barrio=="VERSALLES"~"Versalles",
                            barrio=="VILLA CRESPO"~"Villa Crespo",
                            barrio=="VILLA DEVOTO"~"Villa Devoto",
                            barrio=="VILLA GRAL. MITRE"~"Villa General Mitre",
                            barrio=="VILLA LUGANO"~"Villa Lugano",
                            barrio=="VILLA LURO"~"Villa Luro",
                            barrio=="VILLA PUEYRREDON"~"Villa Pueyrredón",
                            barrio=="VILLA SOLDATI"~"Villa Soldati",
                            barrio=="VILLA URQUIZA"~"Villa Urquiza"),
           barrio=as.factor(barrio),
           comuna=as.factor(comuna),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria="Comisarias",
           region=as.factor(region),
           categoria=as.factor(categoria))

camaras <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/camaras-fijas-de-control-vehicular/camaras-fijas-de-control-vehicular.csv', sep=",", encoding = 'UTF-8') %>% 
    select(id, long, lat, tipo, direccion_, barrio, comuna) %>% 
    mutate(barrio=case_when(barrio=='Agronomia'~'Agronomía',
                            barrio=='AGRONOMIA'~'Agronomía',
                            barrio=='ALMAGRO'~'Almagro',
                            barrio=='BALVANERA'~'Balvanera',
                            barrio=='BARRACAS'~'Barracas',
                            barrio=='BELGRANO'~'Belgrano',
                            barrio=='BOEDO'~'Boedo',
                            barrio=='CABALLITO'~'Caballito',
                            barrio=='CHACARITA'~'Chacarita',
                            barrio=='COGHLAN'~'Coghlan',
                            barrio=='COLEGIALES'~'Colegiales',
                            barrio=='CONSTITUCION'~'Constitución',
                            barrio=='FLORES'~'Flores',
                            barrio=='LINIERS'~'Liniers',
                            barrio=='MATADEROS'~'Mataderos',
                            barrio=='MONSERRAT'~'Monserrat',
                            barrio=='Nu\\u00f1ez'~'Núñez',
                            barrio=='NUÑEZ'~'Núñez',
                            barrio=='PALERMO'~'Palermo',
                            barrio=='PARQUE AVELLANEDA'~'Parque Avellaneda',
                            barrio=='PARQUE CHACABUCO'~'Parque Chacabuco',
                            barrio=='PARQUE CHAS'~'Parque Chas',
                            barrio=='PARQUE PATRICIOS'~'Parque Patricios',
                            barrio=='RECOLETA'~'Recoleta',
                            barrio=='RETIRO'~'Retiro',
                            barrio=='San Cristobal'~'San Cristóbal',
                            barrio=='SAN CRISTOBAL'~'San Cristóbal',
                            barrio=='SAN NICOLAS'~'San Nicolás',
                            barrio=='SAN TELMO'~'San Telmo',
                            barrio=='VERSALLES'~'Versalles',
                            barrio=='ViILLA GRAL. MITRE'~'Villa General Mitre', #esto es así
                            barrio=='VILLA CRESPO'~'Villa Crespo',
                            barrio=='VILLA LUGANO'~'Villa Lugano',
                            barrio=='VILLA PUEYRREDON'~'Villa Pueyrredón',
                            barrio=='Villa Pueyrredon'~'Villa Pueyrredón',
                            barrio=='VILLA RIACHUELO'~'Villa Riachuelo',
                            TRUE~as.character(barrio)),
           long=as.numeric(as.character(long)),
           comuna=as.factor(comuna),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria='Camaras de control vehicular',
           region=as.factor(region),
           categoria=as.factor(categoria)) %>% 
    rename('direccion'=direccion_)

#trabajo

ceil <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/centro-de-integracion-laboral/centro-de-integracion-laboral.csv', encoding='UTF-8') %>% 
    mutate(comuna=case_when(comuna=='Comuna 1'~'1',
                            comuna=='Comuna 2'~'2',
                            comuna=='Comuna 3'~'3',
                            comuna=='Comuna 4'~'4',
                            comuna=='Comuna 5'~'5',
                            comuna=='Comuna 6'~'6',
                            comuna=='Comuna 7'~'7',
                            comuna=='Comuna 8'~'8',
                            comuna=='Comuna 9'~'9',
                            comuna=='Comuna 10'~'10',
                            comuna=='Comuna 11'~'11',
                            comuna=='Comuna 12'~'12',
                            comuna=='Comuna 13'~'13',
                            comuna=='Comuna 14'~'14',
                            comuna=='Comuna 15'~'15'),
           barrio=case_when(barrio=='San Cristobal'~'San Cristóbal',
                            barrio=='Nuñez'~'Núñez',
                            barrio=='San Nicolas'~'San Nicolás',
                            TRUE~as.character(barrio)),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria='Centros de Integracion Laboral') %>% 
    select(long, lat, nombre, direccion, barrio, comuna, region, categoria)

#administracion publica

edifpub <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/edificios-publicos-del-gcba/edificios-publicos-del-gcba.csv', sep=",", encoding = 'UTF-8') %>% 
    mutate(barrio=case_when(barrio=="ALMAGRO"~"Almagro",
                            barrio=="BALVANERA"~"Balvanera",
                            barrio=="BARRACAS"~"Barracas",
                            barrio=="BOCA"~"La Boca",
                            barrio=="BOEDO"~"Boedo",
                            barrio=="CABALLITO"~"Caballito",
                            barrio=="CHACARITA"~"Chacarita",
                            barrio=="COLEGIALES"~"Colegiales",
                            barrio=="CONSTITUCION"~"Constitución",
                            barrio=="FLORES"~"Flores",
                            barrio=="FLORESTA"~"Floresta",
                            barrio=="MATADEROS"~"Mataderos",
                            barrio=="MONTE CASTRO"~"Monte Castro",
                            barrio=="MONSERRAT"~"Monserrat",
                            barrio=="NUEVA POMPEYA"~"Nueva Pompeya",
                            barrio=="NUÑEZ"~"Núñez",
                            barrio=="PALERMO"~"Palermo",
                            barrio=="PARQUE CHACABUCO"~"Parque Chacabuco",
                            barrio=="PARQUE PATRICIOS"~"Parque Patricios",
                            barrio=="PATERNAL"~"Paternal",
                            barrio=="PUERTO MADERO"~"Puerto Madero",
                            barrio=="RECOLETA"~"Recoleta",
                            barrio=="RETIRO"~"Retiro",
                            barrio=="SAN CRISTOBAL"~"San Cristóbal",
                            barrio=="SAN NICOLAS"~"San Nicolás",
                            barrio=="SAN TELMO"~"San Telmo",
                            barrio=="VILLA CRESPO"~"Villa Crespo",
                            barrio=="VILLA DEVOTO"~"Villa Devoto",
                            barrio=="VILLA GRAL. MITRE"~"Villa General Mitre",
                            barrio=="VILLA LUGANO"~"Villa Lugano",
                            barrio=="VILLA ORTUZAR"~"Villa Ortúzar",
                            barrio=="VILLA RIACHUELO"~"Villa Riachuelo",
                            barrio=="VILLA SOLDATI"~"Villa Soldati",
                            barrio=="VILLA URQUIZA"~"Villa Urquiza"),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria='Edificios publicos',
           barrio=as.factor(barrio),
           comuna=as.factor(comuna),
           categoria=as.factor(categoria),
           direccion=paste(calle, altura)) %>% 
    rename('tipo'=nivel_gest) %>% 
    select(long, lat, barrio, comuna, direccion, region, tipo, categoria)


habilitaciones <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/habilitaciones-aprobadas/habilitaciones-2019.csv', encoding='UTF-8') %>% 
    mutate(comuna=case_when(comuna=='Comuna 1'~'1',
                            comuna=='Comuna 2'~'2',
                            comuna=='Comuna 3'~'3',
                            comuna=='Comuna 4'~'4',
                            comuna=='Comuna 5'~'5',
                            comuna=='Comuna 6'~'6',
                            comuna=='Comuna 7'~'7',
                            comuna=='Comuna 8'~'8',
                            comuna=='Comuna 9'~'9',
                            comuna=='Comuna 10'~'10',
                            comuna=='Comuna 11'~'11',
                            comuna=='Comuna 12'~'12',
                            comuna=='Comuna 13'~'13',
                            comuna=='Comuna 14'~'14',
                            comuna=='Comuna 15'~'15'),
           barrio=case_when(barrio=='Nu\\u00f1ez'~'Núñez',
                            barrio=='Agronomia'~'Agronomía',
                            barrio=='Constitucion'~'Constitución',
                            barrio=='San Cristobal'~'San Cristóbal',
                            barrio=='San Nicolas'~'San Nicolás',
                            barrio=='Villa Del Parque'~'Villa del Parque',
                            barrio=='Villa Gral. Mitre'~'Villa General Mitre',
                            barrio=='Villa Ortuzar'~'Villa Ortúzar',
                            barrio=='Villa Pueyrredon'~'Villa Pueyrredón',
                            TRUE~as.character(barrio)),
           categoria='Habilitaciones',
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           direccion=paste(calle_nombre, calle_altura))

sedescomunales <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/sedes-comunales/sedes-comunales.csv', sep=",", encoding = 'UTF-8') %>% 
    mutate(comuna=case_when(comuna=='Comuna 1'~'1',
                            comuna=='Comuna 2'~'2',
                            comuna=='Comuna 3'~'3',
                            comuna=='Comuna 4'~'4',
                            comuna=='Comuna 5'~'5',
                            comuna=='Comuna 6'~'6',
                            comuna=='Comuna 7'~'7',
                            comuna=='Comuna 8'~'8',
                            comuna=='Comuna 9'~'9',
                            comuna=='Comuna 10'~'10',
                            comuna=='Comuna 11'~'11',
                            comuna=='Comuna 12'~'12',
                            comuna=='Comuna 13'~'13',
                            comuna=='Comuna 14'~'14',
                            comuna=='Comuna 15'~'15'),
           barrio=case_when(barrio=='Nu\\u00f1ez'~'Núñez',
                            barrio=='San Cristobal'~'San Cristóbal',
                            barrio=='Villa Del Parque'~'Villa del Parque',
                            TRUE~as.character(barrio)),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria='Sedes comunales') %>% 
    rename('direccion'=domicilio) %>% 
    select(long, lat, direccion, nombre, barrio, region, comuna, categoria)

#Economía y finanzas

empresastech <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/empresas-distrito-tecnologico/empresas-del-distrito-tecnologico.csv', sep = ",", encoding = 'UTF-8') %>% 
    mutate(comuna=case_when(comuna=='Comuna 4'~'4',
                            comuna=='Comuna 5'~'5'),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           direccion=paste(calle, altura),
           categoria='Empresas del Distrito Tecnologico') %>% 
    select(long, lat, nombre, direccion, region, comuna, barrio, categoria, sector, origen)

empresadis <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/empresas-del-distrito-del-diseno/empresas-del-distrito-del-diseno.csv', sep=",", encoding = 'UTF-8') %>%
    select(-tipo) %>% 
    rename('tipo'=categoria,
           'subtipo'=subcategoria) %>% 
    mutate(comuna=case_when(comuna=='Comuna 4'~'4'),
           direccion= paste(calle, altura),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria='Empresas del Distrito de Diseño') %>% 
    select(long, lat, nombre, tipo, subtipo, direccion, barrio, region, comuna, categoria)

empresaarte <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/empresas-del-distrito-de-las-artes/empresas-del-distrito-de-las-artes.csv', sep=",", encoding = 'UTF-8') %>% 
    mutate(comuna=case_when(comuna=='Comuna 1'~'1',
                            comuna=='Comuna 4'~'4'),
           barrio=case_when(barrio=='Constitucion'~'Constitución',
                            TRUE~as.character(barrio)),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria='Empresas del Distrito de Artes',
           direccion=paste(calle, altura)) %>% 
    rename('tipo'=rubro) %>% 
    select(long, lat, nombre, tipo, direccion, barrio, region, comuna, categoria)

#Social####
#educacion
escuseguros <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/senderos-seguros/escuelas-en-senderos-seguros.csv', sep=",", encoding = 'UTF-8') %>% 
    mutate(barrio=case_when(barrio=='Montserrat'~'Monserrat',
                            barrio=='Villa Gral. Mitre'~'Villa General Mitre',
                            barrio=='Villa Sta. Rita'~'Villa Santa Rita',
                            TRUE~as.character(barrio)),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria='Escuelas en senderos seguros',
           direccion=paste(calle, altura)) %>% 
    select(long, lat, nombre, sendero, sector, direccion, barrio, comuna, region, categoria)

universidades <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/universidades/universidades.csv', sep=",", encoding = 'UTF-8') %>% 
    mutate(barrio=case_when(barrio=='Agronomia'~'Agronomía',
                            barrio=='Constitucion'~'Constitución',
                            barrio=='San Nicolas'~'San Nicolás',
                            barrio=='Villa Ortuzar'~'Villa Ortúzar',
                            TRUE~as.character(barrio)),
           comuna=case_when(comuna=='Comuna 1'~'1',
                            comuna=='Comuna 2'~'2',
                            comuna=='Comuna 3'~'3',
                            comuna=='Comuna 4'~'4',
                            comuna=='Comuna 5'~'5',
                            comuna=='Comuna 6'~'6',
                            comuna=='Comuna 7'~'7',
                            comuna=='Comuna 8'~'8',
                            comuna=='Comuna 9'~'9',
                            comuna=='Comuna 10'~'10',
                            comuna=='Comuna 11'~'11',
                            comuna=='Comuna 12'~'12',
                            comuna=='Comuna 13'~'13',
                            comuna=='Comuna 14'~'14',
                            comuna=='Comuna 15'~'15'),
           categoria='Universidades',
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur")) %>% 
    rename('nombre'=universida,
           'unidad'=unidad_aca,
           'direccion'=direccion_norm) %>% 
    select(long, lat, nombre, regimen, unidad, direccion, barrio, region, comuna, categoria)

estabedu <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/establecimientos-educativos/establecimientos-educativos.csv', encoding = 'UTF-8') %>% 
    select("dom_establ", "nombre_estab", "nivel", "comunas", "barrio", "areaprogra", "long", "lat") %>% 
    mutate(barrio=case_when(barrio=="AGRONOMIA"~"Agronomía",
                            barrio=="ALMAGRO"~"Almagro",
                            barrio=="BALVANERA"~"Balvanera",
                            barrio=="BARRACAS"~"Barracas",
                            barrio=="BELGRANO"~"Belgrano",
                            barrio=="BOCA"~"La Boca",
                            barrio=="BOEDO"~"Boedo",
                            barrio=="CABALLITO"~"Caballito",
                            barrio=="CHACARITA"~"Chacarita",
                            barrio=="COGHLAN"~"Coghlan",
                            barrio=="COLEGIALES"~"Colegiales",
                            barrio=="CONSTITUCION"~"Constitución",
                            barrio=="FLORES"~"Flores",
                            barrio=="FLORESTA"~"Floresta",
                            barrio=="LINIERS"~"Liniers",
                            barrio=="MATADEROS"~"Mataderos",
                            barrio=="MONTE CASTRO"~"Monte Castro",
                            barrio=="MONTSERRAT"~"Monserrat",
                            barrio=="NUEVA POMPEYA"~"Nueva Pompeya",
                            barrio=="NUÑEZ"~"Núñez",
                            barrio=="PALERMO"~"Palermo",
                            barrio=="PARQUE AVELLANEDA"~"Parque Avellaneda",
                            barrio=="PARQUE CHACABUCO"~"Parque Chacabuco",
                            barrio=="PARQUE CHAS"~"Parque Chas",
                            barrio=="PARQUE PATRICIOS"~"Parque Patricios",
                            barrio=="PATERNAL"~"Paternal",
                            barrio=="PUERTO MADERO"~"Puerto Madero",
                            barrio=="RECOLETA"~"Recoleta",
                            barrio=="RETIRO"~"Retiro",
                            barrio=="SAAVEDRA"~"Saavedra",
                            barrio=="SAN CRISTOBAL"~"San Cristóbal",
                            barrio=="SAN NICOLAS"~"San Nicolás",
                            barrio=="SAN TELMO"~"San Telmo",
                            barrio=="VELEZ SARSFIELD"~"Vélez Sarsfield",
                            barrio=="VERSALLES"~"Versalles",
                            barrio=="VILLA CRESPO"~"Villa Crespo",
                            barrio=="VILLA DEL PARQUE"~"Villa del Parque",
                            barrio=="VILLA DEVOTO"~"Villa Devoto",
                            barrio=="VILLA GRAL. MITRE"~"Villa General Mitre",
                            barrio=="VILLA LUGANO"~"Villa Lugano",
                            barrio=="VILLA LURO"~"Villa Luro",
                            barrio=="VILLA ORTUZAR"~"Villa Ortúzar",
                            barrio=="VILLA PUEYRREDON"~"Villa Pueyrredón",
                            barrio=="VILLA REAL"~"Villa Real",
                            barrio=="VILLA RIACHUELO"~"Villa Riachuelo",
                            barrio=="VILLA SANTA RITA"~"Villa Santa Rita",
                            barrio=="VILLA SOLDATI"~"Villa Soldati",
                            barrio=="VILLA URQUIZA"~"Villa Urquiza"),
           subtipo=case_when(nivel %in%c('Inicial Común', 'Inicial Común - Otros Servicios Educativos de la modalidad Común', 'Inicial Especial')~'Inicial',
                             nivel %in%c('Primaria Común', 'Primario Común', 'Primario de Jóvenes y Adultos', 'Primario de Jóvenes y Adultos - Otros Servicios Educativos de la modalidad de Jóvenes y Adultos',
                                         'Primario Especial', 'Primario Especial - Otros Servicios Educativos de la modalidad Especial')~'Primaria',
                             nivel %in% c('Secundario Común', 'Secundario Común - Otros Servicios Educativos de la modalidad Común', 'Secundario Común - Otros Servicios Educativos de la modalidad de Jóvenes y Adultos',
                                          'Secundario Común - Otros Servicios Educativos de las modalidadades Común y de Jóvenes y Adultos', 'Secundario Común - Secundario de Jóvenes y Adultos', 'Secundario Común - Secundario de Jóvenes y Adultos - Superior No Universitario Común - Otros Servicios Educativos de la modalidad Común',
                                          'Secundario Común - Superior No Universitario Común - Otros Servicios Educativos de la modalidad Común', 'Secundario Común - Superior No Universitario Común - Otros Servicios Educativos de las modalidadades Común y de Jóvenes y Adultos', 
                                          'Secundario de Jóvenes y Adultos', 'Secundario de Jóvenes y Adultos - Superior No Universitario Común', 'Secundario de Jóvenes y Adultos - Superior No Universitario Común - Otros Servicios Educativos de la modalidad de Jóvenes y Adultos')~'Secundaria',
                             nivel %in% c('Superior No Universitario Común', 'Superior No Universitario Común - Otros Servicios Educativos de la modalidad Común', 'Superior No Universitario Común - Otros Servicios Educativos de la modalidad de Jóvenes y Adultos')~'Superior',
                             nivel %in% c('Otros Servicios Educativos de la modalidad Común', 'Otros Servicios Educativos de la modalidad de Jóvenes y Adultos', 'Otros Servicios Educativos de la modalidad Especial', 'Otros Servicios Educativos de las modalidades Común y de Jóvenes y Adultos')~'Otros',
                             TRUE~'Mixto')) %>% 
    rename("comuna" = comunas,
           "direccion" = dom_establ,
           "nombre"=nombre_estab) %>% 
    mutate(barrio=as.factor(barrio),
           comuna=as.factor(comuna),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria="Establecimientos educativos",
           region=as.factor(region),
           categoria=as.factor(categoria))


#salud

cmb <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/centros-medicos-barriales/centros-medicos-barriales.csv', sep=",", encoding = 'UTF-8') %>% 
    mutate(barrio=case_when(barrio=='Nuñez'~'Núñez',
                            barrio=='San Cristobal'~'San Cristóbal',
                            TRUE~as.character(barrio)),
           direccion=paste(calle, altura),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria='Centros Médicos Barriales') %>% 
    rename('area'=area_prog,
           'especialidad'=especialid) %>% 
    select(long, lat, nombre, area, especialidad, direccion, barrio, region, comuna, categoria)


hospitales <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/hospitales/hospitales.csv', encoding = 'UTF-8') %>% 
    select("nombre", "calle", "altura", "tipo", "barrio", "comuna", "tipo_espec", "long", "lat") %>% 
    mutate(barrio=case_when(barrio=="ALMAGRO"~"Almagro",
                            barrio=="BALVANERA"~"Balvanera",
                            barrio=="BARRACAS"~"Barracas",
                            barrio=="BELGRANO"~"Belgrano",
                            barrio=="BOCA"~"La Boca",
                            barrio=="CABALLITO"~"Caballito",
                            barrio=="COGHLAN"~"Coghlan",
                            barrio=="FLORES"~"Flores",
                            barrio=="LINIERS"~"Liniers",
                            barrio=="MONTE CASTRO"~"Monte Castro",
                            barrio=="PALERMO"~"Palermo",
                            barrio=="PARQUE CHAS"~"Parque Chas",
                            barrio=="PARQUE PATRICIOS"~"Parque Patricios",
                            barrio=="PATERNAL"~"Paternal",
                            barrio=="RECOLETA"~"Recoleta",
                            barrio=="SAN CRISTOBAL"~"San Cristóbal",
                            barrio=="VILLA DEVOTO"~"Villa Devoto",
                            barrio=="VILLA GRAL. MITRE"~"Villa General Mitre",
                            barrio=="VILLA LUGANO"~"Villa Lugano"),
           direccion=paste(calle, altura)) %>% 
    select(-calle, -altura) %>% 
    mutate(barrio=as.factor(barrio),
           comuna=as.factor(comuna),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria="Hospitales",
           region=as.factor(region),
           categoria=as.factor(categoria))

cesac <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/centros-de-salud-y-accion-comunitaria-cesac/centros-de-salud-y-accion-comunitaria.csv', sep=",", encoding = 'UTF-8') %>% 
    mutate(barrio=case_when(barrio=='RAMON CARRILLO'~'VILLA SOLDATI',
                            barrio=="ALMAGRO"~"Almagro",
                            barrio=="BALVANERA"~"Balvanera",
                            barrio=="BARRACAS"~"Barracas",
                            barrio=="BELGRANO"~"Belgrano",
                            barrio=="BOCA"~"La Boca",
                            barrio=="BOEDO"~"Boedo",
                            barrio=="CABALLITO"~"Caballito",
                            barrio=="CHACARITA"~"Chacarita",
                            barrio=="CONSTITUCION"~"Constitución",
                            barrio=="FLORES"~"Flores",
                            barrio=="FLORESTA"~"Floresta",
                            barrio=="MATADEROS"~"Mataderos",
                            barrio=="MONSERRAT"~"Monserrat",
                            barrio=="NUEVA POMPEYA"~"Nueva Pompeya",
                            barrio=="NUÑEZ"~"Núñez",
                            barrio=="PALERMO"~"Palermo",
                            barrio=='PATERNAL'~'Paternal',
                            barrio=='VILLA DEL PARQUE'~'Villa del Parque',
                            barrio=="PARQUE AVELLANEDA"~"Parque Avellaneda",
                            barrio=="PARQUE PATRICIOS"~"Parque Patricios",
                            barrio=="PUERTO MADERO"~"Puerto Madero",
                            barrio=="RECOLETA"~"Recoleta",
                            barrio=="RETIRO"~"Retiro",
                            barrio=="SAAVEDRA"~"Saavedra",
                            barrio=="SAN CRISTOBAL"~"San Cristóbal",
                            barrio=="SAN NICOLAS"~"San Nicolás",
                            barrio=="SAN TELMO"~"San Telmo",
                            barrio=="VERSALLES"~"Versalles",
                            barrio=="VILLA CRESPO"~"Villa Crespo",
                            barrio=="VILLA DEVOTO"~"Villa Devoto",
                            barrio=="VILLA GRAL. MITRE"~"Villa General Mitre",
                            barrio=="VILLA LUGANO"~"Villa Lugano",
                            barrio=="VILLA LURO"~"Villa Luro",
                            barrio=="VILLA PUEYRREDON"~"Villa Pueyrredón",
                            barrio=="VILLA SOLDATI"~"Villa Soldati",
                            barrio=="VILLA URQUIZA"~"Villa Urquiza"),
           comuna=case_when(comuna=='COMUNA 1'~'1',
                            comuna=='COMUNA 2'~'2',
                            comuna=='COMUNA 3'~'3',
                            comuna=='COMUNA 4'~'4',
                            comuna=='COMUNA 5'~'5',
                            comuna=='COMUNA 6'~'6',
                            comuna=='COMUNA 7'~'7',
                            comuna=='COMUNA 8'~'8',
                            comuna=='COMUNA 9'~'9',
                            comuna=='COMUNA 10'~'10',
                            comuna=='COMUNA 11'~'11',
                            comuna=='COMUNA 12'~'12',
                            comuna=='COMUNA 13'~'13',
                            comuna=='COMUNA 14'~'14',
                            comuna=='COMUNA 15'~'15'),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria='Centros de Salud Comunitarios') %>%
    mutate(barrio=case_when(barrio=='VILLA SOLDATI'~'Villa Soldati',
                            TRUE~as.character(barrio))) %>% 
    rename('especialidad'=especialid) %>% 
    select(long, lat, nombre, area_progr, especialidad, direccion, barrio, region, comuna, categoria)

farmacias <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/farmacias/farmacias.csv', sep=",", encoding = 'UTF-8') %>% 
    filter(barrio!="") %>%  #26 no localizadas
    filter(comuna!="") %>% 
    mutate(barrio=case_when(barrio=='Agronomia'~'Agronomía',
                            barrio=='Boca'~'La Boca',
                            barrio=='Constitucion'~'Constitución',
                            barrio=='Nuñez'~'Núñez',
                            barrio=='San Cristobal'~'San Cristóbal',
                            barrio=='San Nicolas'~'San Nicolás',
                            barrio=='Velez Sarsfield'~'Vélez Sarsfield',
                            barrio=='Villa Del Parque'~'Villa del Parque',
                            barrio=='Villa Gral. Mitre'~'Villa General Mitre',
                            barrio=='Villa Ortuzar'~'Villa Ortúzar',
                            barrio=='Villa Pueyredon'~'Villa Pueyrredón',
                            TRUE~as.character(barrio)),
           comuna=case_when(comuna=='Comuna 1'~'1',
                            comuna=='Comuna 2'~'2',
                            comuna=='Comuna 3'~'3',
                            comuna=='Comuna 4'~'4',
                            comuna=='Comuna 5'~'5',
                            comuna=='Comuna 6'~'6',
                            comuna=='Comuna 7'~'7',
                            comuna=='Comuna 8'~'8',
                            comuna=='Comuna 9'~'9',
                            comuna=='Comuna 10'~'10',
                            comuna=='Comuna 11'~'11',
                            comuna=='Comuna 12'~'12',
                            comuna=='Comuna 13'~'13',
                            comuna=='Comuna 14'~'14',
                            comuna=='Comuna 15'~'15'),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria='Farmacias',
           direccion=paste(calle_nombre, calle_altura)) %>% 
    select(long, lat, direccion, categoria, barrio, region, comuna)

#desarrollo humano y hábitat
centrosdia <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/centros-de-dia/centros-de-dia.csv', sep=",", encoding = 'UTF-8') %>% 
    mutate(comuna=case_when(comuna=='COMUNA 01'~'1',
                            comuna=='COMUNA 02'~'2',
                            comuna=='COMUNA 03'~'3',
                            comuna=='COMUNA 04'~'4',
                            comuna=='COMUNA 05'~'5',
                            comuna=='COMUNA 06'~'6',
                            comuna=='COMUNA 07'~'7',
                            comuna=='COMUNA 08'~'8',
                            comuna=='COMUNA 09'~'9',
                            comuna=='COMUNA 10'~'10',
                            comuna=='COMUNA 11'~'11',
                            comuna=='COMUNA 12'~'12',
                            comuna=='COMUNA 13'~'13',
                            comuna=='COMUNA 14'~'14',
                            comuna=='COMUNA 15'~'15'),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           barrio=case_when(barrio=="ALMAGRO"~"Almagro",
                            barrio=="BARRACAS"~"Barracas",
                            barrio=="BOCA"~"La Boca",
                            barrio=="CABALLITO"~"Caballito",
                            barrio=="CONSTITUCION"~"Constitución",
                            barrio=="FLORES"~"Flores",
                            barrio=="MATADEROS"~"Mataderos",
                            barrio=="MONTE CASTRO"~"Monte Castro",
                            barrio=="PALERMO"~"Palermo",
                            barrio=="PARQUE AVELLANEDA"~"Parque Avellaneda",
                            barrio=="PARQUE CHACABUCO"~"Parque Chacabuco",
                            barrio=="PARQUE PATRICIOS"~"Parque Patricios",
                            barrio=="SAAVEDRA"~"Saavedra",
                            barrio=="SAN TELMO"~"San Telmo",
                            barrio=="VILLA CRESPO"~"Villa Crespo",
                            barrio=="VILLA LUGANO"~"Villa Lugano",
                            barrio=="VILLA LURO"~"Villa Luro",
                            barrio=="VILLA ORTUZAR"~"Villa Ortúzar",
                            barrio=="VILLA PUEYRREDON"~"Villa Pueyrredón",
                            barrio=="VILLA RIACHUELO"~"Villa Riachuelo",
                            barrio=="VILLA SOLDATI"~"Villa Soldati"),
           categoria='Centros de dia') %>% 
    select(long, lat, nombre, direccion, barrio, region, comuna, categoria)

centrosinfantil <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/centros-de-desarrollo-infantil/centros-de-desarrollo-infantil.csv', sep=",", encoding = 'UTF-8') %>% 
    filter(localidad!='Ituzaingó') %>% 
    mutate(barrio=case_when(barrio=="ALMAGRO"~"Almagro",
                            barrio=='AGRONOMIA'~'Agronomía',
                            barrio=="BARRACAS"~"Barracas",
                            barrio=="BOCA"~"La Boca",
                            barrio=="CABALLITO"~"Caballito",
                            barrio=='CHACARITA'~'Chacarita',
                            barrio=="CONSTITUCION"~"Constitución",
                            barrio=="NUEVA POMPEYA"~"Nueva Pompeya",
                            barrio=="PARQUE AVELLANEDA"~"Parque Avellaneda",
                            barrio=="PARQUE CHACABUCO"~"Parque Chacabuco",
                            barrio=="PARQUE PATRICIOS"~"Parque Patricios",
                            barrio=='PATERNAL'~'Paternal',
                            barrio=="SAAVEDRA"~"Saavedra",
                            barrio=="SAN NICOLAS"~"San Nicolás",
                            barrio=="VILLA LUGANO"~"Villa Lugano",
                            barrio=="VILLA LURO"~"Villa Luro",
                            barrio=="VILLA PUEYRREDON"~"Villa Pueyrredón",
                            TRUE~as.character(barrio)),
           comuna=case_when(comuna=='COMUNA 01'~'1',
                            comuna=='COMUNA 02'~'2',
                            comuna=='COMUNA 03'~'3',
                            comuna=='COMUNA 04'~'4',
                            comuna=='COMUNA 05'~'5',
                            comuna=='COMUNA 06'~'6',
                            comuna=='COMUNA 07'~'7',
                            comuna=='Comuna 8'~'8',
                            comuna=='COMUNA 08'~'8',
                            comuna=='COMUNA 09'~'9',
                            comuna=='COMUNA 10'~'10',
                            comuna=='COMUNA 11'~'11',
                            comuna=='COMUNA 12'~'12',
                            comuna=='COMUNA 13'~'13',
                            comuna=='COMUNA 14'~'14',
                            comuna=='COMUNA 15'~'15'),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria='Centros de desarrollo infantil') %>% 
    select(long, lat, nombre, direccion, barrio, region, comuna, categoria)

centrosjubilados <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/centros-de-jubilados-reempadronados/centros-de-jubilados-reempadronados.csv', sep=",", encoding = 'UTF-8') %>% 
    filter(comuna!=0) %>% #quedan 22 afuera con todos estos filtros
    filter(!is.na(lat)) %>% 
    filter(barrio!="") %>% 
    mutate(barrio=case_when(barrio=='BARRACAS'~'Barracas',
                            barrio=='BELGRANO'~'Belgrano',
                            barrio=='BOEDO'~'Boedo',
                            barrio=='CABALLITO'~'Caballito',
                            barrio=='FLORES'~'Flores',
                            barrio=='LA BOCA'~'La Boca',
                            barrio=='LA PATERNAL'~'Paternal',
                            barrio=='PALERMO'~'Palermo',
                            barrio=='PARQUE AVELLANEDA'~'Parque Avellaneda',
                            barrio=='PARQUE CHACABUCO'~'Parque Chacabuco',
                            barrio=='PARQUE CHAS'~'Parque Chas',
                            barrio=='PARQUE PATRICIOS'~'Parque Patricios',
                            barrio=='San Cristobal'~'San Cristóbal',
                            barrio=='San Nicolas'~'San Nicolás',
                            barrio=='VERSALLES'~'Versalles',
                            barrio=='VILLA CRESPO'~'Villa Crespo',
                            barrio=='VILLA GRAL. MITRE'~'Villa General Mitre',
                            barrio=='VILLA LUGANO'~'Villa Lugano',
                            barrio=='VILLA RIACHUELO'~'Villa Riachuelo',
                            barrio=='VILLA URQUIZA'~'Villa Urquiza',
                            TRUE~as.character(barrio)),
           direccion=paste(calle, altura),
           categoria='Centros de jubilados',
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur")) %>% 
    rename('nombre'=nombre_centro) %>% 
    select(long, lat, nombre, direccion, barrio, region, comuna, categoria)

juegotecas <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/juegotecas-barriales/juegotecas-barriales.csv', sep=",", encoding = 'UTF-8')%>% 
    mutate(barrio=case_when(barrio=='BARRACAS'~'Barracas',
                            barrio=='BELGRANO'~'Belgrano',
                            barrio=='BOCA'~'La Boca',
                            barrio=='BOEDO'~'Boedo',
                            barrio=='FLORES'~'Flores',
                            barrio=='LINIERS'~'Liniers',
                            barrio=='RECOLETA'~'Recoleta',
                            barrio=='SAAVEDRA'~'Saavedra',
                            barrio=='VILLA DEVOTO'~'Villa Devoto',
                            barrio=='VILLA LUGANO'~'Villa Lugano',
                            barrio=='VILLA SANTA RITA'~'Villa Santa Rita'),
           comuna=case_when(comuna=='COMUNA 02'~'2',
                            comuna=='COMUNA 04'~'4',
                            comuna=='COMUNA 05'~'5',
                            comuna=='COMUNA 07'~'7',
                            comuna=='COMUNA 08'~'8',
                            comuna=='COMUNA 09'~'9',
                            comuna=='COMUNA 11'~'11',
                            comuna=='COMUNA 12'~'12',
                            comuna=='COMUNA 13'~'13'),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria='Juegotecas barriales') %>% 
    select(long, lat, nombre, direccion, barrio, region, comuna, categoria)

cpi <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/centros-de-primera-infancia/centros-de-primera-infancia.csv', sep=";", encoding = 'UTF-8') %>%
    setNames(tolower(names(.))) %>%
    filter(!is.na(x)) %>% 
    mutate(barrio=case_when(barrio=="ALMAGRO"~"Almagro",
                            barrio=="BALVANERA"~"Balvanera",
                            barrio=="BARRACAS"~"Barracas",
                            barrio=="BELGRANO"~"Belgrano",
                            barrio=="BOCA"~"La Boca",
                            barrio=="BOEDO"~"Boedo",
                            barrio=="CHACARITA"~"Chacarita",
                            barrio=='COLEGIALES'~'Colegiales',
                            barrio=="CONSTITUCION"~"Constitución",
                            barrio=="FLORES"~"Flores",
                            barrio=="FLORESTA"~"Floresta",
                            barrio=='LINIERS'~'Liniers',
                            barrio=="MATADEROS"~"Mataderos",
                            barrio=="NUEVA POMPEYA"~"Nueva Pompeya",
                            barrio=="PARQUE AVELLANEDA"~"Parque Avellaneda",
                            barrio=="PARQUE CHACABUCO"~"Parque Chacabuco",
                            barrio=='PARQUE PATRICIOS'~'Parque Patricios',
                            barrio=='PATERNAL'~'Paternal',
                            barrio=='RETIRO'~'Retiro',
                            barrio=='SAN CRISTOBAL'~'San Cristóbal',
                            barrio=="SAN TELMO"~"San Telmo",
                            barrio=="VILLA CRESPO"~"Villa Crespo",
                            barrio=="VILLA LUGANO"~"Villa Lugano",
                            barrio=="VILLA SOLDATI"~"Villa Soldati",
                            barrio=="VILLA RIACHUELO"~"Villa Riachuelo"),
           comuna=case_when(comuna=='COMUNA 01'~'1',
                            comuna=='COMUNA 02'~'2',
                            comuna=='COMUNA 03'~'3',
                            comuna=='COMUNA 04'~'4',
                            comuna=='COMUNA 05'~'5',
                            comuna=='COMUNA 06'~'6',
                            comuna=='COMUNA 07'~'7',
                            comuna=='COMUNA 08'~'8',
                            comuna=='COMUNA 09'~'9',
                            comuna=='COMUNA 10'~'10',
                            comuna=='COMUNA 11'~'11',
                            comuna=='COMUNA 12'~'12',
                            comuna=='COMUNA 13'~'13',
                            comuna=='COMUNA 14'~'14',
                            comuna=='COMUNA 15'~'15',
                            comuna=='COMUNA 3'~'3',
                            comuna=='COMUNA 4'~'4',
                            comuna=='COMUNA 7'~'7',
                            comuna=='COMUNA 8'~'8',
                            comuna=='COMUNA 9'~'9'),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria='Centros de primera infancia') %>% 
    rename('long'=x,
           'lat'=y) %>% 
    select(long, lat, nombre, direccion, barrio, region, comuna, categoria)

casanino <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/casas-de-ninas-ninos-y-adolescentes/casas-de-ninas-ninos-yo-adolescentes.csv', sep=";", encoding = 'UTF-8') %>% 
    setNames(tolower(names(.))) %>%
    rename('long'=x,
           'lat'=y) %>% 
    mutate(barrio=case_when(barrio=="BARRACAS"~"Barracas",
                            barrio=="BOCA"~"La Boca",
                            barrio=="NUEVA POMPEYA"~"Nueva Pompeya",
                            barrio=="VILLA LUGANO"~"Villa Lugano"),
           comuna=case_when(comuna=='COMUNA 04'~'4',
                            comuna=='COMUNA 08'~'8'),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria='Casa del niño y el adolescente') %>% 
    select(long, lat, nombre, direccion, barrio, region, comuna, categoria)

#integracion

paradores <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/hogares-y-paradores/hogares-y-paradores.csv', sep=",", encoding = 'UTF-8') %>% 
    setNames(tolower(names(.))) %>%
    filter(!is.na(x)) %>% 
    mutate(region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria='Hogares y paradores') %>% 
    rename('long'=x,
           'lat'=y) %>% 
    select(long, lat, nombre, destinatario, direccion, barrio, region, comuna, categoria)

#genero

cim <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/centros-integrales-de-la-mujer/centros-integrales-de-la-mujer.csv', sep=",", encoding = 'UTF-8') %>% 
    mutate(comuna=case_when(comuna=='Comuna 1'~'1',
                            comuna=='Comuna 2'~'2',
                            comuna=='Comuna 3'~'3',
                            comuna=='Comuna 4'~'4',
                            comuna=='Comuna 5'~'5',
                            comuna=='Comuna 6'~'6',
                            comuna=='Comuna 7'~'7',
                            comuna=='Comuna 8'~'8',
                            comuna=='Comuna 9'~'9',
                            comuna=='Comuna 10'~'10',
                            comuna=='Comuna 11'~'11',
                            comuna=='Comuna 12'~'12',
                            comuna=='Comuna 13'~'13',
                            comuna=='Comuna 14'~'14',
                            comuna=='Comuna 15'~'15'),
           categoria='Centros integrales de la mujer',
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           direccion=paste(calle, calle_nro)) %>% 
    rename('nombre'=CIM) %>% 
    select(long, lat, nombre, direccion, barrio, region, comuna, categoria)

#Disfrute#####
#mayep

veterinaria <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/centros-de-atencion-veterinaria/centros-de-atencion-veterinaria.csv', sep=",", encoding = 'UTF-8') %>% 
    mutate(comuna=case_when(comuna=='COMUNA 1'~'1',
                            comuna=='COMUNA 2'~'2',
                            comuna=='COMUNA 3'~'3',
                            comuna=='COMUNA 4'~'4',
                            comuna=='COMUNA 5'~'5',
                            comuna=='COMUNA 6'~'6',
                            comuna=='COMUNA 7'~'7',
                            comuna=='COMUNA 8'~'8',
                            comuna=='COMUNA 9'~'9',
                            comuna=='COMUNA 9 '~'9',
                            comuna=='COMUNA 11'~'11',
                            comuna=='COMUNA 12'~'12',
                            comuna=='COMUNA 13'~'13',
                            comuna=='COMUNA 14'~'14',
                            comuna=='COMUNA 15'~'15'),
           categoria='Veterinarias',
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur")) %>% 
    rename('nombre'=lugar) %>% 
    select(long, lat, nombre, direccion, barrio, region, comuna, categoria, consultas)


puntosverdes <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/puntos-verdes/puntos-verdes.csv', encoding = 'UTF-8') %>% 
    mutate(barrio=case_when(barrio=='Agronomia'~'Agronomía',
                            barrio=='Constitucion'~'Constitución',
                            barrio=='Nuñez'~'Núñez',
                            barrio=='San Cristobal'~'San Cristóbal',
                            barrio=='San Nicolas'~'San Nicolás',
                            barrio=='Velez Sarsfield'~'Vélez Sarsfield',
                            barrio=='Villa Del Parque'~'Villa del Parque',
                            barrio=='Villa Gral. Mitre'~'Villa General Mitre',
                            barrio=='Villa Ortuzar'~'Villa Ortúzar',
                            barrio=='Villa Pueyrredon'~'Villa Pueyrredón',
                            TRUE~as.character(barrio)),
           comuna=case_when(comuna=='Comuna 1'~'1',
                            comuna=='Comuna 2'~'2',
                            comuna=='Comuna 3'~'3',
                            comuna=='Comuna 4'~'4',
                            comuna=='Comuna 5'~'5',
                            comuna=='Comuna 6'~'6',
                            comuna=='Comuna 7'~'7',
                            comuna=='Comuna 8'~'8',
                            comuna=='Comuna 9'~'9',
                            comuna=='Comuna 10'~'10',
                            comuna=='Comuna 11'~'11',
                            comuna=='Comuna 12'~'12',
                            comuna=='Comuna 13'~'13',
                            comuna=='Comuna 14'~'14',
                            comuna=='Comuna 15'~'15'),
           categoria='Puntos verdes',
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           barrio=as.factor(barrio),
           region=as.factor(region),
           comuna=as.factor(comuna)) %>%
    rename('nombre'=plaza) %>% 
    select(id, long, lat, tipo, nombre, materiales, direccion, barrio, comuna, region, categoria)

campanas <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/campanas-verdes/campanas-verdes.csv', sep=",", encoding = 'UTF-8') %>% 
    mutate(barrio=case_when(barrio=='Agronomia'~'Agronomía',
                            barrio=='Boca'~'La Boca',
                            barrio=='Constitucion'~'Constitución',
                            barrio=='Nuñez'~'Núñez',
                            barrio=='San Cristobal'~'San Cristóbal',
                            barrio=='San Nicolas'~'San Nicolás',
                            barrio=='Velez Sarsfield'~'Vélez Sarsfield',
                            barrio=='Villa Del Parque'~'Villa del Parque',
                            barrio=='Villa Gral. Mitre'~'Villa General Mitre',
                            TRUE~as.character(barrio)),
           comuna=case_when(comuna=='Comuna 1'~'1',
                            comuna=='Comuna 2'~'2',
                            comuna=='Comuna 3'~'3',
                            comuna=='Comuna 4'~'4',
                            comuna=='Comuna 5'~'5',
                            comuna=='Comuna 6'~'6',
                            comuna=='Comuna 7'~'7',
                            comuna=='Comuna 8'~'8',
                            comuna=='Comuna 9'~'9',
                            comuna=='Comuna 10'~'10',
                            comuna=='Comuna 11'~'11',
                            comuna=='Comuna 12'~'12',
                            comuna=='Comuna 13'~'13',
                            comuna=='Comuna 14'~'14',
                            comuna=='Comuna 15'~'15'),
           categoria='Campanas verdes',
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur")) %>% 
    rename('direccion'=nombre) %>% 
    select(long, lat, direccion, modelo, barrio, region, comuna, categoria)

monumentos <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/monumentos/monumentos.csv', sep=";", encoding = 'UTF-8') %>%
    setNames(tolower(names(.))) %>%
    filter(!is.na(longitud)) %>% 
    mutate(comuna=case_when(comuna=='COMUNA 1'~'1',
                            comuna=='COMUNA 2'~'2',
                            comuna=='COMUNA 3'~'3',
                            comuna=='COMUNA 4'~'4',
                            comuna=='COMUNA 5'~'5',
                            comuna=='COMUNA 6'~'6',
                            comuna=='COMUNA 7'~'7',
                            comuna=='COMUNA 8'~'8',
                            comuna=='COMUNA 9'~'9',
                            comuna=='COMUNA 10'~'10',
                            comuna=='COMUNA 11'~'11',
                            comuna=='COMUNA 12'~'12',
                            comuna=='COMUNA 13'~'13',
                            comuna=='COMUNA 14'~'14',
                            comuna=='COMUNA 15'~'15'),
           barrio=case_when(barrio=='AGRONOMIA'~'Agronomía',
                            barrio=="ALMAGRO"~"Almagro",
                            barrio=="BALVANERA"~"Balvanera",
                            barrio=="BARRACAS"~"Barracas",
                            barrio=="BELGRANO"~"Belgrano",
                            barrio=="BOCA"~"La Boca",
                            barrio=="BOEDO"~"Boedo",
                            barrio=="CABALLITO"~"Caballito",
                            barrio=="CHACARITA"~"Chacarita",
                            barrio=='COGHLAN'~'Coghlan',
                            barrio=='COLEGIALES'~'Colegiales',
                            barrio=="CONSTITUCION"~"Constitución",
                            barrio=="FLORES"~"Flores",
                            barrio=="FLORESTA"~"Floresta",
                            barrio=='LINIERS'~'Liniers',
                            barrio=="MATADEROS"~"Mataderos",
                            barrio=="MONSERRAT"~"Monserrat",
                            barrio=="NUEVA POMPEYA"~"Nueva Pompeya",
                            barrio=="NUÑEZ"~"Núñez",
                            barrio=="PALERMO"~"Palermo",
                            barrio=='PATERNAL'~'Paternal',
                            barrio=='PARQUE CHACABUCO'~'Parque Chacabuco',
                            barrio=='PARQUE CHAS'~'Parque Chas',
                            barrio=='VILLA DEL PARQUE'~'Villa del Parque',
                            barrio=="PARQUE AVELLANEDA"~"Parque Avellaneda",
                            barrio=="PARQUE PATRICIOS"~"Parque Patricios",
                            barrio=="PUERTO MADERO"~"Puerto Madero",
                            barrio=="RECOLETA"~"Recoleta",
                            barrio=="RETIRO"~"Retiro",
                            barrio=="SAAVEDRA"~"Saavedra",
                            barrio=="SAN CRISTOBAL"~"San Cristóbal",
                            barrio=="SAN NICOLAS"~"San Nicolás",
                            barrio=="SAN TELMO"~"San Telmo",
                            barrio=='VELEZ SARSFIELD'~'Vélez Sarsfield',
                            barrio=="VERSALLES"~"Versalles",
                            barrio=="VILLA CRESPO"~"Villa Crespo",
                            barrio=="VILLA DEVOTO"~"Villa Devoto",
                            barrio=='VILLA DEL PARQUE'~'Villa del Parque',
                            barrio=="VILLA GRAL. MITRE"~"Villa General Mitre",
                            barrio=="VILLA LUGANO"~"Villa Lugano",
                            barrio=="VILLA LURO"~"Villa Luro",
                            barrio=='VILLA ORTUZAR'~'Villa Ortúzar',
                            barrio=="VILLA PUEYRREDON"~"Villa Pueyrredón",
                            barrio=="VILLA SOLDATI"~"Villa Soldati",
                            barrio=="VILLA URQUIZA"~"Villa Urquiza",
                            barrio=='VILLA REAL'~'Villa Real',
                            barrio=='VILLA RIACHUELO'~'Villa Riachuelo',
                            barrio=='VILLA SANTA RITA'~'Villa Santa Rita'),
           categoria='Monumentos',
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur")) %>% 
    rename('nombre'=denominacion_simboliza,
           'long'=longitud,
           'lat'=latitud,
           'direccion'=direccion_normalizada) %>% 
    select(long, lat, nombre, direccion, material, autores, barrio, region, comuna, categoria)

centros <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/centros-de-clasificacion-de-residuos/centros-de-clasificacion-de-residuos.csv', encoding='UTF-8') %>% 
    mutate(barrio=case_when(id==7~'Núñez',
                            TRUE~as.character(barrio)),
           comuna=case_when(id==1~'4',
                            id==3~'8',
                            id==4~'8',
                            id==5~'8',
                            id==6~'1',
                            id==7~'13',
                            id==8~'12',
                            id==9~'4',
                            id==10~'6'),
           direccion=paste(calle, altura),
           categoria='Centros de clasificacion',
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           barrio=as.factor(barrio),
           comuna=as.factor(comuna),
           region=as.factor(region),
           categoria=as.factor(categoria)) %>% 
    select(id, long, lat, nombre, administra, direccion, barrio, comuna, region, categoria)

#cultura

cultura <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/dependencias-culturales/dependencias-culturales.csv', encoding = 'UTF-8', sep=";") %>% 
    setNames(tolower(names(.))) %>% 
    select('lat', 'lng', 'id', 'nombre', 'direccion_normalizada', 'barrio', 'comuna', 'publico', 'actividad') %>%
    mutate(barrio=case_when(barrio=="AGRONOMIA"~"Agronomía",
                            barrio=="ALMAGRO"~"Almagro",
                            barrio=="BALVANERA"~"Balvanera",
                            barrio=="BARRACAS"~"Barracas",
                            barrio=="BELGRANO"~"Belgrano",
                            barrio=="BOCA"~"La Boca",
                            barrio=="BOEDO"~"Boedo",
                            barrio=="CABALLITO"~"Caballito",
                            barrio=="CHACARITA"~"Chacarita",
                            barrio=="COGHLAN"~"Coghlan",
                            barrio=="COLEGIALES"~"Colegiales",
                            barrio=="CONSTITUCION"~"Constitución",
                            barrio=="FLORES"~"Flores",
                            barrio=="FLORESTA"~"Floresta",
                            barrio=="LINIERS"~"Liniers",
                            barrio=="MATADEROS"~"Mataderos",
                            barrio=="MONTE CASTRO"~"Monte Castro",
                            barrio=="MONSERRAT"~"Monserrat",
                            barrio=="NUEVA POMPEYA"~"Nueva Pompeya",
                            barrio=="NUÑEZ"~"Núñez",
                            barrio=="PALERMO"~"Palermo",
                            barrio=="PARQUE AVELLANEDA"~"Parque Avellaneda",
                            barrio=="PARQUE CHACABUCO"~"Parque Chacabuco",
                            barrio=="PARQUE CHAS"~"Parque Chas",
                            barrio=="PARQUE PATRICIOS"~"Parque Patricios",
                            barrio=="PATERNAL"~"Paternal",
                            barrio=="PUERTO MADERO"~"Puerto Madero",
                            barrio=="RECOLETA"~"Recoleta",
                            barrio=="RETIRO"~"Retiro",
                            barrio=="SAAVEDRA"~"Saavedra",
                            barrio=="SAN CRISTOBAL"~"San Cristóbal",
                            barrio=="SAN NICOLAS"~"San Nicolás",
                            barrio=="SAN TELMO"~"San Telmo",
                            barrio=="VELEZ SARSFIELD"~"Vélez Sarsfield",
                            barrio=="VERSALLES"~"Versalles",
                            barrio=="VILLA CRESPO"~"Villa Crespo",
                            barrio=="VILLA DEL PARQUE"~"Villa del Parque",
                            barrio=="VILLA DEVOTO"~"Villa Devoto",
                            barrio=="VILLA GRAL. MITRE"~"Villa General Mitre",
                            barrio=="VILLA LUGANO"~"Villa Lugano",
                            barrio=="VILLA LURO"~"Villa Luro",
                            barrio=="VILLA ORTUZAR"~"Villa Ortúzar",
                            barrio=="VILLA PUEYRREDON"~"Villa Pueyrredón",
                            barrio=="VILLA REAL"~"Villa Real",
                            barrio=="VILLA RIACHUELO"~"Villa Riachuelo",
                            barrio=="VILLA SANTA RITA"~"Villa Santa Rita",
                            barrio=="VILLA SOLDATI"~"Villa Soldati",
                            barrio=="VILLA URQUIZA"~"Villa Urquiza"),
           comuna=case_when(comuna=='COMUNA 01'~'1',
                            comuna=='COMUNA 02'~'2',
                            comuna=='COMUNA 03'~'3',
                            comuna=='COMUNA 04'~'4',
                            comuna=='COMUNA 05'~'5',
                            comuna=='COMUNA 06'~'6',
                            comuna=='COMUNA 07'~'7',
                            comuna=='COMUNA 08'~'8',
                            comuna=='COMUNA 09'~'9',
                            comuna=='COMUNA 10'~'10',
                            comuna=='COMUNA 11'~'11',
                            comuna=='COMUNA 12'~'12',
                            comuna=='COMUNA 13'~'13',
                            comuna=='COMUNA 14'~'14',
                            comuna=='COMUNA 15'~'15'),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria='Dependencias culturales',
           barrio=as.factor(barrio),
           comuna=as.factor(comuna),
           categoria=as.factor(categoria)) %>%
    rename('long'=lng,
           'direccion'=direccion_normalizada)

bibliotecas <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/bibliotecas/bibliotecas.csv', sep=",", encoding = 'UTF-8') %>% 
    mutate(barrio=case_when(barrio=="BELGRANO"~"Belgrano",
                            barrio=="BOCA"~"La Boca",
                            barrio=="BOEDO"~"Boedo",
                            barrio=="CHACARITA"~"Chacarita",
                            barrio=="FLORES"~"Flores",
                            barrio=="FLORESTA"~"Floresta",
                            barrio=='LINIERS'~'Liniers',
                            barrio=="MATADEROS"~"Mataderos",
                            barrio=="MONSERRAT"~"Monserrat",
                            barrio=="PALERMO"~"Palermo",
                            barrio=='PARQUE CHACABUCO'~'Parque Chacabuco',
                            barrio=='PARQUE PATRICIOS'~'Parque Patricios',
                            barrio=="RETIRO"~"Retiro",
                            barrio=="SAAVEDRA"~"Saavedra",
                            barrio=="SAN NICOLAS"~"San Nicolás",
                            barrio=="VILLA CRESPO"~"Villa Crespo",
                            barrio=="VILLA DEVOTO"~"Villa Devoto",
                            barrio=="VILLA LUGANO"~"Villa Lugano",
                            barrio=='VILLA SANTA RITA'~'Villa Santa Rita'),
           comuna=case_when(comuna=='COMUNA 1'~'1',
                            comuna=='COMUNA 2'~'2',
                            comuna=='COMUNA 3'~'3',
                            comuna=='COMUNA 4'~'4',
                            comuna=='COMUNA 5'~'5',
                            comuna=='COMUNA 6'~'6',
                            comuna=='COMUNA 7'~'7',
                            comuna=='COMUNA 8'~'8',
                            comuna=='COMUNA 9'~'9',
                            comuna=='COMUNA 10'~'10',
                            comuna=='COMUNA 11'~'11',
                            comuna=='COMUNA 12'~'12',
                            comuna=='COMUNA 13'~'13',
                            comuna=='COMUNA 14'~'14',
                            comuna=='COMUNA 15'~'15'),
           categoria='Bibliotecas',
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur")) %>% 
    rename('nombre'=biblioteca,
           'direccion'=direccion_normalizada) %>% 
    select(long, lat, nombre, direccion, barrio, region, comuna, categoria)

#vicejefatura

ferias <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/ferias-y-mercados/ferias.csv', sep=";", encoding = 'UTF-8') %>% 
    setNames(tolower(names(.))) %>%
    mutate(barrio=case_when(barrio=="BELGRANO"~"Belgrano",
                            barrio=="BOCA"~"La Boca",
                            barrio=="CHACARITA"~"Chacarita",
                            barrio=="CABALLITO"~"Caballito",
                            barrio=="MATADEROS"~"Mataderos",
                            barrio=="PALERMO"~"Palermo",
                            barrio=='PARQUE AVELLANEDA'~'Parque Avellaneda',
                            barrio=='PARQUE PATRICIOS'~'Parque Patricios',
                            barrio=="PUERTO MADERO"~"Puerto Madero",
                            barrio=="SAAVEDRA"~"Saavedra",
                            barrio=="SAN NICOLAS"~"San Nicolás",
                            barrio=="RECOLETA"~"Recoleta",
                            barrio=="SAN TELMO"~"San Telmo"),
           categoria='Ferias',
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur")) %>% 
    rename('long'=lng) %>% 
    select(long, lat, nombre, direccion, barrio, region, comuna, categoria, tipo)

mercados <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/ferias-y-mercados/mercados.csv', sep=";", encoding = 'UTF-8') %>% 
    setNames(tolower(names(.))) %>%
    mutate(barrio=case_when(barrio=="BELGRANO"~"Belgrano",
                            barrio=="CHACARITA"~"Chacarita",
                            barrio=="CABALLITO"~"Caballito",
                            barrio=="NUEVA POMPEYA"~"Nueva Pompeya",
                            barrio=="SAN NICOLAS"~"San Nicolás",
                            barrio=="VILLA PUEYRREDON"~"Villa Pueyrredón"),
           comuna=case_when(barrio=='Belgrano'~'13',
                            barrio=='Chacarita'~'15',
                            barrio=='Caballito'~'6',
                            barrio=='Nueva Pompeya'~'4',
                            barrio=='San Nicolás'~'1',
                            barrio=='Villa Pueyrredón'~'12'),
           categoria='Mercados',
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur")) %>% 
    rename('direccion'=ubicacion,
           'long'=lon) %>% 
    mutate(long = as.numeric(gsub(",", ".", .$long)),
           lat = as.numeric(gsub(",", ".", .$lat))) %>% 
    select(long, lat, nombre, direccion, barrio, region, comuna, categoria)


estacionessaludables <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/estaciones-saludables/estaciones-saludables.csv', encoding = 'UTF-8') %>% 
    mutate(comuna=case_when(comuna=='Comuna 1'~'1',
                            comuna=='Comuna 2'~'2',
                            comuna=='Comuna 3'~'3',
                            comuna=='Comuna 4'~'4',
                            comuna=='Comuna 5'~'5',
                            comuna=='Comuna 6'~'6',
                            comuna=='Comuna 7'~'7',
                            comuna=='Comuna 8'~'8',
                            comuna=='Comuna 9'~'9',
                            comuna=='Comuna 10'~'10',
                            comuna=='Comuna 11'~'11',
                            comuna=='Comuna 12'~'12',
                            comuna=='Comuna 13'~'13',
                            comuna=='Comuna 14'~'14',
                            comuna=='Comuna 15'~'15'),
           barrio=case_when(barrio=='Constitucion'~'Constitución',
                            barrio=='San Nicolas'~'San Nicolás',
                            barrio=='Villa Del Parque'~'Villa del Parque',
                            barrio=='Villa Pueyrredon'~'Villa Pueyrredón',
                            TRUE~as.character(barrio)),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria='Estaciones saludables') %>%
    rename('direccion'=ubicacion) %>% 
    select(nombre, direccion, tipo, estructura, servicios, atencion, barrio, region, comuna, long, lat, categoria)

gastro <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/establecimientos-gastronomicos/establecimientos-gastronomicos.csv', sep=",", encoding = 'UTF-8') %>% 
    filter(!is.na(altura)) %>% 
    mutate(barrio=case_when(barrio=='Boca'~'La Boca',
                            barrio=='Nu\\u00f1ez'~'Núñez',
                            barrio=='San Nicolas'~'San Nicolás',
                            barrio=='Villa Ortuzar'~'Villa Ortúzar',
                            TRUE~as.character(barrio)),
           comuna=case_when(comuna=='Comuna 1'~'1',
                            comuna=='Comuna 2'~'2',
                            comuna=='Comuna 3'~'3',
                            comuna=='Comuna 4'~'4',
                            comuna=='Comuna 5'~'5',
                            comuna=='Comuna 6'~'6',
                            comuna=='Comuna 10'~'10',
                            comuna=='Comuna 11'~'11',
                            comuna=='Comuna 12'~'12',
                            comuna=='Comuna 13'~'13',
                            comuna=='Comuna 14'~'14',
                            comuna=='Comuna 15'~'15'),
           categoria='Establecimientos gastronomicos',
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           direccion=paste(calle, altura)) %>% 
    rename('nombre'=rubro,
           'tipo'=establecimiento) %>% 
    select(long, lat, nombre, direccion, barrio, region, comuna, categoria, tipo)

#deportes

polideportivos <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/polideportivos/polideportivos.csv', encoding = 'UTF-8') %>% 
    mutate(barrio=case_when(barrio=='PARQUE AVELLANEDA'~'Parque Avellaneda',
                            barrio=='PARQUE CHACABUCO'~'Parque Chacabuco',
                            barrio=='COLEGIALES'~'Colegiales',
                            barrio=='AGRONOMIA'~'Agronomía',
                            barrio=='BARRACAS'~'Barracas',
                            barrio=='MATADEROS'~'Mataderos',
                            barrio=='SAN CRISTOBAL'~'San Cristóbal',
                            barrio=='VILLA DEVOTO'~'Villa Devoto',
                            barrio=='PARQUE PATRICIOS'~'Parque Patricios',
                            barrio=='FLORESTA'~'Floresta',
                            barrio=='LINIERS'~'Liniers',
                            barrio=='BARRACAS'~'Barracas',
                            barrio=='PALERMO'~'Palermo',
                            barrio=='VILLA SOLDATI'~'Villa Soldati'),
           categoria='Polideportivos',
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           barrio=as.factor(barrio),
           region=as.factor(region),
           comuna=as.factor(comuna),
           categoria=as.factor(categoria)) %>% 
    rename('nombre'=nom_polid)

skate <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/pistas-de-skate/pistas-de-skate.csv', sep=",", encoding = 'UTF-8') %>% 
    rename('tipo'=categoria) %>% 
    mutate(categoria='Pistas de skate',
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur")) %>% 
    select(long, lat, nombre, direccion, tipo, categoria, barrio, region, comuna)

#sitios de interes

regcivil <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/registro-civil/sedes-registro-civil.csv', encoding='UTF-8', sep=";") %>% 
    setNames(tolower(names(.))) %>% 
    mutate(comuna=case_when(comuna=='COMUNA 1'~'1',
                            comuna=='COMUNA 2'~'2',
                            comuna=='COMUNA 3'~'3',
                            comuna=='COMUNA 4'~'4',
                            comuna=='COMUNA 5'~'5',
                            comuna=='COMUNA 6'~'6',
                            comuna=='COMUNA 7'~'7',
                            comuna=='COMUNA 8'~'8',
                            comuna=='COMUNA 9'~'9',
                            comuna=='COMUNA 10'~'10',
                            comuna=='COMUNA 11'~'11',
                            comuna=='COMUNA 12'~'12',
                            comuna=='COMUNA 13'~'13',
                            comuna=='COMUNA 14'~'14',
                            comuna=='COMUNA 15'~'15'),
           barrio=case_when(barrio=="AGRONOMIA"~"Agronomía",
                            barrio=="ALMAGRO"~"Almagro",
                            barrio=="BALVANERA"~"Balvanera",
                            barrio=="BARRACAS"~"Barracas",
                            barrio=="BELGRANO"~"Belgrano",
                            barrio=="BOCA"~"La Boca",
                            barrio=="BOEDO"~"Boedo",
                            barrio=="CABALLITO"~"Caballito",
                            barrio=="CHACARITA"~"Chacarita",
                            barrio=="COGHLAN"~"Coghlan",
                            barrio=="COLEGIALES"~"Colegiales",
                            barrio=="CONSTITUCION"~"Constitución",
                            barrio=="FLORES"~"Flores",
                            barrio=="FLORESTA"~"Floresta",
                            barrio=="LINIERS"~"Liniers",
                            barrio=="MATADEROS"~"Mataderos",
                            barrio=="MONTE CASTRO"~"Monte Castro",
                            barrio=="MONSERRAT"~"Monserrat",
                            barrio=="NUEVA POMPEYA"~"Nueva Pompeya",
                            barrio=="NUÑEZ"~"Núñez",
                            barrio=="PALERMO"~"Palermo",
                            barrio=="PARQUE AVELLANEDA"~"Parque Avellaneda",
                            barrio=="PARQUE CHACABUCO"~"Parque Chacabuco",
                            barrio=="PARQUE CHAS"~"Parque Chas",
                            barrio=="PARQUE PATRICIOS"~"Parque Patricios",
                            barrio=="PATERNAL"~"Paternal",
                            barrio=="PUERTO MADERO"~"Puerto Madero",
                            barrio=="RECOLETA"~"Recoleta",
                            barrio=="RETIRO"~"Retiro",
                            barrio=="SAAVEDRA"~"Saavedra",
                            barrio=="SAN CRISTOBAL"~"San Cristóbal",
                            barrio=="SAN NICOLAS"~"San Nicolás",
                            barrio=="SAN TELMO"~"San Telmo",
                            barrio=="VELEZ SARSFIELD"~"Vélez Sarsfield",
                            barrio=="VERSALLES"~"Versalles",
                            barrio=="VILLA CRESPO"~"Villa Crespo",
                            barrio=="VILLA DEL PARQUE"~"Villa del Parque",
                            barrio=="VILLA DEVOTO"~"Villa Devoto",
                            barrio=="VILLA GRAL. MITRE"~"Villa General Mitre",
                            barrio=="VILLA LUGANO"~"Villa Lugano",
                            barrio=="VILLA LURO"~"Villa Luro",
                            barrio=="VILLA ORTUZAR"~"Villa Ortúzar",
                            barrio=="VILLA PUEYRREDON"~"Villa Pueyrredón",
                            barrio=="VILLA REAL"~"Villa Real",
                            barrio=="VILLA RIACHUELO"~"Villa Riachuelo",
                            barrio=="VILLA SANTA RITA"~"Villa Santa Rita",
                            barrio=="VILLA SOLDATI"~"Villa Soldati",
                            barrio=="VILLA URQUIZA"~"Villa Urquiza"),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria='Registro Civil') %>% 
    rename('long'=lng) %>% 
    select(long, lat, tramites, direccion, barrio, region, comuna, categoria)



#Transformación####

#obras urbanas
obras <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/ba-obras/observatorio-de-obras-urbanas.csv', encoding = 'UTF-8') %>% 
    mutate(eje=case_when(tipo=="Arquitectura"~"Transformación",
                         tipo=="Hidráulica e Infraestructura"~"Transformación",
                         tipo=="Vivienda"~"Social",
                         tipo=="Transporte"~"Transformación",
                         tipo=="Escuelas"~"Social",
                         tipo=="Salud"~"Social",
                         tipo=="Espacio Público"~"Disfrute")) %>% 
    mutate(id=as.factor(id),
           lng=as.numeric(as.character(lng))) %>% 
    rename("long"=lng) %>% 
    select("id", "nombre", "etapa", "eje", "area_responsable", "descripcion", "comuna", "barrio", "fecha_inicio", "fecha_fin_inicial", "beneficiarios", "compromiso", "long", "lat", "tipo") %>% 
    mutate(id=as.factor(id),
           comuna=as.factor(comuna),
           comuna=case_when(id=="25305"~"10",
                            id=="25321"~"8",
                            id=="25303"~"7",
                            id=="25320"~"1",
                            TRUE ~ as.character(comuna)),
           barrio=case_when(id=="25303"~"Flores",
                            id=="25305"~"Villa Luro",
                            id=="25320"~"Retiro",
                            id=="25321"~"Parque Patricios",
                            TRUE~as.character(barrio)),
           barrio=case_when(barrio=="Boca"~"La Boca",
                            barrio=="Nuñez"~"Núñez",
                            barrio=="Villa Del Parque"~"Villa del Parque",
                            barrio=="Villa Pueyrredon"~"Villa Pueyrredón",
                            barrio=="Montserrat"~"Monserrat",
                            TRUE~barrio),
           eje=as.factor(eje),
           comuna=as.factor(comuna),
           fecha_inicio=dmy(as.character(fecha_inicio)),
           fecha_fin_inicial=dmy(as.character(fecha_fin_inicial)),
           barrio=as.factor(barrio),
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur"),
           categoria="Obras urbanas",
           region=as.factor(region),
           categoria=as.factor(categoria)) %>% 
    filter(id!="25191") #eliminar el que está mal georreferenciado

#transporte

taxis <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/paradas-de-taxis/paradas-de-taxis.csv', sep=",", encoding = 'UTF-8') %>% 
    filter(!is.na(codigo_postal)) %>% 
    mutate(barrio=case_when(barrio=='Boca'~'La Boca',
                            barrio=='Constitucion'~'Constitución',
                            barrio=='Nu\\u00f1ez'~'Núñez',
                            barrio=='San Nicolas'~'San Nicolás',
                            barrio=='San Cristobal'~'San Cristóbal',
                            barrio=='San Nicolas'~'San Nicolás',
                            barrio=='Villa Del Parque'~'Villa del Parque',
                            barrio=='Villa Gral. Mitre'~'Villa General Mitre',
                            barrio=='Villa Ortuzar'~'Villa Ortúzar',
                            barrio=='Villa Pueyredon'~'Villa Pueyrredón',
                            TRUE~as.character(barrio)),
           comuna=case_when(comuna=='Comuna 1'~'1',
                            comuna=='Comuna 2'~'2',
                            comuna=='Comuna 3'~'3',
                            comuna=='Comuna 4'~'4',
                            comuna=='Comuna 5'~'5',
                            comuna=='Comuna 6'~'6',
                            comuna=='Comuna 7'~'7',
                            comuna=='Comuna 8'~'8',
                            comuna=='Comuna 9'~'9',
                            comuna=='Comuna 10'~'10',
                            comuna=='Comuna 11'~'11',
                            comuna=='Comuna 12'~'12',
                            comuna=='Comuna 13'~'13',
                            comuna=='Comuna 14'~'14',
                            comuna=='Comuna 15'~'15'),
           categoria='Paradas de taxi',
           region=case_when(comuna %in% c("12","13","14","2")~"Norte",
                            comuna %in% c("5", "6", "7", "10", "11", "15", "1", "3")~"Centro",
                            comuna %in% c("8", "4", "9")~"Sur")) %>% 
    rename('direccion'=dom_normalizado) %>% 
    select(long, lat, direccion, barrio, region, comuna, categoria, capacidad)

#MapaBA####

#base
comunas <- read_sf('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/comunas/comunas.csv') %>% 
    rename("comuna"=comunas)

#movilidad
ciclovias <- read_sf('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/ciclovias/ciclovias.csv') %>% 
    select(geometry, nomoficial) %>% 
    rename("nombre"=nomoficial) %>% 
    mutate(categoria="Ciclovias")

metrobus_lineas <- read_sf('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/metrobus/recorrido-de-metrobus.csv') %>% 
    select(geometry, metrobus) %>% 
    rename("nombre"=metrobus) %>% 
    mutate(categoria="Metrobus")

tren_lineas <- read_sf('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/estaciones-de-ferrocarril/red-de-ferrocarril.csv') %>% 
    select(geometry, linea_2) %>% 
    rename("nombre"=linea_2) %>% 
    mutate(categoria="Ferrocarriles")

subte_lineas <- read_sf('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/subte-estaciones/lineas-de-subte.csv') %>% 
    select(geometry, lineasub) %>% 
    rename("nombre"=lineasub) %>% 
    mutate(categoria="Subtes")

movilidad_lineas <- rbind(metrobus_lineas, ciclovias, tren_lineas) %>% 
    mutate(categoria=as.factor(categoria))

#senderos
senderos_lineas <- read_sf('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/senderos-seguros/senderos-seguros.csv') %>% 
    mutate(categoria='Senderos escolares') %>% 
    select(nombre, categoria, geometry)

#distritos

distritos <- read_sf('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/distritos-economicos/distritos-economicos.csv')%>% 
    mutate(categoria=case_when(id=='5'~'Distrito del Deporte',
                               id=='4'~'Distrito Tecnologico',
                               id=='3'~'Distrito Audiovisual',
                               id=='2'~'Distrito de las Artes',
                               id=='1'~'Distrito de Diseño')) %>% 
    select(nombre, categoria, geometry)

distritos_escolares <- read_sf('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/distritos-escolares/distritos-escolares.csv') %>% 
    mutate(categoria='Distritos escolares') %>% 
    select(nombre, categoria, geometry)

distritos_lineas <- rbind(distritos, distritos_escolares) %>% 
    mutate(categoria=as.factor(categoria))

#iconos

iconos_mapa <- iconList(
    Comisarias=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/comisaria.png', 24, 24),
    'Camaras de control vehicular'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/camara.png', 24, 24),
    'Centros de Integracion Laboral'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/trabajo.png', 24, 24),
    'Edificios publicos'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/gobierno.png', 24, 24),
    'Habilitaciones'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/habilitaciones.png', 24, 24),
    'Sedes comunales'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/gobierno.png', 24, 24),
    'Empresas del Distrito Tecnologico'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/empresas.png', 24, 24),
    'Empresas del Distrito de Diseño'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/empresas.png', 24, 24),
    'Empresas del Distrito de Artes'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/empresas.png', 24, 24),
    'Escuelas en senderos seguros'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/escuela.png', 24, 24),
    'Establecimientos educativos'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/escuela.png', 24, 24),
    'Universidades'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/universidad.png', 24, 24),
    Hospitales=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/hospital.png', 24, 24),
    'Centros Médicos Barriales'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/hospital.png', 24, 24),
    'Centros de Salud Comunitarios'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/hospital.png', 24, 24),
    'Farmacias'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/farmacia.png', 24, 24),
    'Centros de dia'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/centros2.png', 24, 24),
    'Centros de desarrollo infantil'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/marketing.png', 24, 24),
    'Centros de jubilados'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/centros2.png', 24, 24),
    'Juegotecas barriales'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/marketing.png', 24, 24),
    'Centros de primera infancia'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/infancia.png', 24, 24),
    'Casa del niño y el adolescente'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/marketing.png', 24, 24),
    'Hogares y paradores'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/parador.png', 24, 24),
    'Centros integrales de la mujer'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/mujeres.png', 24, 24),
    'Veterinarias'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/veterinaria.png', 24, 24),
    'Monumentos'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/monumentos.png', 24, 24),
    'Obras urbanas' = makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/obras.png', 24, 24),
    'Dependencias culturales' = makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/cultura.png', 24, 24),
    'Bibliotecas'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/biblioteca.png', 24, 24),
    'Ferias'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/ferias.png', 24, 24),
    'Mercados'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/ferias.png', 24, 24),
    'Estaciones saludables'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/hospital.png', 24, 24),
    'Establecimientos gastronomicos'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/gastronomicos.png', 24, 24),
    'Pistas de skate'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/skateboard.png', 24, 24),
    Polideportivos = makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/polideportivos.png', 24, 24),
    'Registro civil'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/gobierno.png', 24, 24),
    'Bus turistico'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/taxis.png', 24, 24),
    'Paradas de taxi'=makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/taxis.png', 24, 24),
    'Campanas verdes' = makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/campanas.png', 24, 24),
    'Centros de clasificacion'= makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/centros.png', 24, 24),
    'Puntos verdes'= makeIcon('https://raw.githubusercontent.com/hernanescu/demo_shiny/master/data/puntosverdes.png', 24, 24)
)

##2 - UI#### 

ui <- dashboardPage(
    dashboardHeader(title='BA Interactiva'),
    dashboardSidebar(
        div(id='barra', #esto es necesario para el botón que resetea la barra
        br(),
        pickerInput("reggral",
                    "Región",
                    choices = c("Norte","Centro","Sur"),
                    multiple = TRUE,
                    options= list(`actions-box`=TRUE,
                                  `select-all-text` = "Seleccionar todo",
                                  `deselect-all-text` = "Deseleccionar todo",
                                  `none-selected-text` = "Elija opciones")),
        br(),
        pickerInput("comugral",
                    "Comuna",
                    choices = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"),
                    multiple = TRUE,
                    options= list(`actions-box`=TRUE,
                                  `select-all-text` = "Seleccionar todo",
                                  `deselect-all-text` = "Deseleccionar todo",
                                  `none-selected-text` = "Elija opciones"))
        
        ),
        br(),
        hr(),
        br(),
        actionButton('resetbarra', 'Resetear la barra'),
        br(),
        actionButton('resetcuerpo', 'Resetear el cuerpo')
    ),#cierre del sidebar
    dashboardBody(
        useShinyjs(),
        tags$head(tags$script(src = jsfile),
                  tags$style(HTML('
                  /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #177e89;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #177e89;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #177e89;
                                }
                                
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #177e89;
                                }
                                
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #fdfdf9;
                                }
                                
                                .nav-tabs>li>a {
                                color: #000000;
                                }
                                
                                  '))),
        div(id='cuerpo', #esto es necesario para el botón que resetea el cuerpo
        tabsetPanel(
            tabPanel(h4('Inicio'),
                     tabPanel('Manual instructivo',
                                  h1(p(strong("Buenos Aires - Una visión interactiva"))),
                                  h3('Desarrollado por Hernán Escudero'),
                                  hr(),
                                  h3(p(strong("¡Bienvenidx!"))),
                                  p('Soy Hernán Escudero, sociólogo, periodista y data scientist. Esta herramienta fue creada utilizando R y diversos paquetes 
                                    complementarios. Toda la magia de manipulación del código y datasets opera tras bambalinas, con lo que un 
                                    usuario final puede manipular distintos elementos sin preocuparse por los detalles técnicos.'),
                                  br(),
                                  p('Esta plataforma incorpora más de 40 datasets con información georeferenciada. Mientras que la solapa Resumen crea gráficos a partir de 
                                    algunos de los datasets seleccionados, en MapaBA puede plotear toda la información y ver en qué ubicación de la Ciudad se encuentran 
                                    los distintos elementos.'),
                                  br(),
                                  p('Toda la información está extraída del Portal de Datos del Gobierno de la Ciudad de Buenos Aires.'), 
                                  tags$a(href="https://data.buenosaires.gob.ar/", 'Buenos Aires Data'),
                                  hr(),
                                  h3(strong(p('Uso general'))),
                                  p('La plataforma cuenta con una barra desplegable a la izquierda, donde puede hacer una 
                                    primera selección general por comunas y por regiones. Clickeando en las tres líneas horizontales de la parte superior, esa barra se muestra 
                                    y se oculta a su elección. A la vez, dentro de cada una de las bases de datos cuenta con múltiples selectores individuales, lo que le 
                                    permite hacer una selección cruzada de más de una base.'),
                                  p('El selector de la barra es general, lo que quiere decir que aplica a todas las bases de datos a la vez. De este modo, si usted quiere 
                                    ver información de escuelas de la comuna 7 y hospitales de la comuna 15, el selector general deberá ser desactivado y usar sólo los que 
                                    están adentro de cada solapa.'),
                                  p('Puede bajar los contenidos filtrados en formato excel (.xlsx) clickeando en el botón de Descarga, y lo mismo ocurre con los gráficos (.png). 
                                    En cuanto al mapa interactivo, en el ploteo mismo también tendrá la opción de descargarlo clickeando en el botón con forma de flecha hacia 
                                    abajo que aparece en el mapa. Es importante notar que la descarga del mapa no es compatible con los exploradores IE y Edge, por lo que 
                                    se sugiere fuertemente acceder a través de Chrome o Firefox.'),
                                  p('Los botones de la barra le permiten resetear las opciones seleccionadas, tanto en la misma barra como en el 
                                    contenido general filtrado a partir de los selectores individuales de cada pestaña.'),
                                  p('Tenga presente que Obras tiene selectores adicionales que deberá elegir para ver la información tanto en el Resumen como en el MapaBA.'),
                                  hr(),
                                  h3(strong(p('Contacto'))),
                                  p('Puede contactarme tanto por correo como por LinkedIn. ¡Cualquier comentario es bien recibido!'),
                                  tags$a(href="mailto:hernanescu@gmail.com", 'Gmail'),
                                  br(),
                                  tags$a(href="https://www.linkedin.com/in/hernanescudero/", 'LinkedIn')
                     )
                     ),
            tabPanel(h4('Seguridad y administración'), #además del general, cada pestaña tiene su selector interno
                     tabsetPanel(
                         tabPanel('Seguridad',
                                  fluidRow(
                                      column(6,
                                             br(),
                                             pickerInput("regseg",
                                                         "Región",
                                                         choices = c("Norte","Centro","Sur"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      ),
                                      column(6,
                                             br(),
                                             pickerInput("comuseg",
                                                         "Comuna",
                                                         choices = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      )
                                    ),
                                  br(),
                                  h4('Comisarías'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tablecomisarias"),
                                  br(),
                                  downloadButton(outputId = 'dltablecomisarias', label = 'Descarga'),
                                  br(),
                                  h4('Cámaras de control vehicular'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tablecamaras"),
                                  br(),
                                  downloadButton(outputId = 'dltablecamaras', label = 'Descarga')
                                  ),
                         tabPanel('Trabajo',
                                  fluidRow(
                                      column(6,
                                             br(),
                                             pickerInput("regtrabajo",
                                                         "Región",
                                                         choices = c("Norte","Centro","Sur"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      ),
                                      column(6,
                                             br(),
                                             pickerInput("comutrabajo",
                                                         "Comuna",
                                                         choices = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      )
                                  ),
                                  br(),
                                  h4('Centros de Integración Laboral'),
                                  br(),
                                  DT::dataTableOutput(outputId = 'tableceil'),
                                  br(),
                                  downloadButton(outputId = 'dltableceil', label='Descarga')
                                  ),
                         tabPanel('Administración Pública',
                                  fluidRow(
                                      column(6,
                                             br(),
                                             pickerInput("regadmi",
                                                         "Región",
                                                         choices = c("Norte","Centro","Sur"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      ),
                                      column(6,
                                             br(),
                                             pickerInput("comuadmi",
                                                         "Comuna",
                                                         choices = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      )
                                  ),
                                  br(),
                                  h4('Sedes Comunales'),
                                  br(),
                                  DT::dataTableOutput(outputId = 'tablesedecomu'),
                                  br(),
                                  downloadButton(outputId = 'dltablesedecomu', label='Descarga'),
                                  br(),
                                  br(),
                                  h4('Edificios públicos'),
                                  br(),
                                  DT::dataTableOutput(outputId = 'tableedifpub'),
                                  br(),
                                  downloadButton(outputId = 'dltableedifpub', label='Descarga'),
                                  br(),
                                  br(),
                                  h4('Habilitaciones'),
                                  br(),
                                  DT::dataTableOutput(outputId = 'tablehabi'),
                                  br(),
                                  downloadButton(outputId = 'dltablehabi', label='Descarga')
                                  ),
                         tabPanel('Economía y Finanzas',
                                  fluidRow(
                                      column(6,
                                             br(),
                                             pickerInput("regecon",
                                                         "Región",
                                                         choices = c("Norte","Centro","Sur"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      ),
                                      column(6,
                                             br(),
                                             pickerInput("comuecon",
                                                         "Comuna",
                                                         choices = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      )
                                  ),
                                  br(),
                                  h4('Empresas del distrito tecnológico'),
                                  br(),
                                  DT::dataTableOutput(outputId = 'tabledistrtecno'),
                                  br(),
                                  downloadButton(outputId = 'dltabledistrtecno', label='Descarga'),
                                  br(),
                                  br(),
                                  h4('Empresas del distrito de diseño'),
                                  br(),
                                  DT::dataTableOutput(outputId = 'tabledistrdis'),
                                  br(),
                                  downloadButton(outputId = 'dltabledistrdis', label='Descarga'),
                                  br(),
                                  br(),
                                  h4('Empresas del distrito de artes'),
                                  br(),
                                  DT::dataTableOutput(outputId = 'tabledistrartes'),
                                  br(),
                                  downloadButton(outputId = 'dltabledistrartes', label='Descarga')
                         )
                     )),
            tabPanel(h4('Social'),
                     tabsetPanel(
                         tabPanel('Educación',
                                  fluidRow(
                                      column(6,
                                             br(),
                                             pickerInput("regedu",
                                                         "Región",
                                                         choices = c("Norte","Centro","Sur"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      ),
                                      column(6,
                                             br(),
                                             pickerInput("comuedu",
                                                         "Comuna",
                                                         choices = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      )
                                  ),
                                  br(),
                                  h4('Escuelas en senderos seguros'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tableescuseguros"),
                                  br(),
                                  downloadButton(outputId = 'dltableescuseguros', label = 'Descarga'),
                                  br(),
                                  br(),
                                  h4('Universidades'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tableuniv"),
                                  br(),
                                  downloadButton(outputId = 'dltableuniv', label = 'Descarga'),
                                  br(),
                                  br(),
                                  h4('Establecimientos educativos'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tableestabedu"),
                                  br(),
                                  downloadButton(outputId = 'dltableestabedu', label = 'Descarga')
                         ),
                         tabPanel('Salud',
                                  fluidRow(
                                      column(6,
                                             br(),
                                             pickerInput("regsalud",
                                                         "Región",
                                                         choices = c("Norte","Centro","Sur"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      ),
                                      column(6,
                                             br(),
                                             pickerInput("comusalud",
                                                         "Comuna",
                                                         choices = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      )
                                  ),
                                  br(),
                                  h4('Centros Médicos Barriales'),
                                  br(),
                                  DT::dataTableOutput(outputId = 'tablecmb'),
                                  br(),
                                  downloadButton(outputId = 'dltablecmb', label='Descarga'),
                                  br(),
                                  br(),
                                  h4('Hospitales'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tablehospitales"),
                                  br(),
                                  downloadButton(outputId = 'dltablehospitales', label = 'Descarga'),
                                  br(),
                                  br(),
                                  h4('Centros de Salud Comunitarios'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tablecesac"),
                                  br(),
                                  downloadButton(outputId = 'dltablecesac', label = 'Descarga'),
                                  br(),
                                  br(),
                                  h4('Farmacias'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tablefarmacias"),
                                  br(),
                                  downloadButton(outputId = 'dltablefarmacias', label = 'Descarga')
                         ),
                         tabPanel('Desarrollo humano y hábitat',
                                  fluidRow(
                                      column(6,
                                             br(),
                                             pickerInput("regdhh",
                                                         "Región",
                                                         choices = c("Norte","Centro","Sur"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      ),
                                      column(6,
                                             br(),
                                             pickerInput("comudhh",
                                                         "Comuna",
                                                         choices = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      )
                                  ),
                                  br(),
                                  h4('Centros de día'),
                                  br(),
                                  DT::dataTableOutput(outputId = 'tablecentrosdia'),
                                  br(),
                                  downloadButton(outputId = 'dltablecentrosdia', label='Descarga'),
                                  br(),
                                  br(),
                                  h4('Centros de desarrollo infantil'),
                                  br(),
                                  DT::dataTableOutput(outputId = 'tablecentrosinfantil'),
                                  br(),
                                  downloadButton(outputId = 'dltablecentrosinfantil', label='Descarga'),
                                  br(),
                                  br(),
                                  h4('Centros de jubilados'),
                                  br(),
                                  DT::dataTableOutput(outputId = 'tablecentrosjubilados'),
                                  br(),
                                  downloadButton(outputId = 'dltablecentrosjubilados', label='Descarga'),
                                  br(),
                                  br(),
                                  h4('Juegotecas barriales'),
                                  br(),
                                  DT::dataTableOutput(outputId = 'tablejuegotecas'),
                                  br(),
                                  downloadButton(outputId = 'dltablejuegotecas', label='Descarga'),
                                  br(),
                                  br(),
                                  h4('Centros de Primera Infancia'),
                                  br(),
                                  DT::dataTableOutput(outputId = 'tablecpi'),
                                  br(),
                                  downloadButton(outputId = 'dltablecpi', label='Descarga'),
                                  br(),
                                  br(),
                                  h4('Casa del Niño y el Adolescente'),
                                  br(),
                                  DT::dataTableOutput(outputId = 'tablecasanino'),
                                  br(),
                                  downloadButton(outputId = 'dltablecasanino', label='Descarga')
                                  ),
                         tabPanel('Integración',
                                  fluidRow(
                                      column(6,
                                             br(),
                                             pickerInput("reginteg",
                                                         "Región",
                                                         choices = c("Norte","Centro","Sur"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      ),
                                      column(6,
                                             br(),
                                             pickerInput("comuinteg",
                                                         "Comuna",
                                                         choices = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      )
                                  ),
                                  br(),
                                  h4('Hogares y paradores'),
                                  br(),
                                  DT::dataTableOutput(outputId = 'tableparadores'),
                                  br(),
                                  downloadButton(outputId = 'dltableparadores', label='Descarga')
                                  ),
                         tabPanel('Género',
                                  fluidRow(
                                      column(6,
                                             br(),
                                             pickerInput("reggenero",
                                                         "Región",
                                                         choices = c("Norte","Centro","Sur"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      ),
                                      column(6,
                                             br(),
                                             pickerInput("comugenero",
                                                         "Comuna",
                                                         choices = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      )
                                  ),
                                  br(),
                                  h4('Centros Integrales de la Mujer'),
                                  br(),
                                  DT::dataTableOutput(outputId = 'tablecim'),
                                  br(),
                                  downloadButton(outputId = 'dltablecim', label='Descarga')
                                  )
                     )),
            tabPanel(h4('Disfrute'),
                     tabsetPanel(
                         tabPanel('Ambiente y espacios públicos', 
                                  fluidRow(
                                      column(6,
                                             br(),
                                             pickerInput("regmayep",
                                                         "Región",
                                                         choices = c("Norte","Centro","Sur"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      ),
                                      column(6,
                                             br(),
                                             pickerInput("comumayep",
                                                         "Comuna",
                                                         choices = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      )
                                  ),
                                  br(),
                                  h4('Centros de Atención Veterinaria'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tableveterinaria"),
                                  br(),
                                  downloadButton(outputId = 'dltableveterinaria', label = 'Descarga'),
                                  br(),
                                  br(),
                                  h4('Puntos verdes'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tablepuntosverdes"),
                                  br(),
                                  downloadButton(outputId = 'dltablepuntosverdes', label = 'Descarga'),
                                  br(),
                                  br(),
                                  h4('Campanas verdes'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tablecampanas"),
                                  br(),
                                  downloadButton(outputId = 'dltablecampanas', label = 'Descarga'),
                                  br(),
                                  br(),
                                  h4('Monumentos'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tablemonumentos"),
                                  br(),
                                  downloadButton(outputId = 'dltablemonumentos', label = 'Descarga'),
                                  br(),
                                  br(),
                                  h4('Centros de clasificacion'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tablecentrosclas"),
                                  br(),
                                  downloadButton(outputId = 'dltablecentrosclas', label = 'Descarga')
                                  
                         ),
                         tabPanel('Cultura',
                                  fluidRow(
                                      column(6,
                                             br(),
                                             pickerInput("regcult",
                                                         "Región",
                                                         choices = c("Norte","Centro","Sur"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      ),
                                      column(6,
                                             br(),
                                             pickerInput("comucult",
                                                         "Comuna",
                                                         choices = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      )
                                  ),
                                  br(),
                                  h4('Dependencias culturales'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tablecult"),
                                  br(),
                                  downloadButton(outputId = 'dltablecult', label = 'Descarga'),
                                  br(),
                                  br(),
                                  h4('Bibliotecas'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tablebiblio"),
                                  br(),
                                  downloadButton(outputId = 'dltablebiblio', label = 'Descarga')
                         ),
                         tabPanel('Disfrute',
                                  fluidRow(
                                      column(6,
                                             br(),
                                             pickerInput("regvice",
                                                         "Región",
                                                         choices = c("Norte","Centro","Sur"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      ),
                                      column(6,
                                             br(),
                                             pickerInput("comuvice",
                                                         "Comuna",
                                                         choices = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      )
                                  ),
                                  h4('Ferias'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tableferias"),
                                  br(),
                                  downloadButton(outputId = 'dltableferias', label = 'Descarga'),
                                  br(),
                                  br(),
                                  h4('Mercados'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tablemercados"),
                                  br(),
                                  downloadButton(outputId = 'dltablemercados', label = 'Descarga'),
                                  br(),
                                  br(),
                                  h4('Estaciones saludables'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tableestsal"),
                                  br(),
                                  downloadButton(outputId = 'dltableestsal', label = 'Descarga'),
                                  br(),
                                  br(),
                                  h4('Establecimientos gastronómicos'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tablegastro"),
                                  br(),
                                  downloadButton(outputId = 'dltablegastro', label = 'Descarga')
                                  
                         ),
                         tabPanel('Deportes',
                                  fluidRow(
                                      column(6,
                                             br(),
                                             pickerInput("regdepo",
                                                         "Región",
                                                         choices = c("Norte","Centro","Sur"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      ),
                                      column(6,
                                             br(),
                                             pickerInput("comudepo",
                                                         "Comuna",
                                                         choices = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      )
                                  ),
                                  h4('Polideportivos'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tablepolideportivos"),
                                  br(),
                                  downloadButton(outputId = 'dltablepolideportivos', label = 'Descarga'),
                                  br(),
                                  br(),
                                  h4('Pistas de skate'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tableskate"),
                                  br(),
                                  downloadButton(outputId = 'dltableskate', label = 'Descarga')
                                  ),
                        tabPanel('Sitios de interés',
                                 fluidRow(
                                     column(6,
                                            br(),
                                            pickerInput("regsitios",
                                                        "Región",
                                                        choices = c("Norte","Centro","Sur"),
                                                        multiple = TRUE,
                                                        options= list(`actions-box`=TRUE,
                                                                      `select-all-text` = "Seleccionar todo",
                                                                      `deselect-all-text` = "Deseleccionar todo",
                                                                      `none-selected-text` = "Elija opciones"))
                                     ),
                                     column(6,
                                            br(),
                                            pickerInput("comusitios",
                                                        "Comuna",
                                                        choices = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"),
                                                        multiple = TRUE,
                                                        options= list(`actions-box`=TRUE,
                                                                      `select-all-text` = "Seleccionar todo",
                                                                      `deselect-all-text` = "Deseleccionar todo",
                                                                      `none-selected-text` = "Elija opciones"))
                                     )
                                 ),
                                 br(),
                                 h4('Registro civil'),
                                 br(),
                                 DT::dataTableOutput(outputId = "tableregistrocivil"),
                                 br(),
                                 downloadButton(outputId = 'dltableregistrocivil', label = 'Descarga')
                        )
                     )),
            tabPanel(h4('Transformación'),
                     tabsetPanel(
                         tabPanel('Obras urbanas',
                                  fluidRow(
                                      column(3,
                                             br(),
                                             pickerInput(inputId = "ejemdu",
                                                         label="Eje de obras",
                                                         choices = c("Disfrute", "Social", "Transformación"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      ),
                                      column(3,
                                             br(),
                                             pickerInput("tipomdu",
                                                         "Tipo de obras",
                                                         choices = c("Arquitectura", "Escuelas", "Espacio Público", "Hidráulica e Infraestructura",
                                                                     "Salud", "Transporte", "Vivienda"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      ),
                                      column(3,
                                             br(),
                                             dateRangeInput(inputId = "inimdu",
                                                            label="Rango de inicio de obras",
                                                            start = "2011-01-01",
                                                            end = "2019-10-01",
                                                            min="2011-01-01",
                                                            max="2019-10-01")
                                      ),
                                      column(3,
                                             br(),
                                             dateRangeInput(inputId = "finmdu",
                                                            label="Rango de fin de obras",
                                                            start = "2015-04-01",
                                                            end = "2021-08-01",
                                                            min="2015-04-01",
                                                            max="2021-08-01")
                                      )
                                  ),
                                  fluidRow(
                                      column(6,
                                             br(),
                                             pickerInput("regobras",
                                                         "Región",
                                                         choices = c("Norte","Centro","Sur"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      ),
                                      column(6,
                                             br(),
                                             pickerInput("comuobras",
                                                         "Comuna",
                                                         choices = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      )
                                  ),
                                  br(),
                                  br(),
                                  DT::dataTableOutput(outputId = "tableobras"),
                                  br(),
                                  downloadButton(outputId = 'dltableobras', label = 'Descarga')
                                  
                         ),
                         tabPanel('Transporte',
                                  fluidRow(
                                      column(6,
                                             br(),
                                             pickerInput("regtrans",
                                                         "Región",
                                                         choices = c("Norte","Centro","Sur"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      ),
                                      column(6,
                                             br(),
                                             pickerInput("comutrans",
                                                         "Comuna",
                                                         choices = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"),
                                                         multiple = TRUE,
                                                         options= list(`actions-box`=TRUE,
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `none-selected-text` = "Elija opciones"))
                                      )
                                  ),
                                  br(),
                                  h4('Paradas de taxi'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tabletaxi"),
                                  br(),
                                  downloadButton(outputId = 'dltabletaxi', label = 'Descarga')
                         )
                     )),
            tabPanel(h4('Resumen'),
                     tabsetPanel(
                         tabPanel('Comunas',
                                  h4('Servicios sociales'),
                                  br(),
                                  plotOutput(outputId = 'serviciossocialescomu'),
                                  br(),
                                  downloadButton(outputId = 'dlserviciossocialescomu', label = 'Descarga'),
                                  br(),
                                  br(),
                                  h4('Disfrute'),
                                  br(),
                                  plotOutput(outputId = 'disfrutecomu'),
                                  br(),
                                  downloadButton(outputId = 'dldisfrutecomu', label = 'Descarga'),
                                  br(),
                                  br(),
                                  h4('Obras'),
                                  br(),
                                  plotOutput(outputId = 'obrascomu'),
                                  br(),
                                  downloadButton(outputId = 'dlobrascomu', label = 'Descarga')),
                         tabPanel('Barrios',
                                  h4('Servicios sociales'),
                                  br(),
                                  plotOutput(outputId = 'serviciossocialesbarrios'),
                                  br(),
                                  downloadButton(outputId = 'dlserviciossocialesbarrios', label = 'Descarga'),
                                  br(),
                                  br(),
                                  h4('Disfrute'),
                                  br(),
                                  plotOutput(outputId = 'disfrutebarrios'),
                                  br(),
                                  downloadButton(outputId = 'dldisfrutebarrios', label = 'Descarga'),
                                  br(),
                                  br(),
                                  h4('Obras'),
                                  br(),
                                  plotOutput(outputId = 'obrasbarrios'),
                                  br(),
                                  downloadButton(outputId = 'dlobrasbarrios', label = 'Descarga')),
                         tabPanel('Tablas de resumen',
                                  h4('Servicios sociales'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tableserviciosocialesresumen"),
                                  br(),
                                  downloadButton(outputId = 'dltableserviciossocialesresumen', label = 'Descarga'),
                                  br(),
                                  br(),
                                  h4('Disfrute'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tabledisfrutetotalresumen"),
                                  br(),
                                  downloadButton(outputId = 'dltabledisfrutetotalresumen', label = 'Descarga'),
                                  br(),
                                  br(),
                                  h4('Obras'),
                                  br(),
                                  DT::dataTableOutput(outputId = "tableobrasresumen"),
                                  br(),
                                  br(),
                                  downloadButton(outputId = 'dltableobrasresumen', label = 'Descarga')
                         )
                     )),
            tabPanel(h4('MapaBA'),
                     tags$head(tags$script(src=jsfile)), #este es el agregado para poder bajar el mapa de leaflet
                     fluidRow(
                         column(3,
                                br(),
                                pickerInput("filtersoc",
                                            "Seguridad y administración",
                                            choices = c('Comisarias', 'Camaras de control vehicular', 'Centros de Integracion Laboral',
                                                        'Edificios publicos', 'Habilitaciones', 'Sedes comunales',
                                                        'Empresas del Distrito Tecnologico', 'Empresas del Distrito de Diseño', 'Empresas del Distrito de Artes'),
                                            multiple = TRUE,
                                            options= list(`actions-box`=TRUE,
                                                          `select-all-text` = "Seleccionar todo",
                                                          `deselect-all-text` = "Deseleccionar todo",
                                                          `none-selected-text` = "Elija opciones"))
                         ),
                         column(3,
                                br(),
                                pickerInput("filtersocial",
                                            "Social",
                                            choices = c('Escuelas en senderos seguros', 'Universidades', 'Establecimientos educativos',
                                                        'Centros Médicos Barriales', 'Hospitales', 'Centros de Salud Comunitarios', 'Farmacias',
                                                        'Centros de dia', 'Centros de desarrollo infantil', 'Centros de jubilados', 'Juegotecas barriales', 'Centros de primera infancia', 'Casa del niño y el adolescente',
                                                        'Hogares y paradores', 'Centros integrales de la mujer'),
                                            multiple = TRUE,
                                            options= list(`actions-box`=TRUE,
                                                          `select-all-text` = "Seleccionar todo",
                                                          `deselect-all-text` = "Deseleccionar todo",
                                                          `none-selected-text` = "Elija opciones"))
                         ),
                         column(3,
                                br(),
                                pickerInput("filterdisfrute",
                                            "Disfrute",
                                            choices = c('Veterinarias', 'Puntos verdes', 'Campanas verdes', 'Monumentos', 'Centros de clasificacion',
                                                        'Dependencias culturales', 'Bibliotecas', 
                                                        'Ferias', 'Mercados', 'Estaciones saludables', 'Establecimientos gastronomicos',
                                                        'Polideportivos', 'Pistas de skate', 
                                                        'Registro civil', 'Bus turistico'),
                                            multiple = TRUE,
                                            options= list(`actions-box`=TRUE,
                                                          `select-all-text` = "Seleccionar todo",
                                                          `deselect-all-text` = "Deseleccionar todo",
                                                          `none-selected-text` = "Elija opciones"))
                         ),
                         column(3,
                                br(),
                                pickerInput("filtertransformacion",
                                            "Transformación",
                                            choices = c('Obras urbanas', 'Paradas de taxi'),
                                            multiple = TRUE,
                                            options= list(`actions-box`=TRUE,
                                                          `select-all-text` = "Seleccionar todo",
                                                          `deselect-all-text` = "Deseleccionar todo",
                                                          `none-selected-text` = "Elija opciones"))
                         )
                     ),
                     fluidRow(
                         column(3,
                                br(),
                                pickerInput("filtermovilidad",
                                            "Movilidad",
                                            choices = c("Metrobus", "Ciclovias", "Subtes", "Ferrocarriles"),
                                            multiple = TRUE,
                                            options= list(`actions-box`=TRUE,
                                                          `select-all-text` = "Seleccionar todo",
                                                          `deselect-all-text` = "Deseleccionar todo",
                                                          `none-selected-text` = "Elija opciones"))
                         ),
                         column(3,
                                br(),
                                pickerInput("filtersenderos",
                                            "Senderos",
                                            choices = c('Senderos escolares'),
                                            multiple = TRUE,
                                            options= list(`actions-box`=TRUE,
                                                          `select-all-text` = "Seleccionar todo",
                                                          `deselect-all-text` = "Deseleccionar todo",
                                                          `none-selected-text` = "Elija opciones"))
                         ),
                         column(3,
                                br(),
                                pickerInput("filterdistritos",
                                            "Distritos",
                                            choices = c('Distrito del Deporte', 'Distrito Tecnologico', 'Distrito Audiovisual', 
                                                        'Distrito de las Artes', 'Distrito de Diseño'),
                                            multiple = TRUE,
                                            options= list(`actions-box`=TRUE,
                                                          `select-all-text` = "Seleccionar todo",
                                                          `deselect-all-text` = "Deseleccionar todo",
                                                          `none-selected-text` = "Elija opciones"))
                         ),
                         column(3,
                                br(),
                                br(),
                                actionButton('plot', 'Plotear')
                         )
                     ),
                     br(),
                     hr(),
                     br(),
                     leafletOutput(outputId = 'mapaBA', height = 800))
        )
        )#cierre del div 'cuerpo'  
    ),#cierre del cuerpo
    
)#cierre de UI

##3 - Server####
server <- function(input, output, session) {
    
    observeEvent(input$resetbarra, {
        reset('barra')
    })
    
    observeEvent(input$resetcuerpo, {
        reset('cuerpo')
    })
#Seguridad y administracion####

#La forma de tratar esto es siempre igual: un primer subset (holder) donde se junta lo filtrado en el input general,
#seguido de un segundo subset (holder2) donde se junta lo filtrado en el input individual de cada pestaña.
#Luego se junta toda la base en una sola y es el subset a partir del cual se trabaja.
    
#seguridad
    subsetcomi <- reactive({
        holder <- comisarias %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Direccion"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   "Comuna"=comuna)
        
        holder2 <- comisarias %>% 
            filter(comuna %in% input$comuseg) %>% 
            filter(region %in% input$regseg) %>%
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Direccion"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   "Comuna"=comuna)
        
        rbind(holder, holder2)
    })
    
    output$tablecomisarias <- DT::renderDataTable({
        DT::datatable(data=subsetcomi())
    })
    
    output$dltablecomisarias <- downloadHandler(
        filename=function(){
            paste('comisarias-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetcomi(), file)
        }
    )
    
    subsetcamaras <- reactive({
        holder <- camaras %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(tipo, direccion, barrio, region, comuna) %>% 
            rename("Tipo"=tipo,
                   "Direccion"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   "Comuna"=comuna)
        
        holder2 <- camaras %>% 
            filter(comuna %in% input$comuseg) %>% 
            filter(region %in% input$regseg) %>%
            select(tipo, direccion, barrio, region, comuna) %>% 
            rename("Tipo"=tipo,
                   "Direccion"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   "Comuna"=comuna)
        
        rbind(holder, holder2)
    })
    
    output$tablecamaras <- DT::renderDataTable({
        DT::datatable(data=subsetcamaras())
    })
    
    output$dltablecamaras <- downloadHandler(
        filename=function(){
            paste('camaras-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetcamaras(), file)
        }
    )
    
#trabajo
    
    subsetceil <- reactive({
        holder <- ceil %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Direccion"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   "Comuna"=comuna)
        
        holder2 <- ceil %>% 
            filter(comuna %in% input$comutrabajo) %>% 
            filter(region %in% input$regtrabajo) %>%
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Direccion"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   "Comuna"=comuna)
        
        rbind(holder, holder2)
    })
    
    output$tableceil <- DT::renderDataTable({
        DT::datatable(data=subsetceil())
    })
    
    output$dltableceil <- downloadHandler(
        filename=function(){
            paste('ceil-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetceil(), file)
        }
    )
    
#administración pública
    
    subsethabi <- reactive({
        holder <- habilitaciones %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(titulares, tipo_tramite, fecha_habilitacion, direccion, barrio, region, comuna) %>% 
            rename("Titulares"=titulares,
                   'Tipo de trámite'=tipo_tramite,
                   'Fecha'=fecha_habilitacion,
                   "Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   "Comuna"=comuna)
        
        holder2 <- habilitaciones %>% 
            filter(comuna %in% input$comuadmi) %>% 
            filter(region %in% input$regadmi) %>%
            select(titulares, tipo_tramite, fecha_habilitacion, direccion, barrio, region, comuna) %>% 
            rename("Titulares"=titulares,
                   'Tipo de trámite'=tipo_tramite,
                   'Fecha'=fecha_habilitacion,
                   "Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   "Comuna"=comuna)
        
        rbind(holder, holder2)
    })
    
    output$tablehabi <- DT::renderDataTable({
        DT::datatable(data=subsethabi())
    })
    
    output$dltablehabi <- downloadHandler(
        filename=function(){
            paste('habilitaciones-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsethabi(), file)
        }
    )
    
    subsetedifpub <- reactive({
        holder <- edifpub %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(tipo, direccion, barrio, region, comuna) %>% 
            rename("Tipo"=tipo,
                   "Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   "Comuna"=comuna)
        
        holder2 <- edifpub %>% 
            filter(comuna %in% input$comuadmi) %>% 
            filter(region %in% input$regadmi) %>%
            select(tipo, direccion, barrio, region, comuna) %>% 
            rename("Tipo"=tipo,
                   "Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   "Comuna"=comuna)
        
        rbind(holder, holder2)
    })
    
    output$tableedifpub <- DT::renderDataTable({
        DT::datatable(data=subsetedifpub())
    })
    
    output$dltableedifpub <- downloadHandler(
        filename=function(){
            paste('edificios-publicos-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetedifpub(), file)
        }
    )
    
    subsetsedecomu <- reactive({
        holder <- sedescomunales %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   "Comuna"=comuna)
        
        holder2 <- sedescomunales %>% 
            filter(comuna %in% input$comuadmi) %>% 
            filter(region %in% input$regadmi) %>%
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   "Comuna"=comuna)
        
        rbind(holder, holder2)
    })
    
    output$tablesedecomu <- DT::renderDataTable({
        DT::datatable(data=subsetsedecomu())
    })
    
    output$dltablesedecomu <- downloadHandler(
        filename=function(){
            paste('sedes-comunales-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetsedecomu(), file)
        }
    )
    
#economia y finanzas
    
    subsetdistrtecno <- reactive({
        holder <- empresastech %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, direccion, sector, origen, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   "Comuna"=comuna,
                   'Origen'=origen,
                   'Sector'=sector)
        
        holder2 <- empresastech %>% 
            filter(comuna %in% input$comuecon) %>% 
            filter(region %in% input$regecon) %>%
            select(nombre, direccion, sector, origen, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   "Comuna"=comuna,
                   'Origen'=origen,
                   'Sector'=sector)
        
        rbind(holder, holder2)
    })
    
    output$tabledistrtecno <- DT::renderDataTable({
        DT::datatable(data=subsetdistrtecno())
    })
    
    output$dltabledistrtecno <- downloadHandler(
        filename=function(){
            paste('empresas-tecnologico-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetdistrtecno(), file)
        }
    )
    
    subsetdistrdis <- reactive({
        holder <- empresadis %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, direccion, tipo, subtipo, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   "Comuna"=comuna,
                   'Tipo'=tipo,
                   'Subtipo'=subtipo)
        
        holder2 <- empresadis %>% 
            filter(comuna %in% input$comuecon) %>% 
            filter(region %in% input$regecon) %>%
            select(nombre, direccion, tipo, subtipo, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   "Comuna"=comuna,
                   'Tipo'=tipo,
                   'Subtipo'=subtipo)
        
        rbind(holder, holder2)
    })
    
    output$tabledistrdis <- DT::renderDataTable({
        DT::datatable(data=subsetdistrdis())
    })
    
    output$dltabledistrdis <- downloadHandler(
        filename=function(){
            paste('empresas-diseño-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetdistrdis(), file)
        }
    )
    
    subsetdistrartes <- reactive({
        holder <- empresaarte %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, direccion, tipo, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   "Comuna"=comuna,
                   'Tipo'=tipo)
        
        holder2 <- empresaarte %>% 
            filter(comuna %in% input$comuecon) %>% 
            filter(region %in% input$regecon) %>%
            select(nombre, direccion, tipo, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   "Comuna"=comuna,
                   'Tipo'=tipo)
        
        rbind(holder, holder2)
    })
    
    output$tabledistrartes <- DT::renderDataTable({
        DT::datatable(data=subsetdistrartes())
    })
    
    output$dltabledistrartes <- downloadHandler(
        filename=function(){
            paste('empresas-artes-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetdistrartes(), file)
        }
    )
    
#Social####
#educacion   
    subsetescuseguros <- reactive({
        holder <- escuseguros %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, sendero, sector, direccion, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Sendero"=sendero,
                   "Sector"=sector,
                   "Direccion"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna)
        
        holder2 <- escuseguros %>% 
            filter(comuna %in% input$comuedu) %>% 
            filter(region %in% input$regedu) %>% 
            select(nombre, sendero, sector, direccion, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Sendero"=sendero,
                   "Sector"=sector,
                   "Direccion"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna)
        
        rbind(holder, holder2)
    })
    
    output$tableescuseguros <- DT::renderDataTable({
        DT::datatable(data=subsetescuseguros())
    })
    
    output$dltableescuseguros <- downloadHandler(
        filename=function(){
            paste('escuelas-senderos-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetescuseguros(), file)
        }
    )
    
    subsetuniv <- reactive({
        holder <- universidades %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, unidad, regimen, direccion, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Unidad"=unidad,
                   "Régimen"=regimen,
                   "Direccion"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna)
        
        holder2 <- universidades %>% 
            filter(comuna %in% input$comuedu) %>% 
            filter(region %in% input$regedu) %>% 
            select(nombre, unidad, regimen, direccion, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Unidad"=unidad,
                   "Régimen"=regimen,
                   "Direccion"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna)
        
        rbind(holder, holder2)
    })
    
    output$tableuniv <- DT::renderDataTable({
        DT::datatable(data=subsetuniv())
    })
    
    output$dltableuniv <- downloadHandler(
        filename=function(){
            paste('universidades-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetuniv(), file)
        }
    )
    
    subsetestabedu <- reactive({
        holder <- estabedu %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, nivel, areaprogra, direccion, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Nivel"=nivel,
                   "Area"=areaprogra,
                   "Direccion"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna)
        
        holder2 <- estabedu %>% 
            filter(comuna %in% input$comuedu) %>% 
            filter(region %in% input$regedu) %>% 
            select(nombre, nivel, areaprogra, direccion, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Nivel"=nivel,
                   "Area"=areaprogra,
                   "Direccion"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna)
        
        rbind(holder, holder2)
    })
    
    output$tableestabedu <- DT::renderDataTable({
        DT::datatable(data=subsetestabedu())
    })
    
    output$dltableestabedu <- downloadHandler(
        filename=function(){
            paste('establecimientos-educativos-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetestabedu(), file)
        }
    )
    
    
    
#salud
    
    subsethospi <- reactive({
        holder <- hospitales %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, tipo, tipo_espec, direccion, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Tipo"=tipo,
                   "Especialidad"=tipo_espec,
                   "Direccion"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna)
        
        holder2 <- hospitales %>% 
            filter(comuna %in% input$comusalud) %>% 
            filter(region %in% input$regsalud) %>% 
            select(nombre, tipo, tipo_espec, direccion, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Tipo"=tipo,
                   "Especialidad"=tipo_espec,
                   "Direccion"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna)
        
        rbind(holder, holder2)
        
    })
    
    output$tablehospitales <- DT::renderDataTable({
        DT::datatable(data=subsethospi())
    })
    
    output$dltablehospitales <- downloadHandler(
        filename=function(){
            paste('hospitales-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsethospi(), file)
        }
    )
    
    subsetcmb <- reactive({
        holder <- cmb %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, area, especialidad, direccion, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Área"=area,
                   "Especialidad"=especialidad,
                   "Direccion"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna)
        
        holder2 <- cmb %>% 
            filter(comuna %in% input$comusalud) %>% 
            filter(region %in% input$regsalud) %>% 
            select(nombre, area, especialidad, direccion, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Área"=area,
                   "Especialidad"=especialidad,
                   "Direccion"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna)
        
        rbind(holder, holder2)
        
    })
    
    output$tablecmb <- DT::renderDataTable({
        DT::datatable(data=subsetcmb())
    })
    
    output$dltablecmb <- downloadHandler(
        filename=function(){
            paste('cmb-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetcmb(), file)
        }
    )
    
    subsetcesac <- reactive({
        holder <- cesac %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, area_progr, especialidad, direccion, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Área"=area_progr,
                   "Especialidad"=especialidad,
                   "Direccion"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna)
        
        holder2 <- cesac %>% 
            filter(comuna %in% input$comusalud) %>% 
            filter(region %in% input$regsalud) %>% 
            select(nombre, area_progr, especialidad, direccion, barrio, region, comuna) %>% 
            rename("Nombre"=nombre,
                   "Área"=area_progr,
                   "Especialidad"=especialidad,
                   "Direccion"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna)
        
        rbind(holder, holder2)
        
    })
    
    output$tablecesac <- DT::renderDataTable({
        DT::datatable(data=subsetcesac())
    })
    
    output$dltablecesac <- downloadHandler(
        filename=function(){
            paste('cesac-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetcesac(), file)
        }
    )
    
    subsetfarmacias <- reactive({
        holder <- farmacias %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(direccion, barrio, region, comuna) %>% 
            rename("Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna)
        
        holder2 <- farmacias %>% 
            filter(comuna %in% input$comusalud) %>% 
            filter(region %in% input$regsalud) %>% 
            select(direccion, barrio, region, comuna) %>% 
            rename("Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna)
        
        rbind(holder, holder2)
        
    })
    
    output$tablefarmacias <- DT::renderDataTable({
        DT::datatable(data=subsetfarmacias())
    })
    
    output$dltablefarmacias <- downloadHandler(
        filename=function(){
            paste('farmacias-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetfarmacias(), file)
        }
    )
    
   
#desarrollo humano y habitat
    
    subsetcentrosdia <- reactive({
        holder <- centrosdia %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename("Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna,
                   'Nombre'=nombre)
        
        holder2 <- centrosdia %>% 
            filter(comuna %in% input$comudhh) %>% 
            filter(region %in% input$regdhh) %>% 
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename("Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna,
                   'Nombre'=nombre)
        
        rbind(holder, holder2)
        
    })
    
    output$tablecentrosdia <- DT::renderDataTable({
        DT::datatable(data=subsetcentrosdia())
    })
    
    output$dltablecentrosdia <- downloadHandler(
        filename=function(){
            paste('centros-de-dia-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetcentrosdia(), file)
        }
    )
    
    subsetcentrosinfantil <- reactive({
        holder <- centrosinfantil %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename("Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna,
                   'Nombre'=nombre)
        
        holder2 <- centrosinfantil %>% 
            filter(comuna %in% input$comudhh) %>% 
            filter(region %in% input$regdhh) %>% 
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename("Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna,
                   'Nombre'=nombre)
        
        rbind(holder, holder2)
        
    })
    
    output$tablecentrosinfantil <- DT::renderDataTable({
        DT::datatable(data=subsetcentrosinfantil())
    })
    
    output$dltablecentrosinfantil <- downloadHandler(
        filename=function(){
            paste('centros-desarrollo-infantil-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetcentrosinfantil(), file)
        }
    )
    
    subsetcentrosjubilados <- reactive({
        holder <- centrosjubilados %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename("Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna,
                   'Nombre'=nombre)
        
        holder2 <- centrosjubilados %>% 
            filter(comuna %in% input$comudhh) %>% 
            filter(region %in% input$regdhh) %>% 
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename("Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna,
                   'Nombre'=nombre)
        
        rbind(holder, holder2)
        
    })
    
    output$tablecentrosjubilados <- DT::renderDataTable({
        DT::datatable(data=subsetcentrosjubilados())
    })
    
    output$dltablecentrosjubilados <- downloadHandler(
        filename=function(){
            paste('centros-jubilados-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetcentrosjubilados(), file)
        }
    )
    
    subsetjuegotecas <- reactive({
        holder <- juegotecas %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename("Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna,
                   'Nombre'=nombre)
        
        holder2 <- juegotecas %>% 
            filter(comuna %in% input$comudhh) %>% 
            filter(region %in% input$regdhh) %>% 
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename("Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna,
                   'Nombre'=nombre)
        
        rbind(holder, holder2)
        
    })
    
    output$tablejuegotecas <- DT::renderDataTable({
        DT::datatable(data=subsetjuegotecas())
    })
    
    output$dltablejuegotecas <- downloadHandler(
        filename=function(){
            paste('juegotecas-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetjuegotecas(), file)
        }
    )
    
    subsetcpi <- reactive({
        holder <- cpi %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename("Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna,
                   'Nombre'=nombre)
        
        holder2 <- cpi %>% 
            filter(comuna %in% input$comudhh) %>% 
            filter(region %in% input$regdhh) %>% 
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename("Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna,
                   'Nombre'=nombre)
        
        rbind(holder, holder2)
        
    })
    
    output$tablecpi <- DT::renderDataTable({
        DT::datatable(data=subsetcpi())
    })
    
    output$dltablecpi <- downloadHandler(
        filename=function(){
            paste('cpi-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetcpi(), file)
        }
    )
    
    subsetcasanino <- reactive({
        holder <- casanino %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename("Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna,
                   'Nombre'=nombre)
        
        holder2 <- casanino %>% 
            filter(comuna %in% input$comudhh) %>% 
            filter(region %in% input$regdhh) %>% 
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename("Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna,
                   'Nombre'=nombre)
        
        rbind(holder, holder2)
        
    })
    
    output$tablecasanino <- DT::renderDataTable({
        DT::datatable(data=subsetcasanino())
    })
    
    output$dltablecasanino <- downloadHandler(
        filename=function(){
            paste('casa-nino-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetcasanino(), file)
        }
    )
    
#integracion
    
    subsetparadores <- reactive({
        holder <- paradores %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, destinatario, direccion, barrio, region, comuna) %>% 
            rename("Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna,
                   'Nombre'=nombre,
                   'Destinatario'=destinatario)
        
        holder2 <- paradores %>% 
            filter(comuna %in% input$comuinteg) %>% 
            filter(region %in% input$reginteg) %>% 
            select(nombre, destinatario, direccion, barrio, region, comuna) %>% 
            rename("Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna,
                   'Nombre'=nombre,
                   'Destinatario'=destinatario)
        
        rbind(holder, holder2)
        
    })
    
    output$tableparadores <- DT::renderDataTable({
        DT::datatable(data=subsetparadores())
    })
    
    output$dltableparadores <- downloadHandler(
        filename=function(){
            paste('hogares-paradores-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetparadores(), file)
        }
    )
    
#género
    
    subsetcim <- reactive({
        holder <- cim %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename("Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna,
                   'Nombre'=nombre)
        
        holder2 <- cim %>% 
            filter(comuna %in% input$comugenero) %>% 
            filter(region %in% input$reggenero) %>% 
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename("Dirección"=direccion,
                   "Barrio"=barrio,
                   "Region"=region,
                   'Comuna'=comuna,
                   'Nombre'=nombre)
        
        rbind(holder, holder2)
        
    })
    
    output$tablecim <- DT::renderDataTable({
        DT::datatable(data=subsetcim())
    })
    
    output$dltablecim <- downloadHandler(
        filename=function(){
            paste('centros-mujer-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetcim(), file)
        }
    )

#Disfrute####
    
    subsetveterinaria <- reactive({
        holder <- veterinaria %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, consultas, direccion, barrio, region, comuna) %>% 
            rename('Nombre'=nombre,
                   'Consultas'=consultas,
                   'Dirección'=direccion,
                   'Barrio'=barrio,
                   'Región'=region,
                   'Comuna'=comuna)
        
        holder2 <- veterinaria %>% 
            filter(comuna %in% input$comumayep) %>% 
            filter(region %in% input$regmayep) %>% 
            select(nombre, consultas, direccion, barrio, region, comuna) %>% 
            rename('Nombre'=nombre,
                   'Consultas'=consultas,
                   'Dirección'=direccion,
                   'Barrio'=barrio,
                   'Región'=region,
                   'Comuna'=comuna)
        
        rbind(holder, holder2)
        
    })
    
    output$tableveterinaria <- DT::renderDataTable({
        DT::datatable(data=subsetveterinaria())
    })
    
    output$dltableveterinaria <- downloadHandler(
        filename=function(){
            paste('veterinarias-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetveterinaria(), file)
        }
    )
    
    subsetpuntosverdes <- reactive({
        holder <- puntosverdes %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, materiales, tipo, direccion, barrio, region, comuna) %>% 
            rename('Nombre'=nombre,
                   'Materiales'=materiales,
                   'Tipo'=tipo,
                   'Dirección'=direccion,
                   'Barrio'=barrio,
                   'Región'=region,
                   'Comuna'=comuna)
        
        holder2 <- puntosverdes %>% 
            filter(comuna %in% input$comumayep) %>% 
            filter(region %in% input$regmayep) %>% 
            select(nombre, materiales, tipo, direccion, barrio, region, comuna) %>% 
            rename('Nombre'=nombre,
                   'Materiales'=materiales,
                   'Tipo'=tipo,
                   'Dirección'=direccion,
                   'Barrio'=barrio,
                   'Región'=region,
                   'Comuna'=comuna)
        
        rbind(holder, holder2)
        
    })
    
    output$tablepuntosverdes <- DT::renderDataTable({
        DT::datatable(data=subsetpuntosverdes())
    })
    
    output$dltablepuntosverdes <- downloadHandler(
        filename=function(){
            paste('puntos-verdes-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetpuntosverdes(), file)
        }
    )
    
    subsetcampanas <- reactive({
        holder <- campanas %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(direccion, modelo, barrio, region, comuna) %>% 
            rename('Dirección'=direccion,
                   'Barrio'=barrio,
                   'Región'=region,
                   'Comuna'=comuna,
                   'Modelo'=modelo)
        
        holder2 <- campanas %>% 
            filter(comuna %in% input$comumayep) %>% 
            filter(region %in% input$regmayep) %>% 
            select(direccion, modelo, barrio, region, comuna) %>% 
            rename('Dirección'=direccion,
                   'Barrio'=barrio,
                   'Región'=region,
                   'Comuna'=comuna,
                   'Modelo'=modelo)
        
        rbind(holder, holder2)
        
    })
    
    output$tablecampanas <- DT::renderDataTable({
        DT::datatable(data=subsetcampanas())
    })
    
    output$dltablecampanas <- downloadHandler(
        filename=function(){
            paste('campanas-verdes-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetcampanas(), file)
        }
    )
    
    subsetmonumentos <- reactive({
        holder <- monumentos %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, direccion, material, autores, barrio, region, comuna) %>% 
            rename('Dirección'=direccion,
                   'Barrio'=barrio,
                   'Región'=region,
                   'Comuna'=comuna,
                   'Nombre'=nombre,
                   'Material'=material,
                   'Autores'=autores)
        
        holder2 <- monumentos %>% 
            filter(comuna %in% input$comumayep) %>% 
            filter(region %in% input$regmayep) %>% 
            select(nombre, direccion, material, autores, barrio, region, comuna) %>% 
            rename('Dirección'=direccion,
                   'Barrio'=barrio,
                   'Región'=region,
                   'Comuna'=comuna,
                   'Nombre'=nombre,
                   'Material'=material,
                   'Autores'=autores)
        
        rbind(holder, holder2)
        
    })
    
    output$tablemonumentos <- DT::renderDataTable({
        DT::datatable(data=subsetmonumentos())
    })
    
    output$dltablemonumentos <- downloadHandler(
        filename=function(){
            paste('monumentos-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetmonumentos(), file)
        }
    )
    
    subsetcentros <- reactive({
        holder <- centros %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, administra, direccion, barrio, region, comuna) %>% 
            rename('Dirección'=direccion,
                   'Administración'=administra,
                   'Barrio'=barrio,
                   'Región'=region,
                   'Comuna'=comuna,
                   'Dirección'=direccion)
        
        holder2 <- centros %>% 
            filter(comuna %in% input$comumayep) %>% 
            filter(region %in% input$regmayep) %>% 
            select(nombre, administra, direccion, barrio, region, comuna) %>% 
            rename('Dirección'=direccion,
                   'Administración'=administra,
                   'Barrio'=barrio,
                   'Región'=region,
                   'Comuna'=comuna,
                   'Dirección'=direccion)
        
        rbind(holder, holder2)
        
    })
    
    output$tablecentrosclas <- DT::renderDataTable({
        DT::datatable(data=subsetcentros())
    })
    
    output$dltablecentrosclas <- downloadHandler(
        filename=function(){
            paste('centros-clasificacion-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetcentros(), file)
        }
    )
    
#cultura
    
    subsetcult <- reactive({
        holder <- cultura %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral)%>%
            select(nombre, actividad, direccion, publico, barrio, region, comuna) %>% 
            rename('Nombre'=nombre,
                   'Actividad'=actividad,
                   'Direccion'=direccion,
                   'Barrio'=barrio,
                   'Region'=region,
                   'Publico'=publico,
                   'Comuna'=comuna)
        
        holder2 <- cultura %>% 
            filter(comuna %in% input$comucult) %>% 
            filter(region %in% input$regcult)%>%
            select(nombre, actividad, direccion, publico, barrio, region, comuna) %>% 
            rename('Nombre'=nombre,
                   'Actividad'=actividad,
                   'Direccion'=direccion,
                   'Barrio'=barrio,
                   'Region'=region,
                   'Publico'=publico,
                   'Comuna'=comuna)
        
        rbind(holder, holder2)
    })
    
    output$tablecult <- DT::renderDataTable({
        DT::datatable(data=subsetcult())
    })
    
    output$dltablecult <- downloadHandler(
        filename=function(){
            paste('dependencias-culturales-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetcult(), file)
        }
    )
    
    subsetbiblio <- reactive({
        holder <- bibliotecas %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral)%>%
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename('Nombre'=nombre,
                   'Direccion'=direccion,
                   'Barrio'=barrio,
                   'Region'=region,
                   'Comuna'=comuna)
        
        holder2 <- bibliotecas %>% 
            filter(comuna %in% input$comucult) %>% 
            filter(region %in% input$regcult)%>%
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename('Nombre'=nombre,
                   'Direccion'=direccion,
                   'Barrio'=barrio,
                   'Region'=region,
                   'Comuna'=comuna)
        
        rbind(holder, holder2)
    })
    
    output$tablebiblio <- DT::renderDataTable({
        DT::datatable(data=subsetbiblio())
    })
    
    output$dltablebiblio <- downloadHandler(
        filename=function(){
            paste('bibliotecas-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetbiblio(), file)
        }
    )

#vicejefatura  
    subsetferias <- reactive({
        holder <-  ferias %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral)%>%
            select(nombre, tipo, direccion, barrio, region, comuna) %>% 
            rename('Nombre'=nombre,
                   'Tipo'=tipo,
                   'Dirección'=direccion,
                   'Barrio'=barrio,
                   'Región'=region,
                   'Comuna'=comuna)
        
        holder2 <- ferias %>% 
            filter(comuna %in% input$comuvice) %>% 
            filter(region %in% input$regvice)%>%
            select(nombre, tipo, direccion, barrio, region, comuna) %>% 
            rename('Nombre'=nombre,
                   'Tipo'=tipo,
                   'Dirección'=direccion,
                   'Barrio'=barrio,
                   'Región'=region,
                   'Comuna'=comuna)
        
        rbind(holder, holder2)
    })
    
    output$tableferias <- DT::renderDataTable({
        DT::datatable(data=subsetferias())
    })
    
    output$dltableferias <- downloadHandler(
        filename=function(){
            paste('ferias-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetferias(), file)
        }
    )
    
    subsetmercados <- reactive({
        holder <-  mercados %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral)%>%
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename('Nombre'=nombre,
                   'Dirección'=direccion,
                   'Barrio'=barrio,
                   'Región'=region,
                   'Comuna'=comuna)
        
        holder2 <- mercados %>% 
            filter(comuna %in% input$comuvice) %>% 
            filter(region %in% input$regvice)%>%
            select(nombre, direccion, barrio, region, comuna) %>% 
            rename('Nombre'=nombre,
                   'Dirección'=direccion,
                   'Barrio'=barrio,
                   'Región'=region,
                   'Comuna'=comuna)
        
        rbind(holder, holder2)
    })
    
    output$tablemercados <- DT::renderDataTable({
        DT::datatable(data=subsetmercados())
    })
    
    output$dltablemercados <- downloadHandler(
        filename=function(){
            paste('mercados-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetmercados(), file)
        }
    )
    
    
    subsetestsal <- reactive({
        holder <-  estacionessaludables %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral)%>%
            select(nombre, tipo, estructura, servicios, atencion, barrio, region, comuna) %>% 
            rename('Nombre'=nombre,
                   'Tipo'=tipo,
                   'Estructura'=estructura,
                   'Servicios'=servicios,
                   'Atención'=atencion,
                   'Barrio'=barrio,
                   'Región'=region,
                   'Comuna'=comuna)
        
        holder2 <- estacionessaludables %>% 
            filter(comuna %in% input$comuvice) %>% 
            filter(region %in% input$regvice)%>%
            select(nombre, tipo, estructura, servicios, atencion, barrio, region, comuna) %>% 
            rename('Nombre'=nombre,
                   'Tipo'=tipo,
                   'Estructura'=estructura,
                   'Servicios'=servicios,
                   'Atención'=atencion,
                   'Barrio'=barrio,
                   'Región'=region,
                   'Comuna'=comuna)
        
        rbind(holder, holder2)
    })
    
    output$tableestsal <- DT::renderDataTable({
        DT::datatable(data=subsetestsal())
    })
    
    output$dltableestsal <- downloadHandler(
        filename=function(){
            paste('estaciones-saludables-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetestsal(), file)
        }
    )
    
    subsetgastro <- reactive({
        holder <-  gastro %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral)%>%
            select(nombre, tipo, direccion, barrio, region, comuna) %>% 
            rename('Nombre'=nombre,
                   'Tipo'=tipo,
                   'Dirección'=direccion,
                   'Barrio'=barrio,
                   'Región'=region,
                   'Comuna'=comuna)
        
        holder2 <- gastro %>% 
            filter(comuna %in% input$comuvice) %>% 
            filter(region %in% input$regvice)%>%
            select(nombre, tipo, direccion, barrio, region, comuna) %>% 
            rename('Nombre'=nombre,
                   'Tipo'=tipo,
                   'Dirección'=direccion,
                   'Barrio'=barrio,
                   'Región'=region,
                   'Comuna'=comuna)
        
        rbind(holder, holder2)
    })
    
    output$tablegastro <- DT::renderDataTable({
        DT::datatable(data=subsetgastro())
    })
    
    output$dltablegastro <- downloadHandler(
        filename=function(){
            paste('establecimientos-gastronomicos-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetgastro(), file)
        }
    )

#deporte
    
    subsetpolideportivos <- reactive({
        holder <- polideportivos %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, domicilio, valor_ent, barrio, region, comuna) %>% 
            rename('Nombre'=nombre, 'Direccion'=domicilio, 'Barrio'=barrio, 'Comuna'=comuna, 'Region'=region, 'Valor'=valor_ent)
        
        holder2 <- polideportivos %>% 
            filter(comuna %in% input$comudepo) %>% 
            filter(region %in% input$regdepo) %>% 
            select(nombre, domicilio, valor_ent, barrio, region, comuna) %>% 
            rename('Nombre'=nombre, 'Direccion'=domicilio, 'Barrio'=barrio, 'Comuna'=comuna, 'Region'=region, 'Valor'=valor_ent)
        
        rbind(holder, holder2)
        
    })
    
    output$tablepolideportivos <- DT::renderDataTable({
        DT::datatable(data=subsetpolideportivos())
    })
    
    output$dltablepolideportivos <- downloadHandler(
        filename=function(){
            paste('polideportivos-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetpolideportivos(), file)
        }
    )
    
    subsetskate <- reactive({
        holder <- skate %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, direccion, tipo, barrio, region, comuna) %>% 
            rename('Nombre'=nombre, 'Direccion'=direccion, 'Barrio'=barrio, 'Comuna'=comuna, 'Region'=region, 'Tipo'=tipo)
        
        holder2 <- skate %>% 
            filter(comuna %in% input$comudepo) %>% 
            filter(region %in% input$regdepo) %>% 
            select(nombre, direccion, tipo, barrio, region, comuna) %>% 
            rename('Nombre'=nombre, 'Direccion'=direccion, 'Barrio'=barrio, 'Comuna'=comuna, 'Region'=region, 'Tipo'=tipo)
        
        rbind(holder, holder2)
        
    })
    
    output$tableskate <- DT::renderDataTable({
        DT::datatable(data=subsetskate())
    })
    
    output$dltableskate <- downloadHandler(
        filename=function(){
            paste('skate-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetskate(), file)
        }
    )
    
    subsetregcivil <- reactive({
        holder <-  regcivil %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral)%>%
            select(tramites, direccion, barrio, region, comuna) %>% 
            rename('Trámites'=tramites,
                   'Direccion'=direccion,
                   'Barrio'=barrio,
                   'Region'=region,
                   'Comuna'=comuna)
        
        holder2 <- regcivil %>% 
            filter(comuna %in% input$comusitios) %>% 
            filter(region %in% input$regsitios)%>%
            select(tramites, direccion, barrio, region, comuna) %>% 
            rename('Trámites'=tramites,
                   'Direccion'=direccion,
                   'Barrio'=barrio,
                   'Region'=region,
                   'Comuna'=comuna)
        
        rbind(holder, holder2)
    })
    
    output$tableregistrocivil <- DT::renderDataTable({
        DT::datatable(data=subsetregcivil())
    })
    
    output$dltableregistrocivil <- downloadHandler(
        filename=function(){
            paste('registro-civil-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetregcivil(), file)
        }
    )
    
    
    
#Transformacion####
    
    subsetobras <- reactive({
        holder <- obras %>% 
            mutate(fecha_inicio=as.Date(fecha_inicio),
                   fecha_fin_inicial=as.Date(fecha_fin_inicial),
                   ano_inicio=year(fecha_inicio)) %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            filter(eje %in%input$ejemdu) %>%
            filter(tipo %in%input$tipomdu) %>% 
            filter(., between(fecha_inicio, input$inimdu[1], input$inimdu[2])) %>% 
            filter(., between(fecha_fin_inicial, input$finmdu[1], input$finmdu[2]))%>% 
            select(nombre, area_responsable, barrio, compromiso, fecha_inicio, fecha_fin_inicial) %>% 
            rename("Nombre"=nombre,
                   "Area responsable"=area_responsable,
                   "Barrio"=barrio,
                   "Compromiso BA"=compromiso,
                   'Fecha de inicio'=fecha_inicio,
                   'Fecha de fin'=fecha_fin_inicial)
        
        holder2 <- obras %>% 
            mutate(fecha_inicio=as.Date(fecha_inicio),
                   fecha_fin_inicial=as.Date(fecha_fin_inicial),
                   ano_inicio=year(fecha_inicio)) %>% 
            filter(comuna %in% input$comuobras) %>% 
            filter(region %in% input$regobras) %>% 
            filter(eje %in%input$ejemdu) %>%
            filter(tipo %in%input$tipomdu) %>% 
            filter(., between(fecha_inicio, input$inimdu[1], input$inimdu[2])) %>% 
            filter(., between(fecha_fin_inicial, input$finmdu[1], input$finmdu[2]))%>% 
            select(nombre, area_responsable, barrio, compromiso, fecha_inicio, fecha_fin_inicial) %>% 
            rename("Nombre"=nombre,
                   "Area responsable"=area_responsable,
                   "Barrio"=barrio,
                   "Compromiso BA"=compromiso,
                   'Fecha de inicio'=fecha_inicio,
                   'Fecha de fin'=fecha_fin_inicial)
        
        rbind(holder, holder2)
    })
    
    output$tableobras <- DT::renderDataTable({
        DT::datatable(data=subsetobras())
    })
    
    output$dltableobras <- downloadHandler(
        filename=function(){
            paste('obras-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetobras(), file)
        }
    )
    
    output$dlplotmdu <- downloadHandler( #ver parámetros de tamaño de la imagen descargada
        filename=function(){
            paste('obras-estado-', Sys.Date(),'.png', sep="")
        },
        content = function(file){
            png(file)
            print(
                ggplot(data=subsetplotobras(), aes(x=ano_inicio, y=n, fill=etapa, label=n))+
                    geom_col(position='dodge')+
                    coord_flip()+
                    labs(x="Año",
                         y="Cantidad de obras")+
                    theme_solarized()+
                    scale_fill_manual(values = c("En ejecución"="#ab9353",
                                                 "En licitación"="#0b3c5d",
                                                 "En proyecto"="#ec6651",
                                                 "Finalizada"="#10c92f"),
                                      name="Estado de las obras")
            )
            dev.off()
        }
    )
    
    subsetporcentajeobras <- reactive({
        df <- obras %>% 
            mutate(fecha_inicio=as.Date(fecha_inicio),
                   fecha_fin_inicial=as.Date(fecha_fin_inicial),
                   ano_inicio=year(fecha_inicio)) %>%
            filter(comuna %in% input$comu) %>% 
            filter(eje %in%input$eje) %>%
            filter(tipo %in% input$tipo) %>% 
            filter(., between(fecha_inicio, input$fechaini[1], input$fechaini[2])) %>% 
            filter(., between(fecha_fin_inicial, input$fechafin[1], input$fechafin[2])) %>%
            group_by(ano_inicio) %>% 
            count(etapa, eje) %>% 
            ungroup() %>% 
            mutate(ano_inicio=as.factor(ano_inicio)) %>%
            filter(ano_inicio%in%c(2016, 2017, 2018, 2019)) %>% 
            group_by(ano_inicio, etapa) %>% 
            summarise(Cantidad=sum(n)) %>% 
            mutate(Frecuencia=paste0(round(100*Cantidad/sum(Cantidad),1),"%")) %>% 
            rename('Año'=ano_inicio,
                   'Etapa'=etapa)
        
    })
    
    output$porcentajetablemdu <- DT::renderDataTable({
        DT::datatable(data=subsetporcentajeobras())
    })
    
    output$dltableporcentajemdu <- downloadHandler(
        filename=function(){
            paste('obras-estado-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetporcentajeobras(), file)
        }
    )

#transporte
    
    subsettaxis <- reactive({
        holder <-  taxis %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral)%>%
            select(direccion, capacidad, barrio, region, comuna) %>% 
            rename('Capacidad'=capacidad,
                   'Direccion'=direccion,
                   'Barrio'=barrio,
                   'Region'=region,
                   'Comuna'=comuna)
        
        holder2 <- taxis %>% 
            filter(comuna %in% input$comutrans) %>% 
            filter(region %in% input$regtrans)%>%
            select(direccion, capacidad, barrio, region, comuna) %>% 
            rename('Capacidad'=capacidad,
                   'Direccion'=direccion,
                   'Barrio'=barrio,
                   'Region'=region,
                   'Comuna'=comuna)
        
        rbind(holder, holder2)
    })
    
    output$tabletaxi <- DT::renderDataTable({
        DT::datatable(data=subsettaxis())
    })
    
    output$dltabletaxi <- downloadHandler(
        filename=function(){
            paste('taxis-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsettaxis(), file)
        }
    )

##Resumen####
    
    #servicios sociales
    subsetgraficoserviciossociales <- reactive({
        
        holdercomi1 <- comisarias %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, categoria, barrio, region, comuna) 
        
        holdercomi2 <- comisarias %>% 
            filter(comuna %in% input$comuseg) %>% 
            filter(region %in% input$regseg) %>%
            select(nombre, categoria, barrio, region, comuna)
        
        holderestabedu1 <- estabedu %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, subtipo, barrio, region, comuna) %>% 
            rename('categoria'=subtipo)
        
        holderestabedu2 <- estabedu %>% 
            filter(comuna %in% input$comuedu) %>% 
            filter(region %in% input$regedu) %>% 
            select(nombre, subtipo, barrio, region, comuna) %>% 
            rename('categoria'=subtipo)
        
        holderuniv1 <- universidades %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, categoria, barrio, region, comuna)
        
        holderuniv2 <- universidades %>% 
            filter(comuna %in% input$comuedu) %>% 
            filter(region %in% input$regedu) %>% 
            select(nombre, categoria, barrio, region, comuna) 
        
        holderhospi1 <- hospitales %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, categoria, barrio, region, comuna)
        
        holderhospi2 <- hospitales %>% 
            filter(comuna %in% input$comusalud) %>% 
            filter(region %in% input$regsalud) %>% 
            select(nombre, categoria, barrio, region, comuna)
        
        holdercmb1 <- cmb %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, categoria, barrio, region, comuna) 
        
        holdercmb2 <- cmb %>% 
            filter(comuna %in% input$comusalud) %>% 
            filter(region %in% input$regsalud) %>% 
            select(nombre, categoria, barrio, region, comuna)
        
        holdercesac1 <- cesac %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, categoria, barrio, region, comuna)
        
        holdercesac2 <- cesac %>% 
            filter(comuna %in% input$comusalud) %>% 
            filter(region %in% input$regsalud) %>% 
            select(nombre, categoria, barrio, region, comuna)
        
        base <- rbind(holdercomi1, holdercomi2, holderestabedu1, holderestabedu2, holderuniv1, holderuniv2,
                      holderhospi1, holderhospi2, holdercmb1, holdercmb2, holdercesac1, holdercesac2) %>% 
            group_by(barrio, categoria, comuna) %>% 
            summarise(cantidad=n()) 
        
    })
    
# Esta es la parte que involucra la automatización. Primero creamos una escala de colores para cada categoría.
# Luego creamos un objeto reactivo que contenga los números de las comunas filtradas.
# De ahí creamos otro objeto reactivo para fusionar el título con el número de la comuna. Este es el input que irá en el ploteo.
    
    
    escala_servicios <- c('Inicial'='#cc0066',
                          'Mixto'='#8acafe',
                          'Otros'='#2e8b57',
                          'Primaria'='#15dbcc',
                          'Secundaria'='#ff9933',
                          'Superior'='#363942',
                          'Universidades'='#c16c00',
                          'Hospitales'='#f36b2c',
                          'Centros Médicos Barriales'='#dfa290',
                          'Centros de Salud Comunitarios'='#ee8f1b',
                          'Comisarias'='#9b59b6')
    
    comu_title_ss <- reactive({unique(subsetgraficoserviciossociales()$comuna) %>% 
        str_c(., collapse = ", ")})
        
    graf_title_sscomu <- reactive({paste0('Servicios sociales en Comuna: ', comu_title_ss())})
    
    graficoserviciossocialescomu <- reactive({
        ggplot(data=subsetgraficoserviciossociales() %>%
                   group_by(categoria) %>% 
                   summarise(cantidad=sum(cantidad)), aes(x=categoria, y=cantidad, fill=categoria))+
                   geom_col(position = 'dodge')+
                   geom_text(aes(label=cantidad), position = position_dodge(width=1), hjust=-0.5)+
                   coord_flip()+
                   scale_fill_manual(values = escala_servicios)+
                   labs(title=graf_title_sscomu(),
                   x='Tipo de servicio',
                   y='Cantidad',
                   fill='Tipo de servicio',
                   caption='Dashboard GCBA-Placo')
        })

    output$serviciossocialescomu <- renderPlot({
        print(graficoserviciossocialescomu())
    })
    
    output$dlserviciossocialescomu <- downloadHandler(
        filename=function(){
            paste('servicios-sociales-comuna-', Sys.Date(),'.png', sep="")
        },
        content = function(file){
            ggsave(file, plot = graficoserviciossocialescomu(), width = 31, height = 17, units = 'cm', device='png', dpi=300)
        }
    )
    
    graf_title_ssbarrio <- reactive({paste0('Servicios sociales por barrio en Comuna: ', comu_title_ss())})
    
    graficoserviciossocialesbarrio <- reactive({
        ggplot(data=subsetgraficoserviciossociales(), aes(x=barrio, weight=cantidad, fill=categoria))+
            geom_bar()+
            coord_flip()+
            scale_fill_manual(values = escala_servicios)+
            labs(title=graf_title_ssbarrio(),
                 x='Barrio',
                 y='Cantidad',
                 fill='Tipo de servicio',
                 caption='Dashboard GCBA-Placo')
    })
    
    output$serviciossocialesbarrios <- renderPlot({
        print(graficoserviciossocialesbarrio())
    })
    
    output$dlserviciossocialesbarrio <- downloadHandler(
        filename=function(){
            paste('servicios-sociales-barrio-', Sys.Date(),'.png', sep="")
        },
        content = function(file){
            ggsave(file, plot = graficoserviciossocialesbarrio(), width = 31, height = 17, units = 'cm', device='png', dpi=300)
        }
    )
    
    output$dltableserviciossocialesresumen <- downloadHandler(
        filename=function(){
            paste('servicios-sociales-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetgraficoserviciossociales(), file)
        }
    )
    
    output$tableserviciosocialesresumen <- DT::renderDataTable({
        DT::datatable(data=subsetgraficoserviciossociales() %>% 
                          rename('Barrio'=barrio,
                                 'Categoría'=categoria,
                                 'Comuna'=comuna,
                                 'Cantidad'=cantidad))
    })
 
##Disfrute   
    
    subsetgraficodisfrute <- reactive({
        cultura1 <- cultura %>%
            mutate(actividad=case_when(actividad=='BAR CULTURAL'~'Bar',
                                       actividad=='BAR ELEMENTO'~'Bar',
                                       actividad=='BAR IDENTIFICADO'~'Bar',
                                       actividad=='BAR NOTABLE'~'Bar',
                                       actividad=='ARCHIVO'~'Biblioteca',
                                       actividad=='BIBLIOTECA'~'Biblioteca',
                                       actividad=='CALESITA'~'Calesita',
                                       actividad=='CIRCO'~'Calesita',
                                       actividad=='CENTRO CULTURAL'~'Espacio cultural',
                                       actividad=='ESPACIO CULTURAL'~'Espacio cultural',
                                       actividad=='ESPACIO ESCENICO (TEATRO, MUSICA Y DANZA)'~'Espacio cultural',
                                       actividad=='ESPACIO DE FORMACION (SEMINARIOS Y TALLERES)'~'Espacio cultural',
                                       actividad=='CINE'~'Cine',
                                       actividad=='PRODUCTORA AUDIOVISUAL'~'Cine',
                                       actividad=='DISQUERIA'~'Disquería',
                                       actividad=='SELLO DISCOGRAFICO'~'Disquería',
                                       actividad=='LIBRERIA'~'Librería',
                                       actividad=='EDITORIAL'~'Librería',
                                       actividad=='GALERIA DE ARTE'~'Galería de arte',
                                       actividad=='SALA DE EXPOSICION'~'Galería de arte',
                                       actividad=='MILONGA Y/O TANGUERIA'~'Milonga',
                                       actividad=='PEÑA'~'Milonga',
                                       actividad=='MUSEO'~'Museo',
                                       actividad=='PLANETARIO'~'Museo',
                                       actividad=='RADIO LOCAL (AM Y FM)'~'Radio y TV',
                                       actividad=='TELEVISION LOCAL'~'Radio y TV')) %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral)%>%
            select(nombre, actividad, barrio, region, comuna) %>% 
            rename('categoria'=actividad)
        
        cultura2 <- cultura %>%
            mutate(actividad=case_when(actividad=='BAR CULTURAL'~'Bar',
                                       actividad=='BAR ELEMENTO'~'Bar',
                                       actividad=='BAR IDENTIFICADO'~'Bar',
                                       actividad=='BAR NOTABLE'~'Bar',
                                       actividad=='ARCHIVO'~'Biblioteca',
                                       actividad=='BIBLIOTECA'~'Biblioteca',
                                       actividad=='CALESITA'~'Calesita',
                                       actividad=='CIRCO'~'Calesita',
                                       actividad=='CENTRO CULTURAL'~'Espacio cultural',
                                       actividad=='ESPACIO CULTURAL'~'Espacio cultural',
                                       actividad=='ESPACIO ESCENICO (TEATRO, MUSICA Y DANZA)'~'Espacio cultural',
                                       actividad=='ESPACIO DE FORMACION (SEMINARIOS Y TALLERES)'~'Espacio cultural',
                                       actividad=='CINE'~'Cine',
                                       actividad=='PRODUCTORA AUDIOVISUAL'~'Cine',
                                       actividad=='DISQUERIA'~'Disquería',
                                       actividad=='SELLO DISCOGRAFICO'~'Disquería',
                                       actividad=='LIBRERIA'~'Librería',
                                       actividad=='EDITORIAL'~'Librería',
                                       actividad=='GALERIA DE ARTE'~'Galería de arte',
                                       actividad=='SALA DE EXPOSICION'~'Galería de arte',
                                       actividad=='MILONGA Y/O TANGUERIA'~'Milonga',
                                       actividad=='PEÑA'~'Milonga',
                                       actividad=='MUSEO'~'Museo',
                                       actividad=='PLANETARIO'~'Museo',
                                       actividad=='RADIO LOCAL (AM Y FM)'~'Radio y TV',
                                       actividad=='TELEVISION LOCAL'~'Radio y TV')) %>% 
            filter(comuna %in% input$comucult) %>% 
            filter(region %in% input$regcult)%>%
            select(nombre, actividad, barrio, region, comuna) %>% 
            rename('categoria'=actividad)
        
        mercados1 <-  mercados %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral)%>%
            select(nombre, categoria, barrio, region, comuna)
        
        mercados2 <- mercados %>% 
            filter(comuna %in% input$comuvice) %>% 
            filter(region %in% input$regvice)%>%
            select(nombre, categoria, barrio, region, comuna) 
        
        estsal1 <-  estacionessaludables %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral)%>%
            select(nombre, categoria, barrio, region, comuna)
        
        estsal2 <- estacionessaludables %>% 
            filter(comuna %in% input$comuvice) %>% 
            filter(region %in% input$regvice)%>%
            select(nombre, categoria, barrio, region, comuna)
        
        skate1 <- skate %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, categoria, barrio, region, comuna)
        
        skate2 <- skate %>% 
            filter(comuna %in% input$comudepo) %>% 
            filter(region %in% input$regdepo) %>% 
            select(nombre, categoria, barrio, region, comuna) 
        
        poli1 <- polideportivos %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, categoria, barrio, region, comuna)
        
        poli2 <- polideportivos %>% 
            filter(comuna %in% input$comudepo) %>% 
            filter(region %in% input$regdepo) %>% 
            select(nombre, categoria, barrio, region, comuna)
        
        base <- rbind(cultura1, cultura2, mercados1, mercados2,
                      estsal1, estsal2, skate1, skate2, poli1, poli2) %>% 
            group_by(barrio, categoria, comuna) %>% 
            summarise(cantidad=length(categoria)) %>% 
            ungroup()
    })
    
    escala_disfrute <- c('Bar'='#cc0066',
                         'Biblioteca'='#ff9933',
                         'Calesita'='#2e8b57',
                         'Espacio cultural'='#dfa290',
                         'Cine'='#ff9933',
                         'Disquería'='#363942',
                         'Librería'='#007f66',
                         'Galería de arte'='#cc3399',
                         'Milonga'='#ffaa00',
                         'Museo'='#00ff55',
                         'Radio y TV'='#022445',
                         'Mercados'='#ffff44',
                         'Estaciones saludables'='#5f2b1e',
                         'Pistas de skate'='#cc3399',
                         'Polideportivos'='#c72121')
    
    comu_title_disfrute <- reactive({unique(subsetgraficodisfrute()$comuna) %>% 
        str_c(., collapse = ", ")})
    
    graf_title_disfrute_comuna <- reactive({paste0('Locaciones de disfrute en Comuna: ', comu_title_disfrute())})
    
    graf_title_disfrute_barrio <- reactive({paste0('Locaciones de disfrute por barrio en Comuna: ', comu_title_disfrute())})
    
    graficodisfrutecomu <- reactive({
        
        ggplot(data=subsetgraficodisfrute() %>% 
                   group_by(categoria) %>% 
                   summarise(cantidad=sum(cantidad)), aes(x=categoria, y=cantidad, fill=categoria))+
            geom_col()+
            geom_text(aes(label=cantidad), position = position_dodge(width=1), hjust=-0.5)+
            coord_flip()+
            scale_fill_manual(values = escala_disfrute)+
            labs(title=graf_title_disfrute_comuna(),
                 x='Categoria',
                 y='Cantidad',
                 fill='Categoria',
                 caption='Dashboard GCBA-Placo')
        
    })
    
    output$disfrutecomu <- renderPlot({
        
        print(graficodisfrutecomu())

    })
    
    graficodisfrutebarrio <- reactive({
        ggplot(data=subsetgraficodisfrute(), aes(x=barrio, weight=cantidad, fill=categoria))+
            geom_bar()+
            coord_flip()+
            scale_fill_manual(values = escala_disfrute)+
            labs(title=graf_title_disfrute_barrio(),
                 x='Barrio',
                 y='Cantidad',
                 fill='Categoria',
                 caption='Dashboard GCBA-Placo') 
    })
    
    output$disfrutebarrios <- renderPlot({
        
        print(graficodisfrutebarrio())
    
    })
    
    output$tabledisfrutetotalresumen <- DT::renderDataTable({
        DT::datatable(data=subsetgraficodisfrute() %>% 
                          rename('Barrio'=barrio,
                                 'Categoría'=categoria,
                                 'Comuna'=comuna,
                                 'Cantidad'=cantidad))
    })
    
    output$dldisfrutecomu <- downloadHandler(
        filename=function(){
            paste('disfrute-comuna-', Sys.Date(),'.png', sep="")
        },
        content = function(file){
            ggsave(file, plot = graficodisfrutecomu(), width = 31, height = 17, units = 'cm', device='png', dpi=300)
        }
    )
    
   output$dldisfrutebarrio <- downloadHandler(
       filename=function(){
           paste('disfrute-barrio-', Sys.Date(),'.png', sep="")
       },
       content = function(file){
           ggsave(file, plot = graficodisfrutebarrio(), width = 31, height = 17, units = 'cm', device='png', dpi=300)
       }
   ) 
   
   output$dltabledisfrutetotalresumen <- downloadHandler(
       filename=function(){
           paste('disfrute-', Sys.Date(),'.xlsx', sep="")
       },
       content = function(file){
           write.xlsx(subsetgraficodisfrute(), file)
       }
   )

##obras
    
    subsetgraficoobras <- reactive({
        holder <- obras %>% 
            mutate(fecha_inicio=as.Date(fecha_inicio),
                   fecha_fin_inicial=as.Date(fecha_fin_inicial),
                   ano_inicio=year(fecha_inicio)) %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            filter(eje %in%input$ejemdu) %>%
            filter(tipo %in%input$tipomdu) %>% 
            filter(., between(fecha_inicio, input$inimdu[1], input$inimdu[2])) %>% 
            filter(., between(fecha_fin_inicial, input$finmdu[1], input$finmdu[2]))
        
        holder2 <- obras %>% 
            mutate(fecha_inicio=as.Date(fecha_inicio),
                   fecha_fin_inicial=as.Date(fecha_fin_inicial),
                   ano_inicio=year(fecha_inicio)) %>% 
            filter(comuna %in% input$comuobras) %>% 
            filter(region %in% input$regobras) %>% 
            filter(eje %in%input$ejemdu) %>%
            filter(tipo %in%input$tipomdu) %>% 
            filter(., between(fecha_inicio, input$inimdu[1], input$inimdu[2])) %>% 
            filter(., between(fecha_fin_inicial, input$finmdu[1], input$finmdu[2]))
        
        base <- rbind(holder, holder2) %>% 
            group_by(barrio, tipo, comuna) %>% 
            summarise(cantidad=length(nombre))
        
    })
    
    escala_obras <- c('Arquitectura'='#cc0066',
                      'Escuelas'='#8acafe',
                      'Espacio Público'='#2e8b57',
                      'Hidráulica e Infraestructura'='#15dbcc',
                      'Salud'='#ff9933',
                      'Transporte'='#363942',
                      'Vivienda'='#c16c00')
    
    comu_title_obras <- reactive({unique(subsetgraficoobras()$comuna) %>% 
        str_c(., collapse = ", ")})
    
    graf_title_obras_comuna <- reactive({paste0('Obras en Comuna: ', comu_title_obras())})
    
    graf_title_obras_barrio <- reactive({paste0('Obras por barrio en Comuna: ', comu_title_obras())})
    
    graficoobrascomuna <- reactive({
        
        ggplot(data=subsetgraficoobras() %>% 
                   group_by(tipo) %>% 
                   summarise(cantidad=sum(cantidad)), aes(x=tipo, y=cantidad, fill=tipo))+
            geom_col()+
            geom_text(aes(label=cantidad), position = position_dodge(width=1), hjust=-0.5)+
            coord_flip()+
            scale_fill_manual(values = escala_obras)+
            labs(title=graf_title_obras_comuna(),
                 x='Categoria',
                 y='Cantidad',
                 fill='Categoria')
        
    })
    
    output$obrascomu <- renderPlot({
        print(graficoobrascomuna())
    })
    
    graficoobrasbarrio <- reactive({
        
        ggplot(data=subsetgraficoobras(), aes(x=barrio, weight=cantidad, fill=tipo))+
            geom_bar()+
            coord_flip()+
            scale_fill_manual(values = escala_obras)+
            labs(title=graf_title_obras_barrio(),
                 x='barrio',
                 y='Cantidad',
                 fill='Categoria',
                 caption='Dashboard GCBA-Placo')
        
    })
    
    output$obrasbarrios <- renderPlot({
        
        print(graficoobrasbarrio())
        
    })
    
    output$tableobrasresumen <- DT::renderDataTable({
        DT::datatable(data=subsetgraficoobras() %>% 
                          rename('Barrio'=barrio,
                                 'Tipo'=tipo,
                                 'Comuna'=comuna,
                                 'Cantidad'=cantidad))
    })
    
    output$dlobrascomu <- downloadHandler(
        filename=function(){
            paste('obras-comuna-', Sys.Date(),'.png', sep="")
        },
        content = function(file){
            ggsave(file, plot = graficoobrascomuna(), width = 31, height = 17, units = 'cm', device='png', dpi=300)
        }
    )
    
    output$dlobrasbarrio <- downloadHandler(
        filename=function(){
            paste('obras-barrio-', Sys.Date(),'.png', sep="")
        },
        content = function(file){
            ggsave(file, plot = graficoobrasbarrio(), width = 31, height = 17, units = 'cm', device='png', dpi=300)
        }
    ) 
    
    output$dltableobrasresumen <- downloadHandler(
        filename=function(){
            paste('obras-', Sys.Date(),'.xlsx', sep="")
        },
        content = function(file){
            write.xlsx(subsetgraficoobras(), file)
        }
    )
    
#espaciosverdes
    
    
#Mapa####

#Para el mapa nos quedamos sólo con nombre, coordenadas y su categoría. La categoría (por ejemplo, "comisarías") nos va a servir más adelante.
    
###Seguridad y administracion####
#seguridad
    mapacomisarias <- reactive({
        holder <- comisarias %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- comisarias %>% 
            filter(comuna %in% input$comuseg) %>% 
            filter(region %in% input$regseg) %>%
            select(nombre, long, lat, categoria) 
        
        rbind(holder, holder2)
        
    })
    
    mapacamaras <- reactive({
        holder <- camaras %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>%
            rename('nombre'=tipo) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- camaras %>% 
            filter(comuna %in% input$comuseg) %>% 
            filter(region %in% input$regseg) %>%
            rename('nombre'=tipo) %>% 
            select(nombre, long, lat, categoria) 
        
        rbind(holder, holder2)
        
    })

#trabajo
    
    mapaceil <- reactive({
        holder <- ceil %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- ceil %>% 
            filter(comuna %in% input$comutrabajo) %>% 
            filter(region %in% input$regtrabajo) %>%
            select(nombre, long, lat, categoria) 
        
        rbind(holder, holder2)
        
    })

#administracion publica
    mapaedifpub <- reactive({
        holder <- edifpub %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            rename('nombre'=tipo) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- edifpub %>% 
            filter(comuna %in% input$comuadmi) %>% 
            filter(region %in% input$regadmi) %>%
            rename('nombre'=tipo) %>% 
            select(nombre, long, lat, categoria) 
        
        rbind(holder, holder2)
        
    })
    
    mapahabilitaciones <- reactive({
        holder <- habilitaciones %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            rename('nombre'=descripcion_rubro) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- habilitaciones %>% 
            filter(comuna %in% input$comuadmi) %>% 
            filter(region %in% input$regadmi) %>%
            rename('nombre'=descripcion_rubro) %>% 
            select(nombre, long, lat, categoria) 
        
        rbind(holder, holder2)
        
    })
    
    mapasedescomunales <- reactive({
        holder <- sedescomunales %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- sedescomunales %>% 
            filter(comuna %in% input$comuadmi) %>% 
            filter(region %in% input$regadmi) %>%
            select(nombre, long, lat, categoria) 
        
        rbind(holder, holder2)
        
    })

#economia y finanzas
    mapaempresastech <- reactive({
        holder <- empresastech %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- empresastech %>% 
            filter(comuna %in% input$comuecon) %>% 
            filter(region %in% input$regecon) %>%
            select(nombre, long, lat, categoria) 
        
        rbind(holder, holder2)
        
    })
    
    mapaempresadis <- reactive({
        holder <- empresadis %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- empresadis %>% 
            filter(comuna %in% input$comuecon) %>% 
            filter(region %in% input$regecon) %>%
            select(nombre, long, lat, categoria) 
        
        rbind(holder, holder2)
        
    })
    
    mapaempresaarte <- reactive({
        holder <- empresaarte %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- empresaarte %>% 
            filter(comuna %in% input$comuecon) %>% 
            filter(region %in% input$regecon) %>%
            select(nombre, long, lat, categoria) 
        
        rbind(holder, holder2)
        
    })

# Acá ya podemos intuir por qué sirve la categoría: para fusionar todo en un solo dataset de la solapa en cuestión que contenga toda la información
# necesaria. 
    
    mapa_soc <- eventReactive(input$plot,{
        df <- rbind(mapacomisarias(), mapacamaras(), mapaceil(), mapaedifpub(), mapahabilitaciones(),
                    mapasedescomunales(), mapaempresaarte(), mapaempresadis(), mapaempresastech()) %>% 
            filter(categoria %in% input$filtersoc)
    })

##Social#####

#educacion
    
    mapaescuseguros <- reactive({
        holder <- escuseguros %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- escuseguros %>% 
            filter(comuna %in% input$comuedu) %>% 
            filter(region %in% input$regedu) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
    mapauniversidades <- reactive({
        holder <- universidades %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- universidades %>% 
            filter(comuna %in% input$comuedu) %>% 
            filter(region %in% input$regedu) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
    mapaestabedu <- reactive({
        holder <- estabedu %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- estabedu %>% 
            filter(comuna %in% input$comuedu) %>% 
            filter(region %in% input$regedu) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })

#salud
    
    
    mapacmb <- reactive({
        holder <- cmb %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- cmb %>% 
            filter(comuna %in% input$comusalud) %>% 
            filter(region %in% input$regsalud) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
    mapahospitales <- reactive({
        holder <- hospitales %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- hospitales %>% 
            filter(comuna %in% input$comusalud) %>% 
            filter(region %in% input$regsalud) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
    mapacesac <- reactive({
        holder <- cesac %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- cesac %>% 
            filter(comuna %in% input$comusalud) %>% 
            filter(region %in% input$regsalud) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
    mapafarmacias <- reactive({
        holder <- farmacias %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>%
            rename('nombre'=direccion) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- farmacias %>% 
            filter(comuna %in% input$comusalud) %>% 
            filter(region %in% input$regsalud) %>%
            rename('nombre'=direccion) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
#desarrollo humano y habitat
    
    mapacentrosdia <- reactive({
        holder <- centrosdia %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- centrosdia %>% 
            filter(comuna %in% input$comudhh) %>% 
            filter(region %in% input$regdhh) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
    mapacentrosinfantil <- reactive({
        holder <- centrosinfantil %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- centrosinfantil %>% 
            filter(comuna %in% input$comudhh) %>% 
            filter(region %in% input$regdhh) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
    mapacentrosjubilados <- reactive({
        holder <- centrosjubilados %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- centrosjubilados %>% 
            filter(comuna %in% input$comudhh) %>% 
            filter(region %in% input$regdhh) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
    mapajuegotecas <- reactive({
        holder <- juegotecas %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- juegotecas %>% 
            filter(comuna %in% input$comudhh) %>% 
            filter(region %in% input$regdhh) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
    mapacpi <- reactive({
        holder <- cpi %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- cpi %>% 
            filter(comuna %in% input$comudhh) %>% 
            filter(region %in% input$regdhh) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
    mapacasanino <- reactive({
        holder <- casanino %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- casanino %>% 
            filter(comuna %in% input$comudhh) %>% 
            filter(region %in% input$regdhh) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })

#integracion 
    mapaparadores <- reactive({
        holder <- paradores %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- paradores %>% 
            filter(comuna %in% input$comuinteg) %>% 
            filter(region %in% input$reginteg) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })

#genero     
    mapacim <- reactive({
        holder <- cim %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- cim %>% 
            filter(comuna %in% input$comugenero) %>% 
            filter(region %in% input$reggenero) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
    
    mapa_social <- eventReactive(input$plot,{
        df <- rbind(mapaescuseguros(), mapauniversidades(), mapaestabedu(),
                    mapacmb(), mapahospitales(), mapacesac(), mapafarmacias(),
                    mapacentrosdia(), mapacentrosinfantil(), mapacentrosjubilados(), mapajuegotecas(), mapacpi(), mapacasanino(),
                    mapaparadores(),
                    mapacim()) %>% 
            filter(categoria %in% input$filtersocial)
    })
    
##Disfrute####

#mayep    
    mapaveterinaria <- reactive({
        holder <- veterinaria %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- veterinaria %>% 
            filter(comuna %in% input$comumayep) %>% 
            filter(region %in% input$regmayep) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    
    })
    
    mapapuntosverdes <- reactive({
        holder <- puntosverdes %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- puntosverdes %>% 
            filter(comuna %in% input$comumayep) %>% 
            filter(region %in% input$regmayep) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
    mapacampanas <- reactive({
        holder <- campanas %>%
            rename('nombre'=direccion) %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- campanas %>% 
            rename('nombre'=direccion) %>% 
            filter(comuna %in% input$comumayep) %>% 
            filter(region %in% input$regmayep) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
    mapamonumentos <- reactive({
        holder <- monumentos %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- monumentos %>% 
            filter(comuna %in% input$comumayep) %>% 
            filter(region %in% input$regmayep) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
    
    mapacentros <- reactive({
        holder <- centros %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- centros %>% 
            filter(comuna %in% input$comumayep) %>% 
            filter(region %in% input$regmayep) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })

#cultura
    mapacultura <- reactive({
        holder <- cultura %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- cultura %>% 
            filter(comuna %in% input$comucult) %>% 
            filter(region %in% input$regcult) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
    mapabibliotecas <- reactive({
        holder <- bibliotecas %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- bibliotecas %>% 
            filter(comuna %in% input$comucult) %>% 
            filter(region %in% input$regcult) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
#vicejefatura
    mapaferias <- reactive({
        holder <- ferias %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- ferias %>% 
            filter(comuna %in% input$comuvice) %>% 
            filter(region %in% input$regvice) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
    mapamercados <- reactive({
        holder <- mercados %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- mercados %>% 
            filter(comuna %in% input$comuvice) %>% 
            filter(region %in% input$regvice) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
    mapaestacionessaludables <- reactive({
        holder <- estacionessaludables %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- estacionessaludables %>% 
            filter(comuna %in% input$comuvice) %>% 
            filter(region %in% input$regvice) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
    mapagastro <- reactive({
        holder <- gastro %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- gastro %>% 
            filter(comuna %in% input$comuvice) %>% 
            filter(region %in% input$regvice) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })

#deportes
    
    mapapolideportivos <- reactive({
        holder <- polideportivos %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- polideportivos %>% 
            filter(comuna %in% input$comudepo) %>% 
            filter(region %in% input$regdepo) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
    mapaskate <- reactive({
        holder <- skate %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- skate %>% 
            filter(comuna %in% input$comudepo) %>% 
            filter(region %in% input$regdepo) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
#sitios de interes
    
    maparegcivil <- reactive({
        holder <- regcivil %>%
            rename('nombre'=direccion) %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- regcivil %>% 
            rename('nombre'=direccion) %>% 
            filter(comuna %in% input$comusitios) %>% 
            filter(region %in% input$regsitios) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
    
    
    
    mapa_disfrute <- eventReactive(input$plot,{
        df <- rbind(mapaveterinaria(), mapapuntosverdes(), mapacampanas(), mapamonumentos(), mapacentros(),
                    mapacultura(), mapabibliotecas(),
                    mapaferias(), mapamercados(), mapaestacionessaludables(), mapagastro(), 
                    mapapolideportivos(), mapaskate(),
                    maparegcivil()) %>% 
            filter(categoria %in% input$filterdisfrute)
    })

##Transformacion#####
#mdu
    mapaobras <- reactive({
        holder <- obras %>% 
            mutate(fecha_inicio=as.Date(fecha_inicio),
                   fecha_fin_inicial=as.Date(fecha_fin_inicial),
                   ano_inicio=year(fecha_inicio)) %>%
            filter(comuna %in% input$comugral) %>%
            filter(region %in% input$reggral) %>% 
            filter(eje %in%input$ejemdu) %>%
            filter(tipo%in%input$tipomdu) %>% 
            filter(., between(fecha_inicio, input$inimdu[1], input$inimdu[2])) %>% 
            filter(., between(fecha_fin_inicial, input$finmdu[1], input$finmdu[2])) %>% 
            select(nombre, long, lat, categoria)
        
        holder2 <- obras %>% 
            mutate(fecha_inicio=as.Date(fecha_inicio),
                   fecha_fin_inicial=as.Date(fecha_fin_inicial),
                   ano_inicio=year(fecha_inicio)) %>%
            filter(comuna %in% input$comuobras) %>%
            filter(region %in% input$regobras) %>% 
            filter(eje %in%input$ejemdu) %>%
            filter(tipo%in%input$tipomdu) %>% 
            filter(., between(fecha_inicio, input$inimdu[1], input$inimdu[2])) %>% 
            filter(., between(fecha_fin_inicial, input$finmdu[1], input$finmdu[2])) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
#transporte
    
    mapataxis <- reactive({
        holder <- taxis %>% 
            rename('nombre'=direccion) %>% 
            filter(comuna %in% input$comugral) %>% 
            filter(region %in% input$reggral) %>% 
            select(nombre, long, lat, categoria) 
        
        holder2 <- taxis %>% 
            rename('nombre'=direccion) %>% 
            filter(comuna %in% input$comutrans) %>% 
            filter(region %in% input$regtrans) %>% 
            select(nombre, long, lat, categoria)
        
        rbind(holder, holder2)
    })
    
    mapa_transformacion <- eventReactive(input$plot,{
        df <- rbind(mapaobras(), mapataxis()) %>% 
            filter(categoria%in%input$filtertransformacion)
        
    })

#poligonos
    mapa_movilidad <- eventReactive(input$plot, {
        df <- movilidad_lineas %>% 
            filter(categoria %in% input$filtermovilidad)
    })
    
    mapa_senderos <- eventReactive(input$plot, {
        df <- senderos_lineas %>% 
            filter(categoria %in% input$filtersenderos)
    })
    
    mapa_distritos <- eventReactive(input$plot, {
        df <- distritos_lineas %>% 
            filter(categoria %in% input$filterdistritos)
    })
    
    mapa_subte <- eventReactive(input$plot, {
        df <- subte_lineas %>% 
            filter(categoria %in% input$filtermovilidad)
    })

# Acá vemos el mapa: cada una de las solapas está puesta como su propia capa, con la posibilidad de apagarla y prenderla a voluntad. 
    
    output$mapaBA <- renderLeaflet({
        
        pal <- colorFactor(palette = c('#ff7f11', '#00ce39', '#f2ca4e'),
                           levels = c('Metrobus', 'Ciclovias', 'Ferrocarriles'))
        
        pal_subte <- colorFactor(palette=c('#109DFA', '#EF280F', '#024A86', '#02AC66', '#8C4966', '#E7D40A'),
                                 levels=c('LINEA A', 'LINEA B', 'LINEA C', 'LINEA D', 'LINEA E', 'LINEA H'))
        
        leaflet() %>% 
            addTiles() %>% 
            setView(lng=-58.42909098, lat=-34.61569178, zoom=11.5) %>% 
            addMarkers(data=mapa_soc(), lng=~long, lat =~lat, popup = mapa_soc()$nombre, group='Seguridad', icon = ~iconos_mapa[categoria]) %>% #, icon = ~iconos_mapa[categoria]
            addMarkers(data=mapa_social(), lng=~long, lat=~lat, popup = mapa_social()$nombre, group='Social', icon = ~iconos_mapa[categoria]) %>% 
            addMarkers(data=mapa_disfrute(), lng=~long, lat=~lat, popup = mapa_disfrute()$nombre, group='Disfrute', icon = ~iconos_mapa[categoria]) %>% 
            addMarkers(data=mapa_transformacion(), lng=~long, lat=~lat, popup = mapa_transformacion()$nombre, group='Transformación', icon = ~iconos_mapa[categoria]) %>% 
            addPolylines(data = mapa_movilidad(), group = 'Movilidad', color=~pal(categoria)) %>%
            addPolylines(data = mapa_subte(), group = 'Movilidad', color =~pal_subte(nombre)) %>% 
            addPolylines(data = mapa_senderos(), group = 'Senderos', color = "green", fillOpacity = 0.8, weight = 1) %>% 
            addPolygons(data = mapa_distritos(), group = 'Distritos', color = "red", fillOpacity = 0.4, weight=1) %>% 
            addPolygons(data=comunas, group = 'División de comunas', fillOpacity = 0.1, weight = 2) %>% 
            addLayersControl(overlayGroups = c('Seguridad', 'Social', 'Disfrute', 'Transformación', 'División de comunas', 'Movilidad', 'Senderos', 'Distritos')) %>% 
            onRender( #este es el codigo js para que baje el mapa.
                "function(el, x) {
                L.easyPrint({
                sizeModes: ['Current'],
                filename: 'mapaBA',
                exportOnly: true,
                hideControlContainer: true
                }).addTo(this);
            }"
            )
    })

 
} #cierre del SERVER

# Run the application 
shinyApp(ui = ui, server = server)
