library(dplyr)
library(tidyr)
library(magrittr)
library(stringi)
library(lubridate)

Sys.setlocale("LC_ALL", "sk_SK.UTF-8")

source('./Aux/nase_peniaze_aux.R')

# read variables and dictionaries
LAST_DATE<- readLines('./Aux/last_date.txt')

TYPYZMLUV<- readLines('./Aux/TYPYZMLUV.txt')
REZORTY<- read.delim('./Aux/REZORTY.txt', sep=';', colClasses=c('character','character'))
OBCE<- read.delim('./Aux/OBCE.txt', sep=';', colClasses=c('character','character'))
OBCE_RICH<- read.delim('./Aux/OBCE_RICH.txt', sep=';', colClasses=c('character','character','integer','character'))

# downloading and parsing new data
data<- getCRZ(LAST_DATE, download_method = 'wget') %>%
  parseCRZ()

# preparing DF to join with FULL DF
data %<>% select(ID, predmet, poznamka, rezort, zs1, zs2, sidlo1, sidlo, suma_spolu, datum_ucinnost, datum_platnost_do, datum_zverejnene)
data<- unique(data)

names(data)[8]<- "sidlo2"

# adding REZORT name
data<- left_join(data, REZORTY, by="rezort")

# adding additional geo info
data$sidlo1L<- tolower(data$sidlo1)
data$sidlo2L<- tolower(data$sidlo2)

data$mesto1<- ""
data$mesto2<- ""

#finding cities and villages
system.time(
  for(i in 1:nrow(OBCE))
  {
    data$mesto1<- ifelse(grepl(data$sidlo1L, pattern=OBCE$obecL[i]), OBCE$obec[i], data$mesto1)
    data$mesto2<- ifelse(grepl(data$sidlo2L, pattern=OBCE$obecL[i]), OBCE$obec[i], data$mesto2)
    # print(i)
  })

# country (Slovakia or Czech Republic) if no city or village has been found
data$mesto1<- ifelse(is.na(data$mesto1),ifelse(grepl(data$sidlo1L, pattern="svk|sk|sr"), "Slovensko", 
                                               ifelse(grepl(data$sidlo1L, pattern="cr|cz|čr"), "Česko",data$mesto1)),data$mesto1)
data$mesto2<- ifelse(is.na(data$mesto2),ifelse(grepl(data$sidlo2L, pattern="svk|sk|sr"), "Slovensko", 
                                               ifelse(grepl(data$sidlo2L, pattern="cr|cz|čr"), "Česko",data$mesto2)),data$mesto2)

# adding administrative regions
names(OBCE_RICH)<- c("kraj1","okres1","obyvatelov1","obec")
data %<>% left_join(OBCE_RICH, by = c('mesto1'='obec'))

names(OBCE_RICH)<- c("kraj2","okres2","obyvatelov2","obec")
data %<>% left_join(OBCE_RICH, by = c('mesto2'='obec'))

data %<>% select(-sidlo1L,-sidlo2L)

# price per ...
data$cena_za<- ""
data$cena_za<- ifelse(grepl(tolower(data$poznamka), pattern="mesiac|mesačne|mesačné|mesačný|mesačná|mesační"),"mesiac",data$cena_za)
data$cena_za<- ifelse(grepl(tolower(data$poznamka), pattern="rok|ročne|ročné|ročný|ročná|roční"),"rok",data$cena_za)
data$cena_za<- ifelse(grepl(tolower(data$poznamka), pattern="deň|denne|denné|denná|denný|denní"),"deň",data$cena_za)
data$cena_za<- ifelse(grepl(tolower(data$poznamka), pattern="hodina|hodinu|hodinový|hodinová|hodinové|hodinoví"),"hodina",data$cena_za)

#DPH
data$DPH<- ""
data$DPH<- ifelse(grepl(tolower(data$poznamka), pattern="s.*dph|vrátane.*DPH"),"s DPH",data$DPH)
data$DPH<- ifelse(grepl(tolower(data$poznamka), pattern="bez.*dph"),"bez DPH",data$DPH)


#firma
data$prav_os1<- ""
data$prav_os2<- ""

data$prav_os1<- ifelse(grepl(tolower(data$zs1), pattern="a\\..*s"),"a.s.",data$prav_os1)
data$prav_os2<- ifelse(grepl(tolower(data$zs2), pattern="a\\..*s"),"a.s.",data$prav_os2)

data$prav_os1<- ifelse(grepl(tolower(data$zs1), pattern="s\\..*r\\..*o"),"s.r.o",data$prav_os1)
data$prav_os2<- ifelse(grepl(tolower(data$zs2), pattern="a\\..*s\\..*o"),"s.r.o",data$prav_os2)

data$prav_os1<- ifelse(grepl(tolower(data$zs1), pattern="š\\..*p"),"š.p.",data$prav_os1)
data$prav_os2<- ifelse(grepl(tolower(data$zs2), pattern="š\\..*p"),"š.p.",data$prav_os2)

data$prav_os1<- ifelse(grepl(tolower(data$zs1), pattern="n\\..*o"),"n.o.",data$prav_os1)
data$prav_os2<- ifelse(grepl(tolower(data$zs2), pattern="n\\..*o"),"n.o.",data$prav_os2)


# typy zmluv
data$typ_zmluvy<- ""

for(i in 1:length(TYPYZMLUV))
{
  data$typ_zmluvy<- ifelse(tolower(data$predmet)==TYPYZMLUV[i], TYPYZMLUV[i], data$typ_zmluvy)
}

data$typ_zmluvy<- ifelse(grepl(tolower(data$predmet), pattern="zmluva.*náj|náj.*zmluva"),"nájomná zmluva",data$typ_zmluvy)
data$typ_zmluvy<- ifelse(grepl(tolower(data$predmet), pattern="zmluva.*diel"),"zmluva o dielo",data$typ_zmluvy)
data$typ_zmluvy<- ifelse(grepl(tolower(data$predmet), pattern="dohoda.*splátk"),"dohoda o splátkach",data$typ_zmluvy)

#changing dates from character to date
data$datum_ucinnost %<>% as.Date()

#changing datum platnosti
data$datum_platnost_do<- ifelse(data$datum_platnost_do=="0000-00-00", NA, data$datum_platnost_do)
data$datum_platnost_do<- ifelse(as.numeric(sapply(lapply(stri_extract_all(data$datum_platnost_do, regex="[0-9]"), head, n=4), stri_flatten, collapse=""))<as.numeric(format(Sys.Date(),"%Y")) |
                                  as.numeric(sapply(lapply(stri_extract_all(data$datum_platnost_do, regex="[0-9]"), head, n=4), stri_flatten, collapse=""))>(as.numeric(format(Sys.Date(),"%Y"))+100),
                                NA, data$datum_platnost_do)

data$datum_platnost_do %<>% as.Date()

data$datum_platnost_do<- ifelse(data$datum_platnost_do<data$datum_ucinnost, NA, data$datum_platnost_do)
data$datum_platnost_do<- as.Date(data$datum_platnost_do, origin="1970-01-01")

#changing datum zverejnene to datetime
data$datum_zverejnene<- strptime(x = as.character(data$datum_zverejnene), format = "%Y-%m-%d %H:%M:%S")

#doplnenie datumov o rok a mesiac + doba trvania zmluvy
data$year<- year(data$datum_zverejnene)
data$month<- month(data$datum_zverejnene)
data$ym<- as.Date(strptime(paste(data$year,data$month,"01",sep="-"), format="%Y-%m-%d"))

# POSIXlt dates to character
data$datum_zverejnene %<>% as.character()

# doba trvania zmluvy
data$doba_trvania<- as.numeric(data$datum_platnost_do-data$datum_ucinnost)

# changing suma from character to numeric
data$suma_spolu %<>% as.numeric()

#metrics
data$suma_na_obyvatela1<- data$suma_spolu/data$obyvatelov1
data$suma_na_obyvatela2<- data$suma_spolu/data$obyvatelov2


# joining with currenty year data
data_year<- readRDS('crz_data_new.rds')

data_year<- bind_rows(data, data_year)
data_year %<>% unique()

# saving df with new data
saveRDS(data_year, 'crz_data_new.rds')

# saving new LAST DATE variable
LAST_DATE<- data$datum_zverejnene %>% as.Date() %>% max() %>% as.character()

writeLines(LAST_DATE,'./Aux/last_date.txt')

