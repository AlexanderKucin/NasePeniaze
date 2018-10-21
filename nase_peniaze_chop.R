library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)

library(rsconnect)

Sys.setlocale("LC_ALL", "sk_SK.UTF-8")

# CONST----
PATH = "./prehlad"

TOPx = 100
DO_SUM_LIMIT = 10000

#loading data ----
dataS<- readRDS('crz_data_new.rds')

dataS %<>% select(suma_spolu, NazovRezortu, year, month, ym, kraj1, okres1, kraj2, okres2, obyvatelov1, mesto1, suma_na_obyvatela1, obyvatelov2, mesto2, suma_na_obyvatela2, zs1, prav_os1, zs2, prav_os2, typ_zmluvy, predmet, ID, datum_ucinnost,datum_platnost_do,datum_zverejnene)
dataS$ym %<>% as.character()

# money by "rezort"--------
rezort<- dataS %>% 
  select(suma_spolu, NazovRezortu, year, month, ym) %>%
  group_by(year, month, ym,NazovRezortu) %>% 
  summarise(suma_spolu=sum(suma_spolu, na.rm=T),
            pocet=length(ym),
            avg=suma_spolu/pocet) %>% 
  ungroup()

# money by "kraj, okres" ----------
ko<- dataS %>% 
  select(suma_spolu, kraj1, okres1, ym) %>%
  group_by(ym,kraj1,okres1) %>% 
  summarise(suma_spolu=sum(suma_spolu, na.rm=T),
            pocet=length(ym),
            avg=suma_spolu/pocet) %>% 
  ungroup()

koD<- dataS %>% 
  select(suma_spolu, kraj2, okres2, ym) %>%
  group_by(ym,kraj2,okres2) %>% 
  summarise(suma_spolu=sum(suma_spolu, na.rm=T),
            pocet=length(ym),
            avg=suma_spolu/pocet) %>% 
  ungroup()

names(ko)<- c("ym","kraj","okres","suma_spolu","pocet","priemerna_suma")
names(koD)<- c("ym","kraj","okres","suma_spolu","pocet","priemerna_suma")

ko$strana<- "objednavatel"
koD$strana<- "dodavatel"

ko<- bind_rows(ko, koD)
rm(koD)

ko<- subset(ko, !is.na(ko$kraj))

# money by "kraj, okres, mesto" --------
kom<- dataS %>% 
  select(suma_spolu, kraj1, okres1, obyvatelov1, mesto1, year, suma_na_obyvatela1) %>%
  group_by(year,kraj1,okres1, mesto1) %>% 
  summarise(suma_spolu=sum(suma_spolu, na.rm=T), 
            suma_na_obyv1=sum(suma_na_obyvatela1,na.rm=T),
            pocet=length(year),
            avg=suma_spolu/pocet) %>% 
  ungroup()

komD<- dataS %>% 
  select(suma_spolu, kraj2, okres2, obyvatelov2, mesto2, year, suma_na_obyvatela2) %>%
  group_by(year,kraj2,okres2, mesto2) %>% 
  summarise(suma_spolu=sum(suma_spolu, na.rm=T), 
            suma_na_obyv2=sum(suma_na_obyvatela2,na.rm=T),
            pocet=length(year),
            avg=suma_spolu/pocet) %>% 
  ungroup()

names(kom)<- c("year","kraj","okres","mesto","suma_spolu","suma_na_obyvatela","pocet","priemerna_suma")
names(komD)<- c("year","kraj","okres","mesto","suma_spolu","suma_na_obyvatela","pocet","priemerna_suma")

kom$strana<- "objednavatel"
komD$strana<- "dodavatel"

kom<- bind_rows(kom, komD)
rm(komD)

kom<- subset(kom, !is.na(kom$kraj))
kom<- subset(kom, !is.na(kom$mesto))

# Dodavatelia a objednavatelia----------
do<- dataS %>% 
  select(suma_spolu, NazovRezortu, zs1,prav_os1, year) %>% 
  group_by(year,NazovRezortu,zs1,prav_os1) %>% 
  summarise(suma_spolu=sum(suma_spolu, na.rm=T),
            pocet=length(year),
            avg=suma_spolu/pocet) %>% 
  ungroup() %>% 
  arrange(desc(suma_spolu)) %>%
  filter(suma_spolu > DO_SUM_LIMIT)


doD<- dataS %>% 
  select(suma_spolu, NazovRezortu, zs2, prav_os2,year) %>%
  group_by(year,NazovRezortu,zs2,prav_os2) %>% 
  summarise(suma_spolu=sum(suma_spolu, na.rm=T),
            pocet=length(year),
            avg=suma_spolu/pocet) %>% 
  ungroup() %>% 
  arrange(desc(suma_spolu)) %>%
  filter(suma_spolu > DO_SUM_LIMIT)

names(do)<- c("year","NazovRezortu","zs","prav_os","suma_spolu","pocet","priemerna_suma")
names(doD)<- c("year","NazovRezortu","zs","prav_os","suma_spolu","pocet","priemerna_suma")

do$strana<- "objednavatel"
doD$strana<- "dodavatel"

do<- bind_rows(do, doD)
rm(doD)

do$zs<- gsub('"',' ',do$zs)
do$prav_os<- ifelse(do$prav_os=="", "nie je p.o.", do$prav_os)

# top 100 miest, dodavatelov, objednavatelov, rezortov-----------
# rezorty
rez<- rezort %>% 
  group_by(year, NazovRezortu) %>% 
  summarise(suma_spolu=sum(suma_spolu, na.rm=T),
            pocet=sum(pocet),
            avg=suma_spolu/pocet) %>% 
  ungroup() %>% 
  arrange(desc(suma_spolu)) %>%
  mutate(typ = 'Rezorty')

# kraje
kraj<- kom %>% 
  group_by(year, kraj, strana) %>% 
  summarise(suma_spolu=sum(suma_spolu, na.rm=T),
            pocet=sum(pocet),
            avg=suma_spolu/pocet) %>% 
  ungroup() %>% 
  arrange(strana, year, desc(suma_spolu)) %>%
  group_by(strana, year) %>%
  top_n(TOPx, suma_spolu) %>%
  ungroup() %>%
  mutate(typ = 'Kraj')

# okresy
okres<- kom %>% 
  group_by(year, kraj, okres, strana) %>% 
  summarise(suma_spolu=sum(suma_spolu, na.rm=T),
            pocet=sum(pocet),
            avg=suma_spolu/pocet) %>% 
  ungroup() %>% 
  arrange(strana, year, desc(suma_spolu)) %>%
  group_by(strana, year) %>%
  top_n(TOPx, suma_spolu) %>%
  ungroup() %>%
  mutate(typ = 'Okresy')

# mesta
mesto<- kom %>%
  group_by(year, kraj, okres, mesto, strana) %>% 
  summarise(suma_spolu=sum(suma_spolu, na.rm=T),
            pocet=sum(pocet),
            avg=suma_spolu/pocet) %>% 
  ungroup() %>%
  arrange(strana, year, desc(suma_spolu)) %>%
  group_by(strana, year) %>%
  top_n(TOPx, suma_spolu) %>%
  ungroup() %>%
  mutate(typ = 'Mestá')

# zmluvne strany
zs<- do %>% 
  group_by(year, zs, strana) %>% 
  summarise(suma_spolu=sum(suma_spolu, na.rm=T),
            pocet=sum(pocet),
            avg=suma_spolu/pocet) %>% 
  ungroup() %>% 
  arrange(strana, year, desc(suma_spolu)) %>%
  group_by(strana, year) %>%
  top_n(TOPx, suma_spolu) %>%
  ungroup() %>%
  mutate(typ = 'Zmluvná strana')

top<- bind_rows(rez, kraj, okres, mesto, zs)

# suma podla typu zmluv-------------
typ<- dataS %>% 
  select(suma_spolu, NazovRezortu, typ_zmluvy, year)

typ$typ_zmluvy<- ifelse(typ$typ_zmluvy=="" | is.na(typ$typ_zmluvy), "iný", typ$typ_zmluvy)

typ %<>% 
  group_by(year,NazovRezortu,typ_zmluvy) %>% 
  summarise(suma_spolu=sum(suma_spolu, na.rm=T),
            pocet=length(year),
            avg=suma_spolu/pocet) %>% 
  ungroup()

#overview---------
overview<- dataS %>%
  select(predmet, ID, zs1,zs2,NazovRezortu,datum_ucinnost,datum_platnost_do,suma_spolu, datum_zverejnene,ym)

overview$datum_zverejnene<- as.Date(strptime(overview$datum_zverejnene, format="%Y-%m-%d"))

dates<- unique(overview$ym) %>% sort()
overview<- subset(overview, ym %in% dates[(length(dates)-2):length(dates)])

# monthly overview
monthly<- subset(overview, ym %in% dates[(length(dates)-2):(length(dates)-1)])

monthly %<>% 
  group_by(ym) %>% 
  summarise(suma=sum(suma_spolu, na.rm=T), 
            pocet=length(predmet),
            avg=suma/pocet) %>% 
  ungroup()

topM<- subset(overview,  ym==max(dates)) %>%
  arrange(desc(suma_spolu)) %>%
  top_n(TOPx, suma_spolu) %>%
  mutate(link = paste0("http://www.crz.gov.sk/index.php?ID=",ID)) %>%
  select(-ID,-datum_zverejnene,-ym)

rm(dates)

# daily
dates<- unique(overview$datum_zverejnene) %>% sort()

daily<- subset(overview, datum_zverejnene %in% dates[(length(dates)-1):length(dates)]) %>%
  group_by(datum_zverejnene) %>% 
  summarise(suma=sum(suma_spolu, na.rm=T), 
            pocet=length(predmet),
            avg=suma/pocet) %>% 
  ungroup()

topD<- subset(overview,  datum_zverejnene==max(dates)) %>%
  arrange(desc(suma_spolu)) %>%
  top_n(TOPx, suma_spolu) %>%
  mutate(link = paste0("http://www.crz.gov.sk/index.php?ID=",ID)) %>%
  select(-ID,-datum_zverejnene,-ym)

rm(dates, overview)

# joining with other years
rezort_old<- read.table(paste0(PATH,'/', 'rezort_2011-2017.txt'), sep=';', header = T, stringsAsFactors = F)
ko_old<- read.table(paste0(PATH,'/', 'ko_2011-2017.txt'), sep=';', header = T, stringsAsFactors = F)
kom_old<- read.table(paste0(PATH,'/', 'kom_2011-2017.txt'), sep=';', header = T, stringsAsFactors = F)
do_old<- read.table(paste0(PATH,'/', 'do_2011-2017.txt'), sep=';', header = T, stringsAsFactors = F)
top_old<- read.table(paste0(PATH,'/', 'top_2011-2017.txt'), sep=';', header = T, stringsAsFactors = F)
typ_old<- read.table(paste0(PATH,'/', 'typ_2011-2017.txt'), sep=';', header = T, stringsAsFactors = F)

rezort<- bind_rows(rezort_old, rezort)
ko<- bind_rows(ko_old, ko)
kom<- bind_rows(kom_old, kom)
do<- bind_rows(do_old, do)
top<- bind_rows(top_old, top)
typ<- bind_rows(typ_old, typ)

# EXPORT ALL
write.table(rezort,paste0(PATH,'/', 'rezort.txt'),sep=";",quote=F, row.names=F)

write.table(ko,paste0(PATH,'/','ko.txt'),sep=";",quote=F, row.names=F)

write.table(kom,paste0(PATH,'/','kom.txt'),sep=";",quote=F, row.names=F)

write.table(do,paste0(PATH,'/','do.txt'),sep=";",quote=T, row.names=F)

write.table(top,paste0(PATH,'/','top.txt'),sep=";",quote=T, row.names=F)

write.table(typ, paste0(PATH,'/','typ.txt'),sep=";",quote=T, row.names=F)

write.table(monthly,paste0(PATH,'/','monthly.txt'),sep=";",quote=F, row.names=F)
write.table(daily,paste0(PATH,'/','daily.txt'),sep=";",quote=F, row.names=F)
write.table(topD,paste0(PATH,'/','topD.txt'),sep=";",quote=F, row.names=F)
write.table(topM,paste0(PATH,'/','topM.txt'),sep=";",quote=F, row.names=F)

#DEPLOY----------------------
deployApp(PATH, appName = 'prehlad', launch.browser = F, forceUpdate = T)
