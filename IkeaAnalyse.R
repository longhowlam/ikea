### script was needed to import all the daily scraped files

library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(prophet)

fp = glob2rx("B*.Rds")
tf = list.files(path = "/home/longhowlam/RProjects/IkeaTest/" , pattern = fp)


AllBillies = NULL
for (f in tf)
{
  bl = readRDS(paste0("/home/longhowlam/RProjects/IkeaTest/", f))
  AllBillies = rbind(AllBillies, bl)
  print(f)
  
}

AllBillies$datum      = ymd(str_sub(as.character(AllBillies$tijd),1,10))
AllBillies$datumtijd  = parse_date_time(str_sub(as.character(AllBillies$tijd)), "Ymd HMS")
AllBillies$voorraad   = as.integer(as.character(AllBillies$Opvooraad))
AllBillies$uur        = hour(AllBillies$datumtijd)
AllBillies$dag        = wday(AllBillies$datumtijd, label = TRUE)

## maximale voorraad per dag
perdag = AllBillies %>% group_by(Lokatie, datum) %>% summarize(maxv = max(voorraad), minv = min(voorraad))
pl = ggplot(data = perdag,  aes(x = datum, y = maxv), color = "black") + geom_bar(stat="identity")
pl + facet_grid(~Lokatie) 

## voorraaden per dag per loatie
tmp = AllBillies %>% group_by(uur, dag, Lokatie, kleur) %>% summarise(maxs = max(voorraad, na.rm=TRUE))
p = ggplot(data = tmp,  aes(x = uur, y = maxs, fill=kleur), color = "black")
p2 = p + geom_bar(color = "black", stat="identity") + scale_fill_manual(values=c("black", "white", "brown"))
p3 = p2 + facet_grid(Lokatie ~ dag  )
p3


AllBilliesVerkocht = AllBillies %>% arrange( Lokatie, kleur, datumtijd)
AllBilliesVerkocht2 = AllBilliesVerkocht %>% group_by(Lokatie, kleur) %>% 
  mutate(
    lagorder = lag(voorraad),
    verkocht = ifelse(voorraad - lagorder < 0, lagorder - voorraad ,0) 
  )


saveRDS(AllBilliesVerkocht2,"AllBilliesVerkocht2.Rds")
tail(AllBilliesVerkocht2)


tmp = AllBilliesVerkocht2 %>% group_by(datum) %>% summarise(v = sum(verkocht, na.rm=TRUE))
ggplot(data = tmp,  aes(x = datum, y = v), color = "black") + geom_bar(stat="identity")

tmp = AllBilliesVerkocht2 %>% group_by(uur,  kleur) %>% summarise(n=n(),v = mean(verkocht, na.rm=TRUE), s = sum(verkocht, na.rm=TRUE) )
p = ggplot(data = tmp,  aes(x = uur, y = s, fill=kleur), color = "black")
p + geom_bar(color = "black", stat="identity") + scale_fill_manual(values=c("black", "white", "brown"))

tmp = AllBilliesVerkocht2 %>% group_by(uur, dag, Lokatie, kleur) %>% summarise(s = sum(verkocht, na.rm=TRUE))
p = ggplot(data = tmp,  aes(x = uur, y = s, fill=kleur), color = "black")
p2 = p + geom_bar(color = "black", stat="identity") + scale_fill_manual(values=c("black", "white", "brown"))
p2

p3 = p2 + facet_grid(Lokatie ~ dag  )
p3




tmp = AllBilliesVerkocht2 %>% group_by(datum, Lokatie) %>% summarise(v = sum(verkocht, na.rm=TRUE))
p = ggplot(data = tmp,  aes(x = datum, y = v), color = "black") + geom_bar(stat="identity")
p+facet_grid(Lokatie~.)


tmp = AllBilliesVerkocht2 %>% group_by(dag,  kleur) %>% summarise(n=n(),v = mean(verkocht, na.rm=TRUE), s = sum(verkocht, na.rm=TRUE) )
p = ggplot(data = tmp,  aes(x = dag, y = s, fill=kleur), color = "black")
p + geom_bar(color = "black", stat="identity") + scale_fill_manual(values=c("black", "white", "brown"))
