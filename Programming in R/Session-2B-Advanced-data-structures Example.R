
datos = read.csv2(file="mutua2.csv")

dim(datos)

head(datos, 3)

## ------------------------------------------------------------------------

kd = names(datos)
kable(list(kd[1:13], kd[14:26], kd[27:38]), booktabs = TRUE, linesep = '')

## ------------------------------------------------------------------------

library(dplyr)

datos1 = select(datos, T.TipoPersona, v.Cv)
kable(head(datos1))

## ------------------------------------------------------------------------

head(select(datos, -v.Cv), 3)

## ------------------------------------------------------------------------

kable(head(select(datos, T.SexoCO:anosprop)))

## ------------------------------------------------------------------------

head(select(datos, starts_with("V.")))

## ------------------------------------------------------------------------

head(filter(datos, V.peso >= 125), 4)

## ------------------------------------------------------------------------

head(filter(datos, V.peso >= 125, V.Cilindrada < 1200), 4)

## ------------------------------------------------------------------------

head(filter(datos, D.Estadocivil %in% c("Divorciado", "viudo")))

## ------------------------------------------------------------------------

datos %>%
select(T.TipoPersona, v.Cv) %>%
head %>% 
kable

## ------------------------------------------------------------------------

datos %>% 
arrange(D.Estadocivil) %>% 
head(3)

## ------------------------------------------------------------------------

datos %>%
select(T.TipoPersona, D.Estadocivil, G.zona) %>%
arrange(D.Estadocivil, G.zona) %>%
head %>% 
kable

## ------------------------------------------------------------------------

datos %>%
select(T.TipoPersona, D.Estadocivil, G.zona) %>%
arrange(D.Estadocivil, G.zona) %>%
filter(G.zona == "M" ) %>%
head %>% 
kable

## ------------------------------------------------------------------------

datos %>%
select(T.TipoPersona, D.Estadocivil, V.CapAseg) %>%
arrange(T.TipoPersona, desc(V.CapAseg)) %>%
filter(V.CapAseg > 17000) %>%
head %>% 
kable

## ------------------------------------------------------------------------

datos %>%
mutate(proportion = V.CapAseg/sum(V.CapAseg)) %>%
head(3)

## ------------------------------------------------------------------------

datos %>%
mutate(proportion = V.CapAseg/sum(V.CapAseg), OtroPeso = V.peso/100) %>%
head(3)

## ------------------------------------------------------------------------

datos %>%
summarise(avg.capi = mean(V.CapAseg))

## ------------------------------------------------------------------------

datos %>%
summarise(
 avg.capi = mean(V.CapAseg),
 min.capi = min(V.CapAseg),
 max.capi = max(V.CapAseg),
 total = n()
) %>% 
kable

## ------------------------------------------------------------------------

datos %>%
group_by(D.Estadocivil) %>%
summarise(
 avg.capi = mean(V.CapAseg),
 min.capi = min(V.CapAseg),
 max.capi = max(V.CapAseg),
 total = n()
) %>% 
kable

## ------------------------------------------------------------------------

datos %>%
group_by(D.Estadocivil) %>% 
summarise(
 Min = min(V.CapAseg, na.rm=TRUE),
 Median = median(V.CapAseg, na.rm=TRUE),
 Mean = mean(V.CapAseg, na.rm=TRUE),
 Var = var(V.CapAseg, na.rm=TRUE),
 SD = sd(V.CapAseg, na.rm=TRUE),
 Max = max(V.CapAseg, na.rm=TRUE),
 N = n()
) %>% 
kable

## ------------------------------------------------------------------------

library(data.table)

dt = data.table(datos)
kable(dt[, mean(V.CapAseg), by=D.Estadocivil])

## ------------------------------------------------------------------------

kable(dt[, mean(V.CapAseg), by=.(D.Estadocivil, no.grave)])

## ------------------------------------------------------------------------

kable(dt[V.CapAseg > 20000, mean(V.CapAseg), by=.(D.Estadocivil, no.grave)])

## ------------------------------------------------------------------------

head(dt[order(-V.CapAseg)], 3)

## ------------------------------------------------------------------------

dt[order(-V.CapAseg, V.plazas)][1:3]

## ------------------------------------------------------------------------

head(dt[T.TipoPersona %in% c("F", "J")], 3)

## ------------------------------------------------------------------------

dim(dt[T.TipoPersona %in% c("F", "J")])
