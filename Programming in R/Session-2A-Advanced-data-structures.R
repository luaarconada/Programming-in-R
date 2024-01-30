
## ------------------------------------------------------------------------

df = 
read.table("https://s3.amazonaws.com/assets.datacamp.com/blog_assets/scores_timed.txt", 
header=FALSE, sep="/", strip.white=TRUE, na.strings="EMPTY")
# kable(df)

df

## ------------------------------------------------------------------------

df = read.csv("https://s3.amazonaws.com/assets.datacamp.com/blog_assets/scores_timed.csv", 
       header=TRUE, quote="\"", stringsAsFactors=TRUE, strip.white=TRUE)
# kable(df)

df

## ------------------------------------------------------------------------

df = 
read.csv2("https://s3.amazonaws.com/assets.datacamp.com/blog_assets/scores_timed2.csv", 
        header = FALSE, 
        quote = "\"", 
        dec = ",", 
        row.names = c("M", "N", "O", "P", "Q"), 
        col.names= c("X", "Y", "Z", "A","B"), 
        fill = TRUE, 
        strip.white = TRUE, 
        stringsAsFactors=TRUE)

# kable(df)

df

# Check the type of data
str(df)

## ------------------------------------------------------------------------

options(repos=c(CRAN="http://cran.rstudio.com"))
if (!require(DAAG)) install.packages("DAAG")
library(DAAG)

if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

if (!require(data.table)) install.packages("data.table")

## ------------------------------------------------------------------------

help(headInjury)
head(headInjury)

data = headInjury
dim(data)

names(data)

class(data)

short = c("age.65", "amne.bef", "sk.frac", "GCS.dec", "GCS.13", 
"GCS.15.2", "high.risk", "loss.con", "o.sk.frac", "vomit", "imp.b.inj")

colnames(data) = short

head(data)

## ------------------------------------------------------------------------

data = cbind(1:nrow(data), data)
colnames(data)[1] = "number"

data_sub = select(data,1:3)
head(data_sub)

var_temp = names(data)[4:6]
data_sub = select(data, high.risk:vomit)
head(data_sub)

## ------------------------------------------------------------------------

data_sub = select(data, -(high.risk:vomit))
head(data_sub)

## ------------------------------------------------------------------------

i = match("high.risk", names(data))
j = match("vomit", names(data))
data_sub = data[, -(i:j)]
head(data_sub)

## ------------------------------------------------------------------------

data_sub = select(data, ends_with("c"))
head(data_sub)

## ------------------------------------------------------------------------

data_sub = filter(data,age.65==1)
head(data_sub)

## ------------------------------------------------------------------------

data_sub = data[data$age.65==1,] 
head(data_sub)

## ------------------------------------------------------------------------

data_sub = filter ( data,(age.65==1) & (amne.bef==1) )
head(data_sub)

## ------------------------------------------------------------------------

data_sort = arrange(data, amne.bef)
head(data_sort)

## ------------------------------------------------------------------------

data_sort = arrange(data, desc(amne.bef))
head(data_sort)

## ------------------------------------------------------------------------

data = rename(data, obs_number=number)
head(data)

## ------------------------------------------------------------------------

colnames(data)[1] = "number_obs"
head(data)

## ------------------------------------------------------------------------

data = mutate(data, new_col1=amne.bef - sk.frac)
head(data)

## ------------------------------------------------------------------------

data$new_col2 = data$amne.bef - data$sk.frac

## ------------------------------------------------------------------------

data2 = transmute(data, new_col1=amne.bef - sk.frac)
head(data2)

## ------------------------------------------------------------------------

library(data.table)

w = 
"https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv"

flights = fread(w)

dim(flights)
head(flights)

## ------------------------------------------------------------------------

DT = data.table(
ID = c("b","b","b","a","a","c"),
a = 1:6,
b = 7:12,
c = 13:18
)

DT

## ------------------------------------------------------------------------

ans = flights[origin == "JFK" & month == 6]
head(ans)

## ------------------------------------------------------------------------

ans = flights[1:2]
ans

## ------------------------------------------------------------------------

ans = flights[order(origin, -dest)]
head(ans)

## ------------------------------------------------------------------------

ans = flights[, arr_delay]
head(ans)

## ------------------------------------------------------------------------

ans = flights[, list(arr_delay)]
head(ans)

## ------------------------------------------------------------------------

ans = flights[, .(arr_delay, dep_delay)]
# ans = flights[, list(arr_delay, dep_delay)]
head(ans)

## ------------------------------------------------------------------------

ans = flights[, .(delay_arr=arr_delay, delay_dep=dep_delay)]

head(ans)

## ------------------------------------------------------------------------

ans = flights[, sum( (arr_delay + dep_delay) < 0 )]
ans

## ------------------------------------------------------------------------

ans = flights[origin == "JFK" & month == 6, .(m_arr = mean(arr_delay), 
m_dep = mean(dep_delay))]

ans

## ------------------------------------------------------------------------

ans = flights[origin == "JFK" & month == 6, length(dest)]
ans

## ------------------------------------------------------------------------

ans = flights[origin == "JFK" & month == 6, .N]
ans

