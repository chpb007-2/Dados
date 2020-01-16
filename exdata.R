setwd("/home/chpb007/Documentos/Livros-R/Dados")
base = readRDS("chicago")
library(dplyr)
library(readr)

# select: return a subset of the columns of a data frame, using flexible notation
# filter: extract a subset of rows from a data frame base on logical conditions
# arrange: reorder rows of a data frame
# rename: rename variables in a data frame
# mutate: add new variables/columns or tranform exising variables
# summarise/summarize: generate summary statistics 
# %>% the "pipe" operator used to connect multiple verb actions

# (a) select()
# ?select
dim(base)
str(base)
names(base)[1:2]
subset = select(base, city:tmpd)
subset = select(base, city, tmpd, date)
subset = rename(base, cidade = city) #new_name = old_name

# (b)  filter()
# ?filter
subset = filter(base, pm25tmean2 > 20)
subset = filter(base, pm25tmean2 == 20)
subset = filter(base, pm25tmean2 > 20 & tmpd > 30)

# (c) arrange()
# ?arrange
subset = arrange(base, date)
head(subset) ; tail(subset) #ano-mês-dia
subset = arrange(base, desc(date))

# (d) rename()
# ?rename
head(base[,1:5],2)
subset = rename(base, aloha = dptp, kk = pm25tmean2)
base = rename(base, pm25 = pm25tmean2)

# (e) mutate()
# ?mutate, also loook for ?transmute
subset = mutate(base, pm25tendencia = pm25 - mean(pm25, na.rm = T))

# (f) group_by()
# ?group_by
base = mutate(base, year = as.POSIXlt(date)$year + 1900)
years = group_by(base, year)
summarize(years, pm25 = mean(pm25, na.rm = TRUE),
          o3 = max(o3tmean2, na.rm = TRUE),
          no2 = median(no2tmean2, na.rm = TRUE))

# (g) %>%
# Criando nova variável que é o mês, organizando por essa variável e então obtendo estatística desc.
mutate(base, month = as.POSIXlt(date)$mon + 1) %>%
  group_by(month) %>%
  summarize(pm25 = mean(pm25, na.rm = TRUE), 
            o3 = max(o3tmean2, na.rm = TRUE),
            no2 = median(no2tmean2, na.rmo = TRUE))


# Trabalhando com base de dados - parte 1
# Os dados foram obtidos de: https://aqs.epa.gov/aqsweb/airdata/download_files.html#Raw
base = read.csv("hourly_44201_2014.csv", header = TRUE)
names(base) = make.names(names(base)) #eliminando espaço entre os nomes!
nrow(base) ; ncol(base) ; str(base)
table(base$Time.Local)

filter(base, Time.Local == "00:00") %>% 
  select(State.Name, County.Name, Date.Local, Time.Local, Sample.Measurement)

filter(base, State.Code == "36" & County.Code == "33" & Date.Local == "2014-09-30") %>%
  select(Date.Local, Time.Local, Sample.Measurement)

summary(base$Sample.Measurement)
#Which county in the U.S have the highest levels of ambient ozone pollution?
rank = group_by(base, State.Name, County.Name) %>%
  summarize(ozone = mean(Sample.Measurement)) %>%
  as.data.frame %>%
  arrange(desc(ozone))

# Running bootstrap for (?)
set.seed(1994)
N = nrow(base)
idx = sample(N,N, replace = TRUE)
base2 = base[idx,]

rank2 = group_by(base2, State.Name, County.Name) %>%
  summarize(ozonio = mean(Sample.Measurement)) %>%
  arrange(desc(ozonio))

