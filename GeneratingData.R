library(anytime)
library(tidyverse)
library(tidytext)
library(lubridate)
library(fitdistrplus)

as.numeric(as.POSIXct("2012-08-01 00:00:00"))
as.numeric(as.POSIXct("2012-10-20 00:00:00"))
as.numeric(as.POSIXct("2012-12-13 00:00:00"))
as.numeric(as.POSIXct("2013-01-01 00:00:00"))
as.numeric(as.POSIXct("2013-02-28 00:00:00"))
as.numeric(as.POSIXct("2013-09-14 00:00:00"))
as.numeric(as.POSIXct("2013-11-19 00:00:00"))



#########
# Sandy #
#########

id.sandy <- "1SNQi-y2OXmGWH5GfV86jsojFGsPAMm3N"
sandy  <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.sandy))


########
sandy.data <- sandy %>% 
  dplyr::select(body, created_utc, subreddit) %>%
  mutate(ID = 1:length(sandy$created_utc))

rates.sandy <- sandy.data %>% 
  mutate(hour = floor_date(anytime(created_utc), unit = "1 hour")) %>%
  count(hour) %>%
  rename(count = n)

View(rates.sandy)

ggplot(rates.sandy, aes(x = hour, y = count)) + geom_point(size = .001)


time.data.sa <- sandy$created_utc[sandy$created_utc >= as.numeric(as.POSIXct("2012-10-29 13:00:00"))]
time.data.sa <- time.data.sa - min(time.data.sa)+ 1
time.data.sa <- time.data.sa[time.data.sa <= 4838400]

range(time.data.sa)
hist(time.data.sa)
fit.lnorm.sa <- fitdist(time.data.sa, "lnorm")
summary(fit.lnorm.sa)
plot(fit.lnorm.sa)

max(rates.sandy$count)



#########
# newtown #
#########

id.newtown <- "1SX16yRJAXzs4FR9m90KEG16Txe7Utofw"
id.newtown.dec <- "1SQ1P4RNGxc1TgHJyYtwz5_CP4xpM0TwA"
newtown.nov  <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.newtown))
newtown.dec  <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.newtown.dec))

########
newtown.nov <- newtown.nov %>% 
  dplyr::select(body, created_utc, subreddit) 

newtown.dec <- newtown.dec %>% 
  dplyr::select(body, created_utc, subreddit) 

newtown <- rbind(newtown.nov, newtown.dec)

newtown.data <- newtown %>% 
  dplyr::select(body, created_utc, subreddit) %>%
  mutate(ID = 1:length(newtown$created_utc))

rates.newtown <- newtown.data %>% 
  mutate(hour = floor_date(anytime(created_utc), unit = "1 hour")) %>%
  count(hour) %>%
  rename(count = n)

View(rates.newtown)

ggplot(rates.newtown, aes(x = hour, y = count)) + geom_point(size = .001)


time.data.ne <- newtown$created_utc
time.data.ne <- time.data.ne - min(time.data.ne)+ 1
time.data.ne <- time.data.ne[time.data.ne <= 4838400]

range(time.data.ne)
hist(time.data.ne)
fit.lnorm.ne <- fitdist(time.data.ne, "lnorm")
summary(fit.lnorm.ne)
plot(fit.lnorm.ne)
max(rates.newtown$count)

#############
# oak_creek #
#############

id.oak_creek <- "1SNlss4hxyQ3etBkyuTkIaMpGo3Do1hOx"
oak_creek  <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.oak_creek))


########
oak_creek.data <- oak_creek %>% 
  dplyr::select(body, created_utc, subreddit) %>%
  mutate(ID = 1:length(oak_creek$created_utc))

rates.oak_creek <- oak_creek.data %>% 
  mutate(hour = floor_date(anytime(created_utc), unit = "1 hour")) %>%
  count(hour) %>%
  rename(count = n)

View(rates.oak_creek)

ggplot(rates.oak_creek, aes(x = hour, y = count)) + geom_point(size = .001)


time.data.oc <- oak_creek$created_utc[oak_creek$created_utc >= as.numeric(as.POSIXct("2012-08-04 15:00:00"))]
time.data.oc <- time.data.oc - min(time.data.oc)+ 1
time.data.oc <- time.data.oc[time.data.oc <= 4838400]

range(time.data.oc)
hist(time.data.oc)
fit.lnorm.oc <- fitdist(time.data.oc, "lnorm")
summary(fit.lnorm.oc)
plot(fit.lnorm.oc)

max(rates.oak_creek$count)

#############
# navy_yard #
#############

id.navy_yard <- "1SYrosX7gL6gt3-CmQpco44smknG9VDXB"
navy_yard  <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.navy_yard))


########
navy_yard.data <- navy_yard %>% 
  dplyr::select(body, created_utc, subreddit) %>%
  mutate(ID = 1:length(navy_yard$created_utc))

rates.navy_yard <- navy_yard.data %>% 
  mutate(hour = floor_date(anytime(created_utc), unit = "1 hour")) %>%
  count(hour) %>%
  rename(count = n)

View(rates.navy_yard)

ggplot(rates.navy_yard, aes(x = hour, y = count)) + geom_point(size = .001)


time.data.ny <- navy_yard$created_utc[navy_yard$created_utc >= as.numeric(as.POSIXct("2013-09-16 12:00:00"))]
time.data.ny <- time.data.ny - min(time.data.ny)+ 1
time.data.ny <- time.data.ny[time.data.ny <= 4838400]

range(time.data.ny)
hist(time.data.ny)
fit.lnorm.ny <- fitdist(time.data.ny, "lnorm")
summary(fit.lnorm.ny)
plot(fit.lnorm.ny)

max(rates.navy_yard$count)

###########
# lincoln #
###########

id.lincoln <- "1N-toXMxLxhHd8XDa4JobGFkx08WTT6FI"
id.lincoln.may <- "1N4g6m5A-MEBbf7Rm8nMi7OcQ6ZG8ILgp"
lincoln.june  <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.lincoln))
lincoln.may  <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.lincoln.may))

########
lincoln <- rbind(lincoln.may, lincoln.june)

lincoln.data <- lincoln %>% 
  dplyr::select(body, created_utc, subreddit) %>%
  mutate(ID = 1:length(lincoln$created_utc))

rates.lincoln <- lincoln.data %>% 
  mutate(hour = floor_date(anytime(created_utc), unit = "1 hour")) %>%
  count(hour) %>%
  rename(count = n)

View(rates.lincoln)

ggplot(rates.lincoln, aes(x = hour, y = count)) + geom_point(size = .001)


time.data.li <- lincoln$created_utc
time.data.li <- time.data.li - min(time.data.li)+ 1
time.data.li <- time.data.li[time.data.li <= 4838400]

range(time.data.li)
hist(time.data.li)
fit.lnorm.li <- fitdist(time.data.li, "lnorm")
summary(fit.lnorm.li)
plot(fit.lnorm.li)
max(rates.lincoln$count)

###########
# charleston #
###########

id.charleston.june <- "1Tyui40poBI5PvqODw2JusdDpZHdIC5zJ"
id.charleston.july <- "1TrFmMSzHPC7HmfRFbjk8b9skTQtFQEYG"
id.charleston.aug <- "1TlmLUXtOqH_uE8v498RiI2ftUaovBqAC"

charleston.june  <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.charleston.june))
charleston.july  <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.charleston.july))
charleston.aug  <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.charleston.aug))


########
charleston <- rbind(charleston.june, charleston.july, charleston.aug)

charleston.data <- charleston %>% 
  dplyr::select(body, created_utc, subreddit) %>%
  mutate(ID = 1:length(charleston$created_utc))

rates.charleston <- charleston.data %>% 
  mutate(hour = floor_date(anytime(created_utc), unit = "1 hour")) %>%
  count(hour) %>%
  rename(count = n)

View(rates.charleston)

ggplot(rates.charleston, aes(x = hour, y = count)) + geom_point(size = .001)


time.data.ch <- charleston$created_utc[charleston$created_utc >= as.numeric(as.POSIXct("2015-06-17 23:00:00"))]
time.data.ch <- time.data.ch - min(time.data.ch)+ 1
time.data.ch <- time.data.ch[time.data.ch <= 4838400]

range(time.data.ch)
hist(time.data.ch)
fit.lnorm.ch <- fitdist(time.data.ch, "lnorm")
summary(fit.lnorm.ch)
plot(fit.lnorm.ch)
max(rates.charleston$count)

###########
# baltimore #
###########

id.baltimore.apr<- "1TAANQm6HKxFJdFowG9QJyA7lPooG9-NR"
id.baltimore.may<- "1TY0NkVoGGWYz-C5CM6VGzaSGyNz2JH8L"
id.baltimore.jun <- "1TdqODbdP8fMdmvhG4SGG1XTz2kb2l6wH"

baltimore.apr  <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.baltimore.apr))
baltimore.may  <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.baltimore.may))
baltimore.jun  <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.baltimore.jun))


########
baltimore <- rbind(baltimore.apr, baltimore.may, baltimore.jun)

baltimore.data <- baltimore %>% 
  dplyr::select(body, created_utc, subreddit) %>%
  mutate(ID = 1:length(baltimore$created_utc))

rates.baltimore <- baltimore.data %>% 
  mutate(hour = floor_date(anytime(created_utc), unit = "1 hour")) %>%
  count(hour) %>%
  rename(count = n)

View(rates.baltimore)

ggplot(rates.baltimore, aes(x = hour, y = count)) + geom_point(size = .001)


time.data.ba <- baltimore$created_utc[baltimore$created_utc >= as.numeric(as.POSIXct("2015-04-27 14:00:00"))]
time.data.ba <- time.data.ba - min(time.data.ba)+ 1
time.data.ba <- time.data.ba[time.data.ba <= 4838400]

range(time.data.ba)
hist(time.data.ba)
fit.lnorm.ba <- fitdist(time.data.ba, "lnorm")
summary(fit.lnorm.ba)
plot(fit.lnorm.ba)
max(rates.baltimore$count)

###########
# Dallas #
###########

id.dallas.july<- "1T8WH-mQeezY7lvQV6lAnlbcBUfCwvF1V"
id.dallas.aug<- "1T7EBzU9jQnVQs_HuPIjUN34i2KG1nnLp"
id.dallas.sep <- "1T9di3f1y9y00gbLLPOwwIxeEnHU6fegc"

dallas.july  <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.dallas.july))
dallas.aug  <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.dallas.aug))
dallas.sep  <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.dallas.sep))


########
dallas <- rbind(dallas.july, dallas.aug, dallas.sep)

dallas.data <- dallas %>% 
  dplyr::select(body, created_utc, subreddit) %>%
  mutate(ID = 1:length(dallas$created_utc))

rates.dallas <- dallas.data %>% 
  mutate(hour = floor_date(anytime(created_utc), unit = "1 hour")) %>%
  count(hour) %>%
  rename(count = n)

View(rates.dallas)

ggplot(rates.dallas, aes(x = hour, y = count)) + geom_point(size = .001)


time.data.da <- dallas$created_utc[dallas$created_utc >= as.numeric(as.POSIXct("2016-07-08 01:00:00"))]
time.data.da <- time.data.da - min(time.data.da)+ 1
time.data.da <- time.data.da[time.data.da <= 4838400]

range(time.data.da)
hist(time.data.da)
fit.lnorm.da <- fitdist(time.data.da, "lnorm")
summary(fit.lnorm.da)
plot(fit.lnorm.da)
max(rates.dallas$count)

