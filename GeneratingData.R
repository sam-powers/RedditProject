library(anytime)
library(tidyverse)
library(tidytext)
library(lubridate)


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


time.data.sa <- sandy$created_utc[sandy$created_utc >= as.numeric(as.POSIXct("2016-06-12 07:00:00"))]
time.data.sa <- time.data.sa - min(time.data.sa)+ 1
time.data.sa <- time.data.sa[time.data.sa <= 4838400]

range(time.data.sa)
hist(time.data.sa)
fit.lnorm.sa <- fitdist(time.data.sa, "lnorm")
summary(fit.lnorm.sa)
plot(fit.lnorm.sa)