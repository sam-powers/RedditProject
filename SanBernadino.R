library(anytime) 
library(tidyverse)
library(tidytext)
library(lubridate)
library(ggplot2)



id.nov <- "1LkFsNchdxhYcj-e2hpkRCNnM5aAL-h7r"
id.dec <- "1LWiG0Y3Jmh_8UBI0tN27_s3BtPsKfZEX"
id.jan <- "1LbeZ3b-BJX6D-pZzIknj7gAqLV_68grj"

sbern.nov  <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.nov))
sbern.dec <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.dec))
sbern.jan <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.jan))

sbern <- rbind(sbern.nov, sbern.dec, sbern.jan)
########
# Define the functions I want for quick analysis #

# sentiment #
sentimental <- function(dataframe){
  d <- data.frame(dataframe)
  d$body <- as.character(d$body)
  d <- d %>% 
    unnest_tokens(word, body, token = "words", format = "text") %>%
    inner_join(get_sentiments("bing"), by = "word") %>%
    count(sentiment, ID) %>%
    spread(sentiment, n, fill = 0) 
  dataframe <- left_join(dataframe, d, by = "ID") %>%
    replace_na(list(negative = 0, positive = 0)) %>%
    mutate(sentiment = positive - negative, negative = -1*(negative))
} 

########


# Select the portions I want #
sbern.data <- sbern %>% 
  dplyr::select(body, created_utc, subreddit) %>%
  mutate(ID = 1:length(sbern$created_utc))

#######

# Use formula to get the sentiments #
sbern.data <- sentimental(sbern.data)
plotdata.sbern <- sbern.data %>% gather("sent","n", 5:6 )


# Calculate number of tweets per hour #
rates.sbern <- sbern.data %>% 
  mutate(hour = floor_date(anytime(created_utc), unit = "1 hour")) %>%
  count(hour) %>%
  rename(count = n)


# Average Sentiment per hour #
avg.sbern.data <- sbern.data %>%   
  mutate(hour = floor_date(anytime(created_utc), unit = "1 hour")) %>%
  group_by(hour) %>% 
  summarise(avgsent = mean(abs(sentiment)), avgpos = mean(positive), avgneg = mean(negative))

avg.day <- sbern.data %>%
  mutate(hour = floor_date(anytime(created_utc), unit = "12 hour")) %>%
  group_by(hour) %>% 
  summarise(avgsent = mean(abs(sentiment)), avgpos = mean(positive), avgneg = mean(negative), varsent = var(sentiment))

# Join them
rates.sbern <- rates.sbern %>% left_join(avg.sbern.data)

hourly.pos.neg.sbern <- rates.sbern %>% gather("type", "n", 4:5)

# Plot number of tweets per hour and sentiment #
# count and sentiment
ggplot(avg.sbern.data, aes(x=hour, y = avgsent)) + geom_point(size = .001)
ggplot(rates.sbern, aes(x = hour, y = count)) + geom_point(size = .001)
ggplot(rates.sbern, aes(x = hour, y = log(count))) + geom_point(size = .001)


# Negative/Positive by time
ggplot(plotdata.sbern, aes(x = anytime(created_utc), y = n, color = sent )) + 
  geom_point(size = .001) 

# Sentiment by time
ggplot(plotdata.sbern, aes(x= anytime(created_utc), y = sentiment)) + geom_point(size = .001) 

# Avg pos/negative by hour
ggplot(hourly.pos.neg.sbern, aes(x=hour, y = n, color= type)) + geom_point(size = .001)



###
# by half day #
ggplot(avg.day  %>%
         gather("type", "n", 3:4), aes(x=hour, y = n, color = type)) + geom_point(size = .001)

ggplot(avg.day, aes(x=hour, y = varsent)) + geom_point(size = .001)
