library(anytime) 
library(tidyverse)
library(tidytext)
library(lubridate)
library(ggplot2)



id.sep <- "1LGsiVpQoUD87uEHJx_yy9OkZNA5kiRu3"
id.oct <- "1LJuCObKEgnNrMmAzq37nadRtAXb-Idaz"
id.nov <- "1LM5qng5pE8n8mHDV7GbBbL2Rd6JeQ_pn"

vegas.nov  <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.sep))
vegas.dec <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.oct))
vegas.jan <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.nov))

vegas <- rbind(vegas.nov, vegas.dec, vegas.jan)
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
vegas.data <- vegas %>% 
  dplyr::select(body, created_utc, subreddit) %>%
  mutate(ID = 1:length(vegas$created_utc))

#######

# Use formula to get the sentiments #
vegas.data <- sentimental(vegas.data)
plotdata.vegas <- vegas.data %>% gather("sent","n", 5:6 )


# Calculate number of tweets per hour #
rates.vegas <- vegas.data %>% 
  mutate(hour = floor_date(anytime(created_utc), unit = "1 hour")) %>%
  count(hour) %>%
  rename(count = n)


# Average Sentiment per hour #
avg.vegas.data <- vegas.data %>%   
  mutate(hour = floor_date(anytime(created_utc), unit = "1 hour")) %>%
  group_by(hour) %>% 
  summarise(avgsent = mean(abs(sentiment)), avgpos = mean(positive), avgneg = mean(negative))

avg.day <- vegas.data %>%
  mutate(hour = floor_date(anytime(created_utc), unit = "12 hour")) %>%
  group_by(hour) %>% 
  summarise(avgsent = mean(abs(sentiment)), avgpos = mean(positive), avgneg = mean(negative), varsent = var(sentiment))

# Join them
rates.vegas <- rates.vegas %>% left_join(avg.vegas.data)

hourly.pos.neg.vegas <- rates.vegas %>% gather("type", "n", 4:5)

# Plot number of tweets per hour and sentiment #
# count and sentiment
ggplot(avg.vegas.data, aes(x=hour, y = avgsent)) + geom_point(size = .001)
ggplot(rates.vegas, aes(x = hour, y = count)) + geom_point(size = .001)
ggplot(rates.vegas, aes(x = hour, y = log(count))) + geom_point(size = .001)


# Negative/Positive by time
ggplot(plotdata.vegas, aes(x = anytime(created_utc), y = n, color = sent )) + 
  geom_point(size = .001) 

# Sentiment by time
ggplot(plotdata.vegas, aes(x= anytime(created_utc), y = sentiment)) + geom_point(size = .001) 

# Avg pos/negative by hour
ggplot(hourly.pos.neg.vegas, aes(x=hour, y = n, color= type)) + geom_point(size = .001)



###
# by half day #
ggplot(avg.day  %>%
         gather("type", "n", 3:4), aes(x=hour, y = n, color = type)) + geom_point(size = .001)

ggplot(avg.day, aes(x=hour, y = varsent)) + geom_point(size = .001)

