install.packages("anytime")
library(anytime)
library(tidyverse)
library(tidytext)
library(lubridate)



id <- "1D6SW839rjN9Eq_W1cWBRFMjgcNiP9ocZ"
id.sept <- "1DBDfUhycuZ56yxTwoy__D-LW5iasu82E"
id.oct <- "1DLoeXmBtiQ41xD9gbZGKgn2xwd9hx3ct"
cville.aug  <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
cville.sept <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.sept))
cville.oct <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id.oct))


cville <- rbind(cville.aug, cville.sept, cville.oct)

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
cville.data <- cville %>% 
  dplyr::select(body, created_utc, subreddit) %>%
  mutate(ID = 1:length(cville$created_utc))

#######

# Use formula to get the sentiments #
cville.data <- sentimental(cville.data)
plotdata <- cville.data %>% gather("sent","n", 5:6 )


# Calculate number of tweets per hour #
rates.cville <- cville.data %>% 
  mutate(hour = floor_date(anytime(created_utc), unit = "1 hour")) %>%
  count(hour) %>%
  rename(count = n)

# Average Sentiment per hour #
avg.cville.data <- cville.data %>%
  mutate(hour = floor_date(anytime(created_utc), unit = "1 hour")) %>%
  group_by(hour) %>% 
  summarise(avgsent = mean(sentiment), avgpos = mean(positive), avgneg = mean(negative))

avg.day <- cville.data %>%
  mutate(hour = floor_date(anytime(created_utc), unit = "12 hour")) %>%
  group_by(hour) %>% 
  summarise(avgsent = mean(sentiment), avgpos = mean(positive), avgneg = mean(negative), varsent = var(sentiment))



# Join them
rates.cville <- rates.cville %>% left_join(avg.cville.data)

plot.hourly.pos.neg <- rates.cville %>% gather("type", "n", 4:5)

# Plot number of tweets per hour and sentiment #
 # count and sentiment
ggplot(rates.cville, aes(x=hour, y = avgsent)) + geom_point(size = .001)
ggplot(rates.cville, aes(x = hour, y = count)) + geom_point(size = .001)
  
  # Negative/Positive by time
ggplot(plotdata, aes(x = anytime(created_utc), y = n, color = sent )) + 
  geom_point(size = .001) 

  # Sentiment by time
ggplot(plotdata, aes(x= anytime(created_utc), y = sentiment)) + geom_point(size = .001) 

  # Avg pos/negative by hour
ggplot(plot.hourly.pos.neg, aes(x=hour, y = n, color= type)) + geom_point(size = .001)

###
# by half day #
ggplot(avg.day  %>%
         gather("type", "n", 3:4), aes(x=hour, y = n, color = type)) + geom_point(size = .001)

ggplot(avg.day, aes(x=hour, y = varsent)) + geom_point(size = .001)















####### Trash ########
'

##### getting sentiment into the data set
d <- data.frame(aug1_15)
d$body <- as.character(d$body)
d <- d %>% unnest_tokens(word, body, token = "words", format = "text")
d <- d %>% inner_join(get_sentiments("bing"), by = "word") 
d <- d %>% count(sentiment, ID)
d <- d %>% spread(sentiment, n, fill = 0)
joined <- left_join(aug1_15, d)
joined <- joined %>% replace_na(list(negative = 0, positive = 0)) %>% mutate(sentiment = positive - negative)


#########
# This was a truly inefficient idea, but it ended up working. The above solution is much better.
d <- data.frame(txt = aug1_15$body[1]) 
d$txt <- as.character(d$txt)
d <- d %>% unnest_tokens(word, txt, token = "words", format = "text")
d <- d %>% inner_join(get_sentiments("bing"))

d <- d  %>%  count(sentiment) 
if (length(d$sentiment) > 1) {
d <- d %>% 
spread(sentiment, n, fill = 0) %>%            
mutate(sentiment = positive - negative)
} 
else { 
if (d$sentiment[1] == "positive") {
d <- data.frame(negative = 0, positive = d$n[1], sentiment = d$n[1])
}
else {
d <- data.frame(negative = d$n[1], positive = 0, sentiment = -1* (d$n[1]))
}
}
d <- data.frame(sentiment = c("positive, negative"), n = c(0,0))
d <- d  %>%   spread(sentiment, n, fill = 0)
d <- d  %>%   mutate(sentiment = positive - negative)


# sapply
unnest_tokens(testframe, tokens, char)


sentiments <- NULL      
d <- NULL

get_sent <- function(tweet){
d <- data.frame(txt = tweet) 
d$txt <- as.character(d$txt)
d <- d %>% unnest_tokens(word, txt, token = "words", format = "text") %>%
inner_join(get_sentiments("bing"), by = "word") 
if (length(d$sentiment) > 0){
d <- d %>% count(sentiment) 
if (length(d$sentiment ) > 1) {
d <- d %>% 
spread(sentiment, n, fill = 0) %>%            
mutate(sentiment = positive - negative)
}
else { 
if (d$sentiment[1] == "positive") {
d <- data.frame(negative = 0, positive = d$n[1], sentiment = d$n[1])
}
else {
d <- data.frame(negative = d$n[1], positive = 0, sentiment = -1* (d$n[1]))
}
}
}
else {
d <- tibble(negative = 0, positive = 0, sentiment = 0)
}
}
n <- length(aug1_15$body)
sentiments <- NULL

for (i in 1:n) {
q <- get_sent(aug1_15$body[i])
sentiments <- rbind(sentiments, q)
}
'



