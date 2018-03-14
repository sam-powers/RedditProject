install.packages("anytime")
install.packages("tseries")
library(anytime)
library(tidyverse)
library(tidytext)
library(lubridate)
library(forecast)
library(tseries)


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
  summarise(avgsent = mean(abs(sentiment)), avgpos = mean(positive), avgneg = mean(negative))

avg.day <- cville.data %>%
  mutate(hour = floor_date(anytime(created_utc), unit = "12 hour")) %>%
  group_by(hour) %>% 
  summarise(avgsent = mean(abs(sentiment)), avgpos = mean(positive), avgneg = mean(negative), varsent = var(sentiment))

# Join them
rates.cville <- rates.cville %>% left_join(avg.cville.data)

plot.hourly.pos.neg <- rates.cville %>% gather("type", "n", 4:5)

# Plot number of tweets per hour and sentiment #
 # count and sentiment
ggplot(avg.cville.data, aes(x=hour, y = avgsent)) + geom_point(size = .001)
ggplot(rates.cville, aes(x = hour, y = count)) + geom_point(size = .001) 
ggplot(rates.cville, aes(x = hour, y = log(log(count)))) + geom_point(size = .001) 


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


######### Exponential Modeling #######
ggplot(rates.cville, aes(x = hour, y = count)) + geom_point(size = .001)

rates <- (rates.cville[rates.cville$hour >= "2017-08-12 12:00:00",])

exponential.cville <- lm(log(count) ~ hour, data =modified.rates.cville)
summary(exponential.cville)

cville.mod <- lm(count ~ hour, data = modified.rates.cville)
summary(cville.mod)

## Residuals vs. predicted plot
ggplot(exponential.cville, aes(x=.fitted, y=.resid)) + geom_point() + 
  geom_hline(yintercept=0, linetype="dashed")

## Residuals vs. explanatory variables plot
ggplot(exponential.cville, aes(x= hour, y=.resid)) + geom_point() +
  geom_hline(yintercept=0, linetype="dashed")


## Q-Q plot
X <- data.frame(resid = residuals(exponential.cville))

y <- quantile(X$resid, c(0.25, 0.75)) 
x <- qnorm( c(0.25, 0.75))  
slope <- diff(y)/diff(x) 
int <- y[1] - slope*x[1]   

ggplot(X, aes(sample = resid)) + stat_qq() + 
  geom_abline(intercept=int, slope=slope) 

########## Lets try another approach bc that clearly doesnt work.



ts.cville <- ts(rates.cville[,2], start = as.numeric(rates.cville[,1][1]), end = as.numeric(tail(rates.cville$hour, n=1)), frequency =1)

tail(rates.cville$hour, n=1)

fit <- ets(ts.cville)

library(forecast)
summary(fit)
fit$alpha
plot(forecast(fit, 3))

accuracy(fit)


##### Again #####

View(rates)
rates$hour <- as_datetime(rates$hour)
count_ts = ts(rates[, c('count')])





###### More Trash #####

'
ggplot() +
  geom_line(data = rates, aes(x = hour, y = count)) + ylab("Count")

rates$cnt_ma = ma(rates$count, order=12) # using the clean count with no outliers
rates$cnt_ma24 = ma(rates$count, order=24)

ggplot() +
  geom_line(data = rates, aes(x = hour, y = count, colour = "Counts")) +
  geom_line(data = rates, aes(x = hour, y = cnt_ma,   colour = "Half Day"))  +
  geom_line(data = rates, aes(x = hour, y = cnt_ma24, colour = "Full Day"))  +
  ylab("Comment Count")


count_ma = ts(na.omit(rates$cnt_ma24), frequency=24)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

adf.test(count_ma, alternative = "stationary")

Acf(count_ma, main='')
Pacf(count_ma, main='')

count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

Acf(count_d1, main="ACF for Differenced Series")
Pacf(count_d1, main="PACF for Differenced Series")

auto.arima(deseasonal_cnt, seasonal=FALSE)

fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main="(1,1,1) Model Residuals")



####### Trash ########


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



