# Bachelor Thesis: News Sentiment and Inflation Expectation
#
# Autor: Luis Nägelin, 19-613-926, Gallusstrasse 41, 9000 St.Gallen, luis.naegelin@student.unisg.ch
#
# Supervisor: Prof. Dr. Johannes Binswanger
# Co-Supervisor: Franziska  Bender
#
################################################################################
#
# Disclaimer and declaration of autorship:
# 
# The following code has been written by me (Luis Nägelin) without the direct help of any other person.
# I have used tools like Stack-overflow and ChatGPT to write the code.

#install.packages('magrittr')
#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('zoo')
#install.packages('stargazer')
#install.packages('ppcor')
#install.packages('corrplot')
#install.packages('summarytools')
#install.packages('xtable')

library(magrittr)
library(dplyr)
library(ggplot2)
library(zoo)
library(stargazer)
library(ppcor)
library(corrplot)
library(summarytools)
library(xtable)

## functions:
get_sentiment_index <- function(x, k) {
  n <- length(x)
  y <- rep(NA, n)
  w <- 2^(-seq(k-1, 0, by = -1)) # calculate weights
  w <- w / sum(w) # normalize weights
  for (i in k:n) {
    y[i] <- sum(x[(i-k+1):i] * w)
  }
  return(y)
}
# formate the dates:
format_data <- function(data){
  
  start_year <- data$Date[1] %>% as.integer()
  
  end_year <- data$Date[nrow(data)] %>% as.integer()
  years <- rep(start_year:end_year, each=12)
  months <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
  
  months <- rep(months, ((end_year-start_year) + 1))
  
  
  data <- data %>% 
    mutate(years, .before = 1) %>% mutate(months, .before = 2)
  
  data$Date <- as.Date(paste(data$years, match(data$months, month.name), "01", sep="-"), "%Y-%m-%d")
  
  return(data)
}

path_to_results <- '####################' ### Add own paths!
path_out <- '###########################'

# load sentiment data:
sentiment_data <- read.csv(paste0(path_to_results, '/sentiment_results.csv'))
sentiment_data <- format_data(sentiment_data)
# drop the 1980 rows because something is wrong (no articles about inflation ??)
sentiment_data <- sentiment_data[sentiment_data$Date >= '1981-01-01', ]

# load michigan data:
michigan_data <- read.csv(paste0(path_to_results, '/michigan_relevant.csv'))
michigan_data <- format_data(michigan_data)
# drop rows so that it matches the sentiment data:
michigan_data <- michigan_data[michigan_data$Date >='1981-01-01', ]

#load michigan inflation expectation
inf_exp <- read.csv(paste0(path_to_results, '/mich_inflationexp.csv'))
colnames(inf_exp) <- c('Date', 'inf_exp')
inf_exp$Date <- as.Date(inf_exp$Date, format =  "%Y-%m-%d")
inf_exp$diff <- c(diff(inf_exp$inf_exp), 0)
inf_exp <- inf_exp[inf_exp$Date >= '1981-01-01', ]
inf_exp <- inf_exp[inf_exp$Date <= '2022-12-01', ]

# load cpi
cpi<- read.csv(paste0(path_to_results, '/cpi.csv'))

cpi$CPIAUCSL[13:nrow(cpi)] <- diff(cpi$CPIAUCSL, 12) / cpi$CPIAUCSL[13:nrow(cpi)] *100
cpi$CPIAUCSL[1:12] <- 0

colnames(cpi) <- c("Date", "Inflation")
cpi$Date <- as.Date(cpi$Date, format =  "%Y-%m-%d")

cpi$lag1 <- lag(cpi$Inflation, 1)
cpi$lag12 <- lag(cpi$Inflation, 13)

cpi$diff <- c(0, diff(cpi$Inflation))
cpi<- cpi[cpi$Date >= '1981-01-01', ]
cpi <- cpi[cpi$Date <= '2022-12-01', ]


# Recession Data: Data to indicate the recessions:

recession_data <- data.frame(
  start_date = as.Date(c("1981-07-01", "1990-07-01", "2001-01-01", "2007-12-01", "2020-02-01")),
  end_date = as.Date(c("1982-11-01","1991-03-01", "2001-11-01", "2009-06-01", "2020-04-01")),
  recession = c("1981-1982 recessio", "early 1990s recessio", "Dot-com recession", "Great financial recession", "Coron Shock")
)

# Inflation Trend: mean inflation per decade 
recession_data <- data.frame(
  start_date = as.Date(c("1981-01-01", "1990-01-01", "2000-01-01", "2007-12-01", "2020-02-01")),
  end_date = as.Date(c("1989-12-01","1999-12-01", "2000-11-01", "2009-06-01", "2020-04-01")),
  recession = c("1981-1982 recessio", "early 1990s recessio", "Dot-com recession", "Great financial recession", "Coron Shock")
)

# load frequency overview:

frequ_overview <- read.csv(paste0(path_to_results, '/frequ_article_type.csv'))
frequ_overview <- format_data(frequ_overview)
frequ_overview <- frequ_overview[frequ_overview$Date >= '1981-01-01', ]

# load frequency of type_material:
# (not used i this script)
# shows the total number of publications in the data,set with the given type.
# We see that the chosen 5 types represent almost all of the relevant data.
type_material <- read.csv(paste0(path_to_results, '/type_material.csv'))
type_material <- type_material[order(type_material$value, decreasing = TRUE), ]

#############################################################################################
# Summary Statistic NYT Data

data = data.frame(total_entrys = frequ_overview$frequ_total_entrys, us = frequ_overview$frequ_US_articles,
                  relevant_articles = frequ_overview$frequ_relevant_articles,
                  freq_inflation = frequ_overview$frequ_about_inflation, positive = sentiment_data$positive_articles,
                  negative = sentiment_data$negative_articles)
summary_table_nyt <- descr(data, normal = TRUE)
summary_table_nyt <- summary_table_nyt[, c(5,6,4,1,3,2)]
colnames(summary_table_nyt) <- c('Total Entrys', 'Keyword: United States', 'Relevant Articles*', 'Articles about Inlflation', 'Positive Articles about Inflation', 'Negative Articles about Inflation')
summary_table_nyt<- round(summary_table_nyt, 2)
summary_table_nyt <- summary_table_nyt[c(1:7, 14), ]

summary_table_nyt <- xtable(t(summary_table_nyt))
print(summary_table_nyt, include.rownames = TRUE, floating = FALSE)


#   Number of Entries in Data Set

ggplot(data = frequ_overview, aes(x = Date)) +
  geom_line(aes(y = frequ_total_entrys, color = "Total Entries"), lwd = 1.2) +
  geom_line(aes(y = frequ_US_articles, color = "Keyword: United States"), lwd = 1.2) +
  xlab("") +
  scale_x_date(breaks = seq(as.Date("1981-01-01"), as.Date("2023-01-01"), by = "2 years"),
               date_labels = "%Y")+
  ylab("Number of Entries") +
  theme_classic()+
  scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2023-01-01"), by = "5 years"),
               date_labels = "%Y")+
  theme(legend.text = element_text(size = 15 ),
        axis.title =  element_text( size = 15 ), legend.position = 'top')+
  scale_color_manual(name = "",
                     values = c( "Total Entries" = "black", "Keyword: United States" = "darkred" ))

#   Frequency of Relevant Articles

ggplot(data = frequ_overview, aes(x = Date)) +
  geom_line(aes(y = frequ_relevant_articles, color = "Relevant Articles (100%)"), lwd = 1.2) +
  geom_line(aes(y = frequ_news, color = "News"), lwd = 1.2) +
  geom_line(aes(y = frequ_letter, color = "Letter"), lwd = 1.2) +
  geom_line(aes(y = frequ_oped, color = "Op-Ed"), lwd = 1.2) +
  geom_line(aes(y = frequ_editorial, color = "Editorial"), lwd = 1.2) +
  geom_line(aes(y = frequ_brief, color = "Brief"), lwd = 1.2) +
  xlab("") +
  scale_x_date(breaks = seq(as.Date("1981-01-01"), as.Date("2023-01-01"), by = "2 years"),
               date_labels = "%Y")+
  ylab("Number of Entries") +
  theme_classic()+
  scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2023-01-01"), by = "5 years"),
               date_labels = "%Y")+
  theme(legend.text = element_text(size = 15),
        axis.title =  element_text( size = 15), legend.position = 'top')+
  scale_color_manual(name = "",
                     values = c("Relevant Articles (100%)" = "black", "News" = "blue", "Letter" = "orange", "Op-Ed" = "darkred","Editorial" = "navy", "Brief" = "gray"))


#   Frequency of Articles about Inflation

# comparison with mean inflation per decade.
cpi$decade <- year(cpi$Date) %/% 10
cpi_by_decade <- aggregate(Inflation ~ decade, data = cpi, FUN = mean) # get the mean inflation per decade.
colnames(cpi_by_decade)[2] <- 'mean_inflation_decade'
cpi <- merge(cpi, cpi_by_decade, by = "decade")


ggplot(data = sentiment_data, aes(x = Date)) +
  geom_bar(data = cpi, aes( y = mean_inflation_decade*10),stat = "identity", fill = "orange", alpha = 0.2, color = "transparent")+
  geom_line( aes( y = inflation_articles_per_month), lwd = 1.2)+
  theme_classic()+
  scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2023-01-01"), by = "5 years"),
               date_labels = "%Y")+
  theme(legend.text = element_text(size = 15),
        axis.title =  element_text( size = 15), legend.position = 'top')+
  ylab("Number of Articles") +
  xlab("") 

# Mean number of Articles:
mean_num_articles_1980s <- mean(sentiment_data$inflation_articles_per_month[and(sentiment_data$Date >= '1981-01-01', sentiment_data$Date < '1990-01-01') ])
mean_num_articles_1990s <- mean(sentiment_data$inflation_articles_per_month[and(sentiment_data$Date >= '1990-01-01', sentiment_data$Date < '2000-01-01') ])
mean_num_articles_2000s <- mean(sentiment_data$inflation_articles_per_month[and(sentiment_data$Date >= '2000-01-01', sentiment_data$Date < '2010-01-01') ])
mean_num_articles_2010s <- mean(sentiment_data$inflation_articles_per_month[and(sentiment_data$Date >= '2010-01-01', sentiment_data$Date < '2020-01-01') ])
mean_num_articles_2020s <- mean(sentiment_data$inflation_articles_per_month[and(sentiment_data$Date >= '2020-01-01', sentiment_data$Date < '2030-01-01') ])

# Correlation Frequency and Inflation Rate and Inflation Expectation
cat('Correlation Frequency and Inflation: ' ,cor(sentiment_data$inflation_articles_per_month, cpi$Inflation))

cat('Correlation Frequency and Inflation Expectation: ' ,cor(sentiment_data$inflation_articles_per_month, inf_exp$inf_exp))

#   Sentiment of Articles about Inflation

ggplot(sentiment_data, aes(x = Date)) +
  geom_line(aes(y = positive_articles, color = "Positive"), lwd = 1.2) +
  geom_line(aes(y = negative_articles, color = "Negative"), lwd = 1.2) +
  xlab("") +
  ylab("Number of Articles") +
  theme_classic()+
  geom_rect(data = recession_data, 
            aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf), 
            fill = "blue", alpha = 0.2, inherit.aes = FALSE)+
  scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2023-01-01"), by = "5 years"),
               date_labels = "%Y")+
  theme(legend.text = element_text(size = 15),
        axis.title =  element_text( size = 15), legend.position = 'top')+
  scale_color_manual(name = "",
                     values = c("Positive" = "navy", "Negative" = "darkred"))

# Sentiment Gap

ggplot(data = sentiment_data, aes(x = Date) )+
  geom_line( aes(y = inflation_articles_per_month, color = 'Frequency of Articleas about Inflation'), lwd =1.2)+
  geom_line( aes( y = sentiment_gap ,color = 'Sentiment-Gap'), lwd = 1.2)+
  xlab("") +
  ylab("Negative - Positive / Number of Articles") +
  theme_classic()+
  geom_rect(data = recession_data, 
            aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf), 
            fill = "blue", alpha = 0.2, inherit.aes = FALSE)+
  scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2023-01-01"), by = "5 years"),
               date_labels = "%Y")+
  theme(legend.text = element_text(size = 15),
        axis.title =  element_text( size = 15), legend.position = 'top')+
  scale_color_manual(name = "",
                     values = c("Sentiment-Gap" = "black", "Frequency of Articleas about Inflation" = "grey"))

#   Mean Sentiment Gap

mean_1980s <- mean(sentiment_data$sentiment_gap[and(sentiment_data$Date >= '1981-01-01', sentiment_data$Date < '1990-01-01') ])
mean_1990s <- mean(sentiment_data$sentiment_gap[and(sentiment_data$Date >= '1990-01-01', sentiment_data$Date < '2000-01-01') ])
mean_2000s <- mean(sentiment_data$sentiment_gap[and(sentiment_data$Date >= '2000-01-01', sentiment_data$Date < '2010-01-01') ])
mean_2010s <- mean(sentiment_data$sentiment_gap[and(sentiment_data$Date >= '2010-01-01', sentiment_data$Date < '2020-01-01') ])
mean_2020s <- mean(sentiment_data$sentiment_gap[and(sentiment_data$Date >= '2020-01-01', sentiment_data$Date < '2030-01-01') ])

# Correlation Sentiment-Gap and Inflation Rate and Inflation Expectation
cat('Correlation Sentiment-Gap and Inflation: ' ,cor(sentiment_data$sentiment_gap, cpi$Inflation))

cat('Correlation Sentiment-Gap and Inflation Expectation: ' ,cor(sentiment_data$sentiment_gap, inf_exp$inf_exp))

# Heard of Inflation in News

ggplot(data = michigan_data, aes(x = Date)) +
  geom_line(aes(y = news_inflation_ratio, color = "Total"), lwd = 1.2) +
  geom_line(aes(y = unfav_inflation_ratio, color = "Unfavorable"), lwd = 1.2) +
  geom_line(aes(y = fav_inflation_ratio, color = "Favorable"), lwd = 1.2) +
  ylab('Share of Answers') +
  xlab("") +
  theme_classic()+
  geom_rect(data = recession_data, 
            aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf), 
            fill = "blue", alpha = 0.2, inherit.aes = FALSE)+
  scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2023-01-01"), by = "5 years"),
               date_labels = "%Y")+
  theme(legend.text = element_text(size = 15),
        axis.title =  element_text( size = 15), legend.position = 'top')+
  scale_color_manual(name = "",
                     values = c("Total"= "black", "Favorable" = "navyblue", "Unfavorable" = "darkred"))

# Compare Michigan Survey and Frequency of Articles about inflation

data <- data.frame( Date = sentiment_data$Date, heard_inf = scale(michigan_data$news_inflation_ratio), frequ =scale(sentiment_data$inflation_articles_per_month))

ggplot(data = data, aes(x = Date)) +
  geom_line(aes(y = heard_inf, color = "Michigan Survey: Hear Inflation in News"), lwd = 1.2) +
  geom_line(aes(y = frequ, color = "NYT Data: Frequency of Articles about Inflation"), lwd = 1.2) +
  geom_rect(data = recession_data, 
            aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf), 
            fill = "blue", alpha = 0.2, inherit.aes = FALSE)+
  ylab('Scaled Values (Mean = 0, Var = 1)') +
  xlab("") +
  theme_classic()+
  scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2023-01-01"), by = "5 years"),
               date_labels = "%Y")+
  theme(legend.text = element_text(size = 15 ),
        axis.title =  element_text( size = 15 ), legend.position = 'top')+
  scale_color_manual(name = "",
                     values = c("Michigan Survey: Hear Inflation in News" = "black", "NYT Data: Frequency of Articles about Inflation" = "darkred")) 


cat('Correlation Michigan Data and Frequency: ' ,cor(data$heard_inf, data$frequ))

# Compare Sentiment Gap, Frequency with Inflation and Inflation Expectation:

data <- data.frame( Date = sentiment_data$Date, cpi = scale(cpi$Inflation), exp = scale(inf_exp$inf_exp), gap = scale(sentiment_data$sentiment_gap), frequ = scale(sentiment_data$inflation_articles_per_month) )

#  Sentiment-Gap and Inflation Expectation

ggplot(data = data, aes(x = Date)) +
  geom_line(aes(y = gap, color = "Sentiment-Gap"), lwd = 1.2) +
  geom_line(aes(y = exp, color = "Inflation Expectation"), lwd = 1.2) +
  geom_rect(data = recession_data, 
            aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf), 
            fill = "blue", alpha = 0.2, inherit.aes = FALSE)+
  ylab('Scaled Values (Mean = 0, Var = 1)') +
  xlab("") +
  theme_classic()+
  scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2023-01-01"), by = "5 years"),
               date_labels = "%Y")+
  theme(legend.text = element_text(size = 15 ),
        axis.title =  element_text( size = 15 ), legend.position = 'top')+
  scale_color_manual(name = "",
                     values = c( "Sentiment-Gap" = "black", "Inflation Expectation" = "darkred" ))

# Sentiment-Gap and Inflation rate:

ggplot(data = data, aes(x = Date)) +
  geom_line(aes(y = gap, color = "Sentiment-Gap"), lwd = 1.2) +
  geom_line(aes(y = cpi, color = "Inflation"), lwd = 1.2) +
  geom_rect(data = recession_data, 
            aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf), 
            fill = "blue", alpha = 0.2, inherit.aes = FALSE)+
  ylab('Scaled Values (Mean = 0, Var = 1)') +
  xlab("") +
  theme_classic()+
  scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2023-01-01"), by = "5 years"),
               date_labels = "%Y")+
  theme(legend.text = element_text(size = 15 ),
        axis.title =  element_text( size = 15 ), legend.position = 'top')+
  scale_color_manual(name = "",
                     values = c( "Sentiment-Gap" = "black", "Inflation" = "darkred" ))

# Frequency and Inflation Expectation

ggplot(data = data, aes(x = Date)) +
  geom_line(aes(y = frequ, color = "Frequency of Articles about Inflation"), lwd = 1.2) +
  geom_line(aes(y = exp, color = "Inflation Expectation"), lwd = 1.2) +
  geom_rect(data = recession_data, 
            aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf), 
            fill = "blue", alpha = 0.2, inherit.aes = FALSE)+
  ylab('Scaled Values (Mean = 0, Var = 1)') +
  xlab("") +
  theme_classic()+
  scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2023-01-01"), by = "5 years"),
               date_labels = "%Y")+
  theme(legend.text = element_text(size = 15 ),
        axis.title =  element_text( size = 15 ), legend.position = 'top')+
  scale_color_manual(name = "",
                     values = c( "Frequency of Articles about Inflation" = "black", "Inflation Expectation" = "darkred" ))

#   Frequency and Inflation rate

ggplot(data = data, aes(x = Date)) +
  geom_line(aes(y = frequ, color = "Frequency of Articles about Inflation"), lwd = 1.2) +
  geom_line(aes(y = cpi, color = "Inflation"), lwd = 1.2) +
  geom_rect(data = recession_data, 
            aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf), 
            fill = "blue", alpha = 0.2, inherit.aes = FALSE)+
  ylab('Scaled Values (Mean = 0, Var = 1)') +
  xlab("") +
  theme_classic()+
  scale_x_date(breaks = seq(as.Date("1980-01-01"), as.Date("2023-01-01"), by = "5 years"),
               date_labels = "%Y")+
  theme(legend.text = element_text(size = 15 ),
        axis.title =  element_text( size = 15 ), legend.position = 'top')+
  scale_color_manual(name = "",
                     values = c( "Frequency of Articles about Inflation" = "black", "Inflation" = "darkred" ))

##    Statistical Analysis:

#     Data:
data_for_reg <- data.frame( year = sentiment_data$years %>% as.factor(),
                            month = sentiment_data$months %>% as.factor(),
                            inf_exp = inf_exp$inf_exp ,
                            inflation_l1 = cpi$lag1,
                            inflation_l12 = cpi$lag12,
                            sent_gap = sentiment_data$sentiment_gap,
                            frequ_articles = sentiment_data$inflation_articles_per_month,
                            attention = michigan_data$news_inflation / michigan_data$count_heard_inf 
)

# Summary Statistics

summary_table <- descr(data_for_reg[, -(1:2)], normal = TRUE)
summary_table <- summary_table[, c(3, 4, 5, 6, 2 , 1)]
colnames(summary_table) <- c('Inflation Expectation', 'Inflation lag 1 Month', 'Inflation lag 12 Month', 'Sentiment Gap','Frequency of Articles', 'Heard of Inflation in News')
summary_table <- summary_table[c(1:7, 14), ]
summary_table <- round(summary_table, 2)

summary_table <- xtable(t(summary_table))
print(summary_table, include.rownames = TRUE, floating = FALSE)

# Correlation Table
parameters <- c('Inflation Expectation', 'Inflation lag 1 Month', 'Inflation lag 12 Month', 'Sentiment Gap','Frequency of Articles', 'Heard of Inflation in News')
cor_plot <- cor(data_for_reg[, -(1:2)])
colnames(cor_plot) <- parameters
rownames(cor_plot) <- parameters

corrplot(cor_plot, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         number.cex = 0.8, tl.cex = 0.8, cl.cex = 0.8)


#     Partial Correlation Analysis

#   Inflation Exp and Sentiment Gap controlled for Inflation pervios month
pcor.test(data_for_reg$inf_exp, data_for_reg$sent_gap, data_for_reg$inflation_l1)

#   Inflation Exp and Sentiment Gap controlled for Inflation pervios month
pcor.test(data_for_reg$inf_exp, data_for_reg$frequ_articles, data_for_reg$inflation_l1)

#   Inflation Exp and Sentiment Gap controlled for Inflation pervios month
pcor.test(data_for_reg$inf_exp, data_for_reg$attention, data_for_reg$inflation_l1)


#     Regression Analysis
# Table used in paper

fit_1 <- lm(data = data_for_reg,
            inf_exp ~ sent_gap + inflation_l1+ month + year)

fit_2 <- lm(data = data_for_reg,
            inf_exp ~ frequ_articles + inflation_l1  + month + year)

fit_3 <- lm(data = data_for_reg,
            inf_exp ~ sent_gap + frequ_articles + inflation_l1 + month + year)

stargazer(fit_1, fit_2, fit_3,
          omit = c('year', 'month'), omit.labels = c('year FE', 'month FE'), type = 'text')

reg_table <- stargazer(fit_1, fit_2, fit_3,
                       omit = c('year', 'month'), omit.labels = c('year FE', 'month FE'),
                       align=TRUE, type="latex")

# Big Table 


fit_1 <- lm(data = data_for_reg,
            inf_exp ~ sent_gap + inflation_l1)

fit_2 <- lm(data = data_for_reg,
            inf_exp ~ sent_gap + inflation_l1  + month + year)

fit_3 <- lm(data = data_for_reg,
            inf_exp ~ frequ_articles + inflation_l1 )

fit_4 <- lm(data = data_for_reg,
            inf_exp ~ frequ_articles + inflation_l1  + month + year)

fit_5 <- lm(data = data_for_reg,
            inf_exp ~ sent_gap + frequ_articles + inflation_l1  + month + year)

fit_6 <- lm(data = data_for_reg,
            inf_exp ~ sent_gap + sent_gap * attention + inflation_l1  + month + year)

fit_7 <- lm(data = data_for_reg,
            inf_exp ~ frequ_articles + frequ_articles * attention + inflation_l1  + month + year)

stargazer(fit_1, fit_2, fit_3, fit_4, fit_5, fit_6, fit_7,
          omit = c('year', 'month'), omit.labels = c('year FE', 'month FE'), type = 'text')

reg_table <- stargazer(fit_1, fit_2, fit_3, fit_4, fit_5, fit_6, fit_7,
                       omit = c('year', 'month'), omit.labels = c('year FE', 'month FE'),
                       align=TRUE, type="latex")


##############################################################################################