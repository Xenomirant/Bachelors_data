library(tidyverse)
library(ggridges)

rm(list = ls())

data <- read_csv("data/processed_results.csv")

data %>% 
  mutate(id = factor(id),
         group = factor(group),
         SSP = factor(SSP),
         word_type = factor(word_type),
         position = factor(position)) -> data

data %>% glimpse()

data %>% 
  ggplot(aes(log(Reading.time), SSP, fill = SSP))+
  geom_density_ridges()+
  facet_wrap(~age)


data %>% 
  filter(Reading.time > 4000) %>% 
  View()

data %>% 
  mutate(word_length = factor(word_length),
         cluster_length = factor(cluster_length)) -> data

mean(data$Reading.time)

sd(data$Reading.time)

#mean for Vorontsov
mean(data[data$age == 7, ]$Reading.time)
sd(data[data$age == 7, ]$Reading.time)

mean(data[data$age != 7, ]$Reading.time)
sd(data[data$age != 7, ]$Reading.time)

# 
#filter Vorontsov
data <- 
  data %>% filter(age != 7)

data[order(data$Reading.time)[1:10], ] %>% View()

data %>% glimpse()


#filter outliers

outlier_border <- quantile(data$Reading.time)[4] + 
  1.5*(quantile(data$Reading.time)[4] - quantile(data$Reading.time)[2]) 


#test for residuals and strange data
data[data$Reading.time > outlier_border, ] -> extra_data

extra_data %>% 
  group_by(SSP) %>% 
  summarise(n = n()/length(row.names(extra_data)))

data %>% 
  group_by(SSP) %>% 
  summarise(n = n()/length(row.names(data)))

extra_data %>% 
  group_by(position) %>% 
  summarise(n = n()/length(row.names(extra_data)))

data %>% 
  group_by(position) %>% 
  summarise(n = n()/length(row.names(data)))

extra_data %>% 
  ggplot(aes(word_length, Reading.time, color = SSP, shape = position))+
  geom_jitter(size = 2.5)


data %>% 
  filter(Reading.time < outlier_border) -> data

data %>% 
  ggplot(aes(SSP, log(Reading.time), color = position))+
  geom_boxplot()+
  facet_wrap(~age)

#check dataframe with original data
exp_data <- read_csv("experiment_words - updated_experiment.csv") %>% 
  bind_rows(
    read_csv("experiment_pseudowords - updated_experiment.csv") %>% 
    mutate(word_frequency = as.numeric(word_frequency)))

exp_data %>% group_by(SSP_adh) %>% 
  summarise(n = n()/152)

exp_data %>% group_by(position) %>% 
  summarise(n = n()/152)

round(
  prop.table(
    table(
      exp_data$position
      )
    ),
  3)

round(
  prop.table(
    table(
      exp_data$SSP_adh
    )
  ),
  3)


## everything further is just trash with no real value

# analyze processed data
data %>% 
  filter(word_type == "word") %>% 
  ggplot(aes(Reading.time, fill = position)) +
  geom_density(alpha = 0.6)+
  facet_wrap(~SSP)

data %>% 
  filter(word_type == "pseudoword") %>% 
  ggplot(aes(Reading.time, fill = position)) +
  geom_density(alpha = 0.6)+
  facet_wrap(~SSP)

data %>% 
  filter(word_type == "word") %>% 
  ggplot(aes(Reading.time, fill = SSP)) +
  geom_density(alpha = 0.6)+
  facet_wrap(~position)

data %>% 
  filter(word_type == "pseudoword") %>% 
  ggplot(aes(Reading.time, fill = SSP)) +
  geom_density(alpha = 0.6)+
  facet_wrap(~position)

#needed for text
data %>% 
  ggplot(aes(Reading.time,  fill = SSP))+
  geom_density(alpha = 0.5)+
  facet_wrap(~word_type)


data %>% 
  ggplot(aes(log(Reading.time), fill = position)) +
  geom_density(alpha = 0.6)+
  geom_density(aes(x = rlnorm(n = 1159, meanlog = 1.85615403, 
                              sdlog = 0.08195626)), alpha = 0.3, fill = "red")+
  facet_wrap(~SSP)

rnorm(10000, mean = 706.27, sd = 391.81)

t.test(
  data[data$SSP == "SSP",]$Reading.time,
  data[data$SSP == "nSSP",]$Reading.time
  )

sd(data$Reading.time)

mean(data$Reading.time)

fitdistrplus::fitdist(log(data$Reading.time), distr = "lnorm", method = "mle")


data %>% 
  ggplot(aes(Reading.time, fill = position))+
  geom_density(alpha = 0.5)+
  facet_wrap(~word_type)


t.test(
  data[(data$word_type == "word") & (data$position == "end") & 
         (data$SSP == "nSSP"),]$Reading.time,
  data[(data$word_type == "word") & (data$position == "start") & 
         (data$SSP == "nSSP"),]$Reading.time
)

t.test(
  data[(data$word_type == "pseudoword") & (data$position == "end") & 
         (data$SSP == "nSSP"),]$Reading.time,
  data[(data$word_type == "pseudoword") & (data$position == "start") & 
         (data$SSP == "nSSP"),]$Reading.time
)

t.test(
  data[(data$word_type == "pseudoword") & (data$position == "end") & 
         (data$SSP == "SSP"),]$Reading.time,
  data[(data$word_type == "pseudoword") & (data$position == "end") & 
         (data$SSP == "nSSP"),]$Reading.time
)

t.test(
  data[(data$word_type == "pseudoword") & (data$position == "start") & 
         (data$SSP == "SSP"),]$Reading.time,
  data[(data$word_type == "pseudoword") & (data$position == "start") & 
         (data$SSP == "nSSP"),]$Reading.time
)

t.test(
  data[(data$word_type == "word") & (data$position == "start") & 
         (data$SSP == "SSP"),]$Reading.time,
  data[(data$word_type == "word") & (data$position == "start") & 
         (data$SSP == "nSSP"),]$Reading.time
)

t.test(
  data[(data$word_type == "word") & (data$position == "end") & 
         (data$SSP == "SSP"),]$Reading.time,
  data[(data$word_type == "word") & (data$position == "end") & 
         (data$SSP == "nSSP"),]$Reading.time
)

t.test(
  data[(data$word_type == "word") & (data$position == "start") & 
         (data$SSP == "nSSP"),]$Reading.time,
  data[(data$word_type == "word") & (data$position == "end") & 
         (data$SSP == "nSSP"),]$Reading.time
)

t.test(
  data[(data$word_type == "pseudoword") & (data$position == "start") & 
         (data$SSP == "nSSP"),]$Reading.time,
  data[(data$word_type == "pseudoword") & (data$position == "end") & 
         (data$SSP == "nSSP"),]$Reading.time
)

t.test(
  data[(data$word_type == "pseudoword"),]$Reading.time,
  data[(data$word_type == "word"),]$Reading.time
)


t.test(
  data[data$word_type == "pseudoword" & data$SSP == "filler", ]$Reading.time,
  data[data$word_type == "pseudoword" & data$SSP != "filler", ]$Reading.time
)


t.test(
  data[data$word_type == "word" & data$SSP == "filler", ]$Reading.time,
  data[data$word_type == "word" & data$SSP != "filler", ]$Reading.time
)




# test for secondary factors

data %>% 
  filter(!is.na(word_freq)) %>% 
  ggplot(aes(word_freq, Reading.time))+
  geom_point()

data %>% 
  filter(!is.na(cluster_freq)) %>% 
  ggplot(aes(cluster_freq, Reading.time))+
  geom_point()


data %>% 
  filter(!is.na(cluster_length)) %>% 
  ggplot(aes(cluster_length, Reading.time))+
  geom_boxplot()

data %>% 
  filter(!is.na(cluster_length)) %>% 
  ggplot(aes(word_length, Reading.time))+
  geom_boxplot()

data %>% 
  filter(!is.na(cluster_length)) %>% 
  group_by(cluster_length) %>% 
  summarise(mean = mean(Reading.time), sd = sd(Reading.time))


data %>% 
  group_by(age, position) %>% 
  mutate(mean = mean(Reading.time)) %>% 
  ggplot(aes(age, mean, color = position))+
  geom_line()



words_data <- data %>% 
  filter(word_type == "word")

pseudowords_data <- data %>% 
  filter(word_type == "pseudoword")

#try Bayesian approach

#for words
words_data %>% 
  ggplot(aes(Reading.time))+
  geom_histogram(bins = nclass.FD(words_data$Reading.time))+
  theme_light()
  
  
norm_est <- fitdistrplus::fitdist(words_data$Reading.time, distr = "norm",
                                  method = "mle")

sd_prior <- norm_est$estimate["sd"]
sd_words <- sd(words_data[words_data$SSP == "nSSP", ]$Reading.time)
sd_post <- 1/sqrt(1/sd_prior^2 + 1/sd_words^2)
mean_prior <- norm_est$estimate["mean"]
mean_words <- mean(words_data[words_data$SSP == "nSSP", ]$Reading.time)
mean_post <- weighted.mean(c(mean_prior, mean_words), c(1/sd_prior^2, 1/sd_words^2))


words_data %>% 
  ggplot(aes(Reading.time)) +
  geom_histogram(aes(y = after_stat(density)))+
  stat_function(fun = dnorm, args = list(mean_prior,  sd_prior), color = "lightblue")+
  stat_function(fun = dnorm, args = list(mean_post,  sd_post), color = "red")



#for pseudowords
pseudowords_data %>% 
  ggplot(aes(Reading.time))+
  geom_histogram(bins = nclass.FD(pseudowords_data$Reading.time),
                 color = "lightblue", fill = "white")+
  theme_light()
  
  
  

norm_est <- fitdistrplus::fitdist(pseudowords_data$Reading.time, distr = "norm",
                                  method = "mle")

sd_prior <- norm_est$estimate["sd"]
sd_words <- sd(pseudowords_data[pseudowords_data$SSP == "nSSP", ]$Reading.time)
sd_post <- 1/sqrt(1/sd_prior^2 + 1/sd_words^2)
mean_prior <- norm_est$estimate["mean"]
mean_words <- mean(pseudowords_data[pseudowords_data$SSP == "nSSP", ]$Reading.time)
mean_post <- weighted.mean(c(mean_prior, mean_words), c(1/sd_prior^2, 1/sd_words^2))


pseudowords_data %>% 
  ggplot(aes(Reading.time)) +
  geom_histogram(aes(y = after_stat(density)))+
  stat_function(fun = dnorm, args = list(mean_prior,  sd_prior), color = "lightblue")+
  stat_function(fun = dnorm, args = list(mean_post,  sd_post), color = "red")
  
  

#regression tests

library(brms)

fitdistrplus::fitdist(words_data$Reading.time, distr = "gamma",
                                  method = "mle")

ncores <- parallel::detectCores()-1

data %>% 
  filter(SSP != "filler" | position != "filler") %>% 
  mutate(SSP_bool = ifelse(SSP == "SSP", TRUE, FALSE),
         start_bool = ifelse(position == "start", TRUE, FALSE)) -> data


words_data %>% 
  filter(SSP != "filler" | position != "filler") %>% 
  mutate(SSP_bool = ifelse(SSP == "SSP", TRUE, FALSE),
         start_bool = ifelse(position == "start", TRUE, FALSE)) -> words_data


pseudowords_data %>% 
  filter(SSP != "filler" | position != "filler") %>% 
  mutate(SSP_bool = ifelse(SSP == "SSP", TRUE, FALSE),
         start_bool = ifelse(position == "start", TRUE, FALSE)) -> pseudowords_data



words_data %>% 
  ggplot(aes(SSP_bool, Reading.time))+
  geom_boxplot(aes(fill = SSP_bool), show.legend = FALSE, outlier.alpha = 0)+ 
  geom_jitter(size = 3, width = 0.2)


model <- brm(Reading.time ~ SSP_bool,
    data = words_data,
    chains = 4,
    iter = 2000, 
    cores = parallel::detectCores()-1,
    refresh = 0
)


library(tidybayes)

words_data %>% 
  add_epred_draws(model, ndraws = 50) %>% 
  mutate(SSP_bool = as.numeric(SSP_bool),
         start_bool = as.numeric(start_bool)) %>% 
  ggplot(aes(Reading.time, SSP_bool))+
  stat_lineribbon(aes(y = .epred))


model <- brm(Reading.time ~ SSP_bool + start_bool + age + (1 + word_length|cluster_length) +
               (1|word_freq),
             data = words_data,
             chains = 4,
             iter = 2000, 
             cores = parallel::detectCores()-1,
             refresh = 0
)


#try freq regression models

words_data %>% 
  lm(Reading.time ~ 1, .) -> null.model


words_data %>% 
  lm(Reading.time ~ age + word_freq + cluster_freq + cluster_freq:word_freq, .) %>% 
  summary()

words_data %>% 
  lm(Reading.time ~ age + SSP + position, .) %>% 
  summary()

# words_data %>% 
#   lme4::lmer(Reading.time ~ SSP + position + word_freq:cluster_freq + cluster_length + 
#        (1|id), .) -> model



words_data %>% 
  mutate(prediction = predict(model)) %>% 
  ggplot(aes(age, prediction, color = SSP, shape = position))+
  geom_point() + 
  geom_smooth(aes(age, Reading.time), method = "lm")






pseudowords_data %>% 
  lm(Reading.time ~ SSP + position + cluster_freq + cluster_length + 
       cluster_length:word_length, .) -> model


pseudowords_data %>% 
  mutate(prediction = predict(model)) %>% 
  ggplot(aes(SSP_bool, prediction, color = SSP, shape = position))+
  geom_point() + 
  geom_smooth(aes(SSP_bool, Reading.time), method = "lm")


library(rstatix)
library(gt)

#table for final text

data %>% group_by(word_type, position, SSP) %>% 
  summarise(mean = mean(Reading.time),
            sd = sd(Reading.time)) %>% 
  pivot_wider(names_from = word_type, values_from = mean:sd) %>% 
  mutate(position = factor(position, levels = c("start", "end", "filler"))) %>% 
  arrange(position) %>% gt() %>% 
  tab_header(
    title = "Распределение значений скорости чтения для целевых слов",
    subtitle = "значения указаны в мс"
  )

xtabs(~age, data)

#anova

res.aov_words <- words_data %>% 
  anova_test(Reading.time ~ id + SSP + position)

res.aov_pseudo <- pseudowords_data %>% 
  anova_test(Reading.time ~ id + SSP + position)

words_data %>% 
  ggplot(aes(SSP, Reading.time, color = position))+
  geom_boxplot()

pseudowords_data %>% 
  ggplot(aes(SSP, Reading.time, color = position))+
  geom_boxplot()


data %>% 
  ggplot(aes(SSP, log(Reading.time), color = position))+
  geom_boxplot()+
  facet_wrap(~word_type)


pseudowords_data %>% 
  pairwise_t_test(Reading.time ~ SSP)


pseudowords_data %>% 
  pairwise_t_test(Reading.time ~ position)

pseudowords_data %>% 
  ggplot(aes(position, Reading.time, color = SSP)) +
  geom_boxplot()

  
data %>% group_by(id) %>% freq_table()
    
data %>% 
  anova_test(Reading.time ~ age + SSP + position + word_type)

data %>% 
  lm(log(Reading.time) ~ age + SSP + position + word_type, .) -> model

plot(model, which = 2)
hist(residuals(model))
shapiro.test(residuals(model))

plot(model, which = 1)

plot(model, which = 3)

