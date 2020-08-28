library(tidyverse)
library(lme4) # mixed models package
library(lmerTest) # approximate p-values in mixed models
library(emmeans) # allow us to run follow up tests
library(performance) # check model assumptions
library(arm) # for binned residuals plot
library(ordinal) # to build ordinal models
library(fitdistrplus) # for Cullen and Frey plot

# Let's look at some binomial data
regressions_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/16_mixed_models_pt2/master/data/regressions.csv")

str(regressions_data)

tidied_regressions_data <- regressions_data %>%
  transmute(subject = factor(Subject), item = factor(Item), 
            condition = factor(Condition), DV = DV)

str(tidied_regressions_data)

head(tidied_regressions_data)

tidied_regressions_data %>%
  group_by(condition) %>%
  summarise(mean_DV = mean(DV), sd_DV = sd(DV))

binomial_model <- glmer(DV ~ condition + (1 + condition | subject) +
                          (1 + condition | item), data = tidied_regressions_data,
                        family = binomial)
# Need to simplify model

binomial_model <- glmer(DV ~ condition + (1 | subject), 
                        data = tidied_regressions_data,
                        family = binomial)

summary(binomial_model)

binomial_model_null <- glmer(DV ~ (1 | subject), 
                             data = tidied_regressions_data,
                             family = binomial)

anova(binomial_model, binomial_model_null)

tidied_regressions_data %>%
  group_by(condition, DV) %>%
  summarise(n())

# We would expect 95# of residuals to fall between jagged line (+- 2SEs)
binnedplot(fitted(binomial_model), resid(binomial_model, type="response"))

# Let's look at some ordinal data

ordinal_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/16_mixed_models_pt2/master/data/ordinal_data.csv")

ordinal_data_tidied <- ordinal_data %>%
  mutate(Subject = factor(Subject), SportsType = factor(SportType)) %>%
  mutate(Ratings = ratings) %>%
  mutate(VideoCondition = as.character(VideoCondition)) %>%
  mutate(VideoCondition = factor(recode(VideoCondition, "2" = "Match", 
                                        "3" = "Mismatch", "4" = "Neutral"))) %>%
  dplyr::select(Subject, SportType, VideoCondition, Ratings)

ordinal_data_tidied$Ratings <- as.ordered(ordinal_data_tidied$Ratings)

ordinal_data_tidied %>%
  ggplot(aes(x = VideoCondition, y = Ratings, group = VideoCondition)) +
  geom_jitter(aes(colour = VideoCondition), width = .1, alpha = .25, size = 5) + 
  theme_minimal() +
  guides(colour = FALSE) +
  theme(text = element_text(size = 20)) +
  stat_summary(fun = "median", size = 2, alpha = .5)

ggsave("plot.png", width= 16, height = 8)

ordinal_model <- clmm(Ratings ~ VideoCondition + 
                        (1 + VideoCondition | Subject) +
                        (1 + VideoCondition | SportType), 
                      data = ordinal_data_tidied)   

ordinal_model_null <- clmm(Ratings ~ 1 + 
                             (1 + VideoCondition | Subject) +
                             (1 + VideoCondition | SportType), 
                           data = ordinal_data_tidied)   

summary(ordinal_model)

anova(ordinal_model, ordinal_model_null)

emmeans(ordinal_model, pairwise ~ VideoCondition)

# Plotting data on a Cullen and Frey graph
# Looking at our 2 x 2 factorial design data from the previous workshop

factorial_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/15_mixed_models_pt1/master/data/2x2.csv")

tidied_factorial_data <- factorial_data %>%
  transmute(subject = factor(Subject), item = factor(Item), RT = RT,
            context = factor(Context), sentence = factor(Sentence)) %>%
  filter(!is.na(RT))

hist(tidied_factorial_data$RT)
descdist(tidied_factorial_data$RT)


