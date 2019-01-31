#' ---
#' title: "Mouse Behavioral Analysis"
#' author: "Jeremy Metha"
#' date: "17/1/19"
#' output: github_document
#' ---

#' These experiments aimed to investigate the effects of modulating sleep architecture on learning in a noisy environment. Animals were trained to push levers, then placed in a situation where one lever gives a reward on 80% of the time, while the other lever rewards only 20% of the time. Once animals were behaving optimally (defined by 2x days with > 60 lever presses and > 80% high rewarding lever choice), the high and low rewarding levers were switched, and animals were dosed daily with either TPGS vehicle, Zolpidem, or 1-SORA 51/MK1064  - OXR1/OXR2 selective anatonists, respectively in a dosage shown to increase REM sleep.


## Read and Wrangle

#' Weather dataset consists of trial by trial data from all animals across all days
#'
#' Day: Integer - day of the experiment including all pre-training
#'
#' Batch: 1-4 - session within a day in which animal was ran
#'
#' Subject: Integer - Identifier for each animal
#'
#' Session: Factor - "WTL" is probabilistic learning, "WTR" is reversal learning
#'
#' Time: Integer - Time of response since begining of trial in deciseconds
#'
#' Response: Binary -  -1 is a response on low rewarding lever, +1 is a response on high rewarding lever, -1 is a response on the low rewarding lever
#'
#' Reward: Binary - 1 is a rewarding trial, 0 is a non-rewarding trial
#'
#' Trial: Integer - Trial number within a session indexed from 1 up to 100
#'
#' ContinuousTrial: Integer - Trial number within and across sessions - represents all choices as a continuous stream
#'
#' SessionContinuous Trial:  Integer - As above, but resets for reversal learning
#'
#' SessionDay: Integer - Day number within a session type
#'
#' WeatherDay: Integer - Day number in PL/RL continuously
#'
#' Drug: Factor - Drug type animal was dosed with in RL
#'
#' CumulativeSessionReward: Integer - Counts up total rewards ona given day
#'
#' CumulativeSessionResponse: Integer - Count of lever presses: indexes up by 1 with every high rewarding press, indexes down by one with every low rewarding press
#'
#' CumulativeTotalReward: Integer - As CumulativeSessionReward, but counts over days


## OP005_WTL = probabilistic learning, OP005_WTR = reversal learning with drug
##subject ID's given different drugs in WTR


library(readxl)
library(tidyverse)
library(anchors)
library(lme4)
library(car)
library(nlme)
rm(list = ls())


ORAs <- c(1, 3, 9, 13, 15, 16, 27, 30, 32, 35, 39, 41, 43)
Zolps <- c(4, 10, 11, 17, 18, 21, 23, 25, 29, 37, 42, 44, 45, 47)
TPGS <- c(2, 5, 6, 19, 20, 22, 24, 26, 28, 33, 38, 46)

weather <- read_xlsx("phase 5 data.xlsx", col_names = TRUE)
weather <- weather %>%
  filter(Subject != 8 & Subject != 40 & Subject != 48 & Subject != 29) %>% ## these animals are exluded for various reasons
  filter(is.na(Session) == FALSE) %>%
  group_by(Subject, Session) %>%
  mutate(SessionDay = Day - min(Day)+1) %>% ## days in a given session - either WTL or WTR
  ungroup() %>%
  group_by(Subject) %>%
  mutate(WeatherDay = Day - min(Day)+1) %>% ## days in the probabilistic environment
  mutate(Drug = if (Subject %in% ORAs){
    "DORAs"
  }
  else if (Subject %in% Zolps){
    "Zolpidem"
  }
  else "TPGS"
  ) %>% ## labels which drug was given to which animal in WTR... Ugly code and will throw warnings, but works!
  mutate(Reward = replace(Reward, Reward == -1, 0)) %>%
  group_by(Subject, WeatherDay) %>%
  mutate(CumulativeSessionReward = cumsum(Reward)) %>%
  mutate(CumulativeSessionResponse = cumsum(Response)) %>%
  group_by(Subject) %>%
  mutate(CumulativeTotalReward = cumsum(Reward))


## also need to add win-stay, lose shift stats
ShiftStay <- weather %>%
  mutate(winstay = ifelse(Response == lag(Response) & lag(Reward == 1), 1, 0)) %>%
  mutate(winshift = ifelse(Response != lag(Response) & lag(Reward == 1), 1, 0)) %>%
  mutate(losestay = ifelse(Response == lag(Response) & lag(Reward == 0), 1, 0)) %>%
  mutate(loseshift = ifelse(Response != lag(Response) & lag(Reward == 0), 1, 0)) %>%
  mutate(winstayhigh = ifelse(winstay == 1 & Response == 1, 1, 0)) %>%
  mutate(winstaylow = ifelse(winstay == 1 & Response == -1, 1, 0)) %>%
  mutate(winshifthigh = ifelse(winshift == 1 & Response == 1, 1, 0)) %>%
  mutate(winshiftlow = ifelse(winshift == 1 & Response == -1, 1, 0)) %>%
  mutate(losestayhigh = ifelse(losestay == 1 & Response == 1, 1, 0)) %>%
  mutate(losestaylow = ifelse(losestay == 1 & Response == -1, 1, 0)) %>%
  mutate(loseshifthigh = ifelse(loseshift == 1 & Response == 1, 1, 0)) %>%
  mutate(loseshiftlow = ifelse(loseshift == 1 & Response == -1, 1, 0)) %>%
  group_by(Subject, Session, SessionDay, Drug, WeatherDay) %>%
  summarise(WinStay = sum(winstay, na.rm = TRUE),
            WinShift = sum(winshift, na.rm = TRUE),
            LoseStay = sum(losestay, na.rm = TRUE),
            LoseShift = sum(loseshift, na.rm = TRUE),
            WinStayHigh = sum(winstayhigh, na.rm = TRUE),
            WinShiftHigh = sum(winshifthigh, na.rm = TRUE),
            LoseStayHigh = sum(losestayhigh, na.rm = TRUE),
            LoseShiftHigh = sum(loseshifthigh, na.rm = TRUE),
            WinStayLow = sum(winstaylow, na.rm = TRUE),
            WinShiftLow = sum(winshiftlow, na.rm = TRUE),
            LoseStayLow = sum(losestaylow, na.rm = TRUE),
            LoseShiftLow = sum(loseshiftlow, na.rm = TRUE),
            Trials = max(Trial),
            High = sum(Response == 1),
            Low = sum(Response == -1))



#' Looking at the length of time it takes between groups to complete the weather tasks


diff <- weather %>%
  group_by(SessionType, Subject, Day) %>%
  filter(Trial == max(Trial)) %>%
  group_by(Session, Subject) %>%
  filter(SessionDay == max(SessionDay))

diffsummary <- weather %>%
  group_by(SessionType, Subject, Day) %>%
  filter(Trial == max(Trial)) %>%
  group_by(Session, Subject) %>%
  filter(SessionDay == max(SessionDay)) %>%
  group_by(SessionType, Drug) %>%
  summarise(MeanDay = mean(SessionDay), Error = sd(SessionDay)/length(SessionDay), n = length(SessionDay))

maxday.TPGS <- diff %>%
  filter(Drug == "TPGS") %>%
  filter(Session == "WTR")

maxday.DORAs <- diff %>%
  filter(Drug == "DORAs") %>%
  filter(Session == "WTR")

maxday.Zolpidem <- diff %>%
  filter(Drug == "Zolpidem") %>%
  filter(Session == "WTR")

hist.TPGS <- ggplot(maxday.TPGS, aes(SessionDay)) +
  theme(legend.position = "none") +
  geom_histogram(aes(y = ..count..), colour = "black", fill = "white", binwidth = 1) +
  labs(x = "Max Day TPGS", y = "Density")
hist.TPGS

hist.DORAs <- ggplot(maxday.DORAs, aes(SessionDay)) +
  theme(legend.position = "none") +
  geom_histogram(aes(y = ..count..), colour = "black", fill = "white", binwidth = 1) +
  labs(x = "Max Day DORAs", y = "Density")
hist.DORAs

hist.Zolpidem <- ggplot(maxday.Zolpidem, aes(SessionDay)) +
  theme(legend.position = "none") +
  geom_histogram(aes(y = ..count..), colour = "black", fill = "white", binwidth = 1) +
  labs(x = "Max Day Zolpidem", y = "Density")
hist.Zolpidem

diff2 <- diff %>%
  group_by(Subject, Drug) %>%
  spread(Session, SessionDay) %>%
  summarise(WTL = mean(WTL, na.rm = TRUE),
            WTR = mean(WTR, na.rm = TRUE),
            ratio = mean(WTR, na.rm = TRUE)/mean(WTL, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Drug = as.factor(Drug)) %>%
  mutate(Drug = relevel(Drug, ref = "TPGS")) %>%
  mutate(Subject = as.factor(Subject))

diff2plot <- ggplot(diff2, aes(x = WTL, y = WTR, colour = Drug)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
diff2plot

completionmodel1 <- gls(WTR ~ 1, data = diff2, method = 'ML')
summary(completionmodel1)

completionmodel2 <- update(completionmodel1, .~. + Drug)
summary(completionmodel2)

completionmodel3 <- update(completionmodel2, .~. + WTL)
summary(completionmodel3)

completionmodel4 <- update(completionmodel3, .~. + Drug:WTL)
summary(completionmodel4)

anova(completionmodel1, completionmodel2, completionmodel3, completionmodel4)

#' Looking at days to criterion, there's no significant difference between groups. The next thing to do is start looking at day-by-day data


ratioPresses <- weather %>%
  group_by(Subject, SessionDay, Drug, Session, WeatherDay) %>%
  summarise(Total = max(Trial), PercentHigh = sum(Response == 1)/max(Trial)*100)

ratioPresses <- ratioPresses[-c(285, 309),] ## duplicate data, needed removing. Not sure how it got there?


ratioPressesPlot <- ggplot(ratioPresses, aes(x = WeatherDay, y = PercentHigh, colour = Session, shape = Drug)) +
  geom_point() +
  geom_hline(yintercept = 50)+
  facet_wrap(~Subject)
ratioPressesPlot


justWTRRatios <- ratioPresses %>%
  filter(Session == "WTR") %>%
  group_by(Subject) %>%
  mutate(WTL = min(WeatherDay)-1) %>% # adding the length of time an animal spent in WTL as a predictor possibly?
  ungroup() %>%
  mutate(Subject = as.factor(Subject), Drug = as.factor(Drug)) %>%
  mutate(Drug = relevel(Drug, ref = "TPGS"))

#' Heirachy of data:
#'
#' Bottom-level: percentage of correct lever presses made by an animal on a given day - spans over many days until completion
#' Total presses made by a particular animal on a particular day - may be a proxy for effort
#' could also expand this to include WTL training days
#'
#' Mid-level: Individual animal IDs
#' matched with animal IDs is the number of days it took them to reach criterion in the WTL learning phase - this may be a proxy for intellegence
#'
#' Top-level: Drug group to which animal is assigned - this is the effect we want to look at

WTRsummaries <- weather %>%
  group_by(Subject, Drug, Session) %>%
  summarise(length = max(SessionDay))
WTRsummaries <- WTRsummaries %>%
  spread(Session, length)


bywtlplot <- ggplot(WTRsummaries, aes(x = WTL, y = WTR, colour = Drug)) +
  geom_point() +
  geom_smooth(method = 'lm')
bywtlplot

bydayplot <- ggplot(justWTRRatios, aes(x = SessionDay, y = PercentHigh, colour = Drug)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  facet_wrap(~Drug)
bydayplot


#' first, fit a baseline model (unconditional model)


unconditional <- lme(PercentHigh ~ 1, random = ~1|Subject, data = justWTRRatios, method = "ML", na.action = na.exclude)
summary(unconditional)

#' unconditional growth model - adding day

growth <- lme(PercentHigh ~ SessionDay, random = ~SessionDay|Subject, data = justWTRRatios, method = "ML", na.action = na.exclude)
summary(growth)


#' conditional growth - adding drug

condgrowth <- lme(PercentHigh ~ SessionDay*Drug, random = ~SessionDay|Subject, data = justWTRRatios, method = "ML", na.action = na.exclude)
summary(condgrowth)


#' AR model

AR <- lme(PercentHigh ~ SessionDay*Drug, random = ~SessionDay|Subject, data = justWTRRatios, method = "ML", na.action = na.exclude, correlation = corAR1(), control = lmeControl(opt = 'optim'))
summary(AR)


#' including WTL perforamance?

ARwtl <- lme(PercentHigh ~ SessionDay*Drug + WTL, random = ~SessionDay|Subject, data = justWTRRatios, method = "ML", na.action = na.exclude, correlation = corAR1(), control = lmeControl(opt = 'optim'))
summary(ARwtl)

anova(unconditional, growth, condgrowth, AR, ARwtl)
