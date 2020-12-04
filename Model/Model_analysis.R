library(qwraps2)
library(sqldf)
library(tidyverse)
library(lubridate)
library(fuzzyjoin)
library(data.table)
library(sqldf)
# For Veil of Darkness
#library(lutz)
library(suncalc)
library(splines)
library(doBy)
library(directlabels)
library("ggplot2")    #provides plot object options
#library("sf")
#library("raster")
#library("shapefiles")
#library("sp")          #classes and methods for spatial data
library("gridExtra")  #allows for plotting in grids
library("grid")       #allows grid graphical parameters
#library("RGraphics")  #allows SplitTextGrob
library("extrafont")  #introduces extra fonts
#library("rgdal")       # R wrapper around GDAL/OGR
#library("ggmap")      # for fortifying shapefiles
library("plm")         # clustered standard errors
library("dplyr")       # manipulating data
library("tidyr")
library("lfe")
library("stargazer")
library("zoo")
library("readxl")
library("broom")
library("rdd")
# library("rddtools")
library("rdrobust")
library("foreign")
library("rdd")
#library("sandwich")
library("lmtest")
library("plm")
library("haven")
#library("xtable")
#font_import()
#loadfonts(quiet=TRUE)
library(here)

# ########################################
# ##### IMPORT OUTCOME DATA
# ########################################
#
# df_outcome <- read_csv(here::here("/data/","fatal-police-Police Killings-data-wsp.csv"))
#
# df_outcome$year <- as.numeric(substr(df_outcome$date,0,4))
#
# df_outcome_clean <- df_outcome %>%
#   group_by(state, year) %>%
#   summarise(Police Killings_total = n())
#
# df_outcome_black <- df_outcome %>% filter(race == "B") %>%
#   group_by(state, year) %>%
#   summarise(Police Killings_black = n())
#
# df_outcome_white <- df_outcome %>% filter(race == "W") %>%
#   group_by(state, year) %>%
#   summarise(Police Killings_white = n())
#
# df_outcome_other <- df_outcome %>% filter(!(race %in% c("B", "W"))) %>%
#   group_by(state, year) %>%
#   summarise(Police Killings_other = n())
#
# df_outcome_clean <- df_outcome_clean %>% left_join(df_outcome_black, by = c("state", "year"))
# df_outcome_clean <- df_outcome_clean %>% left_join(df_outcome_white, by = c("state", "year"))
# df_outcome_clean <- df_outcome_clean %>% left_join(df_outcome_other, by = c("state", "year"))
#
# ########################################
# # IMPORT POPULATION DATA
# ########################################
#
# c_seer <- read.delim(here::here("/data/", "us.1969_2018.19ages.txt"), header = FALSE)
# census_seer <- c_seer %>%
#   mutate(year = as.numeric(substr(V1,1,4)),
#          state = substr(V1,5,6),
#          state_fips = as.numeric(substr(V1,7,8)),
#          county_fips = substr(V1,7,11),
#          race = ifelse(as.numeric(substr(V1,14,14)) == 1, "white",
#                        ifelse(as.numeric(substr(V1,14,14)) == 2, "black","other")),
#          sex = ifelse(as.numeric(substr(V1,16,16)) == 1, "male", "female"),
#          age_category = as.numeric(substr(V1,17,18)),
#          population = as.numeric(substr(V1,19,26)))
#
# df_population_total <- census_seer %>%
#   group_by(state, year) %>%
#   summarise(pop_total = sum(population)) %>%
#   filter(year == "2018")
#
# df_population_black <- census_seer %>% filter(race == "black") %>%
#   group_by(state, year) %>%
#   summarise(pop_black = sum(population)) %>%
#   filter(year == "2018")
#
# df_population_white <- census_seer %>% filter(race == "white") %>%
#   group_by(state, year) %>%
#   summarise(pop_white = sum(population)) %>%
#   filter(year == "2018")
#
# df_population_other <- census_seer %>% filter(!(race %in% c("black", "white"))) %>%
#   group_by(state, year) %>%
#   summarise(pop_other = sum(population)) %>%
#   filter(year == "2018")
#
# df_outcome_clean <- df_outcome_clean %>% left_join(select(df_population_total, state, pop_total), by = c("state"))
# df_outcome_clean <- df_outcome_clean %>% left_join(select(df_population_black, state, pop_black), by = c("state"))
# df_outcome_clean <- df_outcome_clean %>% left_join(select(df_population_white, state, pop_white), by = c("state"))
# df_outcome_clean <- df_outcome_clean %>% left_join(select(df_population_other, state, pop_other), by = c("state"))
#
# ########################################
# # CONSTRUCT 'BY INCIDENT' METRICS
# ########################################
# temp <- df_outcome_clean
#
# temp$rate_total <- temp$Police Killings_total / temp$pop_total
# temp$rate_black <- temp$Police Killings_black / temp$pop_total
# temp$rate_white <- temp$Police Killings_white / temp$pop_total
# temp$rate_other <- temp$Police Killings_other / temp$pop_total
#
# ########################################
# # CONSTRUCT CONTROL VARIABLES
# ########################################
# temp$control_totpop <- temp$pop_total
# temp$control_blackshare <- temp$pop_black / temp$pop_total
#
# temp$yr_2015 <- ifelse(temp$year == 2015, 1, 0)
# temp$yr_2016 <- ifelse(temp$year == 2016, 1, 0)
# temp$yr_2017 <- ifelse(temp$year == 2017, 1, 0)
# temp$yr_2018 <- ifelse(temp$year == 2018, 1, 0)
# temp$yr_2019 <- ifelse(temp$year == 2019, 1, 0)
# temp$yr_2020 <- ifelse(temp$year == 2020, 1, 0)
#

# ########################################
# # CONSTRUCT REGION
# ########################################
# xwalk_region <- read_excel(here::here("/data/", "xwalk_region.xlsx"))
# temp <- temp %>% left_join(select(xwalk_region, region, state), by = "state")
#
# write.csv(temp, here::here("/data/","model_dataframe.csv"), row.names = F)

########################################
# RUN MODEL (BY RACE)
########################################
df <- read_csv(here::here("/data/","model_dataframe.csv"))

mod_1_total <- data.frame(tidy(felm(log(rate_total) ~ yr_2016 + yr_2017 + yr_2018 + yr_2019 + yr_2020 | state, weights = df$pop_total, data = df),
                                            conf.int=T))
x <- mod_1_total[1,]
x[,] <- 0
mod_1_total[1,1] <- 2016
mod_1_total[2,1] <- 2017
mod_1_total[3,1] <- 2018
mod_1_total[4,1] <- 2019
mod_1_total[5,1] <- 2020
mod_1_total <- rbind(x, mod_1_total)
mod_1_total[1,1] <- 2015
mod_1_total[,8] <- "total"

gg_mod_1_total <- ggplot() +
  scale_fill_manual(values=c("grey88")) +
  scale_linetype_manual(values = c("longdash","longdash","longdash")) +
  geom_pointrange(data = head(mod_1_total, 5), inherit.aes=FALSE, aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high, color = V8)) +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(title = "Evolution in Police Killing Rate Per Capita", x = "Months from General Election", y = "Log Points (Relative to Year 2015)") +
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.position="none",
        text = element_text(size=15, family="Latin Modern Roman 10 Regular"),
        title = element_text(size=11))
gg_mod_1_total

mod_1_black <- data.frame(tidy(felm(log(rate_black) ~ yr_2016 + yr_2017 + yr_2018 + yr_2019 + yr_2020 | state, weights = df$pop_total, data = df),
                               conf.int=T))
mod_1_black[1,1] <- 2016
mod_1_black[2,1] <- 2017
mod_1_black[3,1] <- 2018
mod_1_black[4,1] <- 2019
mod_1_black[5,1] <- 2020
mod_1_black <- rbind(x, mod_1_black)
mod_1_black[1,1] <- 2015
mod_1_black[,8] <- "black"

gg_mod_1_black <- ggplot() +
  scale_fill_manual(values=c("grey88")) +
  scale_linetype_manual(values = c("longdash","longdash","longdash")) +
  geom_pointrange(data = head(mod_1_black, 5), inherit.aes=FALSE, aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high, color = V8)) +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(title = "Evolution in Police Killing Rate Per Capita", x = "Months from General Election", y = "Log Points (Relative to Year 2015)") +
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.position="none",
        text = element_text(size=15, family="Latin Modern Roman 10 Regular"),
        title = element_text(size=11))
gg_mod_1_black

mod_1_white <- data.frame(tidy(felm(log(rate_white) ~ yr_2016 + yr_2017 + yr_2018 + yr_2019 + yr_2020 | state, weights = df$pop_total, data = df),
                               conf.int=T))
mod_1_white[1,1] <- 2016
mod_1_white[2,1] <- 2017
mod_1_white[3,1] <- 2018
mod_1_white[4,1] <- 2019
mod_1_white[5,1] <- 2020
mod_1_white <- rbind(x, mod_1_white)
mod_1_white[1,1] <- 2015
mod_1_white[,8] <- "white"

gg_mod_1_white <- ggplot() +
  scale_fill_manual(values=c("grey88")) +
  scale_linetype_manual(values = c("longdash","longdash","longdash")) +
  geom_pointrange(data = head(mod_1_white, 5), inherit.aes=FALSE, aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high, color = V8)) +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(title = "Evolution in Police Killing Rate Per Capita", x = "Months from General Election", y = "Log Points (Relative to Year 2015)") +
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.position="none",
        text = element_text(size=15, family="Latin Modern Roman 10 Regular"),
        title = element_text(size=11))
gg_mod_1_white

mod_1_other <- data.frame(tidy(felm(log(rate_other) ~ yr_2016 + yr_2017 + yr_2018 + yr_2019 + yr_2020 | state, weights = df$pop_total, data = df),
                               conf.int=T))
mod_1_other[1,1] <- 2016
mod_1_other[2,1] <- 2017
mod_1_other[3,1] <- 2018
mod_1_other[4,1] <- 2019
mod_1_other[5,1] <- 2020
mod_1_other <- rbind(x, mod_1_other)
mod_1_other[1,1] <- 2015
mod_1_other[,8] <- "other"

gg_mod_1_other <- ggplot() +
  scale_fill_manual(values=c("grey88")) +
  scale_linetype_manual(values = c("longdash","longdash","longdash")) +
  geom_pointrange(data = head(mod_1_other, 5), inherit.aes=FALSE, aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high, color = V8)) +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(title = "Evolution in Police Killing Rate Per Capita -- Other", x = "Months from General Election", y = "Log Points (Relative to Year 2015)") +
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.position="none",
        text = element_text(size=15, family="Latin Modern Roman 10 Regular"),
        title = element_text(size=11))
gg_mod_1_other

mod_1_consol <- rbind(head(mod_1_total, 5), head(mod_1_black, 5), head(mod_1_white, 5), head(mod_1_other, 5))
gg_mod_1_consol <- ggplot() +
  scale_fill_manual(values=c("grey88")) +
  scale_linetype_manual(values = c("longdash","longdash","longdash")) +
  geom_pointrange(data = mod_1_consol, position = position_jitter(w = 0.3, h = 0), inherit.aes=FALSE, aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high, color = V8, shape = V8), size = 1) +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(title = "Evolution in Police Killing Rate Per Capita, by Race", x = "", y = "Log Points (Relative to Year 2015)", caption = "Vertical lines represent two standard deviations.") +
  theme_bw() +
  theme(panel.border = element_blank(),
      #  legend.position="none",
        legend.title = element_blank(),
        text = element_text(size=26, family="Latin Modern Roman 10 Regular"),
        title = element_text(size=16))
gg_mod_1_consol

ggsave(here::here("/Model/","plot_evolutionrace.png"), gg_mod_1_consol, width=8, height=8)

########################################
# RUN MODEL (BY REGION)
########################################

mod_1_west <- data.frame(tidy(felm(log(rate_total) ~ yr_2016 + yr_2017 + yr_2018 + yr_2019 + yr_2020 | state, weights = subset(df$pop_total, df$region == "west"), data = subset(df, df$region == "west")),
                               conf.int=T))
x <- mod_1_west[1,]
x[,] <- 0
mod_1_west[1,1] <- 2016
mod_1_west[2,1] <- 2017
mod_1_west[3,1] <- 2018
mod_1_west[4,1] <- 2019
mod_1_west[5,1] <- 2020
mod_1_west <- rbind(x, mod_1_west)
mod_1_west[1,1] <- 2015
mod_1_west[,8] <- "west"

###

mod_1_south <- data.frame(tidy(felm(log(rate_total) ~ yr_2016 + yr_2017 + yr_2018 + yr_2019 + yr_2020 | state, weights = subset(df$pop_total, df$region == "south"), data = subset(df, df$region == "south")),
                                 conf.int=T))
x <- mod_1_south[1,]
x[,] <- 0
mod_1_south[1,1] <- 2016
mod_1_south[2,1] <- 2017
mod_1_south[3,1] <- 2018
mod_1_south[4,1] <- 2019
mod_1_south[5,1] <- 2020
mod_1_south <- rbind(x, mod_1_south)
mod_1_south[1,1] <- 2015
mod_1_south[,8] <- "south"

###

mod_1_northeast <- data.frame(tidy(felm(log(rate_total) ~ yr_2016 + yr_2017 + yr_2018 + yr_2019 + yr_2020 | state, weights = subset(df$pop_total, df$region == "northeast"), data = subset(df, df$region == "northeast")),
                                 conf.int=T))
x <- mod_1_northeast[1,]
x[,] <- 0
mod_1_northeast[1,1] <- 2016
mod_1_northeast[2,1] <- 2017
mod_1_northeast[3,1] <- 2018
mod_1_northeast[4,1] <- 2019
mod_1_northeast[5,1] <- 2020
mod_1_northeast <- rbind(x, mod_1_northeast)
mod_1_northeast[1,1] <- 2015
mod_1_northeast[,8] <- "northeast"

###

mod_1_midwest <- data.frame(tidy(felm(log(rate_total) ~ yr_2016 + yr_2017 + yr_2018 + yr_2019 + yr_2020 | state, weights = subset(df$pop_total, df$region == "midwest"), data = subset(df, df$region == "midwest")),
                                 conf.int=T))
x <- mod_1_midwest[1,]
x[,] <- 0
mod_1_midwest[1,1] <- 2016
mod_1_midwest[2,1] <- 2017
mod_1_midwest[3,1] <- 2018
mod_1_midwest[4,1] <- 2019
mod_1_midwest[5,1] <- 2020
mod_1_midwest <- rbind(x, mod_1_midwest)
mod_1_midwest[1,1] <- 2015
mod_1_midwest[,8] <- "midwest"

###

mod_1_regionconsol <- rbind(head(mod_1_west, 5), head(mod_1_south, 5), head(mod_1_midwest, 5), head(mod_1_northeast, 5))
gg_mod_1_regionconsol <- ggplot() +
  scale_fill_manual(values=c("grey88")) +
  scale_linetype_manual(values = c("longdash","longdash","longdash")) +
  geom_pointrange(data = mod_1_regionconsol, position = position_jitter(w = 0.3, h = 0), inherit.aes=FALSE, aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high, color = V8, shape = V8), size = 1) +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(title = "Evolution in Police Killing Rate Per Capita, by Region", x = "", y = "Log Points (Relative to Year 2015)", caption = "Vertical lines represent two standard deviations.") +
  theme_bw() +
  theme(panel.border = element_blank(),
        #  legend.position="none",
        legend.title = element_blank(),
        text = element_text(size=26, family="Latin Modern Roman 10 Regular"),
        title = element_text(size=18))
gg_mod_1_regionconsol

ggsave(here::here("/Model/","plot_evolutionrregion.png"), gg_mod_1_regionconsol, width=9, height=8)
