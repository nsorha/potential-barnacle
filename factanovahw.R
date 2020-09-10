# load packages
library(readr)
library(tidyverse)
library(psych)
library(lsr)

# assign data to object
facthw <- read_csv("MaglioandPolman2014.csv")

# transform variables into factor
facthw <- facthw %>%
  mutate(orientationf =recode(orientation, `1` = "toward", `2` = "awayfrom"))

facthw <- facthw %>%
  mutate(stationf =recode(station, `1` = "spadina", `2` = "stgeorge", `3` = "blooryonge",
                          `4` = "shernourne"))

# descriptives

describeBy(facthw$subjective_distance,list(facthw$stationf,facthw$orientationf))

# graph

ggplot(facthw, aes(x = stationf, y = subjective_distance, group = orientationf, color = orientationf)) +
  stat_summary(fun.data = mean_cl_normal) +
  stat_summary(fun = mean, geom = "line")

# Factorial ANOVA

maglio <- aov(subjective_distance ~ orientationf + stationf + orientationf:stationf, data = facthw)

summary(maglio)

etaSquared(maglio)


# Post-hocs
model.tables(maglio, type="means")


TukeyHSD(maglio)

facthwspadina <- facthw %>%
  filter(station == 1)

t.test(subjective_distance ~ orientationf, data = facthwspadina, var.equal = TRUE)

facthwstgeorge <- facthw %>%
  filter(station == 2)

t.test(subjective_distance ~ orientationf, data = facthwstgeorge, var.equal = TRUE)


facthwblooryonge <- facthw %>%
  filter(station == 3)

t.test(subjective_distance ~ orientationf, data = facthwblooryonge, var.equal = TRUE)


facthwshernourne <- facthw %>%
  filter(station == 4)

t.test(subjective_distance ~ orientationf, data = facthwshernourne, var.equal = TRUE)


# Write - up

# We carried out a 2 (orientation: toward, away from) × 4
# (station: Spadina, St. George, Bloor-Yonge, Sherbourne)
# analysis of variance (ANOVA) on closeness ratings, which
# revealed no main effect of orientation, F < 1, and a main
# effect of station, F(3, 194) = 24.10, p < .001, ηp2 = .27. This
# main effect was qualified by the predicted interaction
# between orientation and station, F(3, 194) = 16.28, p < .001,ηp2 = .20.
# We decomposed this interaction by comparing the subjective-distance ratings
# between participants traveling east and west for each of the four subway stations.
# Westbound participants rated the stations to the west of
# Bay Street as closer than did eastbound participants; this
# effect was obtained for both the station one stop to the
# west (St. George, p < .001, ηp2 = .28) and the station two
# stops to the west (Spadina, p = .001, ηp2 = .20). The opposite pattern
# held true for stations to the east of Bay Street.
# Eastbound participants rated the stations to the east of
# Bay Street as closer than did westbound participants; this
# effect was obtained for both the station one stop to the
# east (Bloor-Yonge, p = .053, ηp2 = .08) and the station two
# stops to the east (Sherbourne, p < .001, ηp2 = .24).






