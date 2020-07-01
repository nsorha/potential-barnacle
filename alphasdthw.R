library(readr)
comphw  <- read_csv("comphw.csv")
View(comphw)

library(tidyverse)
comphw<- comphw %>%
  mutate(compavg = (q1 + q2 + q3 + q4)/4)


#test internal validity

alpha <- comphw %>%
  select(q1, q2, q3, q4)

library(psych)
alpha(alpha)

#descriptive stats

comphw %>%
  pull(compavg) %>%
  describe()
#m = 4.56, sd = 2.01

#histogram
ggplot (comphw, aes(x=compavg)) +
  geom_histogram(binwidth=1)





