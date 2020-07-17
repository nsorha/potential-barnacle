library(readr)
library(tidyverse)
library(psych)

#1

wilson <- read_csv("wilson.csv")

#select items
porms <- wilson %>%
  select(item1, item2r, item3, item4r, item5)

#calculate alpha
alpha(porms)
#alpha = .91 (95CI: .87 to .95)

#2. test convergent and discriminant validity
matsen <- read_csv("matsen.csv")

#convergent validity
ggplot(matsen, aes(x=TRESPN, y=SEN24)) +
  geom_point()
#looks like a strong negative relation


#discriminant validity
ggplot(matsen, aes(x=SES, y=SEN24)) +
  geom_point()
#The sensitivity measure is not related to SES, suggesting discriminant validity. We can
#quantify this by calculating the correlation coefficient.

matsen %>%
  select(SES, SEN24) %>%
  corr.test() %>%
  print(short=FALSE)
#r = -.08 (95CI: -.33 to .18). They are not related.


#3. test criterion validity
ggplot(matsen, aes(x = as.factor(ATTACH), y = SEN24)) +
  geom_point(color = "purple") +
  stat_summary(fun.data = mean_cl_normal)
#The figure shows that the sensitivity scores are higher for those classified as securely attached.


