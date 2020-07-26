#1
library(readr)
tworek <- read_csv("tworek.csv")
# Relevant variables: Ought_Score Inherence_Bias RavensProgressiveMatrix_sum educ conserv Belief_in_Just_World


#3
library(tidyverse)
library(psych)

tworek %>%
  select(Inherence_Bias, Ought_Score) %>%
  corr.test() %>%
  print(short=FALSE)
# r = .30; 95%CI = .13 - .46.
# This matches Tworek and Cimpian (2016) see first line of results on page 1112:
# "As predicted, participants with a greater inherence bias in their explanations were also more likely to think that current
# patterns of behavior are good and as they should be, r(120) = .30, 95% CI = [.13, .46], p < .001.")

#4
# Specify model:
tworekregress <- setCor(Ought_Score ~ Inherence_Bias + RavensProgressiveMatrix_sum + educ + conserv + Belief_in_Just_World, data = tworek)
# call model:
tworekregress
# Beta for Inherence_Bias variable is .31 (95%CI: .12 to .49) - controlling for the Raven's progressive matrix. This matches Tworek and Cimpian (2016).
# The second line of the results section on page 1112: "Moreover, this relationship remained significant even when we used multiple regression to
# statistically adjust for participants’ education, fluid intelligence, conservatism, and belief in a just world, β = 0.31, 95% CI = [0.12, 0.49], p = .001.


#5
#see Tworek and Cimpian (2016) quotes above

#6
library(apaTables)


tworekregresslm <- lm(Ought_Score ~ Inherence_Bias + RavensProgressiveMatrix_sum + educ + conserv + Belief_in_Just_World, data = tworek)

apa.reg.table(tworekregresslm, filename = "tworekregresslm.doc", table.number = 2)





