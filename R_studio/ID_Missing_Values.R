# dplyr examples
# https://seananderson.ca/2014/09/13/dplyr-intro/
# https://www.neonscience.org/resources/learning-hub/tutorials/grepl-filter-piping-dplyr-r

# another good practice 
# https://towardsdatascience.com/data-cleaning-with-r-and-the-tidyverse-detecting-missing-values-ea23c519bc62


install.packages("VIM")
library(VIM)
?VIM

data(sleep, package = "VIM")
str(sleep)
summary(sleep)
aggr(sleep)

install.packages("tidyverse")
library(tidyverse)
library(dplyr)

# selecting 3 columns Sleep, Span, and Guest
df <- sleep %>%
  select(Sleep, Span, Gest) 

#plot(lm(Sleep~Span+Gest, data = df))
summary(lm(Sleep~Span+Gest, data = df))

# map of the missing values
install.packages("finalfit")
library(finalfit)
df %>%
  missing_plot()


# df %>% 
#   ff_glimpse(dependent, explanatory)

# Ways to id missing values ####################################################

# show the rows with missing values
df %>%
  filter_all(any_vars(is.na(.))) #%>%
# count() # give the count

which(is.na(df))



# total number of missing data, 12
df %>%
  is.na() %>%
  sum()

# counting NA valuesin a list form for each column
df %>%
  map(is.na) %>%
  map(sum)

# https://medium.com/geekculture/handling-missing-values-in-r-using-tidyr-da49766203f1
# calc the proportion of missing values in each variable
df %>%
  map(is.na) %>%
  map(sum) %>%
  map(~ ./ nrow(df)) %>%
  bind_cols()
# all 3 have 4 missing values 4/62 = 6.25% missing


# project question : 
# Is the amount of sleep associated with lifespan and/or gestational time?
##  E(Sleep) = B0 +B1(Span) + B2(Gest)
# sleep per day ( hrs) lifespan (years), gestational time(weeks)


# filling in missing values with mean or median
list <- c(Sleep, Span, Gest)

df.2 <- df %>%
  mutate(Sleep = replace_na(Sleep, median(Sleep, na.rm = TRUE)))

median(sleep$Sleep)



# more ways to id missing values ###############################################
# https://cran.r-project.org/web/packages/finalfit/vignettes/missing.html







  
