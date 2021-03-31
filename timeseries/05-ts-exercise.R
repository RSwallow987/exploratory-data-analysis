# Repeat the analysis above for the `ChickWeight` dataset. Look for any interesting 
# patterns in individual chicks.

library(dplyr)
library(lubridate)
library(tsibble)
library(ggplot2)
library(brolgar) 
#To install brolgar
#library(remotes)
#remotes::install_github("njtierney/brolgar")

# 1. Load the `ChickWeight` data
data("ChickWeight")

# 2. Turn this into a tsibble. What's the key, what's the index? Is it regular?
chick_weight <- as_tsibble(x = ChickWeight,
                           key = Chick,      # individual identifier
                           index = Time,     # number of days since hatching
                           regular = F)

# Sample a handful of time series
chick_weight%>%sample_n_keys(size=8)%>%ggplot(aes(x = Time, y = weight, group = Chick,col=Diet)) + 
  geom_line() + 
  labs(y="Weight", x="Days Since Hatch")
  
# Plot 4 chicks per facet, in 5 facets
ggplot(chick_weight,
       aes(x = Time, y = weight, group = Chick,col=Diet)) +
  geom_line() + facet_sample(n_per_facet = 3, n_facets = 6) +
  labs(y="Weight", x="Days Since Hatch")
