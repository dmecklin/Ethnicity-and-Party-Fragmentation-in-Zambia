library(tidyverse)
library(data.table)
# becuase data set is big 
CLEA <- clea_lc_20181119
Africa <- filter(CLEA, rg == "Africa")
Zambia <- filter(Africa, ctr_n == "Zambia")
Zambia_multiparty <- filter(Zambia, yr <= 1991 | yr <= 1973)
# one party state from 1973-'91