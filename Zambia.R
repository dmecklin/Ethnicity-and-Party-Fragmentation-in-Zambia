library(tidyverse)
# I: cleaning the data
load("C:/Users/Dunbar/Desktop/Spring 19/Final Project/CLEA_data/data/election_data.rdata")
# http://www.electiondataarchive.org/clea-lower-chamber-elections-archive.php
CLEA <- clea_lc_20181119
Africa <- filter(CLEA, rg == "Africa")
Zambia <- filter(Africa, ctr_n == "Zambia")
Zamb_multiparty <- filter(Zambia, yr >= 1991)
# one party state from 1973-'91, and the 1968 data is not useful for me.
Zamb <- filter(Zamb_multiparty, mag != -990
& sub != -990 
& pty_n != 3996 | 3997 | 3998 | 3999 | 4001 | 4998 | 4999
& pty_n != 6000)
Zamb
#
# Cleaning the data for missing values, independents, uncontested elections, etc. 
# Feel free to look at codebook to see the values I'm excluding: http://www.electiondataarchive.org/clea-lower-chamber-elections-archive.php
# II: Solving for effective number of political parties (ENoP)
#
# vars for ENoP
census_2010
# see census_data.R 
x <- select(census_2010, column_number = 2:28)
y <- x/100
census_sqrd <- y^2
EREG <- census_sqrd %>%
        replace(is.na(.), 0) %>%
        transmute(sum = 1/rowSums(.[1:27]))
EREG
Heterogenity <- EREG %>% mutate(prov = plabs) 
Heterogenity
# Effective Number of Electorally Relevant Ethnoregional Groups 

cvs1_sqrd <- Zamb %>% mutate(cvs1^2)
# ENoP  
"1991"<- filter(cvs1_sqrd, yr == 1991)
"1996"<- filter(cvs1_sqrd, yr == 1996)
"2001"<- filter(cvs1_sqrd, yr == 2001)
"2006"<- filter(cvs1_sqrd, yr == 2006)
"2011"<- filter(cvs1_sqrd, yr == 2011)
"2016"<- filter(cvs1_sqrd, yr == 2016)
# seperating by eleciton 
cvs1()
