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

data<- Zamb %>% transmute(yr,sub,cvs1,cst_n,cst,seat)
ENoP_data <- filter(data, seat == 1)

# ENoP 
# seperating by eleciton 
"1991"<- filter(ENoP_data, yr == 1991)
"1996"<- filter(ENoP_data, yr == 1996)
"2001"<- filter(ENoP_data, yr == 2001)
"2006"<- filter(ENoP_data, yr == 2006)
# 1991 - 2006 don't have consituency level results
"2011"<- filter(ENoP_data, yr == 2011)
"2016"<- filter(ENoP_data, yr == 2016)
#
# computing ENoP
"2011_ENoP" <- `2011` %>% group_by(cst) %>% summarise(ENoP = sum(1/(cvs1^2)))
"2016_ENoP" <- `2016` %>% group_by(cst) %>% summarise(ENoP = sum((1/(cvs1^2))))
                                                      


`2011_ENoP`

`2016_ENoP`
#
# six more districts were added before 2016 election
CST_n <- ENoP_data %>% filter(yr == 2016) %>% transmute(sub, cst_n, cst)
ENoP_total <- `2016_ENoP` %>%  left_join(`2011_ENoP`, by = "cst")
ENoP_total %>% rename(ENoP_total,
  "ENoP.x" == "ENoP_16", 
  "ENoP.y" == "ENoP_11")
ENoP_total
ENoP_cst_n <- CST_n %>% inner_join(ENoP_total, by = "cst")
# now with constituency name, and province of constituency
ENoP_cst_n
# More constituencies added befor 2016 election
#ENoP
# finding average ENoP
`2011_ENoP` %>% summarise(mean = mean(ENoP)) 
# 3.71
`2016_ENoP` %>% summarise(mean = mean(ENoP))
# 2.85
inner <- `2016_ENoP` %>%  inner_join(`2011_ENoP`, by = "cst")
inner%>% summarise(mean = mean((ENoP.x + ENoP.y)/2))
# for both elections: 3.28 
#
# standard deviation

`2016_ENoP` %>% summarise(sd = sd(ENoP))
# 1.44
`2011_ENoP` %>% summarise(sd = sd(ENoP))
# 2.31

# IV graphing it

ggplot(data = ENoP_total) + 
geom_point(mapping = aes(x = cst, y = ENoP.x))

  
ggplot(data = ENoP_total) + 
  geom_point(mapping = aes(x = cst, y = ENoP.y))

# V Regression
regression <- ENoP_cst_n %>%  full_join(Heterogenity, by = "sub")
reg <- lm(sum ~ ENoP.y + ENoP.x, data = regression)
reg
summary(reg)
                                                  


