library(cocor)
library (tidyverse)
library(apaTables)


##bfi data

#load data
bfi <- read_csv("bfi2.csv")

# create correlation table
apa.cor.table(bfi)

# Look at correlation between A1 and C1 (agreeableness and conscientiousness)
# and E1 and O1(extraversion and openness)
cocor(~A1+C1|E1+O1, data=as.data.frame(bfi))


# Does agreeableness correlate more with conscientiousness or 
# does agreeableness correlate more with extraversion - OR
# does CI predict A1 better than E1 does??
cocor(~A1+C1|A1+E1, data=as.data.frame(bfi))

# create data sets for men and women
bfi_men <- bfi %>% filter(gender==1) 
bfi_women <- bfi %>% filter(gender==2) 

# create correlation tables
apa.cor.table(bfi_men)
apa.cor.table(bfi_women)

#compate
bfi_men <- as.data.frame(bfi_men)
bfi_women <- as.data.frame(bfi_women)

cocor(~A1+E1|A1+E1, data=list(bfi_men, bfi_women))


##cor table data
## determine correlation between rating-raises and rating-critical correlation
r.jk <- .59
r.jh <- .16
r.kh <- .38
n <- 30

cocor.dep.groups.overlap(r.jk, r.jh, r.kh, n)

## determine correlation between rating-raises and complaints-critical correlation
r.jk <- .59
r.hm <- .19
r.jh <- .83
r.jm <- .16
r.kh <- .67
r.km <- .38
n <- 30

cocor.dep.groups.nonoverlap(r.jk, r.hm, r.jh, r.jm, r.kh, r.km, n)


## Comparing rating-raises correlations from 2 papers
r1.jk <- .59
r2.hm <- .03
n1=30
n2=3000

cocor.indep.groups(r1.jk, r2.hm, n1, n2)