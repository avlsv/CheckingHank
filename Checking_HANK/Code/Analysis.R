


##[1] "(Intercept)"                     
##[2]"size_dmnd"                      
##[3] "persistence_dmnd"                
##[4] "size_dmnd:persistence_dmnd"     
##[5] "(Intercept)"                     
##[6] "size_dmnd"                      
##[7] "persistence_dmnd"                
##[8] "size_dmnd:persistence_dmnd"     
##[9] "size_dmnd:I(persistence_dmnd^2)"

load("data/boot_8.Rdata")
load("data/boot_10.Rdata")
load("data/boot_12.Rdata")


boot.ci(
  bootstrapped_12,
  type = "perc",
  index = 1,
  conf = 0.95
)
boot.ci(
  bootstrapped_8,
  type = "perc",
  index = 2,
  conf = 0.95
)
boot.ci(
  bootstrapped,
  type = "perc",
  index = 3,
  conf = 0.95
)
boot.ci(
  bootstrapped_8,
  type = "perc",
  index = 8,
  conf = 0.95
)
boot.ci(
  bootstrapped,
  type = "perc",
  index = 5,
  conf =  0.95
)
boot.ci(
  bootstrapped,
  type = "perc",
  index = 6,
  conf = 0.95
)
boot.ci(
  bootstrapped,
  type = "perc",
  index = 7,
  conf = 0.95
)
boot.ci(
  bootstrapped,
  type = "perc",
  index = 8,
  conf = 0.95 
)
boot.ci(
  bootstrapped_10,
  type = "perc",
  index = 9,
  conf =  0.95
)



plot(bootstrapped_12, index =4, nclass = 15)


w_b <- matrix(
  t(bootstrapped_12$t) -  bootstrapped_12$t0,
  nrow = 10000,
  ncol = length(t(bootstrapped_8$t0)),
  byrow = T
)

w <-
  matrix(
    t(bootstrapped_12$t0),
    nrow = 10000,
    ncol = length(t(bootstrapped_8$t0)),
    byrow = T
  )

colMeans(abs(w_b) > abs(w))
colMeans(w_b > w)



mean(bootstrapped_12$t[, 4] > 0)

bootstrapped$t0[6]



bootstrapped |> tidy() |> mutate(stat_name =  names(c(a, b, c)))



m1 <-
  lm(log(consumption) ~ size * persistence,
     size_persistence_consumption_tbl)
m2 <-
  lm(
    log(consumption) ~ size * persistence + size * I(persistence ^ 2),
    size_persistence_consumption_tbl
  )
stargazer(m1, m2, df = F)
