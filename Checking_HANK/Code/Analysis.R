


##[1] "(Intercept)"                     
##[2] "size_dmnd"                      
##[3] "persistence_dmnd"                
##[4] "size_dmnd:persistence_dmnd"  
##   
##[5] "(Intercept)"                     
##[6] "size_dmnd"                      
##[7] "persistence_dmnd"                
##[8] "size_dmnd:persistence_dmnd"     
##[9] "size_dmnd:I(persistence_dmnd^2)"

library(boot)

load("data/boot_8.Rdata")
load("data/boot_10.Rdata")
load("data/boot_12.Rdata")

bootstrapped_8$t0|> stargazer()

intervals_list <- c()
for (i in 1:9) {
  m <-
    boot.ci(
      bootstrapped_8,
      type = "perc",
      index = i,
      conf = 0.95
    )
  intervals_list <- append(intervals_list,m$percent[1,4:5])
}

intervals_mat <- intervals_list |> matrix(nrow = 9, byrow = T)
bootstrapped_8$t0
intervals_mat |> stargazer()


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
