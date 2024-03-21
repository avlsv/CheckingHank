


##[1] "(Intercept)"                     
##[2] "size"                      
##[4] "size:persistence"  
##   
##[5] "(Intercept)"                     
##[6] "size"                      
##[8] "size:persistence"     
##[9] "size:I(persistence^2)"

library(boot)

load("data/boot.Rdata")


bootstrapped$t0 |> stargazer()

intervals_list <- c()
for (i in 1:9) {
  m <-
    boot.ci(
      bootstrapped,
      type = "perc",
      index = i,
      conf = 0.95
    )
  intervals_list <- append(intervals_list,m$percent[1,4:5])
}


intervals_mat <- intervals_list |> matrix(nrow = 9, byrow = T)


intervals_mat

intervals_mat |> stargazer()


t_b <- matrix(
  t(bootstrapped$t) -  bootstrapped$t0,
  nrow = 10000,
  ncol = length(t(bootstrapped$t0)),
  byrow = T
)

t_m <-
  matrix(
    t(bootstrapped$t0),
    nrow = 10000,
    ncol = length(t(bootstrapped$t0)),
    byrow = T
  )


colMeans(abs(t_b) > abs(t_m), na.rm = T)
colMeans(t_b < t_m , na.rm = T)


mean(bootstrapped$t[, 4] > 0)

bootstrapped_12$t0[6]

