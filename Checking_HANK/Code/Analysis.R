


##[1] "(Intercept)"                     
##[2] "size"                      
##[3] "persistence"                
##[4] "size:persistence"  
##   
##[5] "(Intercept)"                     
##[6] "size"                      
##[7] "persistence"                
##[8] "size:persistence"     
##[9] "size:I(persistence^2)"

library(boot)

load("data/boot_8.Rdata")
load("data/boot_10.Rdata")
load("data/boot_12.Rdata")

bootstrapped_8$t0|> stargazer()

intervals_list_8 <- c()
for (i in 1:9) {
  m <-
    boot.ci(
      bootstrapped_8,
      type = "perc",
      index = i,
      conf = 0.95
    )
  intervals_list_8 <- append(intervals_list_8,m$percent[1,4:5])
}


intervals_list_10 <- c()
for (i in 1:9) {
  m <-
    boot.ci(
      bootstrapped_10,
      type = "perc",
      index = i,
      conf = 0.95
    )
  intervals_list_10 <- append(intervals_list_10,m$percent[1,4:5])
}



intervals_list_12 <- c()
for (i in 1:9) {
  m <-
    boot.ci(
      bootstrapped_12,
      type = "perc",
      index = i,
      conf = 0.95
    )
  intervals_list_12 <- append(intervals_list_12,m$percent[1,4:5])
}


intervals_mat_8 <- intervals_list_8 |> matrix(nrow = 9, byrow = T)
intervals_mat_10 <- intervals_list_10 |> matrix(nrow = 9, byrow = T)
intervals_mat_12 <- intervals_list_12 |> matrix(nrow = 9, byrow = T)

intervals_mat_8

bootstrapped_8$t0
intervals_mat |> stargazer()


t_b_8 <- matrix(
  t(bootstrapped_8$t) -  bootstrapped_8$t0,
  nrow = 10000,
  ncol = length(t(bootstrapped_8$t0)),
  byrow = T
)

t_8 <-
  matrix(
    t(bootstrapped_8$t0),
    nrow = 10000,
    ncol = length(t(bootstrapped_8$t0)),
    byrow = T
  )




t_b_10 <- matrix(
  t(bootstrapped_10$t) -  bootstrapped_10$t0,
  nrow = 10000,
  ncol = length(t(bootstrapped_10$t0)),
  byrow = T
)

t_10 <-
  matrix(
    t(bootstrapped_10$t0),
    nrow = 10000,
    ncol = length(t(bootstrapped_10$t0)),
    byrow = T
  )

t_b_12 <- matrix(
  t(bootstrapped_12$t) -  bootstrapped_12$t0,
  nrow = 10000,
  ncol = length(t(bootstrapped_12$t0)),
  byrow = T
)

t_12 <-
  matrix(
    t(bootstrapped_12$t0),
    nrow = 10000,
    ncol = length(t(bootstrapped_8$t0)),
    byrow = T
  )

colMeans(abs(t_b_8) > abs(t_8))
colMeans(t_b_8 > t_8)

colMeans(abs(t_b_10) > abs(t_10))
colMeans(t_b_10 > t_10)

colMeans(abs(t_b_12) > abs(t_12))
colMeans(t_b_12 > t_12)


mean(bootstrapped_12$t[, 4] > 0)

bootstrapped_12$t0[6]


