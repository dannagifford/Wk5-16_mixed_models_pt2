set.seed(1234)

n <- 1000000

y_rgamma <- rgamma(n, shape = 3, scale = 200)

y_rgamma <- y_rgamma + 250

hist(y_rgamma,                                       # Plot of randomly drawn gamma density
     breaks = 1000,
     main = "",
     xlim = c(0, 2500))
