library(rbenchmark)
library(dplyr)
library(biglm)


# feed df test ----------------------------------------------------------

airp <- read.table("http://faculty.washington.edu/tlumley/NO2.dat",
                   col.names=c("logno2","logcars","temp","windsp",
                               "tempgrad","winddir","hour","day"))

w <- oomglm(exp(logno2) ~ logcars + temp + windsp,
            family = Gamma(log),
            start=c(2, 0, 0, 0))

w <- reweight(w, data = oomfeed(airp, chunksize = 150), max_iter = 20)

summary(w)


# feed file test --------------------------------------------------------

# mtcars %>% write.table('data/mtcars.txt', row.names = FALSE)

next_chunk <- oomfeed(file("data/mtcars.txt"),
                      chunksize = 10,
                      col.names = colnames(mtcars))

while(!is.null(chunk <- next_chunk())) {
  print(head(chunk))
}

z <- oomglm(mpg ~ cyl + disp)
z <- reweight(z, data = next_chunk, max_iter = 10)


# feed url test ---------------------------------------------------------

next_chunk <- oomfeed(url("http://faculty.washington.edu/tlumley/NO2.dat"),
                      chunksize=150,
                      col.names=c("logno2","logcars","temp","windsp",
                                  "tempgrad","winddir","hour","day"))

z <- oomglm(exp(logno2) ~ logcars + temp + windsp,
            family = Gamma(log),
            start=c(2, 0, 0, 0))

z <- reweight(z, next_chunk, max_iter = 20)

z


# large data tests ------------------------------------------------------

make_linear <- function(alpha, betas, nrows, sigma = 1) {
  
  p <- length(betas)
  
  X <- matrix(runif(nrows * p, min = 0, max = 100),
              nrow = nrows,
              ncol = p)
  
  Y <- alpha + X %*% betas + rnorm(nrows, 0, sigma)
  
  cbind(Y, X)
  
}

alpha <- runif(1, -10, 10)
betas <- matrix(rnorm(4, 10, 10), ncol = 1)
N     <- 3*10^7
chunk_size <- 3*10^3


df <- make_linear(alpha, betas, N) %>%
  as_tibble() %>%
  rename_all(tolower) %>%
  rename(y = v1)


w <- oomglm(y ~ v2 + v3 + v4 + v5)
w <- reweight(w, oomfeed(df, chunk_size), max_iter = 8)


benchmark(
  "biglm" = {
    v <- biglm(formula = y ~ v2 + v3 + v4 + v5,
               data = df[1:chunk_size, ])
    i <- chunk_size + 1
    for(j in seq(chunk_size*2, N, by = chunk_size)){
      v <- update(v, df[i:j, ])
      i <- j + 1
    }
  },
  "oomglm" = {
    w <- oomlm(formula = y ~ v2 + v3 + v4 + v5)
    i <- 1
    for(j in seq(0, N, by = chunk_size)){
      w <- update(w, df[i:j, ])
      i <- j + 1
    }
  },
  replications = 5,
  columns = c("test", "replications", "elapsed",
              "relative", "user.self", "sys.self")
)

coef(v)
coef(w)


benchmark(
  "bigglm" = {
    v <- bigglm(formula = y ~ v2 + v3 + v4 + v5,
                data = df,
                chunksize = chunk_size)
  },
  "oomglm" = {
    w <- oomglm(y ~ v2 + v3 + v4 + v5)
    w <- reweight(w, oomfeed(df, chunk_size), max_iter = 8)
  },
  replications = 10,
  columns = c("test", "replications", "elapsed",
              "relative", "user.self", "sys.self")
)

coef(v)
coef(w)
