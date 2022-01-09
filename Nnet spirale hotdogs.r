deux_spirales <- function(N = 250, rad = 2 * pi, th0 = pi/2, labels = 0:1) {
    N1 <- floor(N/2)
    N2 <- N - N1
    
    theta <- th0 + runif(N1) * rad
    spiral1 <- cbind(-theta * cos(theta) + runif(N1), theta * sin(theta) + runif(N1))
    spiral2 <- cbind(theta * cos(theta) + runif(N2), -theta * sin(theta) + runif(N2))
    
    points <- rbind(spiral1, spiral2)
    classes <- c(rep(0, N1), rep(1, N2))
    
    data.frame(e1 = points[, 1], e2 = points[, 2], reponse = factor(classes, labels = labels))
}

dataPoints <- deux_spirales(labels = c("not hot dog", "hot dog"))

sigmoide <- function(x) 1 / (1 + exp(-x))

propag <- function(x, w1, w2) {
  z1 <- cbind(1, x) %*% w1
  h <- sigmoide(z1)
  z2 <- cbind(1, h) %*% w2
  list(output = sigmoide(z2), h = h)
}

retropropag <- function(x, y, y_hat, w1, w2, h, pas) {
  dw2 <- t(cbind(1, h)) %*% (y_hat - y)
  dh  <- (y_hat - y) %*% t(w2[-1, , drop = FALSE])
  dw1 <- t(cbind(1, x)) %*% (h * (1 - h) * dh)
  
  w1 <- w1 - pas * dw1
  w2 <- w2 - pas * dw2
  
  list(w1 = w1, w2 = w2)
}

train <- function(x, y, neurones = 5, pas = 1e-2, iterations = 1e4) {
  d <- ncol(x) + 1
  w1 <- matrix(rnorm(d * neurones), d, neurones)
  w2 <- as.matrix(rnorm(neurones + 1))
  for (i in 1:iterations) {
    prpg <- propag(x, w1, w2)
    rtprpg <- retropropag(x, y,
                        y_hat = prpg$output,
                        w1, w2,
                        h = prpg$h,
                        pas = pas)
    w1 <- rtprpg$w1; w2 <- rtprpg$w2
  }
  list(output = prpg$output, w1 = w1, w2 = w2)
}

x <- data.matrix(dataPoints[, c('e1', 'e2')])
y <- dataPoints$reponse == 'hot dog'
nnet <- train(x, y, neurones = 10, iterations = 1e5)

mean((nnet$output > .5) == y)
grid <- expand.grid(e1 = seq(min(dataPoints$e1) - 1,
                             max(dataPoints$e1) + 1,
                             by = .25),
                    e2 = seq(min(dataPoints$e2) - 1,
                             max(dataPoints$e2) + 1,
                             by = .25))
prpg_grid <- propag(x = data.matrix(grid[, c('e1', 'e2')]),
                       w1 = nnet$w1,
                       w2 = nnet$w2)
grid$reponse <- factor((prpg_grid$output > .5) * 1,
                     labels = levels(dataPoints$reponse))


library(ggplot2)
theme_set(theme_minimal())
ggplot(dataPoints) + aes(e1, e2, colour = reponse) +
  geom_point(data = grid, size = .5) +
  geom_point() +
  labs(x = expression(x[1]), y = expression(x[2]))