library(dplyr)
df <- tibble(x = c(6,6,11,7,5,4,4),
             y = c(2,3,9,1,8,7,5),
             variation_x = x - mean(x),
             variation_x_squared = variation_x ^ 2,
             variation_y = y - mean(y),
             variation_y_squared = variation_y ^ 2)
ols <- lm(df$y ~ df$x)

yhat <- predict(ols)
variation_prediction <- sum((yhat - mean(yhat)) ^ 2)

residual <- resid(ols)
variation_residual <- sum(residual^2)
se_beta1 <- sqrt((variation_residual / 5) / sum(df$variation_x_squared))
