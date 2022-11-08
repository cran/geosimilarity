## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.path = 'figs/')

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("geosimilarity")

## -----------------------------------------------------------------------------
library("geosimilarity")
data("zn")
head(zn)

## ---- eval = FALSE------------------------------------------------------------
#  # log-transformation
#  hist(zn$Zn)
#  zn$Zn <- log(zn$Zn)
#  hist(zn$Zn)
#  
#  # remove outliers
#  library(SecDim)
#  k <- rmvoutlier(zn$Zn, coef = 2.5)
#  dt <- zn[-k,]
#  
#  # correlation
#  library("PerformanceAnalytics")
#  cor_dt <- dt[, c(3:12)]
#  chart.Correlation(cor_dt, histogram = TRUE, pch = 19)
#  
#  # multicollinearity
#  library(car)
#  m1 <- lm(Zn ~ Slope + Water + NDVI + SOC + pH + Road + Mine, data = dt)
#  car::vif(m1)

## ---- eval = FALSE------------------------------------------------------------
#  system.time({ # 5.9s
#  b1 <- bestkappa(Zn ~ Slope + Water + NDVI  + SOC + pH + Road + Mine,
#                  data = dt,
#                  kappa = c(0.01, 0.05, 0.1, 0.2, 0.5, 1),
#                  nrepeat = 2)
#  })
#  b1$bestkappa
#  b1$cvmean
#  b1$plot

## ---- eval = FALSE------------------------------------------------------------
#  system.time({ # 97.7ss
#  b2 <- bestkappa(Zn ~ Slope + Water + NDVI  + SOC + pH + Road + Mine,
#                  data = dt,
#                  kappa = c(seq(0.01, 0.1, 0.01), seq(0.2, 1, 0.1)),
#                  nrepeat = 10)
#  })
#  b2$bestkappa
#  b2$cvmean
#  b2$plot

## ---- eval = FALSE------------------------------------------------------------
#  system.time({ # 26s
#  g2 <- gos(Zn ~ Slope + Water + NDVI  + SOC + pH + Road + Mine,
#             data = dt, newdata = grid, kappa = 0.08)
#  })
#  grid$pred <- exp(g2$pred)
#  grid$uc99 <- g2$uncertainty$`0.99`
#  library(ggplot2)
#  library(viridis)
#  ggplot(grid, aes(x = Lon, y = Lat, fill = pred)) +
#    geom_tile() +
#    scale_fill_viridis(option="magma", direction = -1) +
#    coord_equal() +
#    labs(fill='Prediction') +
#    theme_bw()
#  ggplot(grid, aes(x = Lon, y = Lat, fill = uc99)) +
#    geom_tile() +
#    scale_fill_viridis(option="mako", direction = -1) +
#    coord_equal() +
#    labs(fill=bquote(Uncertainty~(zeta==0.99))) +
#    theme_bw()

## ---- eval = FALSE------------------------------------------------------------
#  library(reshape2)
#  uc <- cbind(grid[,2:3], g2$uncertainty)
#  uc <- melt(uc, id = c("Lon", "Lat"))
#  ggplot(uc, aes(x = Lon, y = Lat, fill = value)) +
#    geom_tile() +
#    scale_fill_viridis(option="mako", direction = -1) +
#    coord_equal() +
#    facet_wrap(~ variable) +
#    labs(fill='Uncertainty') +
#    theme_bw()

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(99)
#  # split data for validation: 50% training; 50% testing
#  split <- sample(1:nrow(dt), round(nrow(dt)*0.5))
#  train <- dt[split,]
#  test <- dt[-split,]
#  
#  library(DescTools)
#  # BCS
#  h1 <- gos(Zn ~ Slope + Water + NDVI  + SOC + pH + Road + Mine,
#            data = train, newdata = test, kappa = 1)
#  MAE(test$Zn, h1$pred)
#  RMSE(test$Zn, h1$pred)
#  
#  # GOS
#  h2 <- gos(Zn ~ Slope + Water + NDVI  + SOC + pH + Road + Mine,
#            data = train, newdata = test, kappa = 0.08)
#  MAE(test$Zn, h2$pred)
#  RMSE(test$Zn, h2$pred)

