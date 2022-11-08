#' Function for the best kappa parameter
#'
#' @description Function for determining the best kappa parameter for the optimal similarity
#'
#' @usage bestkappa(formula, data = data, kappa = seq(0.05,1,0.05), nrepeat = 10)
#'
#' @param formula A formula of GOS model
#' @param data A data.frame of observation data
#' @param kappa A numeric vector of the optional percentages of observation locations
#'              with high similarity to a prediction location.
#'              kappa = 1 - tau, where tau is the probability parameter
#'              in quantile operator. kappa = 0.25 means
#'              that 25% of observations with high similarity to a prediction
#'              location are used for modelling.
#' @param nrepeat A numeric value of the number of cross-validation training times.
#'                The default value is 10.
#'
#' @return A list of the result of the best kappa and the computation process.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_x_continuous scale_y_continuous theme_bw
#' @importFrom DescTools RMSE
#' @importFrom dplyr %>% group_by summarise_all funs
#' @importFrom ggrepel geom_label_repel
#'
#' @examples
#' data("zn")
#' # log-transformation
#' hist(zn$Zn)
#' zn$Zn <- log(zn$Zn)
#' hist(zn$Zn)
#' # remove outliers
#' require(SecDim)
#' k <- rmvoutlier(zn$Zn, coef = 2.5)
#' dt <- zn[-k,]
#' # determine the best kappa
#' system.time({
#' b1 <- bestkappa(Zn ~ Slope + Water + NDVI  + SOC + pH + Road + Mine,
#'                 data = dt,
#'                 kappa = c(0.01, 0.1, 1),
#'                 nrepeat = 1)
#' })
#' b1$bestkappa
#' b1$plot
#'
#' @export

bestkappa <- function(formula, data = data,
                      kappa = seq(0.05,1,0.05), nrepeat = 10){
  rmse <- c()

  namey <- all.vars(formula)[1]

  out_rmse <- data.frame("kappa" = rep(kappa, times = nrepeat),
                         "times" = rep(c(1:nrepeat), each = length(kappa)),
                         "rmse" = NA)

  # 50% train, 50% test, repeat 10 times
  for(i in 1:nrow(out_rmse)){
    set.seed(out_rmse$times[i])
    trainindex <- sample.int(n = nrow(data), size = floor(0.5 * nrow(data)), replace = F)
    cvtrain <- data[trainindex, ]
    cvtest <- data[-trainindex, ]

    g1 <- gos(formula, data = cvtrain, newdata = cvtest, kappa = out_rmse$kappa[i])
    pred1 <- g1$pred

    out_rmse$rmse[i] <- RMSE(cvtest[[namey]], pred1)
  }

  cv.out <- out_rmse[, c("kappa", "rmse")] %>%
    group_by(kappa) %>%
    summarise_all(funs(mean))

  ########## plot selecting optimal threshold of similar locations
  k <- which(cv.out$rmse == min(cv.out$rmse))[1]
  best_kappa <- cv.out$kappa[k]

  l1 <- (max(cv.out$rmse)-min(cv.out$rmse))*0.1
  best_x <- cv.out$kappa[k]
  best_y <- cv.out$rmse[k]

  p1 <- ggplot(cv.out, aes(x = kappa, y = rmse))+
    geom_point()+
    geom_line() +
    geom_label_repel(data = data.frame(kappa=best_x, rmse=best_y), label=as.character(best_kappa)) +
    scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
    scale_y_continuous(limits = c(min(cv.out$rmse) - l1, max(cv.out$rmse))) +
    theme_bw()


  out <- list("bestkappa" = best_kappa, "cvrmse" = out_rmse, "cvmean" = cv.out,
              "plot" = p1)

  return(out)
}











