## Fit additive models to the PCurve scores

## Load packages
library("mgcv")
library("ggplot2")

## load the data
coreData <- readRDS("./data/principal-curve-gam-data.rds")

## take data from 1750+
coreData <- subset(coreData, subset = dates >= 1750)

## plot the data, just to see
prc.plt <- ggplot(coreData, aes(x = dates, y = scrs, group = sites))
prc.plt1 <- prc.plt + geom_point() + geom_line()
prc.plt1
prc.plt2 <- prc.plt1 + facet_wrap(~ sites, scales = "free_y")
prc.plt2
prc.plt3 <- prc.plt1 + facet_wrap(~ sites, scales = "free")
prc.plt3

## model fit
am1 <- gam(scrs ~ sites + s(dates, by = sites),
           data = coreData)
summary(am1)

## think we need individual fits

fitGam <- function(i, dat, kk) {
    cdata <- dat[[i]]
    k <- kk[i]
    fit <- gam(scrs ~ s(dates, k = k, bs = "cr"), data = cdata,
               method = "REML", select = TRUE)
}
plotSmooths <- function(i, fits, ...) {
    plot(fits[[i]], ..., main = names(fits)[i])
}
coreDataSplit <- split(coreData, coreData$sites)

k <- c(15, 10, 12, rep(10, 10))
fits <- lapply(seq_along(coreDataSplit), fitGam, dat = coreDataSplit, kk = k)

layout(matrix(1:16, ncol = 4))
op <- par(mar = c(3,4,1,0.4) + 0.1)
lapply(seq_along(fits), plotSmooths, fits = fits, xlab = "", residuals = TRUE,
       pch = 1, cex = 0.4)
par(op)
layout(1)


## Need more flexible fitting...
fitGamm2 <- function(dat, k = 10, bs = "cr", knots, select = TRUE,
                     method = "REML", ...) {
    if (missing(knots)) {
        knots <- list(dates = unname(quantile(dat$dates,
                      seq(0, 1, length = k))))
    }
    fit <- gamm(scrs ~ s(dates, k = k, bs = bs), data = dat,
                method = method, select = select, knots = knots,
                correlation = corCAR1(form = ~ dates), ...)
    fit
}
plotSmooths <- function(i, fits, ...) {
    plot(fits[[i]]$gam, ..., main = names(fits)[i])
}

fits2 <- lapply(coreDataSplit, fitGamm2)

## something odd with KIEP
kiep.knots <- list(dates = unname(quantile(coreDataSplit[["KIEP"]]$dates,
                   seq(0, 1, length = 8))))
kiep.fit <- gamm(scrs ~ s(dates, k = 8, bs = "cr"),
                 data = coreDataSplit[["KIEP"]],
                 method = "REML", select = TRUE, knots = kiep.knots,
                 correlation = corCAR1(value = 0.5, form = ~ dates, fixed = TRUE))
kiep.fit2 <- gamm(scrs ~ s(dates, k = 8, bs = "cr"),
                  data = coreDataSplit[["KIEP"]],
                  method = "REML", select = TRUE, knots = kiep.knots)
## Looks like the CAR(1) term is hardly identified once we have a
## spline or vice versa. Fitting the model without restriction results
## in a phi estimate of ~1 but with 95% CI ~0--1. Which indicates that
## this estimation is very uncertain. Instead, we'll have to make some
## assumptions: fix phi at 0.5 to stop it running off to be too large
## and pull down k a little so that it doesn't grossly overfit the data.
## At least this way we get a fit whilst controling for some modest
## auto correlation.

## OK given the above replace the automatic KIEP fit with `kiep.fit` from
## above.
fits2[["KIEP"]] <- kiep.fit

pdf("./fig/GAMM-time-series-fits.pdf", height = 10, width = 10, pointsize = 12)
layout(matrix(1:16, ncol = 4))
op <- par(mar = c(3,4,1,0.4) + 0.1)
lapply(seq_along(fits2), plotSmooths, fits = fits2, xlab = "",
       residuals = TRUE, pch = 1, cex = 0.4, seWithMean = TRUE)
par(op)
layout(1)
dev.off()

## Derivatives -----------------------------------------------------------------

## Load code from github in lieu of having a package
tmpf <- tempfile()
download.file("https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30/raw/82118ee30c9ef1254795d2ec6d356a664cc138ab/Deriv.R",
              tmpf, method = "wget")
source(tmpf)
rm(tmpf)

derivs <- lapply(fits2, Deriv, n = 200)

Term <- "dates"
confints <- lapply(derivs, confint, term = Term)

makePredData <- function(data, n = 200) {
    data.frame(dates = seq(min(data$dates), max(data$dates), length.out = n))
}

predData <- lapply(coreDataSplit, makePredData)

modelPreds <- function(i, models, newdata, se.fit = TRUE) {
    predict(models[[i]]$gam, newdata = newdata[[i]], se.fit = se.fit)
}
predTrends <- lapply(seq_along(fits2), modelPreds,
                     models = fits2, newdata = predData)
names(predTrends) <- names(fits2)

signifWrap <- function(i, data, derivObj, term, ciObj) {
    signifD(data[[i]]$fit,
            d = derivObj[[i]][[Term]]$deriv,
            ciObj[[i]][[Term]]$upper,
            ciObj[[i]][[Term]]$lower)
}

signifCores <- lapply(seq_along(fits2), signifWrap, data = predTrends,
                      derivObj = derivs, term = Term, ciObj = confints)
names(signifCores) <- names(fits2)

plotTrends <- function(i, trends, dates, signifs, obs, cex = 0.8) {
    ptitle <- names(dates)[i]
    trends <- trends[[i]]
    dates <- dates[[i]]$dates
    signifs <- signifs[[i]]
    fit <- trends$fit
    uci <- fit + (1.96 * trends$se.fit)
    lci <- fit - (1.96 * trends$se.fit)
    scrs <- obs[[i]]$scrs
    incr <- unlist(signifs$incr)
    decr <- unlist(signifs$decr)
    incr.col <- "red"
    decr.col <- "blue"
    if (ptitle %in% c("MJOEA", "MARS")) {
        fit <- -fit
        uci <- -uci
        lci <- -lci
        scrs <- -scrs
        incr <- -incr
        decr <- -decr
        incr.col <- "blue"
        decr.col <- "red"
    }
    ylim <- range(scrs, fit, uci, lci)
    ## No longer need this clause to special case KIEP as the by-hand
    ## fit from above has replaced the automatic one
    ## if (ptitle == "KIEP") {
    ##     ylim <- range(scrs, fit, fit + 3, fit - 6)
    ## }
    plot(dates, fit, ylim = ylim, type = "l",
         ylab = "Principal Curve Score", main = ptitle)
    points(obs[[i]]$dates, scrs, pch = 16, cex = cex)
    lines(dates, uci, lty = "dashed")
    lines(dates, lci, lty = "dashed")
    lines(dates, incr, col = incr.col, lwd = 3)
    lines(dates, decr, col = decr.col, lwd = 3)
}

pdf("./fig/pcurve-fits-with-derivatives-revised-july-2015.pdf",
    height = 8, width = 12,
    pointsize = 12)
layout(matrix(1:16, ncol = 4))
op <- par(mar = c(2,4,2.5,0.4) + 0.1)
lapply(seq_along(fits2), plotTrends, trends = predTrends,
       dates = predData, signifs = signifCores, obs = coreDataSplit, cex = 1.2)
par(op)
layout(1)
dev.off()

## Take only the shallow cores in the order Hel has the strat plots in the
## paper
ordSh <- c("BART", "BOSHC", "LEVE", "LLAN", "MARS")

postscript("./fig/eps/shallow-pcurve-fits-with-derivatives.eps",
           height = 10, width = 5, paper = "special",
           onefile = FALSE, horizontal = FALSE,
           pointsize = 12)
layout(matrix(1:5, ncol = 1))
op <- par(mar = c(2,4,2.5,0.4) + 0.1)
lapply(seq_along(fits2[ordSh]), plotTrends,
       trends = predTrends[ordSh],
       dates = predData[ordSh],
       signifs = signifCores[ordSh],
       obs = coreDataSplit[ordSh], cex = 1.2)
par(op)
layout(1)
dev.off()

## Take only the shallow cores in the order Hel has the strat plots in the
## paper
ordDeep <- c("BLED", "ESTH", "GJER", "KIEP", "LIDZ", "MILL", "MJOEA", "RUMI")

postscript("./fig/eps/deep-pcurve-fits-with-derivatives-revised-july-2015.eps",
           height = 8, width = 10, paper = "special",
           onefile = FALSE, horizontal = FALSE,
           pointsize = 12)
layout(matrix(1:8, ncol = 2))
op <- par(mar = c(2,4,2.5,0.4) + 0.1)
lapply(seq_along(fits2[ordDeep]), plotTrends,
       trends = predTrends[ordDeep],
       dates = predData[ordDeep],
       signifs = signifCores[ordDeep],
       obs = coreDataSplit[ordDeep], cex = 1.2)
par(op)
layout(1)
dev.off()

take <- c("BLED", "GJER")
pdf("~/work/presentations/hjbb-symposium/fig/deep-pcurve-fits-with-derivatives.pdf",
    height = 5, width = 12, paper = "special",
    onefile = FALSE,
    pointsize = 14)
layout(matrix(1:2, ncol = 2))
op <- par(mar = c(2,4,2.5,0.4) + 0.1)
lapply(seq_along(fits2[take]), plotTrends,
       trends = predTrends[take],
       dates = predData[take],
       signifs = signifCores[take],
       obs = coreDataSplit[take], cex = 1.2)
par(op)
layout(1)
dev.off()

phis <- lapply(fits2, function(x) try(nlme::intervals(x$lme, which = "var-cov")$corStruct,
                                      silent = TRUE))
want <- sapply(phis, class) == "try-error"
phis[want] <- list(rep(NA, 3))
phis <- do.call("rbind", phis)
phis <- round(phis, 4)
rownames(phis) <- names(coreDataSplit)
phis <- cbind.data.frame(Site = rownames(phis), phis)
rownames(phis) <- NULL

write.csv(phis, file = "car1-coefs-phi.csv", row.names = FALSE)
