## Do a principal curves analysis of each core in turn

## load packages
library("analogue")

## Load the data
source("core-data-processing.R")

## split deep up by sites
coresSpl <- split(cores, cores$sites)

## Fit the PrC, remove the empty species that were added
## in the data processing, which was needed to overlay the
## the core data on to the training set data
fitPcurve <- function(i, cores, axis, method, ...) {
    axis <- axis[i]
    core <- cores[[i]]
    method <- method[i]
    nc <- ncol(core)
    core <- core[, seq_len(nc - 2)[-1]] # drop dates sites & depths vars
    core <- core[, colSums(core) > 0]
    prc <- prcurve(core, vary = TRUE, trace = TRUE, method = method,
                   axis = axis)
    prc
}

## apply the wrapper to each core
axis <- c(1, 1, 2, 1, 1, 1, 1, 1)
method <- c(rep("pca", 5), rep("ca", 2), rep("pca", 2))
corePRC <- lapply(seq_along(coresSpl), fitPcurve,
                  cores = coresSpl, axis = axis, method = method)
names(corePRC) <- names(coresSpl)

## quick check on the fitted curves via a plot
pcplot <- function(i, obj) {
    plot(obj[[i]], main = names(obj)[i])
}
pdf("./fig/pcurves-ordination-plots-deep.pdf",
    height = 8, width = 8)
layout(matrix(1:9, ncol = 3))
op <- par(mar = c(5,4,2,1) + 0.1)
lapply(seq_along(corePRC), pcplot, obj = corePRC)
par(op)
layout(1)
dev.off()

## Shallow lakes ------------------------------------------------------
## split up by sites
shcoresSpl <- split(shcores, shcores$sites)

## apply the wrapper to each core
shaxis <- c(1, 1, 1, 1, 1)
shmethod <- c(rep("pca", 5))
shcorePRC <- lapply(seq_along(shcoresSpl), fitPcurve,
                    cores = shcoresSpl, axis = shaxis, method = shmethod)
names(shcorePRC) <- names(shcoresSpl)


## quick check on the fitted curves via a plot
pdf("./fig/pcurves-ordination-plots-shallow.pdf",
    height = 5, width = 8)
layout(matrix(1:6, ncol = 3))
op <- par(mar = c(5,4,1,1) + 0.1)
lapply(seq_along(shcorePRC), pcplot, obj = shcorePRC)
par(op)
layout(1)
dev.off()

## Pull out the "scores" for the Pcurves and the ages -- TODO
library("ggplot2")

mynorm <- function(x) {
    (x - min(x)) / (max(x) - min(x))
}

scrs <- lapply(corePRC, scores)
scrs <- lapply(scrs, `[`, , 1)
len <- lapply(scrs, length)
nams <- names(scrs)
depths <- lapply(coresSpl, `[`, , "depths")
depths <- lapply(depths, mynorm)
dates <- lapply(coresSpl, `[`, , "dates")
df <- data.frame(Lake = rep(nams, times = len),
                 PrC  = unlist(scrs),
                 Depth = unlist(depths),
                 Date = unlist(dates))
rownames(df) <- NULL

deep.plt <- ggplot(df, aes(x = Date, y = PrC)) +
    geom_point() + geom_line() +
    coord_flip() +
    facet_wrap( ~ Lake, scales = "fixed")
deep.plt

ggsave("./fig/pcurve-strat-by-depth-deep-wrong.pdf", deep.plt,
       height = 9, width = 7)

## Stick these PrC scores into the cores object
cores <- transform(cores, scrs = unname(unsplit(scrs, cores$sites)))


scrs <- lapply(shcorePRC, scores)
scrs <- lapply(scrs, `[`, , 1)
len <- lapply(scrs, length)
nams <- names(scrs)
depths <- lapply(shcoresSpl, `[`, , "depths")
depths$LLAN <- depths$LLAN / 100
depths <- lapply(depths, mynorm)
dates <- lapply(shcoresSpl, `[`, , "dates")
df <- data.frame(Lake = rep(nams, times = len),
                 PrC  = unlist(scrs),
                 Depth = unlist(depths),
                 Date = unlist(dates))
rownames(df) <- NULL


shallow.plt <- ggplot(df, aes(x = Date, y = PrC)) +
    geom_point() + geom_line() +
    coord_flip() +
    facet_wrap( ~ Lake, scales = "fixed") ## + xlim(c(-100, 0))
shallow.plt

ggsave("./fig/pcurve-strat-by-depth-shallow-wrong.pdf", shallow.plt,
       height = 7, width = 7)

## Stick these PrC scores into the cores object
shcores <- transform(shcores, scrs = unname(unsplit(scrs, shcores$sites)))

## extract the information needed for model fitting
want <- c("sites","depths","dates","scrs")
coreData <- rbind(cores[, want], shcores[, want])

## write this out for use in the GAM modelling script
saveRDS(coreData, file = "./data/principal-curve-gam-data.rds")

## reshape coreData to wide format for C2
library("reshape2")
coreDataM <- transform(coreData, sites = as.character(sites),
                       stringsAsFactors = FALSE)
coreDataM <- dcast(coreDataM, depths + dates ~ sites)

plt <- Stratiplot(dates ~ . - depths, data = coreDataM, rev = FALSE,
                  type = c("l", "p"), subset = dates >= 1800,
                  varTypes = "absolute", pch = 19, cex = 0.5,
                  ylab = expression(phantom()^{210}*Pb ~ Date),
                  xlab = "Principal Curve Score", topPad = 3)

## write out the plot to PDF and the reshaped data
pdf("./fig/principal-curve-scores-stratigraphic-plot",
    width = 12, height = 7, pointsize = 10)
print(plt)
dev.off()

write.csv(coreDataM, file = "./data/pcurve-scores-wide-format.csv",
          row.names = FALSE, na = "")
