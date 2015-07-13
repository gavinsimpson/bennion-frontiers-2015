## need a new plot function as plot.timetrack is broken as it
## doesn't compute common x/y limits
## Actually, not broken, but it could be a bit more finessed
## to allow for wierd ordinations/overlays by having axis limits
## but use this as wrapper for presentation plots
ttplot <- function(obj, choices = 1:2, pch = c(21, 2), cex = 1,
                   col = c("#888a85", "#4e9a06"),
                   lwd = 3, surf = NULL, ccol = "#3465a4",
                   clwd = 2, labcex = 1.2, indicate = TRUE,
                   tpch = 21, bpch = 22, pcex = 1.6,
                   pcol = "#a40000", pbg = "#ef2929",
                   legend = TRUE,
                   ...) {
    scrs <- scores(obj$ord, choices = choices, scaling = obj$scaling,
                   display = "sites")
    fits <- fitted(obj)[, choices]
    xlim <- range(scrs[, 1], fits[, 1])
    ylim <- range(scrs[, 2], fits[, 2])
    plt <- plot(obj$ord, choices = choices, scaling = obj$scaling,
                type = "n", display = "sites", ...,
                ylim = ylim, xlim = xlim)
    points(obj$ord, choices = choices, scaling = obj$scaling,
           display = "sites", pch = pch[1], col = col[1],
           cex = cex, bg = col[1])
    if(!is.null(surf)) {
        op <- par(lwd = clwd)
        on.exit(par(op))
        plot(surf, add = TRUE, col = ccol, labcex = labcex)
        par(op)
    }
    lines(fits, pch = pch[2], col = col[2], lwd = lwd)
    if(indicate) {
        points(fits[1, , drop = FALSE], pch = tpch, cex = pcex,
               col = pcol, bg = pbg)
        points(fits[nrow(fits), , drop = FALSE], pch = bpch, cex = pcex,
               col = pcol, bg = pbg)
    }
    if(legend) {
        legend("topright", col = pcol, pt.bg = pbg, pt.cex = pcex,
               pch = c(tpch, bpch), cex = 1.2,
               legend = c("Top","Bottom"), bty = "n")
    }
}
