## Update feature data and convert any columns that are matrices into additional
## explicit columns in the data.table in the app. Otherwise DT crashes
.convertMatsToCols <- function(msnset) {
  .tn <- length(fvarLabels(msnset))
  chk <- vector(length = .tn)
  for (i in 1:.tn) {
    chk[i] <- is.matrix(fData(msnset)[, i]) | is.data.frame(fData(msnset)[, i])
  }
  if (any(chk)) {
    .ind <- which(chk)
    .nams <- fvarLabels(msnset)[.ind]
    for (i in seq(.nams)) {
      .df <- fData(msnset)[, .nams[i]]
      .newNams <- paste0(.nams[i], ".", colnames(.df))
      colnames(.df) <- .newNams
      fData(msnset) <- cbind(fData(msnset), .df)
    }
    fData(msnset) <- fData(msnset)[, -.ind]

  }
  return(msnset)
}

## appStock colors
stockcol <- c("#E41A1C", "#377EB8", "#309C17", "#FF7F00", "#FFD700", "#00CED1",
              "#A65628", "#F781BF", "#984EA3", "#9ACD32", "#B0C4DE", "#00008A",
              "#FDAE6B", "#EBB7BE", "#3F8F8F", "#CF9802", "#6A51A3", "#21E8AC",
              "#0000FF", "#1D7A3E", "#BF2A6B", "#CD5B45", "#808000", "#F21D56",
              "#67000D", "#7A0C79", "#93EDF5", "#A66A6A", "#0E438A", "#DBBCF7")
appStockcol <- function() {stockcol}

## modified version of plot2D for shiny
.plot2D_shiny <- function(coords, fd, fcol = fcol,
                   unk = FALSE, scheme = c("white"), ...) {
    if(missing(scheme)) scheme <- "white"
    # Set background black on plot
    if (!(any(scheme == "black"| scheme == "white"))) stop("Colour scheme can only be black or white")
    if (scheme == "white") scheme2 <- "black"
    if (scheme == "black") scheme2 <- "white"
    par(bg = scheme, col.axis = scheme2, col.main = scheme2,
        col.lab = scheme2, fg = scheme2)
    .data <- coords
    .xlab <- colnames(.data)[1]
    .ylab <- colnames(.data)[2]
    plot(.data, xlab = .xlab, ylab = .ylab,
         type = "n", ...)
    # ukn <- which(fd[, fcol] == "unknown")
    points(.data[, 1], .data[, 2],
           bg =  paste0("#dbdada"),
           col = darken(paste0("#C8C8C8")),
           pch = 21, cex = 1.2, lwd = .7)
    cl <- names(table(fd[, fcol]))
    cl <- cl[cl != "unknown"]
    if (!unk) {
        for (i in seq(cl)) {
            ind <- which(fd[, fcol] == cl[i])
            points(coords[ind, ,drop = FALSE], bg = appStockcol()[i], pch = 21,
                   col = paste0(appStockcol()[i], 60), cex = 1.5)
        }
    }

}

.highlightOnPlot_shiny <- function(coords, myfoi, labels = FALSE,
                            scheme = c("white"), cex = 1.2) {

    .data <- coords
    if (!(any(scheme == "black"| scheme == "white")))
        stop("Colour scheme can only be black or white")
    if (scheme == "white") scheme2 <- "black"
    if (scheme == "black") scheme2 <- "white"

    points(.data[myfoi, 1], .data[myfoi, 2],
           col = scheme2,
           pch = 21, cex = 1, lwd = 1.3)

    if (labels) {
        text(.data[myfoi, 1], .data[myfoi, 2], myfoi, pos = 3, font  = 2, cex = cex)
    }
}


## JS callback to allow batch searching of data table with space instead of pipes
callback <- '
var x = document.createElement("INPUT");
x.setAttribute("type", "text");
x.setAttribute("id", "mySearch");
x.setAttribute("placeholder", "Search");
x.style.float = "left";
x.style.width = "200%";
$("div.search").append($(x));
$("#mySearch").on("keyup redraw", function(){
  var splits = $("#mySearch").val().split(" ").filter(function(x){return x !=="";})
  var searchString = "(" + splits.join("|") + ")";
  setTimeout(function(){
    table.search(searchString, true).draw(true);
  }, 3000);
});
'

## make data frame for ggplot
plotFacetProfiles <- function(data,
                              fcol,
                              fd,
                              pd,
                              replicate.column.name,
                              col,
                              ...) {

  intensities = NULL
  mrk = NULL
    if (missing(replicate.column.name)) {
        # message(paste("Replicate information not provided, assuming 1 replicate only"))
        repInfo <- rep(1, ncol(data))
        reps <- FALSE
    } else {
        repInfo <- pd[, replicate.column.name]
        reps <- TRUE
    }

    ## prep data for ggplot
    .rn <- rownames(data)
    .cn <- colnames(data)
    plot_data <- data.frame("id" = rep(.rn, ncol(data)),
                            "fraction" = rep(.cn, each = nrow(data)), # variable
                            "intensities" = as.vector(data),  # value
                            "rep" = factor(rep(repInfo, each = nrow(data))),
                            "mrk" = rep(fd[, fcol], ncol(data)))
    plot_data <- within(plot_data, fraction <- factor(fraction, levels = colnames(data)))

    df <- plot_data %>% group_by(mrk, fraction, rep) %>%
      dplyr::summarise(min = min(intensities, na.rm = TRUE),
                       quant_05 = quantile(intensities, 0.05, na.rm = TRUE),
                       mean = mean(intensities, na.rm = TRUE),
                       quant_95 = quantile(intensities, 0.95, na.rm = TRUE),
                       max = max(intensities, na.rm = TRUE), .groups = "keep",
                       na.rm = TRUE)

    fracLev <- levels(df$fraction)
    if (reps == TRUE) {
        repLev <- levels(df$rep)
        p <- ggplot()
        for(i in seq(repLev)){
            p <- p +
                geom_ribbon(data = subset(df, rep == repLev[i]),
                            mapping = aes(fraction, ymin=min, ymax=max, group = mrk,
                                          color = NA, fill = mrk),
                            alpha=0.5) +
                geom_line(data = subset(df, rep == repLev[i]),
                          mapping = aes(fraction, mean, group = mrk, color = mrk))
        }
    } else {
        p <-
            ggplot() + geom_ribbon(data = df,
                                   mapping = aes(fraction, ymin=min, ymax=max, group = mrk,
                                                 color = NA, fill = mrk),
                                   alpha=0.5) +
            geom_line(data = df,
                      mapping = aes(fraction, mean, group = mrk, color = mrk))
    }

    ## extract colours for organelles in the data
    col <- c(col, "unknown" = "darkgrey")
    if (is.factor(df$mrk))
      col <- col[levels(df$mrk)]
    else
      col <- col[unique(df$mrk)]

    ## plot data
    p <- p +
        scale_x_discrete(limits=fracLev) +
        ylab("Normalised intensities") + xlab("") +
        scale_fill_manual(values = col, aesthetics = c("fill","colour")) +
        scale_color_manual(values = col, aesthetics = c("fill, colour")) +
        theme_light() +
        theme(panel.spacing = unit(1, "lines"),
              legend.position = "none",
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
              strip.text.x = element_text(size = 10, face="bold"),
              panel.background = element_rect(fill = "gray95"),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 10 + 2, colour = rgb(0, 0, 0)),
              axis.text.y = element_text(size = 10, colour = rgb(0, 0, 0))) +
        facet_wrap(~ mrk, scales = "free_y", ...)
    return(p)
}

## Remap PCs taken from pRoloc
remap <- function(object, ref = 1) {
    stopifnot(inherits(object, "MSnSetList"))
    if (length(unique(lapply(object, sampleNames))) != 1)
        stop(paste("The sampleNames of the datasets in the MSnSetList are different,",
                   "please check your sampleNames of each item in the MSnSetList"))
    x <- msnsets(object)
    refset <- x[[ref]]
    pca1 <- prcomp(exprs(refset), scale = TRUE, center = TRUE)
    preds <- lapply(x, function(xx) {
        ans <- predict(pca1, newdata = exprs(xx))
        ans
    })
    for (i in seq_along(x)) {
        xx <- object@x[[i]]
        sampleNames(xx) <-
            paste0("PC", 1:ncol(xx))
        exprs(xx) <- preds[[i]]
    }
    pca <- pca1$x
    dimnames(pca) <- dimnames(exprs(object@x[[ref]]))
    exprs(object@x[[ref]]) <- pca
    if (validObject(object))
        return(object)
}
