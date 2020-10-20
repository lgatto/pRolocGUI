# narrowFeatureData <- function(object,
#                               n1 = 6, n2 = 6,
#                               fcol = "markers") {
#     if (length(fcol) > 1)
#         fcol <- NULL
#     if (is.null(fcol)) {
#         i <- selectFeatureData(object)
#         fData(object) <- fData(object)[, i]
#         return(object)
#     }
#     n <- n1 + n2
#     fv <- fvarLabels(object)
#     ln <- length(fv)
#     if (ln <= n) return(object)
#     i <- c(1:n1, (ln-n2+1):ln)
#     if (any(fcol %in% fv) & !any(fcol %in% fv[i]))
#         i <- c(1:n1, (ln-n2+2):ln, match(fcol, fv))
#     fData(object) <- fData(object)[, i]
#     if (validObject(object))
#         return(object)
# }
# 
# mrkVecToMat_lisa <-
#     function (fd, vfcol = "markers", mfcol = "Markers") 
#     {
#         fvl <- colnames(fd)
#         if (!vfcol %in% fvl) 
#             stop(vfcol, " does not exist.")
#         if (mfcol %in% fvl) 
#             stop(mfcol, " already present.")
#         m <- fd[, vfcol]
#         um <- levels(factor(m))
#         if ("unknown" %in% um) 
#             um <- um[um != "unknown"]
#         M <- matrix(0, nrow = nrow(fd), ncol = length(um))
#         rownames(M) <- rownames(fd)
#         colnames(M) <- um
#         for (j in um) M[which(j == m), j] <- 1
#         fd[, mfcol] <- M
#         return(fd)
#     }


stockcol <- c("#E41A1C", "#377EB8", "#309C17", "#FF7F00", "#FFD700", "#00CED1",
              "#A65628", "#F781BF", "#984EA3", "#9ACD32", "#B0C4DE", "#00008A",
              "#FDAE6B", "#EBB7BE", "#3F8F8F", "#CF9802", "#6A51A3", "#21E8AC",
              "#0000FF", "#1D7A3E", "#BF2A6B", "#CD5B45", "#808000", "#F21D56",
              "#67000D", "#7A0C79", "#93EDF5", "#A66A6A", "#0E438A", "#DBBCF7")
getStockcol <- function() {stockcol}


# getStockcol2 <- function() {c(
#   "#C9090B", "#256C9D", "#137333", "#EF7905", "#ECC803",
#   "#02AFB1", "#984B1E", "#DB69A5", "#7A3684", "#80AC24", "#9DB1CC",
#   "#010169", "#736249", "#EA9F5F", "#55AD91", "#1B4C10", "#B16F78",
#   "#4F3981", "#CA9209", "#0202B1", "#77A121", "#AE4975", "#C7523B",
#   "#6E013F", "#5F5F00", "#450109", "#340166", "#5695BB", "#E58061")}

getStockcol2 <- function() {darken(getStockcol())}

plot2D_lisa <- function(coords, fd, fcol = fcol,
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
           bg =  paste0("#C8C8C8"),
           col = paste0("#505050"),
           pch = 21, cex = .8, lwd = .7)
    cl <- names(table(fd[, fcol]))
    cl <- cl[cl != "unknown"]
    if (!unk) {
        for (i in seq(cl)) {
            ind <- which(fd[, fcol] == cl[i])
            points(coords[ind, ,drop = FALSE], bg = getStockcol()[i], pch = 21, 
                   col = paste0(getStockcol()[i], 60), cex = 1.5)
        }
    }
    
}


highlightOnPlot <- function(coords, fd, myfoi, labels = FALSE,
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


setStockcol <- function (cols) {
    prevcols <- getStockcol()
    if (is.null(cols)) {
        assign("stockcol", stockcol)
    }
    else if (cols[1] == "lisacol") {
        setLisacol()
    }
    else assign("stockcol", cols)
    invisible(prevcols)
}

.defineDT <- function(labs, fcol) {
    if (length(labs) > 6) {
        .ind <- which(labs == fcol)
        .fvarL <- labs[-.ind]
        selDT <- c(.fvarL[1:5], fcol)
    } else {
        selDT <- labs[1:length(labs)]
    }
    return(selDT)
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
makeDF <- function(data = profs, fcol = fcol, fd = fd, pd = pd, replicate.column.name) {
    if (missing(replicate.column.name)) {
        # message(paste("Replicate information not provided, assuming 1 replicate only"))
        repInfo <- rep(1, ncol(data))
    } else {
        repInfo <- pd[, replicate.column.name]
    }
    
    ## prep data for ggplot 
    .rn <- rownames(data)
    .cn <- colnames(data)
    plot_data <- data.frame(id = rep(.rn, ncol(data)),  
                            fraction = rep(.cn, each = nrow(data)), # variable
                            intensities = as.vector(data),  # value
                            rep = factor(rep(repInfo, each = nrow(data))),
                            mrk = rep(fd[, fcol], ncol(data)))
    plot_data <- within(plot_data, fraction <- factor(fraction, levels = colnames(data)))
    return(plot_data)
}


# ## Get average intensity profiles and run hclust - see example in ?plotConsProfiles 
# clusterFracMatrix <- function(plot_data, reps = FALSE) {
#     if (reps) {
#         repInfo <- unique(plot_data[, "rep"])
#     } else {
#         repInfo = 1
#     }
#     averMat <- vector("list", length(repInfo))
#     for (i in seq(repInfo)) {
#         averMat[[i]] <- plot_data %>%
#             filter(rep == repInfo[i]) %>%
#             filter(mrk!="unknown") %>% # pointless averaging unknown proteins in this representation
#             group_by(mrk, fraction) %>%
#             summarise(intensities=mean(intensities), .groups = "keep")
#         # Get marker set ordering according to hierarchical clustering
#         # As currently implemented, requires reshape to wide-format matrix for dist
#         average_profile_matrix <- averMat[[i]] %>% spread(key=fraction, value=intensities) %>% data.frame()
#         rownames(average_profile_matrix) <- average_profile_matrix$mrk
#         average_profile_matrix <- average_profile_matrix %>% dplyr::select(-mrk)
#         clust <- hclust(dist(as.matrix(average_profile_matrix)))
#         my_order <- rownames(average_profile_matrix)[clust$order]
#         # Re-level factor by new marker set order
#         averMat[[i]]$mrk <- factor(averMat[[i]]$mrk, my_order)
#         averMat[[i]]$rep <- repInfo[i]
#     }
#     averMat <- do.call(rbind, averMat)
#     return(averMat)
# }
# 
# ## plot heatmap of marker profiles
# ## 
# plotHeatmap <- function(averMat) {
#     g <- ggplot() + geom_tile(data = averMat, 
#                               aes(factor(fraction), mrk, fill=intensities)) +
#         scale_fill_continuous(low="white", high="#53788F", name="Normalised intensities") +
#         facet_wrap(rep~., scales = "free_x", labeller = as_labeller(c("1"="Replicate 1",
#                                                                       "2"="Replicate 2",
#                                                                       "3"="Replicate 3"))) +
#         xlab("Fractions") +
#         theme_light() +
#         theme(strip.text.x = element_text(size = 14),
#               strip.background = element_rect(fill="#E0E0E0"),
#               axis.title = element_text(size=14),
#               legend.text = element_text(size=12),
#               axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14),
#               axis.text.y = element_text(size = 14),
#               # axis.text.x = element_blank(), 
#               # axis.title.x = element_blank(),
#               axis.title.x = element_text(margin = margin(10,0,0,0))     # remove facet strips
#         ) +
#         ylab("Classes")
#     return(g)
# }
# 
# ## generate boxplot of intensities
# ggboxplot <- function(plot_data) {
#     q <- ggplot(data = plot_data, aes(fraction, intensities, group = fraction)) + 
#         geom_boxplot(fill="#53788F") + 
#         # scale_fill_manual("gray") + 
#         theme_light() +
#         xlab("Fractions") +
#         ylab("Normalised intensities") +
#         theme(legend.position = "none", 
#               axis.title = element_text(size=14),
#               strip.text.x = element_text(size = 14),
#               strip.background = element_rect(fill="#E0E0E0"),
#               # strip.text = element_blank(),
#               axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14),
#               axis.text.y = element_text(size = 14),
#               axis.title.x = element_text(margin = margin(10,0,0,0)))     # remove facet strips
#     return(q) 
# } 

## facet of all profile plots
plotAllLayers<-function(df, col, reps = FALSE, ...){
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

## customise the right sidebar so it is open on startup
rightSidebar <- function(..., background = "dark", width = 230, .items = NULL) {
    
    panels <- list(...)
    
    sidebarTag <- shiny::tags$div(
        id = "controlbar",
        shiny::tags$aside(
            class = paste0("control-sidebar control-sidebar-", background),
            style = paste0("width: ", width, "px;"),
            # automatically create the tab menu
            if (length(panels) > 0) shinydashboardPlus:::rightSidebarTabList(shinydashboardPlus:::rigthSidebarPanel(...)),
            if (length(panels) > 0) shinydashboardPlus:::rigthSidebarPanel(...) else shinydashboardPlus:::rigthSidebarPanel(.items)
        ),
        # Add the sidebar background. This div must be placed
        # immediately after the control sidebar
        shiny::tags$div(class = "control-sidebar-bg", style = paste0("width: ", width, "px;"))
    )
    
    shiny::tagList(
        shiny::singleton(
            shiny::tags$head(
                # custom css to correctly handle the width of the rightSidebar
                shiny::tags$style(
                    shiny::HTML(
                        paste0(
                            ".control-sidebar-bg,
               .control-sidebar {
                  top: 0;
                  right: ", -width, "px;
                  width: ", width, "px;
                  -webkit-transition: right 0.3s ease-in-out;
                  -o-transition: right 0.3s ease-in-out;
                  transition: right 0.3s ease-in-out;
                }
                .control-sidebar-open .content-wrapper,.control-sidebar-open .main-footer,.control-sidebar-open .right-side{
                  margin-right:",width,"px
                }"
                        )
                    )
                )
            )
        ),
        sidebarTag
    )
}