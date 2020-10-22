# ## Update feature data and convert any columns that are matrices to
# ## vectors as otherwise in the shiny app these are displayed as a long
# ## vector of (1, 0, 0, 0, 0, 1, 0) etc.
.makeMatsVecs <- function(msnset) {
  .tn <- length(fvarLabels(msnset))
  chk <- vector(length = .tn)
  for (i in 1:.tn) {
    chk[i] <- is.matrix(fData(msnset)[, i])
  }
  if (any(chk)) {
    .ind <- which(chk)
    .nams <- fvarLabels(msnset)[.ind]
    .tmpnams <- paste0(.nams, format(Sys.time(), "%a%b%d%H%M%S%Y"))
    for (i in seq(.nams)) {
      msnset <- pRoloc::mrkMatToVec(msnset, mfcol = .nams[i],
                                    vfcol = .tmpnams[i])
    }
    fData(msnset)[, .nams] <- NULL
    fvarLabels(msnset)[match(.tmpnams, fvarLabels(msnset))] <- .nams
  }
  return(msnset)
}

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


highlightOnPlot_lisa <- function(coords, myfoi, labels = FALSE,
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
    
    p <- p + 
        scale_x_discrete(limits=fracLev) +
        ylab("Normalised intensities") + xlab("") +
        # scale_fill_manual(values = col, aesthetics = c("fill","colour")) + 
        # scale_color_manual(values = col, aesthetics = c("fill, colour")) +
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

## reuqired to fix sidebaropen on startup (see proceeding functions)
.rightSidebarTabList <- function(...) {
  
  tabItems <- list(...)
  tabItems <- tabItems[[1]]$children
  len <- length(tabItems)
  
  if (len > 0) {
    # generate tab items based on panel items
    tabItemList <- lapply(1:len, FUN = function(i) {
      
      item <- tabItems[[i]]
      id <- item$attribs$id
      id <- gsub(x = id, pattern = "control-sidebar-", replacement = "")
      id <- gsub(x = id, pattern = "-tab", replacement = "")
      active <- sum(grep(x = item$attribs$class, pattern = "active")) == 1
      icon <- item$attribs$icon
      
      .rightSidebarTabItem(id = id, icon = icon, active = active)
    })
    
    # put everything inside the container
    shiny::tags$ul(
      class = "nav nav-tabs nav-justified control-sidebar-tabs",
      tabItemList
    )
  }
}
.rightSidebarTabItem <- function(id, icon, active) {
  
  stopifnot(!is.null(id))
  
  shiny::tags$li(
    class = if (isTRUE(active)) "active" else NULL,
    shiny::tags$a(
      href = paste0("#control-sidebar-", id, "-tab"), 
      `data-toggle` = "tab",
      shiny::tags$i(class = paste0("fa fa-", icon))
    )
  )
}
.rightSidebarPanel <- function(...) {
  shiny::tags$div(
    class = "controlbar tab-content",
    ...
  )
}


## customise the right sidebar so it is open on startup
my_rightSidebar <- function(..., background = "dark", width = 230, .items = NULL) {
    
    panels <- list(...)
    
    sidebarTag <- shiny::tags$div(
        id = "controlbar",
        shiny::tags$aside(
            class = paste0("control-sidebar control-sidebar-", background),
            style = paste0("width: ", width, "px;"),
            # automatically create the tab menu
            if (length(panels) > 0) .rightSidebarTabList(.rightSidebarPanel(...)),
            if (length(panels) > 0) .rightSidebarPanel(...) else .rightSidebarPanel(.items)
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
