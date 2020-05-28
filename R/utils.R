narrowFeatureData <- function(object,
                              n1 = 6, n2 = 6,
                              fcol = "markers") {
  if (length(fcol) > 1)
    fcol <- NULL
  if (is.null(fcol)) {
    i <- selectFeatureData(object)
    fData(object) <- fData(object)[, i]
    return(object)
  }
  n <- n1 + n2
  fv <- fvarLabels(object)
  ln <- length(fv)
  if (ln <= n) return(object)
  i <- c(1:n1, (ln-n2+1):ln)
  if (any(fcol %in% fv) & !any(fcol %in% fv[i]))
    i <- c(1:n1, (ln-n2+2):ln, match(fcol, fv))
  fData(object) <- fData(object)[, i]
  if (validObject(object))
    return(object)
}


## plotting functions for main plot
.plot <- function(coords, fd, fcol = fcol,
                  unk = FALSE, ...) {
  # Set background black on plot
  scheme <- "white"
  scheme2 <- "black"
  par(bg = scheme, col.axis = scheme2, col.main = scheme2, 
      col.lab = scheme2, fg = scheme2)
  .data <- coords
  .xlab <- colnames(.data)[1]
  .ylab <- colnames(.data)[2]
  plot(.data, xlab = .xlab, ylab = .ylab,
       type = "n", ...)
  ukn <- which(fd[, fcol] == "unknown")
  points(.data[ukn, 1], .data[ukn, 2],
         bg =  darken(paste0("#C8C8C8")),
         col = paste0("#C8C8C8"),
         pch = 21, cex = .8, lwd = .7)
  cl <- names(table(fd[, fcol]))
  cl <- cl[cl != "unknown"]
  if (!unk) {
    for (i in seq(cl)) {
      ind <- which(fd[, fcol] == cl[i])
      points(coords[ind, ], bg = getStockcol()[i], pch = 21, 
             col = paste0(getStockcol()[i], 60), cex = 1.5)
    }
  }
  
}


.highlight <- function(coords, myfoi, 
                       labels = FALSE) {
  scheme <- "white"
  scheme2 <- "black"
  .data <- coords
  points(.data[myfoi, 1], .data[myfoi, 2],
         col = scheme2,
         pch = 21, cex = 1, lwd = 1.3)
  if (labels) {
    text(.data[myfoi, 1], .data[myfoi, 2], myfoi, pos = 3, 
         font  = 2, cex = 1.2) 
  }
}


## css for selectizeInput colour coding of menus
CSS <- function(values, colors){
  template <- "
.option[data-value='%s'], .item[data-value='%s']{
  background: %s !important;
  color: white !important;
}"
  paste0(
    apply(cbind(values, colors), 1, function(vc){
      sprintf(template, vc[1], vc[1], vc[2])
    }),
    collapse = "\n"
  )
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
  table.search(searchString, true).draw(true);
});
'