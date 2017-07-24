plotComparison <- function(dataset, ampliedDataset, classAttr = "Class", cols = 2, attrs){
  attrs <- attrs[attrs != classAttr]
  plotGrid <- combn(attrs, 2)
  nPlots <- 2*ncol(plotGrid)
  rows <- ceiling(nPlots/cols)
  colorPalette <-  c("#000000", "#E69F00", "#56B4E9", "#009E73",
                     "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  grid.newpage()
  pushViewport(viewport(layout = grid.layout(rows, cols)))

  fillCells <- function(dataset, odds){
    parity <- ifelse(odds, 0, 1)

    sapply(1:ncol(plotGrid), function(index){
      graph <-
        ggplot(dataset, aes_string(plotGrid[1, index], plotGrid[2, index],
                                   col = classAttr)) +
        geom_point(alpha = 0.3) +  scale_color_manual(values = colorPalette)
      vp <- viewport(layout.pos.row = (2 * (index-1) + parity) %/% cols + 1,
                     layout.pos.col = (2 * (index-1) + parity) %% cols + 1)
      print(graph, vp = vp)
    })
  }

  fillCells(dataset, odds = T)
  fillCells(ampliedDataset, odds = F)
}


#' Returns minority class for a dataset
#'
#' Given a \code{dataset} and a \code{classAttr} attribute indicating the class,
#' it calculates the minority class value.
#'
#' @param dataset
#' @param classAttr Attribute indicating class attribute. For speed purposes, it
#' doesn't check that \code{classAttr} exists in \code{dataset}
#'
#' @return the minority class value.
#'
#' @examples
#' iris <- iris[1:125, ]
#' whichMinorityClass(iris, "Species")
whichMinorityClass <- function(dataset, classAttr = "Class"){
  classes <- levels(dataset[, classAttr])
  counts <- sapply(classes, function(c){
    length(which(dataset[, classAttr] == c))
  })
  classes[which.min(counts)]
}


balanceDataset <- function(dataset, ratio, method = c("pdfos", "racog"), class.attr = "class"){
  # Check of arguments
  if (missing(dataset))
    stop("dataset must not be empty")
  if (missing(ratio) || !is.numeric(ratio) || ratio < 1)
    stop("ratio must be a number greater or equal than 1")
  if (!class.attr %in% names(dataset))
    stop(paste("Class attribute '", class.attr, "' not found in dataset", sep = ""))


  classes <- unique(dataset[, class.attr])
  classes.counts <- sapply(classes, function(c){ length(which(dataset[, class.attr] == c)) })
  minority.class <- classes[which.min(classes.counts)]

  minority <- dataset[dataset[, class.attr] == minority.class, ]
  minority.size <- nrow(minority)

  # Delete class attribute
  minority <- minority[, names(minority) != class.attr]


  if (method == "pdfos"){
    n.instances <- minority.size * ratio
    new.samples <- pdfos(minority, n.instances)
    names(new.samples) <- names(minority)
    # Add minority class attribute and bind new instances
    new.samples[ class.attr ] <- minority.class
    result <- rbind(dataset, new.samples)
    rownames(result) <- 1:nrow(result)
  }

  result
}
