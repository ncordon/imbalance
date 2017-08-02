#' Plots comparison between the original and the new balanced dataset.
#'
#' It plots a grid of one to one variables placing the former dataset graphic
#' next to the balanced one, for each pair of attributes.
#'
#' @param dataset A \code{data.frame}. The former and imbalanced dataset.
#' @param balancedData A \code{data.frame}. The balanced dataset. \code{dataset}
#'   and \code{balancedData} must have the same columns.
#' @param classAttr String. Indicates the class attribute from \code{dataset}.
#'   Must exsits in it.
#' @param cols Integer. It indicates the number of columns of resulting grid.
#'   Default value of 2
#' @param attrs Vector of String. Attributes to compare. The function generates
#'   each posible combination of attributes
#'
#' @return plot of 2D comparison between the variables.
#' @export
#' @examples
#' data(haberman)
#' habermanDiscret <- discretizeDataset(haberman)
#'
#' newSamples <- racog(habermanDiscret, burnin = 10, lag = 10,
#'                     iterations = 100, classAttr = "Class")
#' newSamples <- undiscretizeDataset(haberman, habermanDiscret, newSamples)
#' modifiedDataset <- do.call(rbind.data.frame, list(haberman, newSamples))
#'
#' plotComparison(haberman, modifiedDataset, "Class", col=2, names(haberman))
plotComparison <- function(dataset, balancedData, classAttr = "Class", cols = 2, attrs){
  attrs <- attrs[attrs != classAttr]
  plotGrid <- utils::combn(attrs, 2)
  nPlots <- 2*ncol(plotGrid)
  rows <- ceiling(nPlots/cols)
  colorPalette <-  c("#000000", "#E69F00", "#56B4E9", "#009E73",
                     "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(rows, cols)))

  fillCells <- function(dataset, odds){
    parity <- ifelse(odds, 0, 1)

    sapply(1:ncol(plotGrid), function(index){
      graph <-
        ggplot2::ggplot(dataset, ggplot2::aes_string(plotGrid[1, index],
                                                     plotGrid[2, index],
                        col = classAttr)) + ggplot2::geom_point(alpha = 0.3) +
                        ggplot2::scale_color_manual(values = colorPalette)
      vp <- grid::viewport(layout.pos.row = (2 * (index-1) + parity) %/% cols + 1,
                     layout.pos.col = (2 * (index-1) + parity) %% cols + 1)
      print(graph, vp = vp)
    })
  }

  fillCells(dataset, odds = T)
  fillCells(balancedData, odds = F)
  NULL
}


discretizeDataset <- function(dataset, classAttr = "Class"){
  classes <- dataset[, classAttr]
  dataset <- infotheo::discretize(dataset[, names(dataset) != classAttr])

  for(name in names(dataset)){
    dataset[,name] <- as.numeric(dataset[,name])
  }

  dataset[, classAttr] <- as.factor(classes)
  dataset
}


undiscretizeDataset <- function(dataset, discretizedDataset, newSamples, classAttr = "Class"){
  minorityClass <- newSamples[1, classAttr]
  whichAreMinority <- dataset[, classAttr] == minorityClass
  discretizedDataset <- discretizedDataset[whichAreMinority, ]
  dataset <- dataset[whichAreMinority, ]

  newCols <- lapply(1:ncol(newSamples), function(c){
    sapply(newSamples[,c], function(v){
      sample(dataset[ discretizedDataset[,c] == v, c], 1)
    })
  })

  newDataset <- do.call(cbind.data.frame, newCols)
  colnames(newDataset) <- names(newSamples)
  newDataset
}


#' Returns minority class for a dataset.
#'
#' Given a \code{dataset} and a \code{classAttr} attribute indicating the class,
#' it calculates the minority class value.
#'
#' @param dataset
#' @param classAttr Attribute indicating class attribute. For speed purposes, it
#' doesn't check that \code{classAttr} exists in \code{dataset}.
#'
#' @return the minority class value.
#' @keywords internal
#'
#' @examples
#' iris <- iris[1:125, ]
#' whichMinorityClass(iris, "Species")
.whichMinorityClass <- function(dataset, classAttr = "Class"){
  classes <- unique(dataset[, classAttr])
  counts <- sapply(classes, function(c){
    length(which(dataset[, classAttr] == c))
  })
  classes[which.min(counts)]
}

.appendfactor <- function(x, y){
  fcts <- as.factor(c(as.character(x), as.character(y)))
  levels(fcts) <- c(levels(fcts), levels(x))
  fcts
}


.sensitivity <- function(prediction, trueClass){
  length(which(prediction == trueClass)) / length(prediction)
}

# Function to replace a, if it's NA, by b
.naReplace <- function(a, b){
  sapply(a, function(x){
    if(is.na(x))
      b
    else
      x
  })
}

.normalizeNewSamples <- function(newSamples, minorityClass, classAttr){
  if(nrow(newSamples) > 0)
    newSamples[, classAttr] <- minorityClass
  else
    newSamples <- data.frame()

  rownames(newSamples) <- c()
  newSamples
}
