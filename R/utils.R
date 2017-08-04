#' Plots comparison between the original and the new balanced dataset.
#'
#' It plots a grid of one to one variables placing the former dataset graphic
#' next to the balanced one, for each pair of attributes.
#'
#' @param dataset A \code{data.frame}. The former and imbalanced dataset.
#' @param anotherDataset A \code{data.frame}. The balanced dataset. \code{dataset}
#'   and \code{anotherDataset} must have the same columns.
#' @param classAttr String. Indicates the class attribute from \code{dataset}.
#'   Must exsits in it.
#' @param cols Integer. It indicates the number of columns of resulting grid.
#'   Must be an even number. Default value of 2.
#' @param attrs Vector of String. Attributes to compare. The function generates
#'   each posible combination of attributes.
#'
#' @return Plot of 2D comparison between the variables.
#' @export
#' @examples
#' data(iris0)
#' set.seed(12345)
#'
#' rwoSamples <- rwo(iris0, numInstances = 100)
#' rwoBalanced <- rbind.data.frame(iris0, rwoSamples)
#' plotComparison(iris0, rwoBalanced, "Class", col=2, names(iris0))
#'
plotComparison <- function(dataset, anotherDataset, classAttr = "Class", cols = 2, attrs){
  if(!is.data.frame(dataset) || !is.data.frame(anotherDataset) ||
     any(! names(dataset) %in% names(anotherDataset)) ||
     any(! names(anotherDataset) %in% names(dataset)))
    stop("dataset and anotherDataset must be data.frames with same structure")
  if(!classAttr %in% names(dataset))
    stop(paste(classAttr, "attribute not found in dataset"))
  if(any(!attrs %in% names(dataset)))
    stop("All attributes in attr parameter must exist in dataset")
  if(cols %% 2 != 0)
    stop("number of cols must be even")

  attrs <- attrs[attrs != classAttr]
  plotGrid <- utils::combn(attrs, 2)
  nPlots <- 2*ncol(plotGrid)
  rows <- ceiling(nPlots/cols)
  # Palette for plots
  colorPalette <-  c("#000000", "#E69F00", "#56B4E9", "#009E73",
                     "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  # Starts new grid page with layout to make the comparison
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(rows, cols)))

  # fill odd or even cells, depending on odds argument
  fillCells <- function(dataset, odds){
    parity <- ifelse(odds, 0, 1)

    sapply(1:ncol(plotGrid), function(index){
      graph <-
        ggplot2::ggplot(dataset, ggplot2::aes_string(plotGrid[1, index],
                                                     plotGrid[2, index],
                        col = classAttr)) + ggplot2::geom_point(alpha = 0.3) +
                        ggplot2::scale_color_manual(values = colorPalette)

      # if(class(dataset[, plotGrid[1,index]]) %in% c("factor", "character"))
      #   graph <- graph + ggplot2::scale_x_continuous(plotGrid[1, index],
      #                                                breaks = NULL,
      #
      vp <- grid::viewport(layout.pos.row = (2 * (index-1) + parity) %/% cols + 1,
                     layout.pos.col = (2 * (index-1) + parity) %% cols + 1)
      print(graph, vp = vp)
    })
  }

  # Fill odd cells first, even ones later
  fillCells(dataset, odds = TRUE)
  fillCells(anotherDataset, odds = FALSE)
  print("Comparative grid plotted")
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
#'   doesn't check that \code{classAttr} exists in \code{dataset}.
#'
#' @return A single \code{factor} with the minority class value.
#' @noRd
.whichMinorityClass <- function(dataset, classAttr = "Class"){
  classes <- unique(dataset[, classAttr])
  counts <- sapply(classes, function(c){
    length(which(dataset[, classAttr] == c))
  })
  classes[which.min(counts)]
}


#' Append factor \code{y} to a given \code{x}, updating levels
#'
#' @param x A \code{factor}.
#' @param y Another \code{factor}.
#'
#' @return A \code{factor} made by combining \code{x} and \code{y}.
#' @noRd
.appendfactor <- function(x, y){
  fcts <- as.factor(c(as.character(x), as.character(y)))
  levels(fcts) <- c(levels(fcts), levels(x))
  fcts
}


#' Computes sensitivity between a \code{prediction} and the real
#' \code{trueLabels}
#'
#' @param prediction A vector of \code{factor} containing predicitions.
#' @param trueLabels Another vector of \code{factor} with the true labels. For
#'   speed purposes it does not check that lengths of \code{prediction} and
#'   \code{trueLabels} match.
#'
#' @return A \code{factor} made by combining \code{x} and \code{y}.
#' @noRd
.sensitivity <- function(prediction, trueLabels){
  length(which(prediction == trueLabels)) / length(prediction)
}


#' Replaces all NAs in a vector \code{a} by the value \code{b}
#'
#' @param a A \code{vector} which may contain some NAs
#' @param b A value for NAs to be replaced for it.
#'
#' @return The vector with NAs replaced by \code{bs.}
#' @noRd
.naReplace <- function(a, b){
  sapply(a, function(x){
    if(is.na(x))
      b
    else
      x
  })
}

#' Given a \code{data.frame} \code{newSamples}, it changes all the columns names
#' by \code{colNames} and appends a column \code{minority.class} with all values
#' as \code{classAttr}. It also renames all rows to be consecutive.
#'
#' @param newSamples A \code{data.frame} containing new examples.
#' @param minorityClass A \code{factor} or \code{character} corresponding to the
#'   minority class.
#' @param colNames A \code{character} vector corresponding to the new column
#'   names we want for \code{newSamples}. For speed purposes, it does not check
#'   that lengths match.
#' @param classAttr A \code{character} containing the class name attribute.
#'
#' @return A \code{data.frame} with described style applied.
#' @noRd
.normalizeNewSamples <- function(newSamples, minorityClass, colNames, classAttr){
  if(nrow(newSamples) > 0){
    names(newSamples) <- colNames
    newSamples[, classAttr] <- minorityClass
  } else
    newSamples <- data.frame()

  rownames(newSamples) <- c()
  newSamples
}


#' Returns for a given \code{dataset} the classes of all of its columns
#'
#' @param dataset A \code{data.frame}.
#' @param exclude A \code{character} vector containing all column names to be
#'   ignored.
#'
#' @return A named vector with all the column classes.
#' @noRd
.colTypes <- function(dataset, exclude = c()){
  types <- sapply(dataset, class)
  types[!names(types) %in% exclude]
}
