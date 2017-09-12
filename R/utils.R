#' Plots comparison between the original and the new balanced dataset.
#'
#' It plots a grid of one to one variable comparison, placing the former dataset
#' graphics next to the balanced one, for each pair of attributes.
#'
#' @param dataset A \code{data.frame}. The former imbalanced dataset.
#' @param anotherDataset A \code{data.frame}. The balanced dataset.
#'   \code{dataset} and \code{anotherDataset} must have the same columns.
#' @param attrs Vector of \code{character}. Attributes to compare. The function
#'   generates each posible combination of attributes to build the comparison.
#' @param cols Integer. It indicates the number of columns of resulting grid.
#'   Must be an even number. By default, 2.
#' @param classAttr \code{character}. Indicates the class attribute from
#'   \code{dataset}. Must exist in it.
#'
#' @return Plot of 2D comparison between the variables.
#' @export
#' @examples
#' data(iris0)
#' set.seed(12345)
#'
#' rwoSamples <- rwo(iris0, numInstances = 100)
#' rwoBalanced <- rbind(iris0, rwoSamples)
#' plotComparison(iris0, rwoBalanced, names(iris0), cols = 2, classAttr = "Class")
#'
plotComparison <- function(dataset, anotherDataset, attrs, cols = 2, classAttr = "Class"){
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
  nPlots <- 2 * ncol(plotGrid)
  rows <- ceiling(nPlots/cols)
  # Palette for plots
  colorPalette <-  c("#E69F00", "#000000", "#56B4E9", "#009E73",
                     "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  # Starts new grid page with layout to make the comparison
  grid::grid.newpage()
  myLayout = grid::grid.layout(rows, cols) #, heights = c(1, rep(5, rows)))
  grid::pushViewport(grid::viewport(layout = myLayout))

  # Fill odd or even cells, depending on odds argument
  fillCells <- function(dataset, odds){
    parity <- ifelse(odds, 0, 1)
    titleText <- ifelse(odds, "Original dataset", "Modified dataset")

    sapply(seq_len(ncol(plotGrid)), function(index){
      graph <-
        ggplot2::ggplot(dataset, ggplot2::aes_string(plotGrid[1, index],
                                                     plotGrid[2, index],
                        col = classAttr)) + ggplot2::geom_point(alpha = 0.3) +
                        ggplot2::scale_color_manual(values = colorPalette)
                        # + ggplot2::theme(legend.position = "none")

      # Calc position on the grid
      rowPos = (2 * (index-1) + parity) %/% cols + 1
      colPos = (2 * (index-1) + parity) %% cols + 1

      # If first row of plots, add a title pointing out original or modified dataset
      if(rowPos == 1)
        graph <- graph + ggplot2::ggtitle(titleText)

      # Plot graph to grid
      vp <- grid::viewport(layout.pos.row = rowPos, layout.pos.col = colPos)
      print(graph, vp = vp)
    })
  }

  # Fill odd cells first, even ones later
  fillCells(dataset, odds = TRUE)
  fillCells(anotherDataset, odds = FALSE)

  # Return no result
  invisible(dataset)
}


#' Returns minority class for a dataset.
#'
#' Given a \code{dataset} and a \code{classAttr} attribute indicating the class,
#' it calculates the minority class value.
#'
#' @param dataset A \code{data.frame}.
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
#' @param minorityClass A \code{factor} corresponding to the minority class.
#'
#' @return A \code{factor} made by combining \code{x} and \code{y}.
#' @noRd
sensitivity <- function(prediction, trueLabels, minorityClass){
  length(which(prediction == trueLabels & trueLabels == minorityClass)) /
    length(which(prediction == minorityClass))
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


#' Returns for a given \code{dataset} the classes of all of its columns
#'
#' @param dataset A \code{data.frame}.
#' @param exclude \code{character} vector containing columns to be excluded
#'
#' @return A named vector with all the column classes.
#' @noRd
.colTypes <- function(dataset, exclude = c()){
  types <- sapply(dataset, class)
  types[!names(types) %in% exclude]
}



#' Convert columns of a dataset to numeric
#'
#' @param dataset A \code{data.frame} whose columns are to be converted to
#'   numeric type
#' @param exclude \code{character} vector containing columns to be excluded
#'
#' @return The dataset whit factor columns converted
#'
#' @noRd
toNumeric <- function(dataset, exclude = c()){
  colTypes <- .colTypes(dataset, exclude)
  colFactor <- which(colTypes != "numeric")

  dataset[, colFactor] <- sapply(dataset[, colFactor], function(col){
    if(class(col) == "factor"){
      col <- as.numeric(levels(col))[col]
    }
    else{
      col <- as.numeric(col)
    }
    if(anyNA(col)){
      stop("Cannot convert columns to numeric")
    } else{
      col
    }
  })

  dataset
}


#' Check that a class attribute belongs to a dataset
#'
#' @param dataset A \code{data.frame} to check.
#' @param classAttr A \code{character} containing the name of the class column.
#' @param name The name for the \code{dataset}.
#'
#' @noRd
checkDatasetClass <- function(dataset, classAttr, name){
  if(!classAttr %in% names(dataset))
    stop(paste(classAttr, "attribute not found in", name))
}


#' Check that a param is \code{data.frame}
#'
#' @param dataset A \code{data.frame} to check.
#' @param name The name for the \code{dataset}.
#'
#' @noRd
checkDataset <- function(dataset, name){
  if(!is.data.frame(dataset))
    stop(paste(name, "must be a data.frame"))
}



#' Select numInstances randomly (in case we have generated more instances than
#' required)
#'
#' @param newSamples A \code{matrix} or \code{data.frame}
#' @param numInstances Integer. Number of instances to select from newSamples
#'
#' @noRd
selectSamples <- function(newSamples, numInstances){
  indexes <- sample(seq_len(nrow(newSamples)), numInstances, replace = F)
  newSamples <- newSamples[indexes, ]
}


#' Check that all columns of a given \code{data.frame} are numeric
#'
#' @param dataset A \code{data.frame}
#' @param exclude \code{character} vector containing columns to be excluded
#' @param name The name for the \code{dataset}.
#' @return The dataset whit factor columns converted
#'
#' @noRd
checkAllColumnsNumeric <- function(dataset, exclude = c(), name){
  if(any(! .colTypes(dataset, exclude) %in% c("numeric", "integer")))
    stop(paste("all columns of", name, "must be numeric or numeric factors"))
}



datasetStructure <- function(dataset, classAttr){
  datasetShape <- list()
  datasetShape$minorityClass <- .whichMinorityClass(dataset, classAttr)
  datasetShape$classAttr <- classAttr
  datasetShape$colNames <- names(dataset)
  datasetShape$colTypes <- .colTypes(dataset)
  class(datasetShape) <- "datasetStructure"
  datasetShape
}

whichMinority <- function(dataset, classAttr){
  minorityClass <- .whichMinorityClass(dataset, classAttr)
  which(dataset[, classAttr] == .whichMinorityClass(dataset, classAttr))
}

selectMinority <- function(dataset, classAttr){
  minority <- dataset[whichMinority(dataset, classAttr),
                      names(dataset) != classAttr]
  minority
}

selectMajority <- function(dataset, classAttr){
  majority <- dataset[-whichMinority(dataset, classAttr),
                       names(dataset) != classAttr]
  majority
}

normalizeNewSamples <- function(originalShape, ...){
  UseMethod("normalizeNewSamples")
}

#' Given a \code{data.frame} \code{newSamples}, it changes all the columns names
#' by \code{colNames} and appends a column \code{minority.class} with all values
#' as \code{classAttr}. It also renames all rows to be consecutive and converts
#' the columns to the types specified in \code{colTypes}
#'
#' @param newSamples A \code{data.frame} containing new examples.
#' @param minorityClass A \code{factor} or \code{character} corresponding to the
#'   minority class.
#' @param colNames A \code{character} vector corresponding to the new column
#'   names we want for \code{newSamples}. For speed purposes, it does not check
#'   that lengths match.
#' @param classAttr A \code{character} containing the class name attribute.
#' @param colTypes A vector of \code{character} containing the original types of
#'   the columns, if a conversion is needed
#'
#' @return A \code{data.frame} with described style applied.
#' @noRd
normalizeNewSamples.datasetStructure <- function(originalShape, newSamples){
  colTypes <- originalShape$colTypes
  colNames <- originalShape$colNames
  minorityClass <- originalShape$minorityClass
  classAttr <- originalShape$classAttr

  if(nrow(newSamples) > 0){
    rownames(newSamples) <- c()
    newSamples <- data.frame(newSamples)
    names(newSamples) <- colNames[colNames != classAttr]
    newSamples[, classAttr] <- minorityClass
    newSamples <- newSamples[, colNames]

    newSamples <- mapply(function(col, type){
      eval(parse(text = paste("as.", type, "(col)", sep = "")))
    }, as.list(newSamples), colTypes, SIMPLIFY = FALSE)

    newSamples <- data.frame(newSamples)
  } else{
    newSamples <- data.frame()
  }

  newSamples
}

checkSameShape <- function(first, second, firstName, secondName){
  if(any(! names(first) %in% names(second)) ||
     any(! names(second) %in% names(first))){
    stop(paste(firstName, "and", secondName, "must have the same column names"))
  }
}
