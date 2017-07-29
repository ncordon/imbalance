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
#'
#' @examples
#'data(datasets)
#'abalone19 <- classToNumeric(abalone19)
#'abaloneDiscretized <- discretizeDataset(abalone19)
#'
#'newSamples <- racog(abaloneDiscretized, burnInPeriod = 10, lag = 10,
#'                    iterations = 1000, classAttr = "Class")
#'newSamples <- undiscretizeDataset(abalone19, abaloneDiscretized, newSamples)
#'modifiedDataset <- do.call(rbind.data.frame, list(abalone19, newSamples))
#'
#'plotComparison(abalone19, modifiedDataset, "Class", col=2, c("Length", "Diameter"))
plotComparison <- function(dataset, balancedData, classAttr = "Class", cols = 2, attrs){
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
  fillCells(balancedData, odds = F)
}


discretizeDataset <- function(dataset, classAttr = "Class"){
  classes <- dataset[, classAttr]
  dataset <- discretize(dataset[, names(dataset) != classAttr])

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
#'
#' @examples
#' iris <- iris[1:125, ]
#' whichMinorityClass(iris, "Species")
whichMinorityClass <- function(dataset, classAttr = "Class"){
  classes <- levels(dataset[, classAttr])
  counts <- sapply(classes, function(c){
    length(which(dataset[, classAttr] == c))
  })
  minClass <- factor(classes[which.min(counts)])
  levels(minClass) <- classes
  minClass
}

.appendfactor <- function(x, y){
  as.factor(c(as.character(x), as.character(y)))
}


.sensitivity <- function(prediction, trueClass){
  length(which(prediction == trueClass)) / length(prediction)
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
