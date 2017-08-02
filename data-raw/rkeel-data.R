if(!"RKEEL" %in% installed.packages()[,1])
  install.packages("RKEEL")

datasetNames <- c("abalone19", "ecoli1", "glass-0-1-6_vs_2", "haberman",
                  "iris0", "wisconsin", "yeast4", "yeast6")

dfnormalize <- function(dataset){
  for(name in names(dataset)){
    if(name != "Class"){
      # read.keel reads numeric columns as factor and
      # character columns as factors
      if(is.factor(dataset[,name])){
        dataset[, name] <- as.numeric(as.character(dataset[,name]))
      }
      else if(is.factor(dataset[,name])){
        dataset[, name] <- as.character(dataset[,name])
      }
    } else{
      dataset[, name] <- as.factor(dataset[, name])
    }
  }
  dataset
}

# Read data and cleanse it
abalone19 <- dfnormalize(RKEEL::read.keel("../Data/abalone19.dat"))
ecoli1 <- dfnormalize(RKEEL::read.keel("../Data/ecoli1.dat"))
glass <- dfnormalize(RKEEL::read.keel("../Data/glass-0-1-6_vs_2.dat"))
haberman <- dfnormalize(RKEEL::read.keel("../Data/haberman.dat"))
iris0 <- dfnormalize(RKEEL::read.keel("../Data/iris0.dat"))
wisconsin <- dfnormalize(RKEEL::read.keel("../Data/wisconsin.dat"))
yeast4 <- dfnormalize(RKEEL::read.keel("../Data/yeast4.dat"))
yeast6 <- dfnormalize(RKEEL::read.keel("../Data/yeast6.dat"))
