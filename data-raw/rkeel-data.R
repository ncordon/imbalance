if(!"RKEEL" %in% installed.packages()[,1])
  install.packages("RKEEL")

dfnormalize <- function(dataset){
  for(name in names(dataset)){
    if(name != "Class"){
      # read.keel reads numeric columns as factor and
      # character columns as factors
      if(is.factor(dataset[,name])){
        dataset[, name] <- as.numeric(as.character(dataset[,name]))
      }
    } else{
      dataset[, name] <- as.factor(dataset[, name])
    }
  }
  dataset
}

# Read data and cleanse it
ecoli1 <- dfnormalize(RKEEL::read.keel("../Data/ecoli1.dat"))
haberman <- dfnormalize(RKEEL::read.keel("../Data/haberman.dat"))
iris0 <- dfnormalize(RKEEL::read.keel("../Data/iris0.dat"))
glass0 <- dfnormalize(RKEEL::read.keel("../Data/glass0.dat"))
newthyroid1 <- dfnormalize(RKEEL::read.keel("../Data/new-thyroid1.dat"))
wisconsin <- RKEEL::read.keel("../Data/wisconsin.dat")
yeast4 <- dfnormalize(RKEEL::read.keel("../Data/yeast4.dat"))


# abalone19 <- dfnormalize(RKEEL::read.keel("../Data/abalone19.dat"))
# abalone9.18 <- dfnormalize(RKEEL::read.keel("../Data/abalone9-18.dat"))
# ecoli2 <- dfnormalize(RKEEL::read.keel("../Data/ecoli2.dat"))
# ecoli3 <- dfnormalize(RKEEL::read.keel("../Data/ecoli3.dat"))
# ecoli4 <- dfnormalize(RKEEL::read.keel("../Data/ecoli4.dat"))
# ecoli.0.1.3.7.vs.2.6 <- dfnormalize(RKEEL::read.keel("../Data/ecoli-0-1-3-7_vs_2-6.dat"))
# ecoli.0.vs.1 <- dfnormalize(RKEEL::read.keel("../Data/ecoli-0_vs_1.dat"))
# glass.0.1.6.vs.2 <- dfnormalize(RKEEL::read.keel("../Data/glass-0-1-6_vs_2.dat"))
# glass.0.1.6.vs.5 <- dfnormalize(RKEEL::read.keel("../Data/glass-0-1-6_vs_5.dat"))
# glass1 <- dfnormalize(RKEEL::read.keel("../Data/glass1.dat"))
# glass2 <- dfnormalize(RKEEL::read.keel("../Data/glass2.dat"))
# glass4 <- dfnormalize(RKEEL::read.keel("../Data/glass4.dat"))
# glass5 <- dfnormalize(RKEEL::read.keel("../Data/glass5.dat"))
# glass6 <- dfnormalize(RKEEL::read.keel("../Data/glass6.dat"))
# glass.0.1.2.3.vs.4.5.6 <- dfnormalize(RKEEL::read.keel("../Data/glass-0-1-2-3_vs_4-5-6.dat"))
# newthyroid2 <- dfnormalize(RKEEL::read.keel("../Data/newthyroid2.dat"))
# pageblocks0 <- dfnormalize(RKEEL::read.keel("../Data/page-blocks0.dat"))
# pageblocks.1.3.vs.4 <- dfnormalize(RKEEL::read.keel("../Data/page-blocks-1-3_vs_4.dat"))
# pima <- dfnormalize(RKEEL::read.keel("../Data/pima.dat"))
# segment0 <- dfnormalize(RKEEL::read.keel("../Data/segment0.dat"))
# shuttle.c0.vs.c4 <- dfnormalize(RKEEL::read.keel("../Data/shuttle-c0-vs-c4.dat"))
# shuttle.c2.vs.c4 <- dfnormalize(RKEEL::read.keel("../Data/shuttle-c2-vs-c4.dat"))
# vehicle0 <- dfnormalize(RKEEL::read.keel("../Data/vehicle0.dat"))
# vehicle1 <- dfnormalize(RKEEL::read.keel("../Data/vehicle1.dat"))
# vehicle2 <- dfnormalize(RKEEL::read.keel("../Data/vehicle2.dat"))
# vehicle3 <- dfnormalize(RKEEL::read.keel("../Data/vehicle3.dat"))
# vowel0 <- dfnormalize(RKEEL::read.keel("../Data/vowel0.dat"))
# yeast1 <- dfnormalize(RKEEL::read.keel("../Data/yeast1.dat"))
# yeast3 <- dfnormalize(RKEEL::read.keel("../Data/yeast3.dat"))
# yeast5 <- dfnormalize(RKEEL::read.keel("../Data/yeast5.dat"))
# yeast6 <- dfnormalize(RKEEL::read.keel("../Data/yeast6.dat"))
# yeast.0.5.6.7.9.vs.4 <- dfnormalize(RKEEL::read.keel("../Data/yeast-0-5-6-7-9_vs_4.dat"))
# yeast.1.2.8.9.vs.7 <- dfnormalize(RKEEL::read.keel("../Data/yeast-1-2-8-9_vs_7.dat"))
# yeast.1.4.5.8.vs.7 <- dfnormalize(RKEEL::read.keel("../Data/yeast-1-4-5-8_vs_7.dat"))
# yeast.2.vs.4 <- dfnormalize(RKEEL::read.keel("../Data/yeast-2_vs_4.dat"))
# yeast.1.vs.7 <- dfnormalize(RKEEL::read.keel("../Data/yeast-1_vs_7.dat"))
# yeast.2.vs.8 <- dfnormalize(RKEEL::read.keel("../Data/yeast-2_vs_8.dat"))

# Further modifications
ecoli1$Lip <- as.factor(ecoli1$Lip)
ecoli1$Chg <- as.factor(ecoli1$Chg)
yeast4$Erl <- as.factor(yeast4$Erl)
newthyroid1[, -6] <- sapply(newthyroid1[, -6], as.integer)
wisconsin[, -10] <- as.data.frame(lapply(wisconsin[, -10], as.factor))
# abalone19$Sex <- as.factor(abalone19$Sex)
# abalone9.18$Sex <- as.factor(abalone9.18$Sex)

# Write to data folder
dataArgs <- as.list( c(sapply(ls(), as.name), overwrite = TRUE) )
do.call(devtools::use_data, dataArgs)

