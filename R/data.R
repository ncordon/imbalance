#' Imbalanced binary ecoli protein localization sites
#'
#' Imbalanced binary dataset containing protein traits for predicting their cellular
#' localization sites.
#'
#' @format A data frame with 336 instances, 77 of which belong to positive class,
#' and 8 variables:
#'
#' \describe{
#'   \item{Mcg}{McGeoch's method for signal sequence recognition.
#'     Continuous attribute.}
#'   \item{Gvh}{Von Heijne's method for signal sequence recognition.
#'     Continuous attribute.}
#'   \item{Lip}{von Heijne's Signal Peptidase II consensus sequence score.
#'     Discrete attribute.}
#'   \item{Chg}{Presence of charge on N-terminus of predicted lipoproteins.
#'     Discrete attribute.}
#'   \item{Aac}{Score of discriminant analysis of the amino acid content of outer
#'     membrane and periplasmic proteins. Continuous attribute.}
#'   \item{Alm1}{Score of the ALOM membrane spanning region prediction program.
#'     Continuous attribute.}
#'   \item{Alm2}{score of ALOM program after excluding putative cleavable signal
#'     regions from the sequence. Continuous attribute.}
#'   \item{Class}{Two possible classes: positive (type im), negative (the rest).}
#' }
#'
#' @source \href{http://sci2s.ugr.es/keel/datasets.php}{KEEL Repository}.
#' @seealso Original available in \href{https://archive.ics.uci.edu/ml}{UCI ML Repository}.
"ecoli1"


#' Imbalanced binary glass identification
#'
#' Imbalanced binary classification dataset containing variables to
#' identify types of glass.
#'
#' @format A data frame with 214 instances, 70 of which belong to positive class,
#' and 10 variables:
#'
#' \describe{
#'   \item{RI}{Refractive Index. Continuous attribute.}
#'   \item{Na}{Sodium, weight percent in component. Continuous attribute.}
#'   \item{Mg}{Magnesium, weight percent in component. Continuous attribute.}
#'   \item{Al}{Aluminum, weight percent in component. Continuous attribute.}
#'   \item{Si}{Silicon, weight percent in component. Continuous attribute.}
#'   \item{K}{Potasium, weight percent in component. Continuous attribute.}
#'   \item{Ca}{Calcium, weight percent in component. Continuous attribute.}
#'   \item{Ba}{Barium, weight percent in component. Continuous attribute.}
#'   \item{Fe}{Iron, weight percent in component. Continuous attribute.}
#'   \item{Class}{Two possible glass types: positive (building windows, float processed)
#'     and negative (the rest).}
#' }
#'
#' @source \href{http://sci2s.ugr.es/keel/datasets.php}{KEEL Repository}.
#' @seealso Original available in \href{https://archive.ics.uci.edu/ml}{UCI ML Repository}.
"glass0"


#' Haberman's survival data
#'
#' The dataset contains cases from a study that was conducted between
#' 1958 and 1970 at the University of Chicago's Billings Hospital on
#' the survival of patients who had undergone surgery for breast
#' cancer.
#'
#' @format A data frame with 306 instances, 81 of which belong to positive class,
#' and 4 variables:
#'
#' \describe{
#'   \item{Age}{Age of patient at time of operation. Discrete attribute.}
#'   \item{Year}{Patient's year of operation. Discrete attribute.}
#'   \item{Positive}{Number of positive axillary nodes detected. Discrete attribute.}
#'   \item{Class}{Two possible survival status: positive(survival rate of less than 5 years),
#'     negative (survival rate or more than 5 years).}
#' }
#'
#' @source \href{http://sci2s.ugr.es/keel/datasets.php}{KEEL Repository}.
#' @seealso Original available in \href{https://archive.ics.uci.edu/ml}{UCI ML Repository}.
"haberman"


#' Imbalanced binary thyroid gland data
#'
#' Data to predict patient's hyperthyroidism.
#'
#' @format A data frame with 215 instances, 35 of which belong to positive class,
#' and 6 variables:
#'
#' \describe{
#'   \item{T3resin}{T3-resin uptake test, percentage. Discrete attribute.}
#'   \item{Thyroxin}{Total Serum thyroxin as measured by the isotopic
#'     displacement method. Continuous attribute.}
#'   \item{Triiodothyronine}{Total serum triiodothyronine as measured by radioimmuno
#'     assay. Continuous attribute.}
#'   \item{Thyroidstimulating}{Basal thyroid-stimulating hormone (TSH) as measured by
#'     radioimmuno assay. Continuous attribute.}
#'   \item{TSH_value}{Maximal absolute difference of TSH value after injection of 200
#'     micro grams of thyrotropin-releasing hormone as compared to the basal value.
#'     Continuous attribute.}
#'   \item{Class}{Two possible classes: positive as hyperthyroidism, negative as non
#'     hyperthyroidism.}
#' }
#'
#' @source \href{http://sci2s.ugr.es/keel/datasets.php}{KEEL Repository}.
#' @seealso Original available in \href{https://archive.ics.uci.edu/ml}{UCI ML Repository}.
"newthyroid1"


#' Imbalanced binary iris dataset
#'
#' Modification of \code{\link[datasets]{iris}} dataset. Measurements in
#' centimeters of the variables sepal length and width and petal length and
#' width, respectively, for 50 flowers from each of 3 species of iris. The
#' possible classifications are positive (setosa) and negative (versicolor +
#' virginica).
#'
#' @format A data frame with 150 instances, 50 of which belong to positive class,
#' and 5 variables:
#' \describe{
#'   \item{SepalLength}{Measurement of sepal length, in cm. Continuous attribute.}
#'   \item{SepalWidth}{Measurement of sepal width, in cm. Continuous attribute.}
#'   \item{PetalLength}{Measurement of petal length, in cm. Continuous attribute.}
#'   \item{PetalWidth}{Measurement of petal width, in cm. Continuous attribute.}
#'   \item{Class}{Two possible classes: positive (setosa) and negative (versicolor +
#'     virginica).}
#' }
#'
#' @source \href{http://sci2s.ugr.es/keel/datasets.php}{KEEL Repository}.
"iris0"


#' Imbalanced binary breast cancer Wisconsin dataset
#'
#' Binary class dataset containing traits about patients with cancer. Original dataset was
#' obtained from the University of Wisconsin Hospitals, Madison from Dr. William H. Wolberg.
#'
#' @format A data frame with 683 instances, 239 of which belong to positive class,
#' and 10 variables:
#'
#' \describe{
#'   \item{ClumpThickness}{Discrete attribute.}
#'   \item{CellSize}{Discrete attribute.}
#'   \item{CellShape}{Discrete attribute.}
#'   \item{MarginalAdhesion}{Discrete attribute.}
#'   \item{EpithelialSize}{Discrete attribute.}
#'   \item{BareNuclei}{Discrete attribute.}
#'   \item{BlandChromatin}{Disrete attribute.}
#'   \item{NormalNucleoli}{Discrete attribute.}
#'   \item{Mitoses}{Discrete attribute.}
#'   \item{Class}{Two possible classes: positive (cancer) and negative (not cancer).}
#' }
#'
#' @source \href{http://sci2s.ugr.es/keel/datasets.php}{KEEL Repository}.
#' @seealso Original available in \href{https://archive.ics.uci.edu/ml}{UCI ML Repository}.
"wisconsin"


#' Imbalanced binary yeast protein localization sites
#'
#' Imbalanced binary dataset containing protein traits for predicting their cellular
#' localization sites.
#'
#' @format A data frame with 1484 instances, 51 of which belong to positive class,
#' and 9 variables:
#'
#' \describe{
#'   \item{Mcg}{McGeoch's method for signal sequence recognition.
#'     Continuous attribute.}
#'   \item{Gvh}{Von Heijne's method for signal sequence recognition.
#'     Continuous attribute.}
#'   \item{Alm}{Score of the ALOM membrane spanning region prediction program.
#'     Continuous attribute.}
#'   \item{Mit}{Score of discriminant analysis of the amino acid content of the
#'     N-terminal region (20 residues long) of mitochondrial and non-mitochondrial
#'     proteins. Continuous attribute.}
#'   \item{Erl}{Presence of "HDEL" substring (thought to act as a signal for
#'     retention in the endoplasmic reticulum lumen). Binary attribute. Discrete
#'     attribute.}
#'   \item{Pox}{Peroxisomal targeting signal in the C-terminus. Continuous attribute.}
#'   \item{Vac}{Score of discriminant analysis of the amino acid content of vacuolar
#'     and extracellular proteins. Continuous attribute.}
#'   \item{Nuc}{Score of discriminant analysis of nuclear localization signals of
#'     nuclear and non-nuclear proteins. Continuous attribute.}
#'   \item{Class}{Two possible classes: positive (membrane protein, uncleaved signal),
#'     negative (rest of localizations).}
#' }
#'
#' @source \href{http://sci2s.ugr.es/keel/datasets.php}{KEEL Repository}.
#' @seealso Original available in \href{https://archive.ics.uci.edu/ml}{UCI ML Repository}.
"yeast4"


#' Binary banana dataset
#'
#' Dataset containing two attributes as well as a class one, that, if plotted, represent
#' a banana shape
#'
#' @format
#' \describe{
#'   \item{At1}{First attribute.}
#'   \item{At2}{Second attribute.}
#'   \item{Class}{Two possible classes: positive (banana shape), negative (surrounding
#'   of the banana).}
#' }
#'
#' @source \href{http://sci2s.ugr.es/keel/datasets.php}{KEEL Repository}.
#' @section Shape:
#'   \code{banana}: A data frame with 2640 instances, 264 of which belong to positive class,
#' and 3 variables
"banana"


#' @rdname banana
#' @section Shape:
#'   \code{banana_orig}: A data frame with 5300 instances, 2376 of which belong to positive
#'  class, and 3 variables:
"banana_orig"

