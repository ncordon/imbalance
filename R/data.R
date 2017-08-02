#' Imbalanced binary ecoli protein localization sites
#'
#' Imbalanced binary dataset containing protein traits for predicting their cellular
#' localization sites.
#'
#' @format A data frame with 336 instances and 8 variables:
#'
#' \describe{
#'   \item{Mgc}{McGeoch's method for signal sequence recognition.
#'     Numeric variable.}
#'   \item{Gvh}{Von Heijne's method for signal sequence recognition.
#'     Numeric variable.}
#'   \item{Lip}{von Heijne's Signal Peptidase II consensus sequence score.
#'     Discrete variable}
#'   \item{Chg}{Presence of charge on N-terminus of predicted lipoproteins.
#'     Discrete variable.}
#'   \item{Aac}{Score of discriminant analysis of the amino acid content of outer
#'     membrane and periplasmic proteins. Numeric variable.}
#'   \item{Alm1}{Score of the ALOM membrane spanning region prediction program.
#'     Numeric variable.}
#'   \item{Alm2}{score of ALOM program after excluding putative cleavable signal
#'     regions from the sequence. Numeric variable.}
#'   \item{Class}{Two possible celular localization sites.}
#' }
#'
#' @source \href{http://sci2s.ugr.es/keel/datasets.php}{Keel Repository}.
#' @seealso Original available in \href{https://archive.ics.uci.edu}{UCI Repository}.
"ecoli1"


#' Imbalanced binary glass identification
#'
#' Imbalanced binary classification dataset containing variables to
#' identify types of glass.
#'
#' @format A data frame with 214 instances and 10 variables:
#'
#' \describe{
#'   \item{RI}{Refractive Index. Numeric variable.}
#'   \item{Na}{Sodium, weight percent in component. Numeric variable.}
#'   \item{Mg}{Magnesium, weight percent in component. Numeric variable.}
#'   \item{Al}{Aluminum, weight percent in component. Numeric variable.}
#'   \item{Si}{Silicon, weight percent in component. Numeric variable.}
#'   \item{K}{Potasium, weight percent in component. Numeric variable.}
#'   \item{Ca}{Calcium, weight percent in component. Numeric variable.}
#'   \item{Ba}{Barium, weight percent in component. Numeric variable.}
#'   \item{Fe}{Iron, weight percent in component. Numeric variable.}
#'   \item{Class}{Two possible glass types.}
#' }
#'
#' @source \href{http://sci2s.ugr.es/keel/datasets.php}{Keel Repository}.
#' @seealso Original available in \href{https://archive.ics.uci.edu}{UCI Repository}.
"glass0"


#' Haberman's survival data
#'
#' The dataset contains cases from a study that was conducted between
#' 1958 and 1970 at the University of Chicago's Billings Hospital on
#' the survival of patients who had undergone surgery for breast
#' cancer.
#'
#' @format A data frame with 306 instances and 4 variables:
#'
#' \describe{
#'   \item{Age}{Age of patient at time of operation. Numeric variable.}
#'   \item{Year}{Patient's year of operation. Numeric variable.}
#'   \item{Positive}{Number of positive axillary nodes detected. Numeric variable.}
#'   \item{Class}{Two possible survival status.}
#' }
#'
#' @source \href{http://sci2s.ugr.es/keel/datasets.php}{Keel Repository}.
#' @seealso Original available in \href{https://archive.ics.uci.edu}{UCI Repository}.
"haberman"


#' Imbalanced binary thyroid gland data
#'
#' Data to predict patient's hyperthyroidism.
#'
#' @format A data frame with 215 instances and 6 variables:
#'
#' \describe{
#'   \item{T3resin}{T3-resin uptake test, percentage. Numeric attribute.}
#'   \item{Thyroxin}{Total Serum thyroxin as measured by the isotopic
#'     displacement method. Numeric attribute.}
#'   \item{Triiodothyronine}{Total serum triiodothyronine as measured by radioimmuno
#'     assay. Numeric attribute.}
#'   \item{Thyroidstimulating}{Basal thyroid-stimulating hormone (TSH) as measured by
#'     radioimmuno assay. Numeric attribute.}
#'   \item{TSH_value}{Maximal absolute difference of TSH value after injection of 200
#'     micro grams of thyrotropin-releasing hormone as compared to the basal value.
#'     Numeric attribute.}
#'   \item{Class}{Two possible classes: positive as hyperthyroidism, negative as non
#'     hyperthyroidism.}
#' }
#'
#' @source \href{http://sci2s.ugr.es/keel/datasets.php}{Keel Repository}.
#' @seealso Original available in \href{https://archive.ics.uci.edu}{UCI Repository}.
"newthyroid1"


#' Imbalanced binary iris dataset
#'
#' Modification of \code{\link[datasets]{iris}} dataset. Measurements in
#' centimeters of the variables sepal length and width and petal length and
#' width, respectively, for 50 flowers from each of 3 species of iris. The
#' possible classifications are positive (setosa) and negative (versicolor +
#' virginica).
#'
#' @format A data frame with 150 instances and 5 variables:
#' \describe{
#'   \item{SepalLength}{Measurement of sepal length, in cm. Numeric attribute.}
#'   \item{SepalWidth}{Measurement of sepal width, in cm. Numeric attribute.}
#'   \item{PetalLength}{Measurement of petal length, in cm. Numeric attribute.}
#'   \item{PetalWidth}{Measurement of petal width, in cm. Numeric attribute.}
#'   \item{Class}{Two possible classes: positive (setosa) and negative (versicolor +
#'     virginica).}
#' }
#'
#' @source \href{http://sci2s.ugr.es/keel/datasets.php}{Keel Repository}.
"iris0"


#' Imbalanced binary breast cancer Wisconsin dataset
#'
#' Binary class dataset containing traits about patients with cancer. Original dataset was
#' obtained from the University of Wisconsin Hospitals, Madison from Dr. William H. Wolberg.
#'
#' @format A data frame with 683 instances and 10 variables:
#' \describe{
#'   \item{ClumpThickness}{Nominal attribute.}
#'   \item{CellSize}{Nominal attribute.}
#'   \item{CellShape}{Nominal attribute.}
#'   \item{MarginalAdhesion}{Nominal attribute.}
#'   \item{EpithelialSize}{Nominal attribute.}
#'   \item{BareNuclei}{Nominal attribute.}
#'   \item{BlandChromatin}{Nominal attribute.}
#'   \item{NormalNucleoli}{Nominal attribute.}
#'   \item{Mitoses}{Nominal attribute.}
#'   \item{Class}{Two possible classes: positive (cancer) and negative (not cancer).}
#' }
#'
#' @source \href{http://sci2s.ugr.es/keel/datasets.php}{Keel Repository}.
#' @seealso Original available in \href{https://archive.ics.uci.edu}{UCI Repository}.
"wisconsin"


#' Imbalanced binary yeast protein localization sites
#'
#' Imbalanced binary dataset containing protein traits for predicting their cellular
#' localization sites.
#'
#' @format A data frame with 1484 instances and 9 variables:
#'
#' \describe{
#'   \item{Mgc}{McGeoch's method for signal sequence recognition.
#'     Numeric variable.}
#'   \item{Gvh}{Von Heijne's method for signal sequence recognition.
#'     Numeric variable.}
#'   \item{Alm}{Score of the ALOM membrane spanning region prediction program.
#'     Numeric variable.}
#'   \item{Mit}{Score of discriminant analysis of the amino acid content of the
#'     N-terminal region (20 residues long) of mitochondrial and non-mitochondrial
#'     proteins. Numeric attribute.}
#'   \item{Erl}{Presence of "HDEL" substring (thought to act as a signal for
#'     retention in the endoplasmic reticulum lumen). Binary attribute. Numeric
#'     attribute.}
#'   \item{Pox}{Peroxisomal targeting signal in the C-terminus. Numeric attribute.}
#'   \item{Vac}{Score of discriminant analysis of the amino acid content of vacuolar
#'     and extracellular proteins. Numeric attribute.}
#'   \item{Nuc}{Score of discriminant analysis of nuclear localization signals of
#'     nuclear and non-nuclear proteins. Numeric attribute.}
#'   \item{Class}{Two possible classes: positive (membrane protein, uncleaved signal),
#'     negative (rest of localizations).}
#' }
#'
#' @source \href{http://sci2s.ugr.es/keel/datasets.php}{Keel Repository}.
#' @seealso Original available in \href{https://archive.ics.uci.edu}{UCI Repository}.
"yeast4"
