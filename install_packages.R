# Note that all the installations are done by default from a snapshot aligned
# with the build date of the rocker image: 2017-04-21.
# See https://github.com/rocker-org/rocker-versioned/blob/master/r-ver/3.3.3.Dockerfile#L10

packages <- c(
  "ROCR",
  "kknn",
  "e1071",
  "glmnet",
  "doParallel",
  "randomForest",
  "pROC",
  "caTools",
  "foreach",
  "iterators",
  "gridExtra",
  "pheatmap",
  "grid",
  "XLConnect",
  "caret",
  "xgboost"
)
installed_packages <- rownames(installed.packages())

for (package in packages) {
  if(! package %in% installed_packages) {
    cat(sprintf("Installling R package: %s ...\n", package))
    install.packages(package)
  }
}
