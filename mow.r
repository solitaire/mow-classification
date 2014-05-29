library("FNN")
library("e1071")
library("DMwR")
library("randomForest")

# mapowanie pomiędzy rodzajem ataku i odpowiadającą mu klasą
classMapping <- list(
  "back."="dos", "land."="dos", "neptune."="dos", "pod."="dos", "smurf."="dos", "teardrop."="dos",
  "buffer_overflow."="u2r", "loadmodule."="u2r", "perl."="u2r", "rootkit."="u2r",
  "ftp_write."="r2l", "guess_passwd."="r2l", "imap."="r2l", "multihop."="r2l", "phf."="r2l", "spy."="r2l", "warezclient."="r2l", "warezmaster."="r2l",
  "ipsweep."="probe", "nmap."="nmap", "portsweep."="probe", "satan."="probe",
  "normal."="normal")

loadData <- function(path) {
  return (read.csv(path, stringsAsFactors=FALSE))
}

normalizeData <- function(data) {
  #TODO
}

# rodzaj ataku jest zdefiniowany w ostatniej, 42 kolumnie
labelData <- function(data, classMapping) {
  data[c(42)] <- lapply(data[c(42)], function(x) classMapping[x])
  return (data)
}

getSVMModel <- function(train, test, classes) {
  model <- svm(classes, train, probability=TRUE)
  
  predictions = predict(model, test, probability=FALSE)
  rawPredictions =attributes(predict(model, test, probability=TRUE))$probabilities
  
  return (list(model=model, predictions=predictions, rawPredictions=rawPredictions))
}

getKNNModel <- function(train, test, classes) {
  model <- knn(train, test, classes, k = 3, prob=TRUE)
  
  return (list(model=model, predictions=model, rawPredictions=attr(model, "prob")))
}

getNaiveBayesModel <- function(train, test, classes) {
  model <- naiveBayes(classes, train)
  
  predictions = predict(model, test, type="class")
  rawPredictions = predict(model, test, type="raw")
  
  return (list(model=model, predictions=predictions, rawPredictions=rawPredictions))
}
