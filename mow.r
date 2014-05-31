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

# mapowanie pomiędzy typem protokołu i jej ciągłym odpowiednikiem
protocolTypeMapping <- list("icmp"=0, "tcp"=1, "udp"=2)

# mapowanie pomiędzy usługą i jej ciągłym odpowiednikiem
serviceMapping <- list("smtp"=0, "bgp"=1, "imap4"=2, "courier"=3, "name"=4, "exec"=5, "ftp"=6, "echo"=7, "http_2784"=8,
                       "http_443"=9, "discard"=10, "kshell"=11, "login"=12, "http"=13, "Z39_50"=14, "vmnet"=15, "supdup"=16,
                       "gopher"=17, "printer"=18, "aol"=19, "tftp_u"=20, "csnet_ns"=21, "http_8001"=22, "eco_i"=23, "time"=24,
                       "ssh"=25, "efs"=26, "hostnames"=27, "X11"=28, "klogin"=29, "sql_net"=30, "ldap"=31, "private"=32,
                       "auth"=33, "uucp"=34, "pm_dump"=35, "link"=36, "ctf"=37, "IRC"=38, "ecr_i"=39, "netbios_ns"=40, "urp_i"=41,
                       "pop_2"=42, "pop_3"=43, "rje"=44, "systat"=45, "ftp_data"=46,"finger"=47, "tim_i"=48, "remote_job"=49,
                       "other"=50, "domain_u"=51, "urh_i"=52, "iso_tsap"=53, "netstat"=54, "daytime"=55, "whois"=56, "shell"=57,
                       "mtp"=58, "sunrpc"=59, "uucp_path"=60, "red_i"=61, "harvest"=62, "nnsp"=63, "telnet"=64, "domain"=65,
                       "ntp_u"=66, "netbios_dgm"=67, "nntp"=68, "netbios_ssn"=69)
# mapowanie pomiędzy flagą i jej ciągłym odpowiednikiem
flagMapping <- list("REJ"=0, "SF"=1, "SH"=2, "RSTO"=3, "OTH"=4, "RSTR"=5, "RSTOS0"=6, "S0"=7, "S1"=8, "S2"=9, "S3"=10)


loadData <- function(path) {
  return (read.csv(path, stringsAsFactors=FALSE))
}

# kolumny podlegające zmianom dyskretna -> ciągła (tylko te, które nie są binarne):
# protocol_type {icmp,tcp,udp} (2)
# service {smtp,bgp,imap4,courier,name,exec,ftp,echo,http_2784,
#          http_443,discard,kshell,login,http,Z39_50,vmnet,supdup,
#          gopher,printer,aol,tftp_u,csnet_ns,http_8001,eco_i,time,
#          ssh,efs,hostnames,X11,klogin,sql_net,ldap,private,auth,uucp,
#          pm_dump,link,ctf,IRC,ecr_i,netbios_ns,urp_i,pop_2,pop_3,rje,systat,
#          ftp_data,finger,tim_i,remote_job,other,domain_u,urh_i,iso_tsap,
#          netstat,daytime,whois,shell,mtp,sunrpc,uucp_path,red_i,harvest,
#          nnsp,telnet,domain,ntp_u,netbios_dgm,nntp,netbios_ssn}(3)
# flag {REJ,SF,SH,RSTO,OTH,RSTR,RSTOS0,S0,S1,S2,S3} (4)
#
# kolumna 42 - klasa ataku
normalizeData <- function(data) {
  for(i in 1:nrow(data)) {
    data[i, 2] <- protocolTypeMapping[[data[i, 2]]]
    data[i, 3] <- serviceMapping[[data[i, 3]]]
    data[i, 4] <- flagMapping[[data[i, 4]]]
    data[i, 42] <- classMapping[[data[i, 42]]]
    print(i)
  }
  
  data[,2] <- as.numeric(data[,2])
  data[,3] <- as.numeric(data[,3])
  data[,4] <- as.numeric(data[,4])
  
  return (data)
}

selectAttributes <- function(data) {
  #TODO
}

getSVMModel <- function(train, test, classes, kernelFunc, degree, gamma, coef0, cost) {
  model <- svm(classes ~ ., train, probability=TRUE, scale=FALSE, kernel=kernelFunc, 
               degree=kernelDegree, gamma=kernelGamma, coef0=kernelCoef0, cost=svmCost)
  
  predictions <- predict(model, test, probability=FALSE)
  rawPredictions <- attributes(predict(model, test, probability=TRUE))$probabilities
  
  return (list(model=model, predictions=predictions, rawPredictions=rawPredictions))
}

getKNNModel <- function(train, test, classes, knnK, KNNAlgorithm) {
  model <- knn(train, test, classes, k=knnK, algorithm=KNNAlgorithm, prob=TRUE)
  
  return (list(model=model, predictions=model, rawPredictions=attr(model, "prob")))
}

getNaiveBayesModel <- function(train, test, classes, naiveBayesLaplace) {
  model <- naiveBayes(classes ~ ., train, laplace=naiveBayesLaplace)
  
  predictions <- predict(model, test, type="class")
  rawPredictions <- predict(model, test, type="raw")
  
  return (list(model=model, predictions=predictions, rawPredictions=rawPredictions))
}

prepareData <- function(path, sampleSize) {
  data <- loadData(path)
  
  # podział danych na zbiór trenujący i testowy
  set.seed(1)
  sampledData <- data[sample(nrow(data), sampleSize), ]
  trainInd <- sample(seq_len(nrow(sampledData)), size = floor(0.8*nrow(sampledData)))
  train <- sampledData[trainInd, ]
  test <- sampledData[-trainInd, ]
  
  train <- normalizeData(train)
  test <- normalizeData(test)
  
  lastCol = ncol(train) - 1
  
  return (list(trainData=train[,1:lastCol], trainClasses=train[,lastCol+1], testData=test[,1:lastCol], testClasses=test[,lastCol+1]))
}

#TODO - funkcje realizujące eksperymenty
testSVM <- function() {
  
}

testNaiveBayes <- function() {
  
}

testKNN <- function() {
  
}