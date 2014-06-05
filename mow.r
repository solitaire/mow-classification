suppressMessages(library(FNN))
suppressMessages(library(e1071))
suppressMessages(library(DMwR))
suppressMessages(library(randomForest))

# mapowanie pomiędzy rodzajem ataku i odpowiadającą mu klasą
classMapping = list(
  "back."="dos", "land."="dos", "neptune."="dos", "pod."="dos", "smurf."="dos", "teardrop."="dos",
  "buffer_overflow."="u2r", "loadmodule."="u2r", "perl."="u2r", "rootkit."="u2r",
  "ftp_write."="r2l", "guess_passwd."="r2l", "imap."="r2l", "multihop."="r2l", "phf."="r2l", "spy."="r2l", "warezclient."="r2l", "warezmaster."="r2l",
  "ipsweep."="probe", "nmap."="nmap", "portsweep."="probe", "satan."="probe",
  "normal."="normal")

# mapowanie pomiędzy typem protokołu i jej ciągłym odpowiednikiem
protocolTypeMapping = list("icmp"=0, "tcp"=1, "udp"=2)

# mapowanie pomiędzy usługą i jej ciągłym odpowiednikiem
serviceMapping = list("smtp"=0, "bgp"=1, "imap4"=2, "courier"=3, "name"=4, "exec"=5, "ftp"=6, "echo"=7, "http_2784"=8,
                       "http_443"=9, "discard"=10, "kshell"=11, "login"=12, "http"=13, "Z39_50"=14, "vmnet"=15, "supdup"=16,
                       "gopher"=17, "printer"=18, "aol"=19, "tftp_u"=20, "csnet_ns"=21, "http_8001"=22, "eco_i"=23, "time"=24,
                       "ssh"=25, "efs"=26, "hostnames"=27, "X11"=28, "klogin"=29, "sql_net"=30, "ldap"=31, "private"=32,
                       "auth"=33, "uucp"=34, "pm_dump"=35, "link"=36, "ctf"=37, "IRC"=38, "ecr_i"=39, "netbios_ns"=40, "urp_i"=41,
                       "pop_2"=42, "pop_3"=43, "rje"=44, "systat"=45, "ftp_data"=46,"finger"=47, "tim_i"=48, "remote_job"=49,
                       "other"=50, "domain_u"=51, "urh_i"=52, "iso_tsap"=53, "netstat"=54, "daytime"=55, "whois"=56, "shell"=57,
                       "mtp"=58, "sunrpc"=59, "uucp_path"=60, "red_i"=61, "harvest"=62, "nnsp"=63, "telnet"=64, "domain"=65,
                       "ntp_u"=66, "netbios_dgm"=67, "nntp"=68, "netbios_ssn"=69)
                       
# mapowanie pomiędzy flagą i jej ciągłym odpowiednikiem
flagMapping = list("REJ"=0, "SF"=1, "SH"=2, "RSTO"=3, "OTH"=4, "RSTR"=5, "RSTOS0"=6, "S0"=7, "S1"=8, "S2"=9, "S3"=10)

loadData = function(path) {
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
normalizeData = function(data) {
  data[,2] = vapply(data[,2], function(x) protocolTypeMapping[[x]], 0)
  data[,3] = vapply(data[,3], function(x) serviceMapping[[x]], 0)
  data[,4] = vapply(data[,4], function(x) flagMapping[[x]], 0)
  data[,42] = vapply(data[,42], function(x) classMapping[[x]], "")
  
  data[,2] = as.numeric(data[,2])
  data[,3] = as.numeric(data[,3])
  data[,4] = as.numeric(data[,4])
  
  return (data)
}

selectAttributes = function(train, classes) {
  model = randomForest(factor(classes) ~ ., data = train, ntree = 10, keep.forest = FALSE, importance = TRUE)
  varImpPlot(model)
  importance(model)
}

KNN = function(train, test, classes, K = 1, KNNAlgorithm = "kd_tree") {
	result = knn(train, test, classes, k = K, algorithm = KNNAlgorithm)
  return (result[1:nrow(test)])
}

Bayes = function(train, test, classes, naiveBayesLaplace = 0) {
  model = naiveBayes(factor(classes) ~ ., train, laplace=naiveBayesLaplace)
  return (predict(model, test, type="class"))
}

SVM = function(train, test, classes, 
               kernelFunc = "radial",
               kernelDegree = 3,
               kernelGamma = 1 / ncol(train),
               kernelCoef0 = 0,
               svmCost = 1) {
  model = svm(factor(classes) ~ ., train, probability=TRUE, scale=FALSE, kernel=kernelFunc, 
               degree=kernelDegree, gamma=kernelGamma, coef0=kernelCoef0, cost=svmCost)
  return (predict(model, test, probability=FALSE))
}

# generuje zbiór danych dokonując resamplingu
# perc.over (oversampling) 
# perc.under (undersampling)
resampleData = function(formula, data, percOver, percUnder) {
  return (SMOTE(formula, data, perc.over = percOver, perc.under=percUnder))
}

# file - Plik z wejsciowym zbiorem przykladow.
# part - Czesc danych, ktora ma zostac odczytana. Od 0 do 1 (100%).
prepareData = function(file, part = 1) {
  data = loadData(file)
  
  # przyciecie zbioru wejsciowego
  sampledRows = floor(nrow(data) * part)
  sampledData = data[sample(nrow(data), sampledRows), ]
  
  # podział danych na zbiór trenujący i testowy
  trainRows = floor(0.8 * sampledRows)
  trainInd = sample(seq_len(nrow(sampledData)), trainRows)
  sampledTrain = sampledData[trainInd, ]
  sampledTest  = sampledData[-trainInd, ]
  
  sampledTrain = normalizeData(sampledTrain)
  sampledTest  = normalizeData(sampledTest)
  
  lastCol = ncol(sampledTrain) - 1
  
  return (list(
  	train = sampledTrain[,1:lastCol],
  	trainClasses = sampledTrain[,lastCol+1],
  	test = sampledTest[,1:lastCol],
  	testClasses = sampledTest[,lastCol+1]))
}

error = function(classes, testClasses) {
	numberOfErrors = sum(classes != testClasses)
	return (numberOfErrors / length(classes))
}
