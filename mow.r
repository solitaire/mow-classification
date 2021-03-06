suppressMessages(library(FNN))
suppressMessages(library(e1071))
suppressMessages(library(DMwR))
suppressMessages(library(randomForest))

# nazwy kolumn
columns = c("duration", "protocol_type", "service", "flag", "src_bytes", "dst_bytes", "land", 
			"wrong_fragment", "urgent", "hot", "num_failed_logins", "logged_in", "num_compromised", "root_shell", 
			"su_attempted", "num_root", "num_file_creations", "num_shells", "num_access_files", "num_outbound_cmds",
			"is_host_login", "is_guest_login", "count", "srv_count", "serror_rate", "srv_serror_rate", "rerror_rate", 
			"srv_rerror_rate", "same_srv_rate", "diff_srv_rate", "srv_diff_host_rate",
            "dst_host_count", "dst_host_srv_count", "dst_host_same_srv_rate",
			"dst_host_diff_srv_rate", "dst_host_same_src_port_rate", "dst_host_srv_diff_host_rate",
			"dst_host_serror_rate", "dst_host_srv_serror_rate", "dst_host_rerror_rate", "dst_host_srv_rerror_rate", "attack_type")

# mapowanie pomiędzy rodzajem ataku i odpowiadającą mu klasą
classMapping = list(
  "back."="dos", "land."="dos", "neptune."="dos", "pod."="dos", "smurf."="dos", "teardrop."="dos", "apache2."="dos", "mailbomb."="dos", "processtable."="dos", "udpstorm."="dos",
  "buffer_overflow."="u2r", "loadmodule."="u2r", "perl."="u2r", "rootkit."="u2r", "flow."="u2r", "httptunnel."="u2r", "ps."="u2r", "sqlattack."="u2r", "xterm."="u2r",
  "ftp_write."="r2l", "guess_passwd."="r2l", "imap."="r2l", "multihop."="r2l", "phf."="r2l", "spy."="r2l", "warezclient."="r2l", "warezmaster."="r2l", "snmpgetattack."="r2l", "named."="r2l", "xlock."="r2l", "xsnoop."="r2l", "sendmail."="r2l", "worm."="r2l", "snmpguess."="r2l",
  "ipsweep."="probe", "nmap."="probe", "portsweep."="probe", "satan."="probe", "saint."="probe", "mscan."="probe",
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
                       "ntp_u"=66, "netbios_dgm"=67, "nntp"=68, "netbios_ssn"=69, "icmp"=70)
                       
# mapowanie pomiędzy flagą i jej ciągłym odpowiednikiem
flagMapping = list("REJ"=0, "SF"=1, "SH"=2, "RSTO"=3, "OTH"=4, "RSTR"=5, "RSTOS0"=6, "S0"=7, "S1"=8, "S2"=9, "S3"=10)

# wybrany za pomocą importance zbiór kolumn
selectedAttributes = c("count", "dst_bytes", "src_bytes", "logged_in", "service", "dst_host_same_src_port_rate",
					   "srv_count", "protocol_type", "dst_host_srv_diff_host_rate", "duration", "srv_diff_host_rate",
					   "dst_host_diff_srv_rate", "same_srv_rate", "dst_host_serror_rate", "attack_type")

# klasy w alfabetycznej kolejnosci
alfaClasses = c("dos", "normal", "probe", "r2l", "u2r")

# klasy w kolejnosci dla macierzy pomylek
confClasses = c("normal", "probe", "dos", "u2r", "r2l")
					   
confusionMatrix = function(testClasses, classes) {
	t = table(data$testClasses, classes)
	conf = matrix(0, 5, 5, dimnames = list(alfaClasses, alfaClasses))
	conf[1:nrow(t), 1:ncol(t)] = t
	return (conf[confClasses, confClasses])
}

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
discretizeData = function(data) {
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
  model = randomForest(factor(classes) ~ ., data = train, ntree = 1000, keep.forest = FALSE, importance = TRUE)
  varImpPlot(model)
  importance(model)
}

KNN = function(train, test, classes, K = 1, KNNAlgorithm = "kd_tree") {
	result = knn(train, test, classes, k = K, algorithm = KNNAlgorithm)
  return (result[1:nrow(test)])
}

Bayes = function(train, test, classes, naiveBayesLaplace = 0) {
  model = naiveBayes(factor(classes) ~ ., train, laplace = naiveBayesLaplace)
  return (predict(model, test, type="class"))
}

SVM = function(train, test, classes, 
               kernelFunc = "radial",
               kernelDegree = 3,
               kernelGamma = 1 / ncol(train),
               kernelCoef0 = 0,
               classWeights= NULL) {
  model = svm(factor(classes) ~ ., train, probability=TRUE, scale=FALSE, kernel=kernelFunc, 
               degree=kernelDegree, gamma=kernelGamma, coef0=kernelCoef0, class.weights=classWeights)
  return (predict(model, test, probability=FALSE))
}

# generuje zbiór danych dokonując resamplingu
# perc.over (oversampling) 
# perc.under (undersampling)
resampleData = function(classes, fullData, percOver, percUnder) {
  return (SMOTE(classes ~ ., fullData, perc.over = percOver, perc.under=percUnder, k=10))
}

# file - Plik z wejsciowym zbiorem przykladow.
# part - Czesc danych, ktora ma zostac odczytana. Od 0 do 1 (100%).
prepareData = function(trainFile, testFile, part = 1) {
  trainData = loadData(trainFile)
  testData = loadData(testFile)
  colnames(trainData) = columns
  colnames(testData) = columns
  
  # przyciecie zbioru wejsciowego
  sampledRows = floor(nrow(trainData) * part)
  trainData = trainData[sample(nrow(trainData), sampledRows), ]
  
  trainData = discretizeData(trainData)
  testData  = discretizeData(testData)
  
  trainData = subset(trainData, select=selectedAttributes)
  testData = subset(testData, select=selectedAttributes)
  
  lastAttrCol = ncol(trainData) - 1
  
  trainData[,1:lastAttrCol] = scale(trainData[,1:lastAttrCol])
  testData[,1:lastAttrCol] = scale(testData[,1:lastAttrCol])
  
  return (list(
  	train = trainData[,1:lastAttrCol],
  	trainClasses = trainData[,lastAttrCol+1],
  	test = testData[,1:lastAttrCol],
  	testClasses = testData[,lastAttrCol+1]))
}

error = function(classes, testClasses) {
	numberOfErrors = sum(classes != testClasses)
	return (numberOfErrors / length(classes))
}
