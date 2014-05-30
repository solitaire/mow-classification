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
                       "auth"=33, "uucp"=34, "pm_dump"=35, "link,ctf"=36, "IRC"=37, "ecr_i"=38, "netbios_ns"=39, "urp_i"=40,
                       "pop_2"=41, "pop_3"=42, "rje"=43, "systat"=44, "ftp_data"=45,"finger"=45, "tim_i"=46, "remote_job"=47,
                       "other"=48, "domain_u"=49, "urh_i"=50, "iso_tsap"=51, "netstat"=52, "daytime"=53, "whois"=54, "shell"=55,
                       "mtp"=56, "sunrpc"=57, "uucp_path"=58, "red_i"=59, "harvest"=60, "nnsp"=61, "telnet"=62, "domain"=63,
                       "ntp_u"=64, "netbios_dgm"=65, "nntp"=66, "netbios_ssn"=67)
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
  data[c(2)] <- lapply(data[c(2)], function(x) protocolTypeMapping[x])
  data[c(3)] <- lapply(data[c(3)], function(x) serviceMapping[x])
  data[c(4)] <- lapply(data[c(4)], function(x) flagMapping[x])
  data[c(42)] <- lapply(data[c(42)], function(x) classMapping[x])
  return (data)
}

selectAttributes <- function(data) {
  #TODO
}

#TODO - dodać argumenty dla poszczególnych klasyfikatorów
getSVMModel <- function(train, test, classes) {
  model <- svm(classes ~ ., train, probability=TRUE, scale=FALSE)
  
  predictions <- predict(model, test, probability=FALSE)
  rawPredictions <- attributes(predict(model, test, probability=TRUE))$probabilities
  
  return (list(model=model, predictions=predictions, rawPredictions=rawPredictions))
}

getKNNModel <- function(train, test, classes) {
  model <- knn(train, test, classes ~ ., k = 3, prob=TRUE)
  
  return (list(model=model, predictions=model, rawPredictions=attr(model, "prob")))
}

getNaiveBayesModel <- function(train, test, classes) {
  model <- naiveBayes(classes ~ ., train)
  
  predictions <- predict(model, test, type="class")
  rawPredictions <- predict(model, test, type="raw")
  
  return (list(model=model, predictions=predictions, rawPredictions=rawPredictions))
}
