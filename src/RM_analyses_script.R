source("src/library_RM.R")
source("src/RM_fonctions.R")


d <- read.csv2("data/roxane.csv") #version avec l'erreur de VEMS
d <- read.csv2("data/roxane_modif20170214.csv") #version sans l'erreur de VEMS

#NB : J'ai finalement utilisé ce script et pas analyses_RM.Rmd



#datamanagement

d <- d[!is.na(d$patient),]
d$patient <- as.character(d$patient)

#qual <- c(names(d)[c(2,6,8:17,19:22,24,61)],"normCVF")#pnesup_300 pas descriptif
qual <- c(names(d)[c(2,6,8:17,19:22,61)],"normCVF")
quant.descr <- c("age","BMI")

quant <- names(d)[-1]
quant <- quant[! quant %in% c(qual,"DN","DI","age","BMI")]

for (i in qual) {
  d[ ,i] <- factor(d[ ,i])
}
d[ , quant] <- apply(d[ , quant], 2, function(x) as.numeric(x))
d[ , quant.descr] <- apply(d[ , quant.descr], 2, function(x) as.numeric(x))

d$DI <- as.Date(d$DI,"%d/%m/%Y")
d$DN <- as.Date(d$DN,"%d/%m/%Y")

#Description :

range(d$DI)

d$an<-substring(d$DI, 1, 4)
qplot(d$an, main="nombre de patients inclus par an", xlab="annee d'inclusion")


# description generale
table_var_quali <- lapply(qual[!qual%in%"Dist_dyn"], function(i){
  data <- d[,i]
  names_levels <- levels(as.factor(data))
  a <- lapply(names_levels, function(x) {
    tmp <- as.numeric(table(data)[x])
    tmpbis <- round(as.numeric(prop.table(table(data))[x]),3)*100
    tmptot <- paste0(tmp," (",tmpbis,"%)")
    
    nNA <- table(is.na(data))
    pNA <- round(prop.table(table(is.na(data))),3)
    if (is.na(nNA[2]))  {
      if (which(names_levels==x)==1) nNA <- paste0 (0," (0%)")
      else nNA <- ""
    }
    else {
      if (which(names_levels==x)==1){
        nNA <- as.numeric (nNA[names(nNA)==TRUE])
        pNA <- as.numeric (pNA[names(pNA)==TRUE])*100
        nNA <- paste0(nNA," (",pNA,"%)")  
      }
      else nNA <- ""
    }
    cbind(tmptot,nNA)
    
  })
  a <- do.call(rbind,a)
  #a <- cbind (a,nNA)
  rownames(a) <- paste0(i,"_",names_levels) 
  N <- nrow(d)
  colnames(a) <- c(paste0("general N=",N),"valeurs manquantes")
  # a <- rbind (a,nNA)
  # rownames(a)[-nrow(a)] <- paste0(i,"_",names_levels) 
  return(a)
})
table_var_quali <- do.call(rbind,table_var_quali)

table_var_quanti <- lapply(quant.descr, function(i){ #median ou moyenne? (sachant qu'on ne verifie pas normalite des baselines)
  data <- d[,i]
  med <- round(median (data,na.rm=T),2)
  quant <- round(quantile(data,na.rm=T),2)
  Q1 <- quant[2]
  Q3 <- quant[4]
  a <- paste0(med," (",Q1,"-",Q3,")")
  #browser()
  
  nNA <- table(is.na(data))
  pNA <- round(prop.table(table(is.na(data))),3)
  if (is.na(nNA[2]))  nNA <- paste0 (0," (0%)")
  else {
    nNA <- as.numeric (nNA[names(nNA)==TRUE])
    pNA <- as.numeric (pNA[names(pNA)==TRUE])*100
    nNA <- paste0(nNA," (",pNA,"%)")
  }
  # a <- rbind (a,nNA)
  # rownames(a)[-nrow(a)] <- paste0(i,"*") 
  a <- cbind (a,nNA)
  rownames(a) <- paste0(i,"*")
  N <- nrow(d)
  colnames(a) <- c(paste0("general N=",N),"valeurs manquantes")
  return(a)
})
table_var_quanti <- do.call(rbind,table_var_quanti)

table_var <- rbind(table_var_quali,table_var_quanti)

# description dans population sans distension

table_var_quali <- lapply(qual[!qual%in%"Dist_dyn"], function(i){
  data <- d[d$Dist_dyn==0, i]
  names_levels <- levels(as.factor(data))
  a <- lapply(names_levels, function(x) {
    tmp <- as.numeric(table(data)[x])
    tmpbis <- round(as.numeric(prop.table(table(data))[x]),3)*100
    tmptot <- paste0(tmp," (",tmpbis,"%)")
    return(tmptot)
  })
  a <- do.call(rbind,a)
  rownames(a) <- paste0(i,"_",names_levels) 
  N0 <- nrow(d[d$Dist_dyn==0, ])
  colnames(a) <- c(paste0("distension=0 N=",N0))
  return(a)
})
table_var_quali <- do.call(rbind,table_var_quali)

table_var_quanti <- lapply(quant.descr, function(i){ #median ou moyenne? (sachant qu'on ne verifie pas normalite des baselines)
  data <- d[d$Dist_dyn==0, i]
  med <- round(median (data,na.rm=T),2)
  quant <- round(quantile(data,na.rm=T),2)
  Q1 <- quant[2]
  Q3 <- quant[4]
  a <- data.frame(paste0(med," (",Q1,"-",Q3,")"))
  rownames(a) <- paste0(i,"*")
  N0 <- nrow(d[d$Dist_dyn==0, ])
  colnames(a) <- c(paste0("distension=0 N=",N0))
  return(a)
})
table_var_quanti <- do.call(rbind,table_var_quanti)

table_var0 <- rbind(table_var_quali,table_var_quanti)

# description dans population avec distension
table_var_quali <- lapply(qual[!qual%in%"Dist_dyn"], function(i){
  data <- d[d$Dist_dyn==1, i]
  names_levels <- levels(as.factor(data))
  a <- lapply(names_levels, function(x) {
    tmp <- as.numeric(table(data)[x])
    tmpbis <- round(as.numeric(prop.table(table(data))[x]),3)*100
    tmptot <- paste0(tmp," (",tmpbis,"%)")
    return(tmptot)
  })
  a <- do.call(rbind,a)
  #a <- cbind (a,nNA)
  rownames(a) <- paste0(i,"_",names_levels) 
  N1 <- nrow(d[d$Dist_dyn==1, ])
  colnames(a) <- c(paste0("distension=1 N=",N1))
  # a <- rbind (a,nNA)
  # rownames(a)[-nrow(a)] <- paste0(i,"_",names_levels) 
  return(a)
})
table_var_quali <- do.call(rbind,table_var_quali)

table_var_quanti <- lapply(quant.descr, function(i){ #median ou moyenne? (sachant qu'on ne verifie pas normalite des baselines)
  data <- d[d$Dist_dyn==1, i]
  med <- round(median (data,na.rm=T),2)
  quant <- round(quantile(data,na.rm=T),2)
  Q1 <- quant[2]
  Q3 <- quant[4]
  a <- data.frame(paste0(med," (",Q1,"-",Q3,")"))
  rownames(a) <- paste0(i,"*")
  N1 <- nrow(d[d$Dist_dyn==1, ])
  colnames(a) <- c(paste0("distension=1 N=",N1))
  return(a)
})
table_var_quanti <- do.call(rbind,table_var_quanti)

table_var1 <- rbind(table_var_quali,table_var_quanti)

table_var_final <- cbind (table_var0,table_var1,table_var)

write.table(print(table_var_final), file="clipboard", sep="\t")


#----------------------------------
#NORMALITE DES DISTRIBUTION

#normalité dans chaque groupe

#beaucoup non normale, et qd hyp de normalité non rejetée, peut être par manque de puissance
distrib_th_0<- data.frame(pval_g0=sapply(quant,function(x)shapiro.test(d[d$Dist_dyn==0 ,x])$p.value))
distrib_th_1<- data.frame(pval_g1=sapply(quant,function(x)shapiro.test(d[d$Dist_dyn==1 ,x])$p.value))
cbind(distrib_th_0,distrib_th_1)

par(mfrow = c(2,1))
for (x in quant){
  qqnorm(d[d$Dist_dyn==0 ,x],main = paste0(x,"dist=0"))
  qqline(d[d$Dist_dyn==0 ,x]) 
}
par(mfrow = c(2,1))
for (x in quant){
  qqnorm(d[d$Dist_dyn==1 ,x],main = paste0(x,"dist=1"))
  qqline(d[d$Dist_dyn==1 ,x]) 
}
#non normal donc pas de student => wilcoxon? on regarde si les distributions sont semblables entre les groupes

#les distributions sont peut etre semblables (ou manque de puissance...) 
ks.test
distrib_sembl<- data.frame(sapply(quant,function(x)ks.test(d[d$Dist_dyn==0 ,x],d[d$Dist_dyn==1 ,x],
                                                        mean(d[d$Dist_dyn==1 ,x],na.rm=TRUE),
                                                        sd(d[d$Dist_dyn==1 ,x],na.rm=TRUE))$p.value))

#mais beaucoup d'exaequo de toutes façon donc pas de wilcoxon => test de permutation


#-----------------------------------------
#BOOTSTRAP et TESTS EXPLORATOIRES pour les var quanti

#1- IC par boostrap
set.seed(12345)
Meanci <- t(sapply(quant,function(x)BootMCi (x,10000)))
saveRDS(Meanci,"data/Meanci.rds")
Meancin <- data.frame(Meanci)
Meancin$g0meancin <- paste0(Meancin$g0.meanCI,"; ",Meancin$g0.N)
Meancin$g1meancin <- paste0(Meancin$g1.meanCI,"; ",Meancin$g1.N)

# #version recheck VEMS
# set.seed(12345)
# quantVEMS <- quant[grep("VEMS", quant)]
# Meanci <- t(sapply(quantVEMS,function(x)BootMCi (x,10000)))
# Meancin <- data.frame(Meanci)
# Meancin$g0meancin <- paste0(Meancin$g0.meanCI,"; ",Meancin$g0.N)
# Meancin$g1meancin <- paste0(Meancin$g1.meanCI,"; ",Meancin$g1.N)


#2- tests de permutation
#http://astuces-r.blogspot.fr/2014/07/tests-par-permutation-sous-r.html
#http://faculty.washington.edu/kenrice/sisg/SISG-08-06.pdf
set.seed(123456)
pvalquant <- data.frame(pvalue = sapply(quant,function(.x)perm.moyenne.dist(x=.x,d,100000)$p_value))
saveRDS(pvalquant,"data/pvalquant.rds")
pvalquant <- round(pvalquant,3)

# ##version recheck VEMS
# set.seed(123456)
# pvalquant <- data.frame(pvalue = sapply(quantVEMS,function(.x)perm.moyenne.dist(x=.x,d,100000)$p_value))
# pvalquant <- round(pvalquant,3)

#rassembler IC et pvalue
dfquant <- cbind(Meancin$g0meancin, Meancin$g1meancin, pvalquant)
dfquant$significatif <- ifelse(dfquant$pvalue<0.05,"*","")
write.table(print(dfquant), file="clipboard", sep="\t", row.names=T)

#analyses pour ctc_mg
set.seed(12345)
d$ctc_mg <- as.numeric(as.character(d$ctc_mg))
Meanci <- data.frame(t(BootMCi ("ctc_mg",10000)))
Meanci$g0meancin <- paste0(Meanci$g0.meanCI,"; ",Meanci$g0.N)
Meanci$g1meancin <- paste0(Meanci$g1.meanCI,"; ",Meanci$g1.N)
pvalquant <- round(data.frame(pvalue = perm.moyenne.dist(x="ctc_mg",d,100000)$p_value),3)
dfquant <- cbind(Meanci$g0meancin, Meanci$g1meancin, pvalquant)
dfquant$significatif <- ifelse(dfquant$pvalue<0.05,"*","")
write.table(print(dfquant), file="clipboard", sep="\t", row.names=T)

#---------------------------------------
#Analyses exploratoires qui ne sont pas à présenter : tests sur les var quali du tableau descriptif

#chi2 pour var qual
table(d[,qual[1]],d$Dist_dyn )
#Je les ai fait un par un, beaucoup de variables avec conditions non remplies
sapply(qual[-c(1,4)], function(x) chisq.test(d[,x], d$Dist_dyn, correct = FALSE)$p.value)
sapply(qual[-c(1,3,4,5,9,10,11,12,13,15,16)], function(x) chisq.test(d[,x], d$Dist_dyn, correct = FALSE)$p.value)

#conditions non remplies pour:
qual[-c(1,3,4,5,9,10,11,12,13,15,16)]
#HTA3
#Diab5
#PNS 9
#int_ains 10
#ecz 11
#SAS 12
#Xolair 13
#ctc 15
#CPI only 0... 4

#Donc je vais fisher
fish <- data.frame(pvalue = sapply(qual[-c(1,4)], function(x) fisher.test(d[,x], d$Dist_dyn)$p.value))
