#################
#   FONCTIONS   #
#################

#-------------
#BOOTSTRAP MOYENNES 

#1. Je cree mon tableau/vecteur de data que j'appelle .df. Ce sera ensuite dans fonction boot
#2. Je peux modifier mon tableau/vecteur et appliquer une fonction a ce tableau/vecteur. ce sera fonction stat
#3. Je cree une fonction ci qui integre la fonction avec boot.

#Je cree la fonction stat pour avoir la moyenne
#data va prendre la valeur de .df, comme defini dans la fonction suivante
fun.mean <- function(data,indices){
  .dat <- data[indices,]
  .dat <- na.omit (.dat)
  #.int <- .dat[.dat$group=="g0", "score"]
  #.tel <- .dat[.dat$group=="g1", "score"]
  mymean<-by(.dat$score,.dat$group,mean)
  return(mymean)
}

#Je cree la fonction boot. 
#le tableau .df defini l'argument data de la fonction fun mean. 
#boot repete la fonction fun.mean R fois : il genere un nouveau .df[indices,] et fait la moyenne. 
boot.var <- function (.var,R){
  .df <- data.frame (
    score=c (d[d$Dist_dyn==0, .var], d[d$Dist_dyn==1, .var]),
    group=c (rep ("g0", nrow (d[d$Dist_dyn==0, ])), rep ("g1", nrow (d[d$Dist_dyn==1, ])))
  )
  .res<- boot(data=.df,statistic=fun.mean,R=R)
  return(.res)
}

#Je cree une fonction BootMCi qui reintegre resultat de boot.var et ajoute les IC et met en forme
BootMCi <- function (.var, R) {
  browser()
  par(mfrow=c(2,1))
  .bootres <- boot.var (.var=.var, R=R)
  hist(.bootres$t[,1],main=paste0("boot",.var,"g0"))
  hist(.bootres$t[,2],main=paste0("boot",.var,"g1"))
  .n <- length (.bootres$t0) #donne le nombre de resultat boot realise : 1 pour internet, 1 pour telephone
  .list.ci <- lapply(1:.n, function(x) boot.ci(.bootres,index=x,type="perc")) #fct boot.ci : intervalle de confiance pour chaque boot
  .res <- data.frame (t (sapply (.list.ci, function (x) x[["percent"]][4:5]))) #selectionne les valeur de IC
  rownames (.res) <- names (.bootres$t0) #appelle les lignes int et tel mais ca sera perdu ensuite
  colnames (.res) <- c ("CI_L", "CI_U")
  #.res$est <- apply (.bootres$t, 2, median) #pour faire la mediane des échantillons
  .res$est <- as.numeric (.bootres$t0) #selectionne l'estimateur et le rajoute au tableau .res : ici moyenne
  .res$n<-c(sum(!is.na(d[d$Dist_dyn==0,.var])),sum(!is.na(d[d$Dist_dyn==1,.var]))) #true(non manquant) vaut 1, donc nb de non NA
  .res <- .res[, c (4,3, 1, 2)] #remet les colonnes dans l'ordre
  .ans <- round (.res, 2) #fait un arrondi sur chaque valeur
  .ans <- data.frame (N=.res$n, meanCIn=paste0 (.ans$est, " [", .ans$CI_L, "-", .ans$CI_U, "]")) #met en forme les valeurs
  .ans <- sapply  (c (g0=.ans[1, ], g1=.ans[2, ]), as.vector )#c(int= , tel=)donne un titre, mais met en liste, 
  #sapply(as.vector) realigne en chaine de caracteres
  return (.ans)
}


#----------------------
#TESTS DE PERMUTATION

perm.moyenne.dist <- function (x,data,N){
  x0<-data[data$Dist_dyn==0, x]
  x1<-data[data$Dist_dyn==1, x]
  
  keep0 <- na.omit(x0)
  keep1 <- na.omit(x1)
  keep01 <- c(keep0,keep1)
  obs0<- mean(keep0)
  obs1<- mean(keep1)
  diff.obs <- abs(obs0 - obs1)
  #diff.obs <- obsI - obsT
  
  .gpe <- rep(c("g0","g1"),c(length(keep0),length(keep1)))
  
  perm.test<- function(keepIT=keep01,.gpe=.gpe){
    mixgpe <- sample(.gpe,replace = FALSE)
    g0 <- keepIT[mixgpe=="g0"]
    g1 <- keepIT[mixgpe=="g1"]
    m0<-mean(g0)
    m1<-mean(g1)
    diff<-abs(m0-m1)
    #diff<-mi-mt
    return(diff)
  }
  
  many.samp<- replicate (N, perm.test(keep01,.gpe))
  
  p.val <-length(many.samp[many.samp>= diff.obs]) / length(many.samp)
  # #pour tracer courbe
  # hist(many.samp,main=paste0("Difference de moyenne ",x))
  # abline(v=diff.obs,lwd=2,col=2)
  
  return(data.frame(n0=length(keep0),mean0=obs0,n1=length(keep1),mean1=obs1, diff_obs=diff.obs, p_value=p.val,nb_perm=N))
}