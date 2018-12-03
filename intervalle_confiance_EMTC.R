mtp://%5Busb%3A002,011%5D/Card/docs/R_WORKs/intervalle_confiance_EMTC.R
library(binom)
EMTC <- read.csv("~/Rmarkdown/Slim fit project-Synthesis of jobs available to get selection rate - EMTC.csv", stringsAsFactors=FALSE)

EMTC[1,30]='prop_observé'

#ici faire attention, vérifier que les colonnes correspondent bien à ce que l'on a dans 'df' du lab EOLT. 
#les deux entrees ne sont pas exactement les mêmes 
df_emtc<-data.frame(EMTC[[1]],EMTC[[2]],EMTC[[28]],EMTC[[29]],EMTC[[30]])

# On enlève juste les %  et on transforme les facteurs en numérique
# on peut avoir cette erreur quand on a une valeur égale à 0 : Error in 2:nrow(df) : argument of length 0
pchap=(as.numeric(sub("%","",df_emtc[2:8,5])))/100
pchap
intervalle<-function(prm1,prm2,pchap){
  rlt=binom.exact(x = prm1,n = prm2,conf.level = 0.95,"exact",p=pchap)
  rlt
}

# fonction qui transforme les doubles en pourcentage

pourcentage<-function(float){
  res = float * 100
  res = paste(as.character(res),"%",sep = "")
  res
}

# fonction qui donne individuellement les intervalles de confiance binomiaux
intervalle<-function(prm1,prm2,pchap){
  rlt=binom.exact(x = prm1,n = prm2,conf.level = 0.95,"exact",p=pchap)
  rlt
}
# vecteur qui contient tous les intervalles
v=intervalle(as.numeric(as.character(df_emtc[2:nrow(df_emtc),3])),as.numeric(as.character(df_emtc[2:nrow(df_emtc),4])),as.numeric(as.character(df_emtc[2:nrow(df_emtc),5])))
v

IC_exa_binf=c("exact_lower",pourcentage(round(v$lower,4)))
#cas particulier car le taux est 0
#IC_exa_binf = c(IC_exa_binf,0)
df_emtc <- data.frame(df_emtc,IC_exa_binf)
IC_exa_bsup=c("exact_uper",pourcentage(round(v$upper,4)))
#IC_exa_bsup = c(IC_exa_bsup,0)
df_emtc <- data.frame(df_emtc,IC_exa_bsup)
df_emtc

#rajoute les IC de wilson
library(DescTools)
W=BinomCI (as.numeric(as.character(df_emtc[2:nrow(df_emtc),3])),as.numeric(as.character(df_emtc[2:nrow(df_emtc),4])),conf.level=0.95,method="wilson")

IC_Wilson_binf = c("IC_wilson_binf",pourcentage(round(W[,2],4)))
df_emtc<-data.frame(df_emtc,IC_Wilson_binf)
IC_Wilson_bsup =c("IC_wilson_bsup",pourcentage(round(W[,3],4)))
df_emtc<-data.frame(df_emtc,IC_Wilson_bsup)

pourcentage(round(v$lower,4))

#rajoute IC de Wald
pchap=(as.numeric(sub("%","",df_emtc[2:nrow(df_emtc),5])))/100
pchap

n=as.numeric(as.character(df_emtc[2:nrow(df_emtc),4]))
n

binf<-function(pchap,n){
  
  resl=pchap-2*sqrt(pchap*(1-pchap)/n)
  resl
}

bsup<-function(pchap,n){
  
  resl=pchap+2*sqrt(pchap*(1-pchap)/n)
  resl
}

IC_wald_binf =c("IC_wald_binf", pourcentage(round(binf(pchap,n),4)))
IC_wald_bsup = c("IC_wald_bsup",pourcentage(round(bsup(pchap,n),4)))

df_emtc= data.frame(df_emtc,IC_wald_binf)
df_emtc = data.frame(df_emtc,IC_wald_bsup)
df_emtc

# rajoute la colonne des proportions attenduent et export tout le tableau dans un csv
EMTC[1,27]='Attendue'
df_emtc = data.frame(df_emtc,EMTC[[27]])
df_emtc <- data.frame(df_emtc,c("exa_mean",pourcentage(round(v$mean,4))))
write.csv(df_emtc,file = "intervalle_confiance_EMTC.csv")


v2=binom.test(as.numeric(as.character(df_emtc[2,3])),as.numeric(as.character(df_emtc[2,4])),pchap[1])
v2$estimate
v3=binom.test(as.numeric(as.character(df_emtc[3,3])),as.numeric(as.character(df_emtc[3,4])),pchap[2])
v3$estimate
v4=binom.test(as.numeric(as.character(df_emtc[4,3])),as.numeric(as.character(df_emtc[4,4])),pchap[3])
v4$estimate
v5=binom.test(as.numeric(as.character(df_emtc[5,3])),as.numeric(as.character(df_emtc[5,4])),pchap[4])
v5$estimate
v6=binom.test(as.numeric(as.character(df_emtc[6,3])),as.numeric(as.character(df_emtc[6,4])),pchap[5])
v6$estimate
v7=binom.test(as.numeric(as.character(df_emtc[7,3])),as.numeric(as.character(df_emtc[7,4])),pchap[6])
v7$estimate
v8=binom.test(as.numeric(as.character(df_emtc[8,3])),as.numeric(as.character(df_emtc[8,4])),pchap[7])
v8$estimate
v9=binom.test(as.numeric(as.character(df_emtc[9,3])),as.numeric(as.character(df_emtc[9,4])),pchap[8])
v9$estimate
v10=binom.test(as.numeric(as.character(df_emtc[10,3])),as.numeric(as.character(df_emtc[10,4])),pchap[9])
v10$estimate
estimation=c("Exact_forcast",pourcentage(round(v2$estimate,4)),pourcentage(round(v3$estimate,4)),pourcentage(round(v4$estimate,4)),pourcentage(round(v5$estimate,4)),pourcentage(round(v6$estimate,4)),pourcentage(round(v7$estimate,4)),pourcentage(round(v8$estimate,4)),pourcentage(round(v9$estimate,4)),pourcentage(round(v10$estimate,4)))
estimation

df_emtc<- data.frame(df_emtc,estimation)
pertes=as.numeric(sub("%","",df_emtc[2:nrow(df_emtc),12]))-as.numeric(sub("%","",df_emtc[2:nrow(df_emtc),14]))
pertes=c("%_perte",pertes)
pertes
df_emtc <- data.frame(df_emtc,pertes)

write.table(df_emtc, "EMTC.xls", col=NA, sep="\t",dec=".")
