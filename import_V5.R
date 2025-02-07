
library(dplyr)

A <- readLines(con <-file("allafrica/allafrica_en_3GS.txt",encoding="UTF-8"))
head(A)
tab<-data.frame(A)
A[1]

HD<-tab %>% filter(substr(A,1,3)=="HD ")
WC<-tab %>% filter(substr(A,1,3)=="WC ")
PD<-tab %>% filter(substr(A,1,3)=="PD ")
LA<-tab %>% filter(substr(A,1,3)=="LA ")

## Look for text & other meta
text<-rep("",296)
BY<-rep("",296)
CR<-rep("",296)
CO<-rep("",296)
NS<-rep("",296)
RE<-rep("",296)
IP<-rep("",296)
AN<-rep("",296)
t<-0
flagTD<-F
for (i in 1:31222){
  x<-A[i]
  if (substr(x,1,3)=="BY ") {BY[t+1]<-x} 
  if (substr(x,1,3)=="CR ") {CR[t+1]<-x} 
  if (substr(x,1,2)=="TD") {flagTD<-T
                             t<-t+1}
  if (flagTD==T) {text[t]<-paste(text[t],x)}
  if (substr(x,1,2) %in% c("CO","NS","RE","IP","PU","AN")) {flagTD<-F}
  if (substr(x,1,2)=="CO") {CO[t]<-A[i+1]}
  if (substr(x,1,3)=="NS ") {NS[t]<-A[i+1]}
  if (substr(x,1,3)=="RE ") {RE[t]<-A[i+1]}
  if (substr(x,1,3)=="IPD") {IP[t]<-A[i+1]}
  if (substr(x,1,2)=="AN") {AN[t]<-A[i+1]}
}
TD<-text
tabres <- data.frame(AN,PD,CR,LA,BY,IP,RE,CO,NS, WC,HD,TD)
names(tabres)<-c("AN","PD","CR","LA","BY","IP","RE","CO","NS","WC","HD","TD")
head(tabres)

## Clean Fields
tabres$AN<-gsub(pattern = "Document ",replacement = "",x=tabres$AN)
tabres$HD<-gsub(pattern = "HD \t",replacement = "",x=tabres$HD)
tabres$WC<-gsub(pattern = "WC \t",replacement = "",x=tabres$WC)
tabres$WC<-gsub(pattern = " mots",replacement = "",x=tabres$WC)
tabres$WC<-gsub(pattern = "WC \t",replacement = "",x=tabres$WC)
tabres$WC<-as.numeric(tabres$WC)
tabres$PD<-gsub(pattern = "PD \t",replacement = "",x=tabres$PD)
w<-tabres$PD
w<-gsub(pattern = " janvier ",replacement = "-01-",x=w)
w<-gsub(pattern = " février ",replacement = "-02-",x=w)
w<-gsub(pattern = " mars ",replacement = "-03-",x=w)
w<-gsub(pattern = " avril ",replacement = "-04-",x=w)
w<-gsub(pattern = " mai ",replacement = "-05-",x=w)
w<-gsub(pattern = " juin ",replacement = "-06-",x=w)
w<-gsub(pattern = " juillet ",replacement = "-07-",x=w)
w<-gsub(pattern = " août ",replacement = "-08-",x=w)
w<-gsub(pattern = " septembre ",replacement = "-09-",x=w)
w<-gsub(pattern = " octobre ",replacement = "-10-",x=w)
w<-gsub(pattern = " novembre ",replacement = "-11-",x=w)
w<-gsub(pattern = " décembre ",replacement = "-12-",x=w)
w[nchar(w)==9]<-paste0(rep("0", length(w[nchar(w)==9])),w[nchar(w)==9])
w<-paste0(substr(w,7,10),"/",substr(w,4,5),"/",substr(w,1,2))
tabres$PD<-as.Date(w)
summary(tabres$PD)
tabres$CR<-gsub(pattern = "CR \t",replacement = "",x=tabres$CR)
tabres$LA<-gsub(pattern = "LA \t",replacement = "",x=tabres$LA)
tabres$BY<-gsub(pattern = "BY \t",replacement = "",x=tabres$BY)
tabres$TD<-gsub(pattern = " TD  \t  ",replacement = "",x=tabres$TD)







## Dictionaries of factiva code

### Dictionary of regions
x<-tabres$RE
y<-paste(x,collapse = " | ")
z<- strsplit(y,split =  "\\|")
a<-lapply(z, strsplit, split=" : ")
w<-as.data.frame(a)
dic<-data.frame(codreg=as.character(w[1,]),defreg=as.character(w[2,]))
dic$codreg<-substr(dic$codreg,2,nchar(dic$codreg))
dic$defreg<-substr(dic$defreg,1,nchar(dic$defreg)-1)
dicreg <- dic %>% group_by(codreg, defreg) %>% 
  count() %>% 
  filter(codreg !=" ") %>% 
  arrange(-n) %>%
  as.data.frame()
write.table(dicreg,"allafrica/dicreg.csv", sep=";")


rep<-paste(dicreg$defreg, collapse="|")
tabres$RE<-gsub(rep,"",tabres$RE)
tabres$RE<-gsub(":","",tabres$RE)
tabres$RE<-gsub("\\|","",tabres$RE)
tabres$RE<-gsub("  "," ",tabres$RE)



### Dictionary of topics
x<-tabres$NS
y<-paste(x,collapse = " | ")
z<- strsplit(y,split =  "\\|")
a<-lapply(z, strsplit, split=" : ")
w<-as.data.frame(a)
dic<-data.frame(codtop=as.character(w[1,]),deftop=as.character(w[2,]))
dic$codtop<-substr(dic$codtop,2,nchar(dic$codtop))
dic$deftop<-substr(dic$deftop,1,nchar(dic$deftop)-1)
dictop <- dic %>% group_by(codtop, deftop) %>% 
  count() %>% 
  filter(codtop !=" ") %>% 
  arrange(-n) %>%
  as.data.frame()

write.table(dictop,"allafrica/dictop.csv", sep=";")


rep<-paste(dictop$deftop, collapse="|")
tabres$NS<-gsub(rep,"",tabres$NS)
tabres$NS<-gsub(":","",tabres$NS)
tabres$NS<-gsub("\\|","",tabres$NS)
tabres$NS<-gsub("  "," ",tabres$NS)




### Dictionary of organisations
x<-tabres$CO
y<-paste(x,collapse = " | ")
z<- strsplit(y,split =  "\\|")
a<-lapply(z, strsplit, split=" : ")
w<-as.data.frame(a)
dic<-data.frame(codorg=as.character(w[1,]),deforg=as.character(w[2,]))
dic$codorg<-substr(dic$codorg,2,nchar(dic$codorg))
dic$deforg<-substr(dic$deforg,1,nchar(dic$deforg)-1)
dicorg <- dic %>% group_by(codorg, deforg) %>% 
  count() %>% 
  filter(codorg !=" ") %>% 
  arrange(-n) %>%
  as.data.frame()

write.table(dicorg,"allafrica/dicorg.csv", sep=";")


rep<-paste(dicorg$deforg, collapse="|")
tabres$CO<-gsub(rep,"",tabres$CO)
tabres$CO<-gsub(":","",tabres$CO)
tabres$CO<-gsub("\\|","",tabres$CO)
tabres$CO<-gsub("  "," ",tabres$CO)



## Super dictionary (top + reg +org)
dicorg$type="org"
dicreg$type="reg"
dictop$type="top"
names(dicorg)<-c("code","def","freq","type")
names(dicreg)<-c("code","def","freq","type")
names(dictop)<-c("code","def","freq","type")
dickey<-rbind(dicorg, dicreg,dictop)
write.table(dickey,"allafrica/dickey.csv", sep=";")

tabres$KW<-apply(tabres[,c("RE","CO","NS")],1, paste,collapse=" ")
tabres<-tabres[,c(1:9, 13,10:12)]

## Export RDS
saveRDS(tabres,"allafrica/allafrica_en_3GS.RDS")

## Export txt (title + body)
write.table(tabres, "allafrica/allafrica_en_3GS_text.csv", sep=";",quote = T)

## Export txt (without body)
tabres2<-tabres[,1:12]
write.table(tabres2, "allafrica/allafrica_en_3GS_meta.csv", sep=";",quote = T)


