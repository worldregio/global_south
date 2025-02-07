library(dplyr)
library(readxl)
library(writexl)
library(janitor)



# We assume that Factiva's .rtf file has been converted into .xlsx files with a column dindicating the document number
x<-read_xlsx("corpus/all_Africa_en_GS2_raw.xlsx")

# We create a function for the extraction of each tag
extract_var <- function(fic = x, tag="LP"){
  maxdoc<-max(x$doc)
  myvar<-rep("",maxdoc)
  sel<-x %>% filter(code==tag)
  k <- dim(sel)[1]
  for (i in 1:k){ myvar[sel$doc[i]]<-sel$value[i]}
  return(myvar)
}


# We extract each tag and make corrections when necessary

# AN :  Unique Factiva Code
AN<-extract_var(x,"AN")
AN<-gsub("Document ","",AN)

# PD : Publication Date 
PD<-extract_var(x,"PD") %>% 
      as.numeric() %>% 
      excel_numeric_to_date()

# WC : Word count
WC<-extract_var(x,"WC")
WC <- gsub(" mots","",WC)
WC <- gsub(",","",WC)
WC<-as.numeric(WC)
summary(WC)



# CO : Organisation
CO<-extract_var(x,"CO")


# NS : Topic
NS<-extract_var(x,"NS")


# RE : Regions
RE<-extract_var(x,"RE")

# HD : Title
HD<-extract_var(x,"HD")

# LP : Lead Paragraph
LP<-extract_var(x,"LP")

# TD : Body
TD<-extract_var(x,"TD")

tabres<-data.frame(AN,PD,WC, CO, NS,RE, HD, LP, TD)

## Dictionaries of factiva code -----------------------------

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
write.table(dicreg,"corpus/dicreg.csv", sep=";")


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

write.table(dictop,"corpus/dictop.csv", sep=";")


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

write.table(dicorg,"corpus/dicorg.csv", sep=";")


rep<-paste(dicorg$deforg, collapse="|")
tabres$CO<-gsub(rep,"",tabres$CO)
tabres$CO<-gsub(":","",tabres$CO)
tabres$CO<-gsub("\\|","",tabres$CO)
tabres$CO<-gsub("  "," ",tabres$CO)





## Export RDS
saveRDS(tabres,"corpus/All_Africa_en_GS2.RDS")

## Export xlsx)
write_xlsx(tabres,"corpus/Allafrica_en_GS2.xlsx" )
