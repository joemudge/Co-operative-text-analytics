library(tm)
library(SnowballC)
library(tidytext)

#Creating a Stem Dictionary
stem_dict<-c("complicate","company","association","cooperative")
  
#Cleaning corpus function
clean_corpus<-function(corpus){
  corpus<-tm_map(corpus,stripWhitespace)
  corpus<-tm_map(corpus,removeNumbers)
  corpus<-tm_map(corpus,tolower)
  corpus<-tm_map(corpus,removePunctuation)
  corpus<-tm_map(corpus,stemDocument)
  corpus<-tm_map(corpus,removeWords, c(stopwords("english"),"coop","cooper","alberta","feder","granni","growmark","saskatchewan","afa","scotsburn","apa","federe","canadian","canada","manitoba","caiss","british","columbia","blue","cross","fcl","costco","sobey"))
  return(corpus)           
}



#CREATING CORPORA FOR BOTH COOP AND NONCOOP FILES
#CO-OPERATIVES FILES
# lists the file paths of the different groups of companies
sectors1<-list.dirs(path="/Users/christiandeangelis/Desktop/Summer Project/COOP",full.names=T,recursive=F)
# lists the names of the different groups of companies
sectornames1<-list.dirs(path="/Users/christiandeangelis/Desktop/Summer Project/COOP",full.names=F,recursive=F)
#makes an empty list
corpora.list1<-vector("list", length(sectornames1))
names(corpora.list1)<-sectornames1
#loops through each file within each company within each group and adds them to a nested list
for (i in 1:length(sectors1)){
  corpora.list1[i]<-list(temp=list.dirs(path=sectors1[i],full.names=T,recursive=F))
  names(corpora.list1[[i]])<-list.dirs(path=sectors1[i],full.names=F,recursive=F)
  for(j in 1:length(corpora.list1[[i]])) {
    textfiles1<-list.files(as.character(corpora.list1[[i]][j]),full.names=T)
    for (k in 1:length(textfiles1)) {
      textfiles1[k]<-readChar(textfiles1[k],file.info(textfiles1[k])$size)
    }
    corpora.list1[[i]][j]<-list(temp=textfiles1)
    corpora.list1[[i]][j]<-paste(textfiles1,collapse="")     #### if you want content from all page files in one string, not sure it's necessary after all though ###
  }
}

#Creates corpora for all coop words and all non coop words
coop.all<-Corpus(VectorSource(unlist(corpora.list1)))
noncoop.all<-Corpus(VectorSource(unlist(corpora.list2)))
coop.all<-clean_corpus(coop.all)
noncoop.all<-clean_corpus(noncoop.all)


#Creates corpora for different subsets of the non coops nested list then cleans them
Ag.coop.corpus<-Corpus(VectorSource(unlist(corpora.list1[["Agriculture"]])))
Fin.coop.corpus<-Corpus(VectorSource(unlist(corpora.list1[["Finance"]])))
Hea.coop.corpus<-Corpus(VectorSource(unlist(corpora.list1[["Health"]])))
Lab.coop.corpus<-Corpus(VectorSource(unlist(corpora.list1[["Labour"]])))
Ret.coop.corpus<-Corpus(VectorSource(unlist(corpora.list1[["Retail , Consumer"]])))
Serv.coop.corpus<-Corpus(VectorSource(unlist(corpora.list1[["Service"]])))
Ag.coop.corpus<-clean_corpus(Ag.coop.corpus)
Fin.coop.corpus<-clean_corpus(Fin.coop.corpus)
Hea.coop.corpus<-clean_corpus(Hea.coop.corpus)
Lab.coop.corpus<-clean_corpus(Lab.coop.corpus)
Ret.coop.corpus<-clean_corpus(Ret.coop.corpus)
Serv.coop.corpus<-clean_corpus(Serv.coop.corpus)
#NON-CO-OPERATIVE FILES
# lists the file paths of the different groups of companies
sectors2<-list.dirs(path="/Users/christiandeangelis/Desktop/Summer Project/NONCOOP",full.names=T,recursive=F)
# lists the names of the different groups of companies
sectornames2<-list.dirs(path="/Users/christiandeangelis/Desktop/Summer Project/NONCOOP",full.names=F,recursive=F)
#makes an empty list
corpora.list2<-vector("list", length(sectornames2))
names(corpora.list2)<-sectornames2
#loops through each file within each company within each group and adds them to a nested list
for (i in 1:length(sectors2)){
  corpora.list2[i]<-list(temp=list.dirs(path=sectors2[i],full.names=T,recursive=F))
  names(corpora.list2[[i]])<-list.dirs(path=sectors2[i],full.names=F,recursive=F)
  for(j in 1:length(corpora.list2[[i]])) {
    textfiles2<-list.files(as.character(corpora.list2[[i]][j]),full.names=T)
    for (k in 1:length(textfiles2)) {
      textfiles2[k]<-readChar(textfiles2[k],file.info(textfiles2[k])$size)
    }
    corpora.list2[[i]][j]<-list(temp=textfiles2)
    corpora.list2[[i]][j]<-paste(textfiles2,collapse="")     #### if you want content from all page files in one string, not sure it's necessary after all though ###
  }
}
#Creates corpora for different subsets of the non coops nested list
Ag.noncoop.corpus1<-Corpus(VectorSource(unlist(corpora.list2[["Agriculture"]])))
Fin.noncoop.corpus1<-Corpus(VectorSource(unlist(corpora.list2[["Finance"]])))
Hea.noncoop.corpus1<-Corpus(VectorSource(unlist(corpora.list2[["Health"]])))
Lab.noncoop.corpus1<-Corpus(VectorSource(unlist(corpora.list2[["Labour"]])))
Ret.noncoop.corpus1<-Corpus(VectorSource(unlist(corpora.list2[["Retail , Consumer"]])))
Serv.noncoop.corpus1<-Corpus(VectorSource(unlist(corpora.list2[["Service"]])))
#Cleanup of lists
Ag.noncoop.corpus<-clean_corpus(Ag.noncoop.corpus1)
Fin.noncoop.corpus<-clean_corpus(Fin.noncoop.corpus1)
Hea.noncoop.corpus<-clean_corpus(Hea.noncoop.corpus1)
Lab.noncoop.corpus<-clean_corpus(Lab.noncoop.corpus1)
Ret.noncoop.corpus<-clean_corpus(Ret.noncoop.corpus1)
Serv.noncoop.corpus<-clean_corpus(Serv.noncoop.corpus1)


#COOP TDMS
#Agriculture
TDM.AG.COOP<-TermDocumentMatrix(Ag.coop.corpus)
  #This will get us all the terms and frequencies for agriculture companies
M.AG.COOP<-as.matrix(TDM.AG.COOP)
for (i in 1:NROW(M.AG.COOP)){
     for (j in 1:length(Ag.coop.corpus)){
     M.AG.COOP[i,j]<-M.AG.COOP[i,j]*sum(as.numeric(M.AG.COOP[,j]))/1000
     }
}
#Finance
TDM.FIN.COOP<-TermDocumentMatrix(Fin.coop.corpus)
#This will get us all the terms and frequencies for Finance companies
  M.FIN.COOP<-as.matrix(TDM.FIN.COOP)
  for (i in 1:NROW(M.FIN.COOP)){
    for (j in 1:length(Fin.coop.corpus)){
     M.FIN.COOP[i,j]<-M.FIN.COOP[i,j]*sum(as.numeric(M.FIN.COOP[,j]))/1000
    }
  }
#Health
TDM.HEA.COOP<-TermDocumentMatrix(Hea.coop.corpus)
#This will get us all the terms and frequencies for Health companies
M.HEA.COOP<-as.matrix(TDM.HEA.COOP)
for (i in 1:NROW(M.HEA.COOP)){
  for (j in 1:length(Hea.coop.corpus)){
    M.HEA.COOP[i,j]<-M.HEA.COOP[i,j]*sum(as.numeric(M.HEA.COOP[,j]))/1000
  }
}
#Labour
TDM.LAB.COOP<-TermDocumentMatrix(Lab.coop.corpus)
#This will get us all the terms and frequencies for Health companies
M.LAB.COOP<-as.matrix(TDM.LAB.COOP)
for (i in 1:NROW(M.LAB.COOP)){
  for (j in 1:length(Lab.coop.corpus)){
    M.LAB.COOP[i,j]<-M.LAB.COOP[i,j]*sum(as.numeric(M.LAB.COOP[,j]))/1000
  }
}
#Retail
TDM.RET.COOP<-TermDocumentMatrix(Ret.coop.corpus)
#This will get us all the terms and frequencies for Health companies
M.RET.COOP<-as.matrix(TDM.RET.COOP)
for (i in 1:NROW(M.RET.COOP)){
  for (j in 1:length(Ret.coop.corpus)){
    M.RET.COOP[i,j]<-M.RET.COOP[i,j]*sum(as.numeric(M.RET.COOP[,j]))/1000
  }
}
#Services
TDM.SERV.COOP<-TermDocumentMatrix(Serv.coop.corpus)
#This will get us all the terms and frequencies for Health companies
M.SERV.COOP<-as.matrix(TDM.SERV.COOP)
for (i in 1:NROW(M.SERV.COOP)){
  for (j in 1:length(Serv.coop.corpus)){
    M.SERV.COOP[i,j]<-M.SERV.COOP[i,j]*sum(as.numeric(M.SERV.COOP[,j]))/1000
  }
}


#NON COOP TDMS
#Agriculture
TDM.AG.NONCOOP<-TermDocumentMatrix(Ag.noncoop.corpus)
#This will get us all the terms and frequencies for agriculture companies
M.AG.NONCOOP<-as.matrix(TDM.AG.NONCOOP)
for (i in 1:NROW(M.AG.NONCOOP)){
  for (j in 1:length(Ag.noncoop.corpus)){
    M.AG.NONCOOP[i,j]<-M.AG.NONCOOP[i,j]*sum(as.numeric(M.AG.NONCOOP[,j]))/1000
  }
}
#Finance
TDM.FIN.NONCOOP<-TermDocumentMatrix(Fin.noncoop.corpus)
#This will get us all the terms and frequencies for agriculture companies
M.FIN.NONCOOP<-as.matrix(TDM.FIN.NONCOOP)
for (i in 1:NROW(M.FIN.NONCOOP)){
  for (j in 1:length(Fin.noncoop.corpus)){
    M.FIN.NONCOOP[i,j]<-M.FIN.NONCOOP[i,j]*sum(as.numeric(M.FIN.NONCOOP[,j]))/1000
  }
}
#Health
TDM.HEA.NONCOOP<-TermDocumentMatrix(Hea.noncoop.corpus)
#This will get us all the terms and frequencies for agriculture companies
M.HEA.NONCOOP<-as.matrix(TDM.HEA.NONCOOP)
for (i in 1:NROW(M.HEA.NONCOOP)){
  for (j in 1:length(Hea.noncoop.corpus)){
    M.HEA.NONCOOP[i,j]<-M.HEA.NONCOOP[i,j]*sum(as.numeric(M.HEA.NONCOOP[,j]))/1000
  }
}
#Labor
TDM.LAB.NONCOOP<-TermDocumentMatrix(Lab.noncoop.corpus)
#This will get us all the terms and frequencies for agriculture companies
M.LAB.NONCOOP<-as.matrix(TDM.LAB.NONCOOP)
for (i in 1:NROW(M.LAB.NONCOOP)){
  for (j in 1:length(Lab.noncoop.corpus)){
    M.LAB.NONCOOP[i,j]<-M.LAB.NONCOOP[i,j]*sum(as.numeric(M.LAB.NONCOOP[,j]))/1000
  }
}
#Retail
TDM.RET.NONCOOP<-TermDocumentMatrix(Ret.noncoop.corpus)
#This will get us all the terms and frequencies for agriculture companies
M.RET.NONCOOP<-as.matrix(TDM.RET.NONCOOP)
for (i in 1:NROW(M.RET.NONCOOP)){
  for (j in 1:length(Ret.noncoop.corpus)){
    M.RET.NONCOOP[i,j]<-M.RET.NONCOOP[i,j]*sum(as.numeric(M.RET.NONCOOP[,j]))/1000
  }
}
#Service
TDM.SERV.NONCOOP<-TermDocumentMatrix(Serv.noncoop.corpus)
#This will get us all the terms and frequencies for non coop service companies
M.SERV.NONCOOP<-as.matrix(TDM.SERV.NONCOOP)
for (i in 1:NROW(M.SERV.NONCOOP)){
  for (j in 1:length(Serv.noncoop.corpus)){
    M.SERV.NONCOOP[i,j]<-M.SERV.NONCOOP[i,j]*sum(as.numeric(M.SERV.NONCOOP[,j]))/1000
  }
}

#Create Joint Dataframes
#Agriculture
DF.AG.COOP<-as.data.frame(M.AG.COOP)
DF.AG.COOP$words<-rownames(DF.AG.COOP)
DF.AG.NONCOOP<-as.data.frame(M.AG.NONCOOP)
DF.AG.NONCOOP$words<-rownames(DF.AG.NONCOOP)
DF.AG.MERGE<-merge(DF.AG.COOP,DF.AG.NONCOOP,by='words',all.x=T,all.y=T,replace=T)
DF.AG.MERGE[is.na(DF.AG.MERGE)]<-0
          #MEANS
AVG.AG.COOP<-apply(DF.AG.MERGE[,2:length(Ag.coop.corpus)+1],1,mean)
AVG.AG.NONCOOP<-apply(DF.AG.MERGE[,(length(Ag.coop.corpus)+2):(length(Ag.coop.corpus)+length(Ag.noncoop.corpus)+1)],1,mean)
AVG.AG.ALL<-apply(DF.AG.MERGE[,2:(length(Ag.coop.corpus)+length(Ag.noncoop.corpus)+1)],1,mean)
OV.AVG.AG<-mean(AVG.AG.ALL[1:NROW(DF.AG.MERGE)])
        #Bind them and create new DF
DF.AG.MERGE<-cbind(DF.AG.MERGE,AVG.AG.COOP)
DF.AG.MERGE<-cbind(DF.AG.MERGE,AVG.AG.NONCOOP)
DF.AG.MERGE<-cbind(DF.AG.MERGE,AVG.AG.ALL)
DF.AG.MERGE["Ratio- Coop to Noncoop"] <- NA
DF.AG.OTHER<-data.frame(matrix(ncol = 3, nrow = 10000))
colnames(DF.AG.OTHER)<-c("words","Average Freq Coop", "Average Freq Non-Coop")
        #RATIO LOOP
count.ag<-0
for (i in 1:NROW(DF.AG.MERGE)){
  if (DF.AG.MERGE[i,length(Ag.coop.corpus)+length(Ag.noncoop.corpus)+2]>0 & DF.AG.MERGE[i,length(Ag.coop.corpus)+length(Ag.noncoop.corpus)+3]>0){
    DF.AG.MERGE[i,length(Ag.coop.corpus)+length(Ag.noncoop.corpus)+5]<-(DF.AG.MERGE[i,length(Ag.coop.corpus)+length(Ag.noncoop.corpus)+2])/(DF.AG.MERGE[i,length(Ag.coop.corpus)+length(Ag.noncoop.corpus)+3])
    count.ag<-count.ag+1
  }
    else if (DF.AG.MERGE[i,length(Ag.coop.corpus)+length(Ag.noncoop.corpus)+2]>OV.AVG.AG | DF.AG.MERGE[i,length(Ag.coop.corpus)+length(Ag.noncoop.corpus)+3]>OV.AVG.AG){

    DF.AG.OTHER[count.ag,1]<-DF.AG.MERGE[i,1]
    DF.AG.OTHER[count.ag,2]<-DF.AG.MERGE[i,length(Ag.coop.corpus)+length(Ag.noncoop.corpus)+2]
    DF.AG.OTHER[count.ag,3]<-DF.AG.MERGE[i,length(Ag.coop.corpus)+length(Ag.noncoop.corpus)+3]
    DF.AG.MERGE[i,length(Ag.coop.corpus)+length(Ag.noncoop.corpus)+5]<-NA
  }
    else{
      DF.AG.MERGE[i,length(Ag.coop.corpus)+length(Ag.noncoop.corpus)+5]<-NA
   }
}
DF.AG.OTHER<-DF.AG.OTHER[complete.cases(DF.AG.OTHER),]
  #Create New Ratio DF
DF.RAT.AG<-DF.AG.MERGE[,c(1,length(Ag.coop.corpus)+length(Ag.noncoop.corpus)+5)]
DF.RAT.AG<-DF.RAT.AG[complete.cases(DF.RAT.AG), ]
DF.RAT.AG.dec<-DF.RAT.AG[order(DF.RAT.AG[,2]),] 
DF.RAT.AG.inc<-DF.RAT.AG[order(DF.RAT.AG[,2],decreasing=TRUE),] 
par(mfrow=c(1,1))
barplot(DF.RAT.AG.inc[1:10,2],names.arg=DF.RAT.AG.inc[1:10,1],las=0.5,main="Top 10 Ratios of Averages from Co-op to Non-Coop in AG",xlab="words",ylab="Ratio",col="green",cex.lab=0.8,cex.names=0.7)
barplot(DF.RAT.AG.dec[1:10,2],names.arg=DF.RAT.AG.dec[1:10,1],las=0.8,main="Bottom 10 Ratios of Averages from Co-op to Non-Coop in AG",xlab="words",ylab="Ratio",col="green",cex.lab=0.8,cex.names=0.7)
  #Show Words that were common in one but non-existant in other
par(mfrow=c(1,1))
DF.AG.OTHER<-DF.AG.OTHER[order(DF.AG.OTHER[,3],decreasing = T),]
barplot(DF.AG.OTHER[1:10,3],names.arg=DF.AG.OTHER[1:10,1],las=0.5,main="Word Averages in Non-Coop that aren't in Co-op",xlab="Words",ylab="Average Word Frequency",col="green")
          


#Finance
DF.FIN.COOP<-as.data.frame(M.FIN.COOP)
DF.FIN.COOP$words<-rownames(DF.FIN.COOP)
DF.FIN.NONCOOP<-as.data.frame(M.FIN.NONCOOP)
DF.FIN.NONCOOP$words<-rownames(DF.FIN.NONCOOP)
DF.FIN.MERGE<-merge(DF.FIN.COOP,DF.FIN.NONCOOP,by='words',all.x=T,all.y=T)
DF.FIN.MERGE[is.na(DF.FIN.MERGE)]<-0
#MEANS
AVG.FIN.COOP<-apply(DF.FIN.MERGE[,2:length(Fin.coop.corpus)+1],1,mean)
AVG.FIN.NONCOOP<-apply(DF.FIN.MERGE[,(length(Fin.coop.corpus)+2):(length(Fin.coop.corpus)+length(Fin.noncoop.corpus)+1)],1,mean)
AVG.FIN.ALL<-apply(DF.FIN.MERGE[,2:(length(Fin.coop.corpus)+length(Fin.noncoop.corpus)+1)],1,mean)
OV.AVG.FIN<-mean(AVG.FIN.ALL[1:NROW(DF.FIN.MERGE)])
#Bind them and create new DF
DF.FIN.MERGE<-cbind(DF.FIN.MERGE,AVG.FIN.COOP)
DF.FIN.MERGE<-cbind(DF.FIN.MERGE,AVG.FIN.NONCOOP)
DF.FIN.MERGE<-cbind(DF.FIN.MERGE,AVG.FIN.ALL)
DF.FIN.MERGE["Ratio- Coop to Noncoop"] <- NA
DF.FIN.OTHER<-data.frame(matrix(ncol = 3, nrow = 10000))
colnames(DF.FIN.OTHER)<-c("words","Average Freq Coop", "Average Freq Non-Coop")
#RATIO LOOP
count.fin<-0
for (i in 1:NROW(DF.FIN.MERGE)){
  if (DF.FIN.MERGE[i,length(Fin.coop.corpus)+length(Fin.noncoop.corpus)+2]>0 & DF.FIN.MERGE[i,length(Fin.coop.corpus)+length(Fin.noncoop.corpus)+3]>0){
    DF.FIN.MERGE[i,length(Fin.coop.corpus)+length(Fin.noncoop.corpus)+5]<-(DF.FIN.MERGE[i,length(Fin.coop.corpus)+length(Fin.noncoop.corpus)+2])/(DF.FIN.MERGE[i,length(Fin.coop.corpus)+length(Fin.noncoop.corpus)+3])
    count.fin<-count.fin+1
  }
  else if (DF.FIN.MERGE[i,length(Fin.coop.corpus)+length(Fin.noncoop.corpus)+2]>OV.AVG.FIN | DF.FIN.MERGE[i,length(Fin.coop.corpus)+length(Fin.noncoop.corpus)+3]>OV.AVG.FIN){
    
    DF.FIN.OTHER[count.fin,1]<-DF.FIN.MERGE[i,1]
    DF.FIN.OTHER[count.fin,2]<-DF.FIN.MERGE[i,length(Fin.coop.corpus)+length(Fin.noncoop.corpus)+2]
    DF.FIN.OTHER[count.fin,3]<-DF.FIN.MERGE[i,length(Fin.coop.corpus)+length(Fin.noncoop.corpus)+3]
    DF.FIN.MERGE[i,length(Fin.coop.corpus)+length(Fin.noncoop.corpus)+5]<-NA
  }
  else{
    DF.FIN.MERGE[i,length(Fin.coop.corpus)+length(Fin.noncoop.corpus)+5]<-NA
  }
}
DF.FIN.OTHER<-DF.FIN.OTHER[complete.cases(DF.FIN.OTHER),]
#Create New Ratio DF
DF.RAT.FIN<-DF.AG.MERGE[,c(1,length(Ag.coop.corpus)+length(Ag.noncoop.corpus)+5)]
DF.RAT.FIN<-DF.RAT.FIN[complete.cases(DF.RAT.FIN), ]
DF.RAT.FIN.dec<-DF.RAT.AG[order(DF.RAT.FIN[,2]),] 
DF.RAT.FIN.inc<-DF.RAT.AG[order(DF.RAT.FIN[,2],decreasing=TRUE),] 
par(mfrow=c(1,1))
barplot(DF.RAT.FIN.inc[1:10,2],names.arg=DF.RAT.FIN.inc[1:10,1],las=0.5,main="Top 10 Ratios of Averages from Co-op to Non-Coop in FIN",xlab="words",ylab="Ratio",col="blue",cex.lab=0.8,cex.names=0.7)
barplot(DF.RAT.FIN.dec[1:10,2],names.arg=DF.RAT.FIN.dec[1:10,1],las=0.8,main="Bottom 10 Ratios of Averages from Co-op to Non-Coop in FIN",xlab="words",ylab="Ratio",col="blue",cex.lab=0.8,cex.names=0.7)
#Show Words that were common in one but non-existant in other
par(mfrow=c(1,1))
DF.FIN.OTHER<-DF.FIN.OTHER[order(DF.FIN.OTHER[,3],decreasing = T),]
barplot(DF.FIN.OTHER[1:10,3],names.arg=DF.FIN.OTHER[1:10,1],las=0.5,main="Word Averages in Non-Coop that aren't in Co-op",xlab="Words",ylab="Average Word Frequency",col="blue")


#Health
DF.HEA.COOP<-as.data.frame(M.HEA.COOP)
DF.HEA.COOP$words<-rownames(DF.HEA.COOP)
DF.HEA.NONCOOP<-as.data.frame(M.HEA.NONCOOP)
DF.HEA.NONCOOP$words<-rownames(DF.HEA.NONCOOP)
DF.HEA.MERGE<-merge(DF.HEA.COOP,DF.HEA.NONCOOP,by='words',all.x=T,all.y=T)
DF.HEA.MERGE[is.na(DF.HEA.MERGE)]<-0
#MEANS
AVG.HEA.COOP<-apply(DF.HEA.MERGE[,2:(length(Hea.coop.corpus)+1)],1,mean)
AVG.HEA.NONCOOP<-apply(DF.HEA.MERGE[,(length(Hea.coop.corpus)+2):(length(Hea.coop.corpus)+length(Hea.noncoop.corpus)+1)],1,mean)
AVG.HEA.ALL<-apply(DF.HEA.MERGE[,2:(length(Hea.coop.corpus)+length(Hea.noncoop.corpus)+1)],1,mean)
OV.AVG.HEA<-mean(AVG.HEA.ALL[1:NROW(DF.HEA.MERGE)])
#Bind them and create new DF
DF.HEA.MERGE<-cbind(DF.HEA.MERGE,AVG.HEA.COOP)
DF.HEA.MERGE<-cbind(DF.HEA.MERGE,AVG.HEA.NONCOOP)
DF.HEA.MERGE<-cbind(DF.HEA.MERGE,AVG.HEA.ALL)
DF.HEA.MERGE["Ratio- Coop to Noncoop"] <- NA
DF.HEA.OTHER<-data.frame(matrix(ncol = 3, nrow = 10000))
colnames(DF.HEA.OTHER)<-c("words","Average Freq Coop", "Average Freq Non-Coop")
#RATIO LOOP
count.hea<-0
for (i in 1:NROW(DF.HEA.MERGE)){
  if (DF.HEA.MERGE[i,length(Hea.coop.corpus)+length(Hea.noncoop.corpus)+2]>0 & DF.HEA.MERGE[i,length(Hea.coop.corpus)+length(Hea.noncoop.corpus)+3]>0){
    DF.HEA.MERGE[i,length(Hea.coop.corpus)+length(Hea.noncoop.corpus)+5]<-(DF.HEA.MERGE[i,length(Hea.coop.corpus)+length(Hea.noncoop.corpus)+2])/(DF.HEA.MERGE[i,length(Hea.coop.corpus)+length(Hea.noncoop.corpus)+3])
    count.hea<-count.hea+1
  }
  else if (DF.HEA.MERGE[i,length(Hea.coop.corpus)+length(Hea.noncoop.corpus)+2]>OV.AVG.HEA | DF.HEA.MERGE[i,length(Hea.coop.corpus)+length(Hea.noncoop.corpus)+3]>OV.AVG.HEA){
    DF.HEA.OTHER[count.hea,1]<-DF.HEA.MERGE[i,1]
    DF.HEA.OTHER[count.hea,2]<-DF.HEA.MERGE[i,length(Hea.coop.corpus)+length(Hea.noncoop.corpus)+2]
    DF.HEA.OTHER[count.hea,3]<-DF.HEA.MERGE[i,length(Hea.coop.corpus)+length(Hea.noncoop.corpus)+3]
    DF.HEA.MERGE[i,length(Hea.coop.corpus)+length(Hea.noncoop.corpus)+5]<-NA
  }
  else{
    DF.HEA.MERGE[i,length(Hea.coop.corpus)+length(Hea.noncoop.corpus)+5]<-NA
  }
}
DF.HEA.OTHER<-DF.HEA.OTHER[complete.cases(DF.HEA.OTHER),]
#Create New Ratio DF
DF.RAT.HEA<-DF.HEA.MERGE[,c(1,length(Hea.coop.corpus)+length(Hea.noncoop.corpus)+5)]
DF.RAT.HEA<-DF.RAT.HEA[complete.cases(DF.RAT.HEA), ]
DF.RAT.HEA.dec<-DF.RAT.HEA[order(DF.RAT.HEA[,2]),] 
DF.RAT.HEA.inc<-DF.RAT.HEA[order(DF.RAT.HEA[,2],decreasing=TRUE),] 
par(mfrow=c(1,1))
barplot(DF.RAT.HEA.inc[1:10,2],names.arg=DF.RAT.HEA.inc[1:10,1],las=0.5,main="Top 10 Ratios of Averages from Co-op to Non-Coop in HEA",xlab="words",ylab="Ratio",col="yellow",cex.lab=0.8,cex.names=0.7)
barplot(DF.RAT.HEA.dec[1:10,2],names.arg=DF.RAT.HEA.dec[1:10,1],las=0.8,main="Bottom 10 Ratios of Averages from Co-op to Non-Coop in HEA",xlab="words",ylab="Ratio",col="yellow",cex.lab=0.8,cex.names=0.7)
#Show Words that were common in one but non-existant in other
par(mfrow=c(1,1))
DF.HEA.OTHER<-DF.HEA.OTHER[order(DF.HEA.OTHER[,3],decreasing = T),]
barplot(DF.HEA.OTHER[1:10,3],names.arg=DF.HEA.OTHER[1:10,1],las=0.5,main="Word Averages in Non-Coop that aren't in Co-op",xlab="Words",ylab="Average Word Frequency",col="yellow")


#Labour
DF.LAB.COOP<-as.data.frame(M.LAB.COOP)
DF.LAB.COOP$words<-rownames(DF.LAB.COOP)
DF.LAB.NONCOOP<-as.data.frame(M.LAB.NONCOOP)
DF.LAB.NONCOOP$words<-rownames(DF.LAB.NONCOOP)
DF.LAB.MERGE<-merge(DF.LAB.COOP,DF.LAB.NONCOOP,by='words',all.x=T,all.y=T)
View(DF.LAB.MERGE)
DF.LAB.MERGE[is.na(DF.LAB.MERGE)]<-0
#MEANS
AVG.LAB.COOP<-DF.LAB.MERGE[,2]
AVG.LAB.NONCOOP<-DF.LAB.MERGE[,3]
AVG.LAB.ALL<-apply(DF.LAB.MERGE[,2:(length(Lab.coop.corpus)+length(Lab.noncoop.corpus)+1)],1,mean)
OV.AVG.LAB<-mean(AVG.LAB.ALL[1:NROW(DF.LAB.MERGE)])
#Bind them and create new DF
DF.LAB.MERGE<-cbind(DF.LAB.MERGE,AVG.LAB.COOP)
DF.LAB.MERGE<-cbind(DF.LAB.MERGE,AVG.LAB.NONCOOP)
DF.LAB.MERGE<-cbind(DF.LAB.MERGE,AVG.LAB.ALL)
DF.LAB.MERGE["  Ratio- Coop to Noncoop"] <- NA
DF.LAB.OTHER<-data.frame(matrix(ncol = 3, nrow = 10000))
colnames(DF.LAB.OTHER)<-c("words","Average Freq Coop", "Average Freq Non-Coop")
#  RATIO LOOP
count.lab<-0
for (i in 1:NROW(DF.LAB.MERGE)){
  if (DF.LAB.MERGE[i,length(Lab.coop.corpus)+length(Lab.noncoop.corpus)+2]>0 & DF.LAB.MERGE[i,length(Lab.coop.corpus)+length(Lab.noncoop.corpus)+3]>0){
    DF.LAB.MERGE[i,length(Lab.coop.corpus)+length(Lab.noncoop.corpus)+5]<-(DF.LAB.MERGE[i,length(Lab.coop.corpus)+length(Lab.noncoop.corpus)+2])/(DF.LAB.MERGE[i,length(Lab.coop.corpus)+length(Lab.noncoop.corpus)+3])
    count.lab<-count.lab+1
  }
  else if (DF.LAB.MERGE[i,length(Lab.coop.corpus)+length(Lab.noncoop.corpus)+2]>OV.AVG.LAB | DF.LAB.MERGE[i,length(Lab.coop.corpus)+length(Lab.noncoop.corpus)+3]>OV.AVG.LAB){
    DF.LAB.OTHER[count.hea,1]<-DF.LAB.MERGE[i,1]
    DF.LAB.OTHER[count.hea,2]<-DF.LAB.MERGE[i,length(Lab.coop.corpus)+length(Lab.noncoop.corpus)+2]
    DF.LAB.OTHER[count.hea,3]<-DF.LAB.MERGE[i,length(Lab.coop.corpus)+length(Lab.noncoop.corpus)+3]
    DF.LAB.MERGE[i,length(Lab.coop.corpus)+length(Lab.noncoop.corpus)+5]<-NA
  }
  else{
    DF.LAB.MERGE[i,length(Lab.coop.corpus)+length(Lab.noncoop.corpus)+5]<-NA
  }
}
DF.LAB.OTHER<-DF.LAB.OTHER[complete.cases(DF.LAB.OTHER),]
#Create New Ratio DF
DF.RAT.LAB<-DF.LAB.MERGE[,c(1,length(Lab.coop.corpus)+length(Lab.noncoop.corpus)+5)]
DF.RAT.LAB<-DF.RAT.LAB[complete.cases(DF.RAT.LAB), ]
DF.RAT.LAB.dec<-DF.RAT.LAB[order(DF.RAT.LAB[,2]),] 
DF.RAT.LAB.inc<-DF.RAT.LAB[order(DF.RAT.LAB[,2],decreasing=TRUE),] 
par(mfrow=c(1,1))
barplot(DF.RAT.LAB.inc[1:10,2],names.arg=DF.RAT.LAB.inc[1:10,1],las=0.5,main="Top 10 Ratios of Averages from Co-op to Non-Coop in LAB",xlab="words",ylab="Ratio",col="brown",cex.lab=0.8,cex.names=0.7)
barplot(DF.RAT.LAB.dec[1:10,2],names.arg=DF.RAT.LAB.dec[1:10,1],las=0.8,main="Bottom 10 Ratios of Averages from Co-op to Non-Coop in LAB",xlab="words",ylab="Ratio",col="brown",cex.lab=0.8,cex.names=0.7)
#Show Words that were common in one but non-existant in other
par(mfrow=c(1,1))
DF.LAB.OTHER<-DF.LAB.OTHER[order(DF.LAB.OTHER[,3],decreasing = T),]
barplot(DF.LAB.OTHER[1:10,3],names.arg=DF.LAB.OTHER[1:10,1],las=0.5,main="Word Averages in Non-Coop that aren't in Co-op",xlab="Words",ylab="Average Word Frequency",col="brown")


#Retail
DF.RET.COOP<-as.data.frame(M.RET.COOP)
DF.RET.COOP$words<-rownames(DF.RET.COOP)
DF.RET.NONCOOP<-as.data.frame(M.RET.NONCOOP)
DF.RET.NONCOOP$words<-rownames(DF.RET.NONCOOP)
DF.RET.MERGE<-merge(DF.RET.COOP,DF.RET.NONCOOP,by='words',all.x=T,all.y=T)
DF.RET.MERGE[is.na(DF.RET.MERGE)]<-0
#MEANS
AVG.RET.COOP<-apply(DF.RET.MERGE[,2:(length(Ret.coop.corpus)+1)],1,mean)
AVG.RET.NONCOOP<-apply(DF.RET.MERGE[,(length(Ret.coop.corpus)+2):(length(Ret.coop.corpus)+length(Ret.noncoop.corpus)+1)],1,mean)
AVG.RET.ALL<-apply(DF.RET.MERGE[,2:(length(Ret.coop.corpus)+length(Ret.noncoop.corpus)+1)],1,mean)
OV.AVG.RET<-mean(AVG.RET.ALL[1:NROW(DF.RET.MERGE)])
#Bind them and create new DF
DF.RET.MERGE<-cbind(DF.RET.MERGE,AVG.RET.COOP)
DF.RET.MERGE<-cbind(DF.RET.MERGE,AVG.RET.NONCOOP)
DF.RET.MERGE<-cbind(DF.RET.MERGE,AVG.RET.ALL)
DF.RET.MERGE["  Ratio- Coop to Noncoop"] <- NA
DF.RET.OTHER<-data.frame(matrix(ncol = 3, nrow = 10000))
colnames(DF.RET.OTHER)<-c("words","Average Freq Coop", "Average Freq Non-Coop")
#  RATIO LOOP
count.ret<-0
for (i in 1:NROW(DF.RET.MERGE)){
  if (DF.RET.MERGE[i,length(Ret.coop.corpus)+length(Ret.noncoop.corpus)+2]>0 & DF.RET.MERGE[i,length(Ret.coop.corpus)+length(Ret.noncoop.corpus)+3]>0){
    DF.RET.MERGE[i,length(Ret.coop.corpus)+length(Ret.noncoop.corpus)+5]<-(DF.RET.MERGE[i,length(Ret.coop.corpus)+length(Ret.noncoop.corpus)+2])/(DF.RET.MERGE[i,length(Ret.coop.corpus)+length(Ret.noncoop.corpus)+3])
    count.ret<-count.ret+1
  }
  else if (DF.RET.MERGE[i,length(Ret.coop.corpus)+length(Ret.noncoop.corpus)+2]>OV.AVG.RET | DF.RET.MERGE[i,length(Ret.coop.corpus)+length(Ret.noncoop.corpus)+3]>OV.AVG.RET){
    DF.RET.OTHER[count.ret,1]<-DF.RET.MERGE[i,1]
    DF.RET.OTHER[count.ret,2]<-DF.RET.MERGE[i,length(Ret.coop.corpus)+length(Ret.noncoop.corpus)+2]
    DF.RET.OTHER[count.ret,3]<-DF.RET.MERGE[i,length(Ret.coop.corpus)+length(Ret.noncoop.corpus)+3]
    DF.RET.MERGE[i,length(Ret.coop.corpus)+length(Ret.noncoop.corpus)+5]<-NA
  }
  else{
    DF.RET.MERGE[i,length(Ret.coop.corpus)+length(Ret.noncoop.corpus)+5]<-NA
  }
}
DF.RET.OTHER<-DF.RET.OTHER[complete.cases(DF.RET.OTHER),]
#Create New Ratio DF
DF.RAT.RET<-DF.RET.MERGE[,c(1,length(Ret.coop.corpus)+length(Ret.noncoop.corpus)+5)]
DF.RAT.RET<-DF.RAT.LAB[complete.cases(DF.RAT.RET), ]
DF.RAT.RET.dec<-DF.RAT.RET[order(DF.RAT.RET[,2]),] 
DF.RAT.RET.inc<-DF.RAT.RET[order(DF.RAT.RET[,2],decreasing=TRUE),] 
par(mfrow=c(1,1))
barplot(DF.RAT.RET.inc[1:10,2],names.arg=DF.RAT.RET.inc[1:10,1],las=0.5,main="Top 10 Ratios of Averages from Co-op to Non-Coop in RET",xlab="words",ylab="Ratio",col="red",cex.lab=0.8,cex.names=0.7)
barplot(DF.RAT.RET.dec[1:10,2],names.arg=DF.RAT.RET.dec[1:10,1],las=0.8,main="Bottom 10 Ratios of Averages from Co-op to Non-Coop in RET",xlab="words",ylab="Ratio",col="red",cex.lab=0.8,cex.names=0.7)
#Show Words that were common in one but non-existant in other
par(mfrow=c(1,1))
DF.RET.OTHER<-DF.RET.OTHER[order(DF.RET.OTHER[,3],decreasing = T),]
barplot(DF.RET.OTHER[1:10,3],names.arg=DF.RET.OTHER[1:10,1],las=0.5,main="Word Averages in Non-Coop that aren't in Co-op",xlab="Words",ylab="Average Word Frequency",col="red")


#Services
DF.SERV.COOP<-as.data.frame(M.SERV.COOP)
DF.SERV.COOP$words<-rownames(DF.SERV.COOP)
DF.SERV.NONCOOP<-as.data.frame(M.SERV.NONCOOP)
DF.SERV.NONCOOP$words<-rownames(DF.SERV.NONCOOP)
DF.SERV.MERGE<-merge(DF.SERV.COOP,DF.SERV.NONCOOP,by='words',all.x=T,all.y=T)
DF.SERV.MERGE[is.na(DF.SERV.MERGE)]<-0
#MEANS
AVG.SERV.COOP<-apply(DF.SERV.MERGE[,2:(length(Serv.coop.corpus)+1)],1,mean)
AVG.SERV.NONCOOP<-apply(DF.SERV.MERGE[,(length(Serv.coop.corpus)+2):(length(Serv.coop.corpus)+length(Serv.noncoop.corpus)+1)],1,mean)
AVG.SERV.ALL<-apply(DF.SERV.MERGE[,2:(length(Serv.coop.corpus)+length(Serv.noncoop.corpus)+1)],1,mean)
OV.AVG.SERV<-mean(AVG.SERV.ALL[1:NROW(DF.SERV.MERGE)])
#Bind them and create new DF
DF.SERV.MERGE<-cbind(DF.SERV.MERGE,AVG.SERV.COOP)
DF.SERV.MERGE<-cbind(DF.SERV.MERGE,AVG.SERV.NONCOOP)
DF.SERV.MERGE<-cbind(DF.SERV.MERGE,AVG.SERV.ALL)
DF.SERV.MERGE["Ratio- Coop to Noncoop"] <- NA
DF.SERV.OTHER<-data.frame(matrix(ncol = 3, nrow = 10000))
colnames(DF.SERV.OTHER)<-c("words","Average Freq Coop", "Average Freq Non-Coop")
#RATIO LOOP
count.serv<-0
for (i in 1:NROW(DF.SERV.MERGE)){
  if (DF.SERV.MERGE[i,length(Serv.coop.corpus)+length(Serv.noncoop.corpus)+2]>0 & DF.SERV.MERGE[i,length(Serv.coop.corpus)+length(Serv.noncoop.corpus)+3]>0){
    DF.SERV.MERGE[i,length(Serv.coop.corpus)+length(Serv.noncoop.corpus)+5]<-(DF.SERV.MERGE[i,length(Serv.coop.corpus)+length(Serv.noncoop.corpus)+2])/(DF.SERV.MERGE[i,length(Serv.coop.corpus)+length(Serv.noncoop.corpus)+3])
    count.serv<-count.serv+1
  }
  else if (DF.SERV.MERGE[i,length(Serv.coop.corpus)+length(Serv.noncoop.corpus)+2]>OV.AVG.SERV | DF.SERV.MERGE[i,length(Serv.coop.corpus)+length(Serv.noncoop.corpus)+3]>OV.AVG.SERV){
    DF.SERV.OTHER[count.serv,1]<-DF.SERV.MERGE[i,1]
    DF.SERV.OTHER[count.serv,2]<-DF.SERV.MERGE[i,length(Serv.coop.corpus)+length(Serv.noncoop.corpus)+2]
    DF.SERV.OTHER[count.serv,3]<-DF.SERV.MERGE[i,length(Serv.coop.corpus)+length(Serv.noncoop.corpus)+3]
    DF.SERV.MERGE[i,length(Serv.coop.corpus)+length(Serv.noncoop.corpus)+5]<-NA
  }
  else{
    DF.SERV.MERGE[i,length(Serv.coop.corpus)+length(Serv.noncoop.corpus)+5]<-NA
  }
}
DF.SERV.OTHER<-DF.SERV.OTHER[complete.cases(DF.SERV.OTHER),]
#Create New Ratio DF
DF.RAT.SERV<-DF.SERV.MERGE[,c(1,length(Serv.coop.corpus)+length(Serv.noncoop.corpus)+5)]
DF.RAT.SERV<-DF.RAT.SERV[complete.cases(DF.RAT.SERV), ]
DF.RAT.SERV.dec<-DF.RAT.SERV[order(DF.RAT.SERV[,2]),] 
DF.RAT.SERV.inc<-DF.RAT.SERV[order(DF.RAT.SERV[,2],decreasing=TRUE),] 
par(mfrow=c(1,1))
barplot(DF.RAT.SERV.inc[1:10,2],names.arg=DF.RAT.SERV.inc[1:10,1],las=0.5,main="Top 10 Ratios of Averages from Co-op to Non-Coop in SERV",xlab="words",ylab="Ratio",col="orange",cex.lab=0.8,cex.names=0.7)
barplot(DF.RAT.SERV.dec[1:10,2],names.arg=DF.RAT.SERV.dec[1:10,1],las=0.8,main="Bottom 10 Ratios of Averages from Co-op to Non-Coop in SERV",xlab="words",ylab="Ratio",col="orange",cex.lab=0.8,cex.names=0.7)
#Show Words that were common in one but non-existant in other
par(mfrow=c(1,1))
DF.SERV.OTHER<-DF.SERV.OTHER[order(DF.SERV.OTHER[,3],decreasing = T),]
barplot(DF.SERV.OTHER[1:10,3],names.arg=DF.SERV.OTHER[1:10,1],las=0.5,main="Word Averages in Non-Coop that aren't in Co-op",xlab="Words",ylab="Average Word Frequency",col="orange")




#DIFFERENCE OF COOP AND NON COOP STUFF
#Create New Lists.
AgALL<-c(corpora.list1[[1]],corpora.list2[[1]])
FinALL<-c(corpora.list1[[2]],corpora.list2[[2]])
HeaALL<-c(corpora.list1[[3]],corpora.list2[[3]])
LabALL<-c(corpora.list1[[4]],corpora.list2[[4]])
RetALL<-c(corpora.list1[[5]],corpora.list2[[5]])
ServALL<-c(corpora.list1[[6]],corpora.list2[[6]])
#Create Corpora for Sectors for Coop and Noncoop
AgALL<-Corpus(VectorSource(unlist(AgALL)))
FinALL<-Corpus(VectorSource(unlist(FinALL)))
HeaALL<-Corpus(VectorSource(unlist(HeaALL)))
LabALL<-Corpus(VectorSource(unlist(LabALL)))
RetALL<-Corpus(VectorSource(unlist(RetALL)))
ServALL<-Corpus(VectorSource(unlist(ServALL)))
#Clean Corpora
AgALL<-clean_corpus(AgALL)
FinALL<-clean_corpus(FinALL)
HeaALL<-clean_corpus(HeaALL)
LabALL<-clean_corpus(LabALL)
RetALL<-clean_corpus(RetALL)
ServALL<-clean_corpus(ServALL)
#Create TDMS for Full Corpora
TDM.AG.ALL<-TermDocumentMatrix(AgALL)
TDM.FIN.ALL<-TermDocumentMatrix(FinALL)
TDM.HEA.ALL<-TermDocumentMatrix(HeaALL)
TDM.LAB.ALL<-TermDocumentMatrix(LabALL)
TDM.RET.ALL<-TermDocumentMatrix(RetALL)
TDM.SERV.ALL<-TermDocumentMatrix(ServALL)
#Convert to Matrices
MAT.AG.ALL<-as.matrix(TDM.AG.ALL)
MAT.FIN.ALL<-as.matrix(TDM.FIN.ALL)
MAT.HEA.ALL<-as.matrix(TDM.HEA.ALL)
MAT.LAB.ALL<-as.matrix(TDM.LAB.ALL)
MAT.RET.ALL<-as.matrix(TDM.RET.ALL)
MAT.SERV.ALL<-as.matrix(TDM.SERV.ALL)
#Create Submatrices with all words in them
#Agriculture Differences
MAT.AG.ALL.COOP<-MAT.AG.ALL[,-(length(Ag.coop.corpus)+1):(-(length(Ag.coop.corpus)+length(Ag.noncoop.corpus)))]
MAT.AG.ALL.NONCOOP<-MAT.AG.ALL[,-1:-length(Ag.coop.corpus)]
AGcoopAVG<-apply(MAT.AG.ALL.COOP,1,mean)
AGnoncoopAVG<-apply(MAT.AG.ALL.NONCOOP,1,mean)
AGdiff1<-AGcoopAVG-AGnoncoopAVG
AGdiff1<-sort(AGdiff1,decreasing=TRUE)
AGdiff2<-AGnoncoopAVG-AGcoopAVG
AGdiff2<-sort(AGdiff2,decreasing=TRUE)
par(mfrow=c(1,2),mgp=c(1,0.5,0))   #Creating Bar Plots
barplot(AGdiff1[1:10],main="10 Most Prominent Words  in Agriculture for Co-ops",col="green",las=2)
barplot(AGdiff2[1:10],main="10 Most Prominent Words in Agriculture for Non-Coops",col="green",las=2)
#Financial Differences
MAT.FIN.ALL.COOP<-MAT.FIN.ALL[,-(length(Fin.coop.corpus)+1):(-(length(Fin.coop.corpus)+length(Fin.noncoop.corpus)))]
MAT.FIN.ALL.NONCOOP<-MAT.FIN.ALL[,-1:-length(Fin.coop.corpus)]
FINcoopAVG<-apply(MAT.FIN.ALL.COOP,1,mean)
FINnoncoopAVG<-apply(MAT.FIN.ALL.NONCOOP,1,mean)
FINdiff1<-FINcoopAVG-FINnoncoopAVG
FINdiff1<-sort(FINdiff1,decreasing=TRUE)
FINdiff2<-FINnoncoopAVG-FINcoopAVG
FINdiff2<-sort(FINdiff2,decreasing=TRUE)
par(mfrow=c(1,2),mgp=c(1,0.5,0))   #Creating Bar Plots
barplot(FINdiff1[1:10],main="10 Most Prominent Words  in Finance for Co-ops",col="blue",las=2)
barplot(FINdiff2[1:10],main="10 Most Prominent Words in Finance for Non-Coops",col="blue",las=2)
#Health Differences
MAT.HEA.ALL.COOP<-MAT.HEA.ALL[,-(length(Hea.coop.corpus)+1):(-(length(Hea.coop.corpus)+length(Hea.noncoop.corpus)))]
MAT.HEA.ALL.NONCOOP<-MAT.HEA.ALL[,-1:-length(Hea.coop.corpus)]
HEAcoopAVG<-apply(MAT.HEA.ALL.COOP,1,mean)
HEAnoncoopAVG<-apply(MAT.HEA.ALL.NONCOOP,1,mean)
HEAdiff1<-HEAcoopAVG-HEAnoncoopAVG
HEAdiff1<-sort(HEAdiff1,decreasing=TRUE)
HEAdiff2<-HEAnoncoopAVG-HEAcoopAVG
HEAdiff2<-sort(HEAdiff2,decreasing=TRUE)
par(mfrow=c(1,2),mgp=c(1,0.5,0))   #Creating Bar Plots
barplot(HEAdiff1[1:10],main="10 Most Prominent Words  in Health for Co-ops",col="yellow",las=2)
barplot(HEAdiff2[1:10],main="10 Most Prominent Words in Health for Non-Coops",col="yellow",las=2)
#Labour Differences (NOTE THERE IS ONLY ONE LAB COMPANY FOR EACH)
MAT.LAB.ALL.COOP<-MAT.LAB.ALL[,-2]
MAT.LAB.ALL.NONCOOP<-MAT.LAB.ALL[,-1]
LABdiff1<-MAT.LAB.ALL.COOP-MAT.LAB.ALL.NONCOOP
LABdiff1<-sort(LABdiff1,decreasing=TRUE)
LABdiff2<-MAT.LAB.ALL.NONCOOP-MAT.LAB.ALL.COOP
LABdiff2<-sort(LABdiff2,decreasing=TRUE)
par(mfrow=c(1,2),mgp=c(1,0.5,0))   #Creating Bar Plots
barplot(LABdiff1[1:10],main="10 Most Prominent Words  in Labour for Co-ops",col="brown",las=2)
barplot(LABdiff2[1:10],main="10 Most Prominent Words in Labour for Non-Coops",col="brown",las=2)
#Retail
MAT.RET.ALL.COOP<-MAT.RET.ALL[,-(length(Ret.coop.corpus)+1):(-(length(Ret.coop.corpus)+length(Ret.noncoop.corpus)))]
MAT.RET.ALL.NONCOOP<-MAT.RET.ALL[,-1:-length(Ret.coop.corpus)]
RETcoopAVG<-apply(MAT.RET.ALL.COOP,1,mean)
RETnoncoopAVG<-apply(MAT.RET.ALL.NONCOOP,1,mean)
RETdiff1<-RETcoopAVG-RETnoncoopAVG
RETdiff1<-sort(RETdiff1,decreasing=TRUE)
RETdiff2<-RETnoncoopAVG-RETcoopAVG
RETdiff2<-sort(RETdiff2,decreasing=TRUE)
par(mfrow=c(1,2),mgp=c(1,0.5,0))   #Creating Bar Plots
barplot(RETdiff1[1:10],main="10 Most Prominent Words  in Retail for Co-ops",col="red",las=2)
barplot(RETdiff2[1:10],main="10 Most Prominent Words in Retail for Non-Coops",col="red",las=2)
#Services
MAT.SERV.ALL.COOP<-MAT.SERV.ALL[,-(length(Serv.coop.corpus)+1):(-(length(Serv.coop.corpus)+length(Serv.noncoop.corpus)))]
MAT.SERV.ALL.NONCOOP<-MAT.SERV.ALL[,-1:-length(Serv.coop.corpus)]
SERVcoopAVG<-apply(MAT.SERV.ALL.COOP,1,mean)
SERVnoncoopAVG<-apply(MAT.SERV.ALL.NONCOOP,1,mean)
SERVdiff1<-SERVcoopAVG-SERVnoncoopAVG
SERVdiff1<-sort(SERVdiff1,decreasing=TRUE)
SERVdiff2<-SERVnoncoopAVG-SERVcoopAVG
SERVdiff2<-sort(SERVdiff2,decreasing=TRUE)
par(mfrow=c(1,2),mgp=c(1,0.5,0))   #Creating Bar Plots
barplot(SERVdiff1[1:10],main="10 Most Prominent Words  in Services for Co-ops",col="orange",las=2)
barplot(SERVdiff2[1:10],main="10 Most Prominent Words in Services for Non-Coops",col="orange",las=2)




#Sentiment Analysis
#Create Sentiment Matrix
DF.SENT<-get_sentiments("afinn")
DF.SENT1<-get_sentiments("bing")
DF.SENT<-merge(DF.SENT,DF.SENT1,by='word')
DF.SENT2<-get_sentiments("nrc")
DF.SENT<-merge(DF.SENT,DF.SENT2,all.x=T,by='word')
DF.SENT<-DF.SENT[complete.cases(DF.SENT), ]
DF.SENT$word<-stemDocument(DF.SENT$word)
#TDM Matrices for Coop and Noncoop
TDM.COOP.ALL<-TermDocumentMatrix(coop.all)
TDM.NONCOOP.ALL<-TermDocumentMatrix(noncoop.all)
M.COOP.ALL<-as.matrix(TDM.COOP.ALL)
M.NONCOOP.ALL<-as.matrix(TDM.NONCOOP.ALL)
#Totalwords
totwords.coop<-sum(M.COOP.ALL[1:nrow(M.COOP.ALL),1:ncol(M.COOP.ALL)])
totwords.noncoop<-sum(M.NONCOOP.ALL[1:nrow(M.NONCOOP.ALL),1:ncol(M.NONCOOP.ALL)])
#Word Frequency per 1000
M.COOP.ALL<-apply(M.COOP.ALL,1,sum)
DF.COOP.ALL<-as.data.frame(M.COOP.ALL)
DF.COOP.ALL$word<-rownames(DF.COOP.ALL)
names(DF.COOP.ALL)[names(DF.COOP.ALL) == "M.COOP.ALL"] <- "Word Freq per 1000 for Coop"
DF.COOP.ALL$`Word Freq per 1000 for Coop`<-DF.COOP.ALL$`Word Freq per 1000 for Coop`*1000/totwords.coop
M.NONCOOP.ALL<-apply(M.NONCOOP.ALL,1,sum)
DF.NONCOOP.ALL<-as.data.frame(M.NONCOOP.ALL)
DF.NONCOOP.ALL$word<-rownames(DF.NONCOOP.ALL)
names(DF.NONCOOP.ALL)[names(DF.NONCOOP.ALL) == "M.NONCOOP.ALL"] <- "Word Freq per 1000 for Noncoop"
DF.NONCOOP.ALL$`Word Freq per 1000 for Noncoop`<-DF.NONCOOP.ALL$`Word Freq per 1000 for Noncoop`*1000/totwords.noncoop
#Merge with Sentiment
DF.SENT.COOP<-merge(DF.COOP.ALL,DF.SENT,all.x=T,by='word')
DF.SENT.COOP<-DF.SENT.COOP[complete.cases(DF.SENT.COOP), ]
DF.SENT.COOP<-DF.SENT.COOP[order(DF.SENT.COOP[,5]),]
DF.SENT.COOP<-unique(DF.SENT.COOP)
DF.SENT.NONCOOP<-merge(DF.NONCOOP.ALL,DF.SENT,all.x=T,by='word')
DF.SENT.NONCOOP<-DF.SENT.NONCOOP[complete.cases(DF.SENT.NONCOOP), ]
DF.SENT.NONCOOP<-DF.SENT.NONCOOP[order(DF.SENT.NONCOOP[,5]),]
DF.SENT.NONCOOP<-unique(DF.SENT.NONCOOP)
# 1) SIX SENTIMENTS COMPONENT
SENT.TOT.COOP<-aggregate(DF.SENT.COOP$`Word Freq per 1000 for Coop` ~DF.SENT.COOP$sentiment.y , data = DF.SENT.COOP, sum)
SENT.TOT.NONCOOP<-aggregate(DF.SENT.NONCOOP$`Word Freq per 1000 for Noncoop` ~DF.SENT.NONCOOP$sentiment.y , data = DF.SENT.NONCOOP, sum)
names(SENT.TOT.COOP)[names(SENT.TOT.COOP) == "DF.SENT.COOP$sentiment.y"] <- "word"
names(SENT.TOT.NONCOOP)[names(SENT.TOT.NONCOOP) == "DF.SENT.NONCOOP$sentiment.y"] <- "word"
SENT.TOT<-merge(SENT.TOT.COOP,SENT.TOT.NONCOOP)
SENT.TOT$differences<-SENT.TOT[,2]-SENT.TOT[,3]
SENT.TOT$ratios<-SENT.TOT[,2]/SENT.TOT[,3]
#barplots for it
par(mfrow=c(1,1))
myrange <- c(min(SENT.TOT$differences),max(SENT.TOT$differences))
barplot(SENT.TOT[1:10,4],names.arg=SENT.TOT[1:10,1],las=0.5,main="Word Differences in Sentiments between Coop and Noncoop",col = ifelse(SENT.TOT[1:10,4] < 0,'red','green'),ylim=myrange)
mtext("Sentiments", side=1, line=2)
mtext("Differences",side=2,line=2)
barplot(SENT.TOT[1:10,5],names.arg=SENT.TOT[1:10,1],las=0.5,main="Ratio in Coop Sentiments to Noncoop Sentiments",,xpd=F,ylim=c(0.5,1.5),col = ifelse(SENT.TOT[1:10,5] < 1,'red','green'))
mtext("Sentiments", side=1, line=2)
mtext("Ratios of Coops to Non Co-ops",side=2,line=2)
# 2) Positive Vs. Negative Component
POSORNEG.COOP<-aggregate(DF.SENT.COOP$`Word Freq per 1000 for Coop`~DF.SENT.COOP$sentiment.x,data=DF.SENT.COOP,sum)
POSORNEG.NONCOOP<-aggregate(DF.SENT.NONCOOP$`Word Freq per 1000 for Noncoop`~DF.SENT.NONCOOP$sentiment.x,data=DF.SENT.NONCOOP,sum)
names(POSORNEG.COOP)[names(POSORNEG.COOP) == "DF.SENT.COOP$sentiment.x"] <- "posorneg"
names(POSORNEG.NONCOOP)[names(POSORNEG.NONCOOP) == "DF.SENT.NONCOOP$sentiment.x"] <- "posorneg"
POSORNEG.TOT<-merge(POSORNEG.COOP,POSORNEG.NONCOOP)
POSORNEG.TOT$differences<-POSORNEG.TOT[,2]-POSORNEG.TOT[,3]
POSORNEG.TOT$ratios<-POSORNEG.TOT[,2]/POSORNEG.TOT[,3]
#Barplots for it
par(mfrow=c(1,2))
barplot(POSORNEG.TOT[1:2,4],names.arg=POSORNEG.TOT[1:2,1],las=0.5,main="Word Differences in Pos/Neg Words between Coop and Noncoop",col=ifelse(POSORNEG.TOT[1:2,1]=="positive",'green','red'))
mtext("Differences of Coops to Non Co-ops",side=2,line=2)
barplot(POSORNEG.TOT[1:2,5],names.arg=POSORNEG.TOT[1:2,1],las=0.5,main="Word Ratio in Pos/Neg Words between Coop and Noncoop",col=ifelse(POSORNEG.TOT[1:2,1]=="positive",'green','red'))
mtext("Ratios of Coops to Non Co-ops",side=2,line=2)
# 3) Score Analysis
SCORE.SENT.COOP<-aggregate(DF.SENT.COOP$`Word Freq per 1000 for Coop`~DF.SENT.COOP$score,data=DF.SENT.COOP,sum)
SCORE.SENT.NONCOOP<-aggregate(DF.SENT.NONCOOP$`Word Freq per 1000 for Noncoop`~DF.SENT.NONCOOP$score,data=DF.SENT.NONCOOP,sum)
SCORE.SENT.COOP$scoretotals<-SCORE.SENT.COOP[,1]*SCORE.SENT.COOP[,2]
SCORE.SENT.NONCOOP$scoretotals<-SCORE.SENT.NONCOOP[,1]*SCORE.SENT.NONCOOP[,2]
FINAL.SENT.SCORE.COOP<-sum(SCORE.SENT.COOP[,3])
FINAL.SENT.SCORE.NONCOOP<-sum(SCORE.SENT.NONCOOP[,3])
#Barplot of Final Score
par(mfrow=c(1,1))
barplot(c(FINAL.SENT.SCORE.COOP,FINAL.SENT.SCORE.NONCOOP),names.arg=c("Score of Coop","Score of Noncoop"),las=0.5,main="Final Word Scores of Coop and Non-coop",col=ifelse(POSORNEG.TOT[1:2,1]=="positive",'red','green'))


#Creating Co-op Dictionary and Coop words

#Create Principle Dictionary
principle1<-c("voluntary","responsibility")
principle2<-c("democratic","participate")
principle3<-c("contribute","capital","benefits")
principle4<-c("autonomous")
principle5<-c("education","training","inform")
principle6<-c("together")
principle7<-c("sustainable","communities")
allprinciples<-c(principle1,principle2,principle3,principle4,principle5,principle6,principle7)
allprinciples<-stemDocument(allprinciples)
allprinciples<-data.frame(allprinciples)
colnames(allprinciples)<-c("word")
#Merge Coop words and Non coop words with the principles
PRINC.COOP<-merge(DF.COOP.ALL,allprinciples,by="word")
PRINC.NONCOOP<-merge(DF.NONCOOP.ALL,allprinciples,by="word")
tot.principles.coop<-sum(PRINC.COOP[,2])
tot.principles.noncoop<-sum(PRINC.NONCOOP[,2])
#Create a bar plot of Principles in both Coop and Non Coop
par(mfrow=c(1,1))
barplot(c(tot.principles.coop,tot.principles.noncoop),names.arg=c("Number of Co-op Principle words used in Coop","Number of Co-op Principle words used in Non Co-op"),las=0.5,main="Number of Co-operative Principle keywords used in Co-op and Non Co-op",col=ifelse(POSORNEG.TOT[1:2,1]=="positive",'red','green'),ylim=c(0,40))
mtext("Frequency of Co-op Principle Keywords",side=2,line=2)


#ALL COOP Principles
#Create indices
indice1<-length(principle1)
indice2<-indice1+length(principle2)
indice3<-indice2+length(principle3)
indice4<-indice3+length(principle4)
indice5<-indice4+length(principle5)
indice6<-indice5+length(principle6)
indice7<-indice6+length(principle7)
#Create Classifications
allprinciples[2]<-vector(length = indice7)
allprinciples[(1:indice1),2]<-'principle 1'
allprinciples[(indice1+1):indice2,2]<-"principle 2"
allprinciples[(indice2+1):indice3,2]<-"principle 3"
allprinciples[(indice3+1):indice4,2]<-"principle 4"
allprinciples[(indice4+1):indice5,2]<-"principle 5"
allprinciples[(indice5+1):indice6,2]<-"principle 6"
allprinciples[(indice6+1):indice7,2]<-"principle 7"
colnames(allprinciples)<-c("word","principle")
PRINC.COOP<-merge(DF.COOP.ALL,allprinciples,by='word')
PRINC.NONCOOP<-merge(DF.NONCOOP.ALL,allprinciples,by='word')
ALLPRINCIP<-merge(PRINC.COOP,PRINC.NONCOOP)
ALLPRINCIP<-ALLPRINCIP[order(ALLPRINCIP[,2]),]
#Create Difference and Ratio Columns
ALLPRINCIP$differences<-ALLPRINCIP$`Word Freq per 1000 for Coop`-ALLPRINCIP$`Word Freq per 1000 for Noncoop`
ALLPRINCIP$ratio<-ALLPRINCIP$`Word Freq per 1000 for Coop`/ALLPRINCIP$`Word Freq per 1000 for Noncoop`
#Aggregate
PRINCIP.DIFF<-aggregate(ALLPRINCIP$differences~ALLPRINCIP$principle,data=ALLPRINCIP,sum)
PRINCIP.RAT<-aggregate(ALLPRINCIP$ratio~ALLPRINCIP$principle,data=ALLPRINCIP,sum)
PRINCIP.SUM.COOP<-aggregate(ALLPRINCIP$`Word Freq per 1000 for Coop`~ALLPRINCIP$principle,data=ALLPRINCIP,sum)
PRINCIP.SUM.NONCOOP<-aggregate(ALLPRINCIP$`Word Freq per 1000 for Noncoop`~ALLPRINCIP$principle,data=ALLPRINCIP,sum)
PRINCIP.SUM<-merge(PRINCIP.SUM.COOP,data=ALLPRINCIP,PRINCIP.SUM.NONCOOP)
PRINCIP.SUM$ratio<-PRINCIP.SUM[,2]/PRINCIP.SUM[,3]

#Create Barplots
barplot(PRINCIP.DIFF[,2],names.arg=PRINCIP.DIFF[,1],las=0.5,main="Differences of Co-operative Principle keywords between Co-op and Non Co-op",col = ifelse(PRINCIP.DIFF[1:7,2] < 0,'red','green'))
mtext("Co-op Principles", side=1, line=2)
mtext("Differences",side=2,line=2)
barplot(PRINCIP.SUM[,4],names.arg=PRINCIP.SUM[,1],las=0.5,main="Ratios of Co-operative Principle keywords between Co-op and Non Co-op",col = ifelse(PRINCIP.SUM[1:7,4] < 1,'red','green'))
mtext("Co-op Principles", side=1, line=2)
mtext("Ratios",side=2,line=2)


