library(tm)
library(SnowballC)
library(tidytext)
library(ggplot2)

    #Creating Footnotes for plots

      footnote <- paste("Word Frequency per 1000 is calculated at the Company Level",sep=" / ")
      makeFootnote <- function(footnoteText=
                                 format(Sys.time(), "%d %b %Y"),
                               size= .7, color= grey(.5))
      {
        require(grid)
        pushViewport(viewport())
        grid.text(label= footnoteText ,
                  x = unit(1,"npc") - unit(2, "mm"),
                  y= unit(2, "mm"),
                  just=c("right", "bottom"),
                  gp=gpar(cex= size, col=color))
        popViewport()
      }
      

#Creating a Stem Dictionary
stem_dict<-c("complicate","company","association","cooperative")
  
#Cleaning corpus function
clean_corpus<-function(corpus){
  corpus<-tm_map(corpus,stripWhitespace)
  corpus<-tm_map(corpus,removeNumbers)
  corpus<-tm_map(corpus,tolower)
  corpus<-tm_map(corpus,removePunctuation)
  corpus<-tm_map(corpus,removeWords,c("cooperatives","cooperative"))
  corpus<-tm_map(corpus,stemDocument)
  corpus<-tm_map(corpus,removeWords, c(stopwords("english"),"coop","co-op","alberta","feder","granni","growmark","saskatchewan","afa","scotsburn","apa","federe","canadian","canada","manitoba","caiss","british","columbia","blue","cross","fcl","costco","sobey","mike","sail","mikei","chines","bmo","ontario","bcaa","ofsa","aist"))
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
      #Creates corpora for different subsets of coops nested list then cleans them
      #Creating the Non Co-op Nested List
      Ag.coop.orig.corpus<-Corpus(VectorSource(unlist(corpora.list1[["Agriculture"]])))
      Fin.coop.orig.corpus<-Corpus(VectorSource(unlist(corpora.list1[["Finance"]])))
      Hea.coop.orig.corpus<-Corpus(VectorSource(unlist(corpora.list1[["Health"]])))
      Lab.coop.orig.corpus<-Corpus(VectorSource(unlist(corpora.list1[["Labour"]])))
      Ret.coop.orig.corpus<-Corpus(VectorSource(unlist(corpora.list1[["Retail , Consumer"]])))
      Serv.coop.orig.corpus<-Corpus(VectorSource(unlist(corpora.list1[["Service"]])))
      #Cleaning the Co-op Nested List
      Ag.coop.corpus<-clean_corpus(Ag.coop.orig.corpus)
      Fin.coop.corpus<-clean_corpus(Fin.coop.orig.corpus)
      Hea.coop.corpus<-clean_corpus(Hea.coop.orig.corpus)
      Lab.coop.corpus<-clean_corpus(Lab.coop.orig.corpus)
      Ret.coop.corpus<-clean_corpus(Ret.coop.orig.corpus)
      Serv.coop.corpus<-clean_corpus(Serv.coop.orig.corpus)
   
  
      
      
      
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
      Ag.noncoop.orig.corpus<-Corpus(VectorSource(unlist(corpora.list2[["Agriculture"]])))
      Fin.noncoop.orig.corpus<-Corpus(VectorSource(unlist(corpora.list2[["Finance"]])))
      Hea.noncoop.orig.corpus<-Corpus(VectorSource(unlist(corpora.list2[["Health"]])))
      Lab.noncoop.orig.corpus<-Corpus(VectorSource(unlist(corpora.list2[["Labour"]])))
      Ret.noncoop.orig.corpus<-Corpus(VectorSource(unlist(corpora.list2[["Retail , Consumer"]])))
      Serv.noncoop.orig.corpus<-Corpus(VectorSource(unlist(corpora.list2[["Service"]])))
      #Cleanup of lists
      Ag.noncoop.corpus<-clean_corpus(Ag.noncoop.orig.corpus)
      Fin.noncoop.corpus<-clean_corpus(Fin.noncoop.orig.corpus)
      Hea.noncoop.corpus<-clean_corpus(Hea.noncoop.orig.corpus)
      Lab.noncoop.corpus<-clean_corpus(Lab.noncoop.orig.corpus)
      Ret.noncoop.corpus<-clean_corpus(Ret.noncoop.orig.corpus)
      Serv.noncoop.corpus<-clean_corpus(Serv.noncoop.orig.corpus)


      #Creates Overall Corpora for Co-op and Non Co-op Words
      coop.all<-Corpus(VectorSource(unlist(corpora.list1)))
      noncoop.all<-Corpus(VectorSource(unlist(corpora.list2)))
      coop.all<-clean_corpus(coop.all)
      noncoop.all<-clean_corpus(noncoop.all)
      
      
      #Create Dictionaries for Stem Completion 
        #TDMS
          TDM.AG.COOP.ORIG<-TermDocumentMatrix(Ag.coop.orig.corpus)
          TDM.AG.NONCOOP.ORIG<-TermDocumentMatrix(Ag.noncoop.orig.corpus)
          TDM.FIN.COOP.ORIG<-TermDocumentMatrix(Fin.coop.orig.corpus)
          TDM.FIN.NONCOOP.ORIG<-TermDocumentMatrix(Fin.noncoop.orig.corpus)
          TDM.SERV.COOP.ORIG<-TermDocumentMatrix(Serv.coop.orig.corpus)
          TDM.SERV.NONCOOP.ORIG<-TermDocumentMatrix(Serv.noncoop.orig.corpus)
          TDM.OTHER.COOP.ORIG<-c(TermDocumentMatrix(Hea.coop.orig.corpus),TermDocumentMatrix(Lab.coop.orig.corpus),TermDocumentMatrix(Ret.coop.orig.corpus))
          TDM.OTHER.NONCOOP.ORIG<-c(TermDocumentMatrix(Hea.noncoop.orig.corpus),TermDocumentMatrix(Lab.noncoop.orig.corpus),TermDocumentMatrix(Ret.noncoop.orig.corpus))

        #Matrices
          M.AG.COOP.ORIG<-as.matrix(TDM.AG.COOP.ORIG)
          M.AG.NONCOOP.ORIG<-as.matrix(TDM.AG.NONCOOP.ORIG)
          M.FIN.COOP.ORIG<-as.matrix(TDM.FIN.COOP.ORIG)
          M.FIN.NONCOOP.ORIG<-as.matrix(TDM.FIN.NONCOOP.ORIG)
          M.SERV.COOP.ORIG<-as.matrix(TDM.SERV.COOP.ORIG)
          M.SERV.NONCOOP.ORIG<-as.matrix(TDM.SERV.NONCOOP.ORIG)
          M.OTHER.COOP.ORIG<-as.matrix(TDM.OTHER.COOP.ORIG)
          M.OTHER.NONCOOP.ORIG<-as.matrix(TDM.OTHER.NONCOOP.ORIG)
          
        #Dataframes
          DF.AG.COOP.ORIG<-as.data.frame(M.AG.COOP.ORIG)
          DF.AG.COOP.ORIG$words<-rownames(DF.AG.COOP.ORIG)
          DF.AG.NONCOOP.ORIG<-as.data.frame(M.AG.NONCOOP.ORIG)
          DF.AG.NONCOOP.ORIG$words<-rownames(DF.AG.NONCOOP.ORIG)
          DF.FIN.COOP.ORIG<-as.data.frame(M.FIN.COOP.ORIG)
          DF.FIN.COOP.ORIG$words<-rownames(DF.FIN.COOP.ORIG)
          DF.FIN.NONCOOP.ORIG<-as.data.frame(M.FIN.NONCOOP.ORIG)
          DF.FIN.NONCOOP.ORIG$words<-rownames(DF.FIN.NONCOOP.ORIG)
          DF.SERV.COOP.ORIG<-as.data.frame(M.SERV.COOP.ORIG)
          DF.SERV.COOP.ORIG$words<-rownames(DF.SERV.COOP.ORIG)
          DF.SERV.NONCOOP.ORIG<-as.data.frame(M.SERV.NONCOOP.ORIG)
          DF.SERV.NONCOOP.ORIG$words<-rownames(DF.SERV.NONCOOP.ORIG)
          DF.OTHER.COOP.ORIG<-as.data.frame(M.OTHER.COOP.ORIG)
          DF.OTHER.COOP.ORIG$words<-rownames(DF.OTHER.COOP.ORIG)
          DF.OTHER.NONCOOP.ORIG<-as.data.frame(M.OTHER.NONCOOP.ORIG)
          DF.OTHER.NONCOOP.ORIG$words<-rownames(DF.OTHER.NONCOOP.ORIG)
        #Vectors
          #AG
          AGDICT1<-as.matrix(DF.AG.COOP.ORIG$words)
          AGDICT2<-as.matrix(DF.AG.NONCOOP.ORIG$words)
          AGDICT<-matrix(nrow=(nrow(AGDICT1)+nrow(AGDICT2)),ncol=1)
          
          for (i in 1:length(AGDICT)){
            if (i<= length(AGDICT1)){
              AGDICT[i,1]<-AGDICT1[i,1]
            }
            else {
              AGDICT[i,1]<-AGDICT2[i-length(AGDICT1),1]
              }
            }
          AGDICT<-as.vector(AGDICT)
          #FIN
          FINDICT1<-as.matrix(DF.FIN.COOP.ORIG$words)
          FINDICT2<-as.matrix(DF.FIN.NONCOOP.ORIG$words)
          FINDICT<-matrix(nrow=(nrow(FINDICT1)+nrow(FINDICT2)),ncol=1)
          
          for (i in 1:length(FINDICT)){
            if (i<= length(FINDICT1)){
              FINDICT[i,1]<-FINDICT1[i,1]
            }
            else {
              FINDICT[i,1]<-FINDICT2[i-length(FINDICT1),1]
            }
          }
          FINDICT<-as.vector(FINDICT)
          #SERVICE
          SERVDICT1<-as.matrix(DF.SERV.COOP.ORIG$words)
          SERVDICT2<-as.matrix(DF.SERV.NONCOOP.ORIG$words)
          SERVDICT<-matrix(nrow=(nrow(SERVDICT1)+nrow(SERVDICT2)),ncol=1)
          
          for (i in 1:length(SERVDICT)){
            if (i<= length(SERVDICT1)){
              SERVDICT[i,1]<-SERVDICT1[i,1]
            }
            else {
              SERVDICT[i,1]<-SERVDICT2[i-length(SERVDICT1),1]
            }
          }
          SERVDICT<-as.vector(SERVDICT)
          #OTHER
          OTHERDICT1<-as.matrix(DF.OTHER.COOP.ORIG$words)
          OTHERDICT2<-as.matrix(DF.OTHER.NONCOOP.ORIG$words)
          OTHERDICT<-matrix(nrow=(nrow(OTHERDICT1)+nrow(OTHERDICT2)),ncol=1)
          
          for (i in 1:length(OTHERDICT)){
            if (i<= length(OTHERDICT1)){
              OTHERDICT[i,1]<-OTHERDICT1[i,1]
            }
            else {
              OTHERDICT[i,1]<-OTHERDICT2[i-length(OTHERDICT1),1]
            }
          }
          OTHERDICT<-as.vector(OTHERDICT)
          
          
 

#PART ONE: TOP FREQUENCY WORDS IN COOP VS NON COOP 
        
# 1a) CREATING TERM DOCUMENT MATRICES        
    #Agriculture
      #This will get us all the terms and frequencies for agriculture companies
      TDM.AG.COOP<-TermDocumentMatrix(Ag.coop.corpus)
      TDM.AG.NONCOOP<-TermDocumentMatrix(Ag.noncoop.corpus)
      #Create a Matrix and Data Framae
      M.FREQ.AG.COOP<-as.matrix(TDM.AG.COOP)
      M.FREQ.AG.NONCOOP<-as.matrix(TDM.AG.NONCOOP)
      
    #Finance
      #This will get us all the terms and frequencies for Finance companies
      TDM.FIN.COOP<-TermDocumentMatrix(Fin.coop.corpus)
      TDM.FIN.NONCOOP<-TermDocumentMatrix(Fin.noncoop.corpus)
      #Create a Matrix 
      M.FREQ.FIN.COOP<-as.matrix(TDM.FIN.COOP)
      M.FREQ.FIN.NONCOOP<-as.matrix(TDM.FIN.NONCOOP)
     
    #Services
      TDM.SERV.COOP<-TermDocumentMatrix(Serv.coop.corpus)
      TDM.SERV.NONCOOP<-TermDocumentMatrix(Serv.noncoop.corpus)
      #This will get us all the terms and frequencies for Service companies
      M.FREQ.SERV.COOP<-as.matrix(TDM.SERV.COOP)
      M.FREQ.SERV.NONCOOP<-as.matrix(TDM.SERV.NONCOOP)
      
    #Other 
      TDM.OTHER.COOP<-c(TermDocumentMatrix(Hea.coop.corpus),TermDocumentMatrix(Lab.coop.corpus),TermDocumentMatrix(Ret.coop.corpus))
      TDM.OTHER.NONCOOP<-c(TermDocumentMatrix(Hea.noncoop.corpus),TermDocumentMatrix(Lab.noncoop.corpus),TermDocumentMatrix(Ret.noncoop.corpus))
      #This will get us all the terms and frequencies for Service companies
        M.FREQ.OTHER.COOP<-as.matrix(TDM.OTHER.COOP)
        M.FREQ.OTHER.NONCOOP<-as.matrix(TDM.OTHER.NONCOOP)
      
    #All Coops and ALL Non Coops
      TDM.COOP.ALL<-TermDocumentMatrix(coop.all)
      TDM.NONCOOP.ALL<-TermDocumentMatrix(noncoop.all)
      M.FREQ.ALL.COOP<-as.matrix(TDM.COOP.ALL)
      M.FREQ.ALL.NONCOOP<-as.matrix(TDM.NONCOOP.ALL)
  
#1b) CREATING WP1000 DATAFRAMES 
      #Agriculture
          #Coop and Non Coop
            DF.FREQ.AG.COOP<-as.data.frame(M.FREQ.AG.COOP)
            DF.FREQ.AG.NONCOOP<-as.data.frame(M.FREQ.AG.NONCOOP)
            DF.WP1000.AG.COOP<-as.data.frame(matrix(nrow=nrow(DF.FREQ.AG.COOP),ncol=ncol(DF.FREQ.AG.COOP)))
            DF.WP1000.AG.NONCOOP<-as.data.frame(matrix(nrow=nrow(DF.FREQ.AG.NONCOOP),ncol=ncol(DF.FREQ.AG.NONCOOP)))
            colnames(DF.WP1000.AG.COOP)<-colnames(DF.FREQ.AG.COOP)
            colnames(DF.WP1000.AG.NONCOOP)<-colnames(DF.FREQ.AG.NONCOOP)
            rownames(DF.WP1000.AG.COOP)<-rownames(DF.FREQ.AG.COOP)
            rownames(DF.WP1000.AG.NONCOOP)<-rownames(DF.FREQ.AG.NONCOOP)
          #Create the multiplier for words per 1000 per company
            multagwordscoop<-1000/t(as.data.frame(apply(DF.FREQ.AG.COOP[,1:ncol(DF.FREQ.AG.COOP)],2,sum))) #Create totals
            multagwordsnoncoop<-1000/t(as.data.frame(apply(DF.FREQ.AG.NONCOOP[,1:ncol(DF.FREQ.AG.NONCOOP)],2,sum))) #Create totals
          #Loop to create words per 1000
          for (i in 1:(nrow(DF.FREQ.AG.COOP))){
            for (j in 1:(ncol(DF.FREQ.AG.COOP))){
              DF.WP1000.AG.COOP[i,j]<-DF.FREQ.AG.COOP[i,j]*multagwordscoop[1,j]
            }
          }
          for (i in 1:(nrow(DF.FREQ.AG.NONCOOP))){
            for (j in 1:(ncol(DF.FREQ.AG.NONCOOP))){
              DF.WP1000.AG.NONCOOP[i,j]<-DF.FREQ.AG.NONCOOP[i,j]*multagwordsnoncoop[1,j]
            }
          }
          #Create Average Columns
          DF.WP1000.AG.COOP$average<-apply(DF.WP1000.AG.COOP,1,mean)
          DF.WP1000.AG.NONCOOP$average<-apply(DF.WP1000.AG.NONCOOP,1,mean)
          #Create column of Words
          DF.WP1000.AG.COOP$words<-rownames(DF.WP1000.AG.COOP)
          DF.WP1000.AG.NONCOOP$words<-rownames(DF.WP1000.AG.NONCOOP)
          #Create Merged DF
          DF.WP1000.AG.MERGE<-merge(DF.WP1000.AG.COOP,DF.WP1000.AG.NONCOOP,by='words',all.x=T,all.y=T,replace=T)
          DF.WP1000.AG.MERGE[is.na(DF.WP1000.AG.MERGE)]<-0
          #Create Differences/Percent Change Column
          DF.WP1000.AG.MERGE$differences<-DF.WP1000.AG.MERGE$average.x-DF.WP1000.AG.MERGE$average.y
          DF.WP1000.AG.MERGE$pctchange<-100*DF.WP1000.AG.MERGE$differences/DF.WP1000.AG.MERGE$average.x
          #Organize by Differences
          DF.WP1000.AG.MERGE.inc<-DF.WP1000.AG.MERGE[order(DF.WP1000.AG.MERGE$differences),]
          DF.WP1000.AG.MERGE.inc<-DF.WP1000.AG.MERGE.inc[1:10,]
          DF.WP1000.AG.MERGE.dec<-DF.WP1000.AG.MERGE[order(DF.WP1000.AG.MERGE$differences,decreasing=T),]
          DF.WP1000.AG.MERGE.dec<-DF.WP1000.AG.MERGE.dec[1:10,]
          #Stem Complete Words
          DF.WP1000.AG.MERGE.inc[1:9,1]<-stemCompletion(DF.WP1000.AG.MERGE.inc[1:9,1],AGDICT)
          DF.WP1000.AG.MERGE.inc[10,1]<-"creameries"
          DF.WP1000.AG.MERGE.dec$words<-stemCompletion(DF.WP1000.AG.MERGE.dec$words,AGDICT)
          
          #Barplot of Differences lowest to highest
          ggplot(DF.WP1000.AG.MERGE.inc,aes(x=reorder(words,differences)))+geom_bar(aes(weight = differences),fill="green",col="black",position = "dodge") +
            xlab("Words") + ylab("Word Frequency/1000 differences (Co-op and Non Co-op)") +
            ggtitle("Least Prevalent Words in Co-op's Agriculture Sector")+geom_hline(yintercept = 0) +
            theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill=NA, size=1))
          makeFootnote(footnote)
          
          ggplot(DF.WP1000.AG.MERGE.dec,aes(x=reorder(words,-differences)))+geom_bar(aes(weight = differences),fill="green",col="black",position = "dodge") +
            xlab("Words") + ylab("Word Frequency/1000 differences (Co-op and Non Co-op)") +
            ggtitle("Most Prevalent Words in Co-op's Agriculture Sector")+geom_hline(yintercept = 0) +
            theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill=NA, size=1))
          makeFootnote(footnote)
          
      #Finance
          #Coop and Non Coop
          DF.FREQ.FIN.COOP<-as.data.frame(M.FREQ.FIN.COOP)
          DF.FREQ.FIN.NONCOOP<-as.data.frame(M.FREQ.FIN.NONCOOP)
          DF.WP1000.FIN.COOP<-as.data.frame(matrix(nrow=nrow(DF.FREQ.FIN.COOP),ncol=ncol(DF.FREQ.FIN.COOP)))
          DF.WP1000.FIN.NONCOOP<-as.data.frame(matrix(nrow=nrow(DF.FREQ.FIN.NONCOOP),ncol=ncol(DF.FREQ.FIN.NONCOOP)))
          colnames(DF.WP1000.FIN.COOP)<-colnames(DF.FREQ.FIN.COOP)
          colnames(DF.WP1000.FIN.NONCOOP)<-colnames(DF.FREQ.FIN.NONCOOP)
          rownames(DF.WP1000.FIN.COOP)<-rownames(DF.FREQ.FIN.COOP)
          rownames(DF.WP1000.FIN.NONCOOP)<-rownames(DF.FREQ.FIN.NONCOOP)
          #Create the multiplier for words per 1000 per company
          multfinwordscoop<-1000/t(as.data.frame(apply(DF.FREQ.FIN.COOP[,1:ncol(DF.FREQ.FIN.COOP)],2,sum))) #Create totals
          multfinwordsnoncoop<-1000/t(as.data.frame(apply(DF.FREQ.FIN.NONCOOP[,1:ncol(DF.FREQ.FIN.NONCOOP)],2,sum))) #Create totals
          #Loop to create words per 1000
          for (i in 1:(nrow(DF.FREQ.FIN.COOP))){
            for (j in 1:(ncol(DF.FREQ.FIN.COOP))){
              DF.WP1000.FIN.COOP[i,j]<-DF.FREQ.FIN.COOP[i,j]*multfinwordscoop[1,j]
            }
          }
          for (i in 1:(nrow(DF.FREQ.FIN.NONCOOP))){
            for (j in 1:(ncol(DF.FREQ.FIN.NONCOOP))){
              DF.WP1000.FIN.NONCOOP[i,j]<-DF.FREQ.FIN.NONCOOP[i,j]*multfinwordsnoncoop[1,j]
            }
          }
          #Create Average Columns
          DF.WP1000.FIN.COOP$average<-apply(DF.WP1000.FIN.COOP,1,mean)
          DF.WP1000.FIN.NONCOOP$average<-apply(DF.WP1000.FIN.NONCOOP,1,mean)
          #Create column of Words
          DF.WP1000.FIN.COOP$words<-rownames(DF.WP1000.FIN.COOP)
          DF.WP1000.FIN.NONCOOP$words<-rownames(DF.WP1000.FIN.NONCOOP)
          #Create Merged DF
          DF.WP1000.FIN.MERGE<-merge(DF.WP1000.FIN.COOP,DF.WP1000.FIN.NONCOOP,by='words',all.x=T,all.y=T,replace=T)
          DF.WP1000.FIN.MERGE[is.na(DF.WP1000.FIN.MERGE)]<-0
          #Create Differences/Percent Change Column
          DF.WP1000.FIN.MERGE$differences<-DF.WP1000.FIN.MERGE$average.x-DF.WP1000.FIN.MERGE$average.y
          DF.WP1000.FIN.MERGE$pctchange<-100*DF.WP1000.FIN.MERGE$differences/DF.WP1000.FIN.MERGE$average.x
          #Organize by Differences
          DF.WP1000.FIN.MERGE.inc<-DF.WP1000.FIN.MERGE[order(DF.WP1000.FIN.MERGE$differences),]
          DF.WP1000.FIN.MERGE.inc<-DF.WP1000.FIN.MERGE.inc[1:10,]
          DF.WP1000.FIN.MERGE.dec<-DF.WP1000.FIN.MERGE[order(DF.WP1000.FIN.MERGE$differences,decreasing=T),]
          DF.WP1000.FIN.MERGE.dec<-DF.WP1000.FIN.MERGE.dec[1:10,]
          #Stem Complete Words
          DF.WP1000.FIN.MERGE.inc$words<-stemCompletion(DF.WP1000.FIN.MERGE.inc$words,FINDICT)
          DF.WP1000.FIN.MERGE.dec$words<-stemCompletion(DF.WP1000.FIN.MERGE.dec$words,FINDICT)
          #Barplot of Differences lowest to highest
          ggplot(DF.WP1000.FIN.MERGE.inc,aes(x=reorder(words,differences)))+geom_bar(aes(weight = differences),fill="blue",col="black",position = "dodge") +
            xlab("Words") + ylab("Word Frequency/1000 differences (Co-op and Non Co-op)") +
            ggtitle("Least Prevalent Words in Co-op's Finance Sector")+geom_hline(yintercept = 0) +
            theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill=NA, size=1))
          makeFootnote(footnote)
          ggplot(DF.WP1000.FIN.MERGE.dec,aes(x=reorder(words,-differences)))+geom_bar(aes(weight = differences),fill="blue",col="black",position = "dodge") +
            xlab("Words") + ylab("Word Frequency/1000 differences (Co-op and Non Co-op)") +
            ggtitle("Most Prevalent Words in Co-op's Finance Sector")+geom_hline(yintercept = 0) +
            theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill=NA, size=1))
          makeFootnote(footnote)

      #Services
          #Coop and Non Coop
          DF.FREQ.SERV.COOP<-as.data.frame(M.FREQ.SERV.COOP)
          DF.FREQ.SERV.NONCOOP<-as.data.frame(M.FREQ.SERV.NONCOOP)
          DF.WP1000.SERV.COOP<-as.data.frame(matrix(nrow=nrow(DF.FREQ.SERV.COOP),ncol=ncol(DF.FREQ.SERV.COOP)))
          DF.WP1000.SERV.NONCOOP<-as.data.frame(matrix(nrow=nrow(DF.FREQ.SERV.NONCOOP),ncol=ncol(DF.FREQ.SERV.NONCOOP)))
          colnames(DF.WP1000.SERV.COOP)<-colnames(DF.FREQ.SERV.COOP)
          colnames(DF.WP1000.SERV.NONCOOP)<-colnames(DF.FREQ.SERV.NONCOOP)
          rownames(DF.WP1000.SERV.COOP)<-rownames(DF.FREQ.SERV.COOP)
          rownames(DF.WP1000.SERV.NONCOOP)<-rownames(DF.FREQ.SERV.NONCOOP)
          #Create the multiplier for words per 1000 per company
          multSERVwordscoop<-1000/t(as.data.frame(apply(DF.FREQ.SERV.COOP[,1:ncol(DF.FREQ.SERV.COOP)],2,sum))) #Create totals
          multSERVwordsnoncoop<-1000/t(as.data.frame(apply(DF.FREQ.SERV.NONCOOP[,1:ncol(DF.FREQ.SERV.NONCOOP)],2,sum))) #Create totals
          #Loop to create words per 1000
          for (i in 1:(nrow(DF.FREQ.SERV.COOP))){
            for (j in 1:(ncol(DF.FREQ.SERV.COOP))){
              DF.WP1000.SERV.COOP[i,j]<-DF.FREQ.SERV.COOP[i,j]*multSERVwordscoop[1,j]
            }
          }
          for (i in 1:(nrow(DF.FREQ.SERV.NONCOOP))){
            for (j in 1:(ncol(DF.FREQ.SERV.NONCOOP))){
              DF.WP1000.SERV.NONCOOP[i,j]<-DF.FREQ.SERV.NONCOOP[i,j]*multSERVwordsnoncoop[1,j]
            }
          }
          #Create Average Columns
          DF.WP1000.SERV.COOP$average<-apply(DF.WP1000.SERV.COOP,1,mean)
          DF.WP1000.SERV.NONCOOP$average<-apply(DF.WP1000.SERV.NONCOOP,1,mean)
          #Create column of Words
          DF.WP1000.SERV.COOP$words<-rownames(DF.WP1000.SERV.COOP)
          DF.WP1000.SERV.NONCOOP$words<-rownames(DF.WP1000.SERV.NONCOOP)
          #Create Merged DF
          DF.WP1000.SERV.MERGE<-merge(DF.WP1000.SERV.COOP,DF.WP1000.SERV.NONCOOP,by='words',all.x=T,all.y=T,replace=T)
          DF.WP1000.SERV.MERGE[is.na(DF.WP1000.SERV.MERGE)]<-0
          #Create Differences/Percent Change Column
          DF.WP1000.SERV.MERGE$differences<-DF.WP1000.SERV.MERGE$average.x-DF.WP1000.SERV.MERGE$average.y
          DF.WP1000.SERV.MERGE$pctchange<-100*DF.WP1000.SERV.MERGE$differences/DF.WP1000.SERV.MERGE$average.x
          DF.WP1000.SERV.MERGE<-DF.WP1000.SERV.MERGE[-961,]
          #Organize by Differences
          DF.WP1000.SERV.MERGE.inc<-DF.WP1000.SERV.MERGE[order(DF.WP1000.SERV.MERGE$differences),]
          DF.WP1000.SERV.MERGE.inc<-DF.WP1000.SERV.MERGE.inc[1:10,]
          DF.WP1000.SERV.MERGE.dec<-DF.WP1000.SERV.MERGE[order(DF.WP1000.SERV.MERGE$differences,decreasing=T),]
          DF.WP1000.SERV.MERGE.dec<-DF.WP1000.SERV.MERGE.dec[1:10,]
          #Stem Complete Words
          DF.WP1000.SERV.MERGE.inc$words<-stemCompletion(DF.WP1000.SERV.MERGE.inc$words,SERVDICT)
          DF.WP1000.SERV.MERGE.dec$words<-stemCompletion(DF.WP1000.SERV.MERGE.dec$words,SERVDICT)
          #Barplot of Differences lowest to highest
          ggplot(DF.WP1000.SERV.MERGE.inc,aes(x=reorder(words,differences)))+geom_bar(aes(weight = differences),fill="orange",col="black",position = "dodge") +
            xlab("Words") + ylab("Word Frequency/1000 differences (Co-op and Non Co-op)") +
            ggtitle("Least Prevalent Words in Co-op's Services Sector")+geom_hline(yintercept = 0) +
            theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill=NA, size=1))
          makeFootnote(footnote)
          ggplot(DF.WP1000.SERV.MERGE.dec,aes(x=reorder(words,-differences)))+geom_bar(aes(weight = differences),fill="orange",col="black",position = "dodge") +
            xlab("Words") + ylab("Word Frequency/1000 differences (Co-op and Non Co-op)") +
            ggtitle("Most Prevalent Words in Co-op's Services Sector")+geom_hline(yintercept = 0) +
            theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill=NA, size=1))
          makeFootnote(footnote)
          
      #OTHER
          #Coop and Non Coop
            DF.FREQ.OTHER.COOP<-as.data.frame(M.FREQ.OTHER.COOP)
            DF.FREQ.OTHER.NONCOOP<-as.data.frame(M.FREQ.OTHER.NONCOOP)
            DF.WP1000.OTHER.COOP<-as.data.frame(matrix(nrow=nrow(DF.FREQ.OTHER.COOP),ncol=ncol(DF.FREQ.OTHER.COOP)))
            DF.WP1000.OTHER.NONCOOP<-as.data.frame(matrix(nrow=nrow(DF.FREQ.OTHER.NONCOOP),ncol=ncol(DF.FREQ.OTHER.NONCOOP)))
            colnames(DF.WP1000.OTHER.COOP)<-colnames(DF.FREQ.OTHER.COOP)
            colnames(DF.WP1000.OTHER.NONCOOP)<-colnames(DF.FREQ.OTHER.NONCOOP)
            rownames(DF.WP1000.OTHER.COOP)<-rownames(DF.FREQ.OTHER.COOP)
            rownames(DF.WP1000.OTHER.NONCOOP)<-rownames(DF.FREQ.OTHER.NONCOOP)
          #Create the multiplier for words per 1000 per company
            multOTHERwordscoop<-1000/t(as.data.frame(apply(DF.FREQ.OTHER.COOP[,1:ncol(DF.FREQ.OTHER.COOP)],2,sum))) #Create totals
            multOTHERwordsnoncoop<-1000/t(as.data.frame(apply(DF.FREQ.OTHER.NONCOOP[,1:ncol(DF.FREQ.OTHER.NONCOOP)],2,sum))) #Create totals
          #Loop to create words per 1000
            for (i in 1:(nrow(DF.FREQ.OTHER.COOP))){
              for (j in 1:(ncol(DF.FREQ.OTHER.COOP))){
                DF.WP1000.OTHER.COOP[i,j]<-DF.FREQ.OTHER.COOP[i,j]*multOTHERwordscoop[1,j]
              }
            }
            for (i in 1:(nrow(DF.FREQ.OTHER.NONCOOP))){
              for (j in 1:(ncol(DF.FREQ.OTHER.NONCOOP))){
                DF.WP1000.OTHER.NONCOOP[i,j]<-DF.FREQ.OTHER.NONCOOP[i,j]*multOTHERwordsnoncoop[1,j]
              }
            }
          #Create Average Columns
            DF.WP1000.OTHER.COOP$average<-apply(DF.WP1000.OTHER.COOP,1,mean)
            DF.WP1000.OTHER.NONCOOP$average<-apply(DF.WP1000.OTHER.NONCOOP,1,mean)
          #Create column of Words
            DF.WP1000.OTHER.COOP$words<-rownames(DF.WP1000.OTHER.COOP)
            DF.WP1000.OTHER.NONCOOP$words<-rownames(DF.WP1000.OTHER.NONCOOP)
          #Create Merged DF
            DF.WP1000.OTHER.MERGE<-merge(DF.WP1000.OTHER.COOP,DF.WP1000.OTHER.NONCOOP,by='words',all.x=T,all.y=T,replace=T)
            DF.WP1000.OTHER.MERGE[is.na(DF.WP1000.OTHER.MERGE)]<-0
          #Create Differences/Percent Change Column
            DF.WP1000.OTHER.MERGE$differences<-DF.WP1000.OTHER.MERGE$average.x-DF.WP1000.OTHER.MERGE$average.y
            DF.WP1000.OTHER.MERGE$pctchange<-100*DF.WP1000.OTHER.MERGE$differences/DF.WP1000.OTHER.MERGE$average.x
            DF.WP1000.OTHER.MERGE<-DF.WP1000.OTHER.MERGE[-834,]
          #Organize by Differences
            DF.WP1000.OTHER.MERGE.inc<-DF.WP1000.OTHER.MERGE[order(DF.WP1000.OTHER.MERGE$differences),]
            DF.WP1000.OTHER.MERGE.inc<-DF.WP1000.OTHER.MERGE.inc[1:10,]
            DF.WP1000.OTHER.MERGE.dec<-DF.WP1000.OTHER.MERGE[order(DF.WP1000.OTHER.MERGE$differences,decreasing=T),]
            DF.WP1000.OTHER.MERGE.dec<-DF.WP1000.OTHER.MERGE.dec[1:10,]
          #Stem Complete Words
            DF.WP1000.OTHER.MERGE.inc[1,1]<-"surgeries"
            DF.WP1000.OTHER.MERGE.inc[2:10,1]<-stemCompletion(DF.WP1000.OTHER.MERGE.inc[2:10,1],OTHERDICT)
            DF.WP1000.OTHER.MERGE.dec$words<-stemCompletion(DF.WP1000.OTHER.MERGE.dec$words,OTHERDICT)
          #Barplot of Differences lowest to highest
            ggplot(DF.WP1000.OTHER.MERGE.inc,aes(x=reorder(words,differences)))+geom_bar(aes(weight = differences),fill="yellow",col="black",position = "dodge") +
              xlab("Words") + ylab("Word Frequency/1000 differences (Co-op and Non Co-op)") +
              ggtitle("Least Prevalent Words in Co-op's Other Sector")+geom_hline(yintercept = 0) +
              theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill=NA, size=1))
            makeFootnote(footnote)
            
            ggplot(DF.WP1000.OTHER.MERGE.dec,aes(x=reorder(words,-differences)))+geom_bar(aes(weight = differences),fill="yellow",col="black",position = "dodge") +
              xlab("Words") + ylab("Word Frequency/1000 differences (Co-op and Non Co-op)") +
              ggtitle("Most Prevalent Words in Co-op's Other Sector")+geom_hline(yintercept = 0) +
              theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill=NA, size=1))
            makeFootnote(footnote)
            
      #OVERALL COOPS AND NONCOOPS
            #Create Dataframes
              DF.FREQ.ALL.COOP<-data.frame(M.FREQ.ALL.COOP)
              DF.FREQ.ALL.NONCOOP<-data.frame(M.FREQ.ALL.NONCOOP)  
            #Create Word Per 1000 Dataframes
              DF.WP1000.ALL.COOP<-as.data.frame(matrix(nrow=nrow(DF.FREQ.ALL.COOP),ncol=ncol(DF.FREQ.ALL.COOP)))
              DF.WP1000.ALL.NONCOOP<-as.data.frame(matrix(nrow=nrow(DF.FREQ.ALL.NONCOOP),ncol=ncol(DF.FREQ.ALL.NONCOOP)))
            #Create Column and Row Names
              colnames(DF.WP1000.ALL.COOP)<-colnames(DF.FREQ.ALL.COOP)
              colnames(DF.WP1000.ALL.NONCOOP)<-colnames(DF.FREQ.ALL.NONCOOP)
              rownames(DF.WP1000.ALL.COOP)<-rownames(DF.FREQ.ALL.COOP)
              rownames(DF.WP1000.ALL.NONCOOP)<-rownames(DF.FREQ.ALL.NONCOOP)
            #Create the multiplier for words per 1000 per company
              multALLwordscoop<-1000/t(as.data.frame(apply(DF.FREQ.ALL.COOP[,1:ncol(DF.FREQ.ALL.COOP)],2,sum))) #Create totals
              multALLwordsnoncoop<-1000/t(as.data.frame(apply(DF.FREQ.ALL.NONCOOP[,1:ncol(DF.FREQ.ALL.NONCOOP)],2,sum))) #Create totals
            #Loop to create words per 1000
              for (i in 1:(nrow(DF.FREQ.ALL.COOP))){
                for (j in 1:(ncol(DF.FREQ.ALL.COOP))){
                  DF.WP1000.ALL.COOP[i,j]<-DF.FREQ.ALL.COOP[i,j]*multALLwordscoop[1,j]
                }
              }
              for (i in 1:(nrow(DF.FREQ.ALL.NONCOOP))){
                for (j in 1:(ncol(DF.FREQ.ALL.NONCOOP))){
                  DF.WP1000.ALL.NONCOOP[i,j]<-DF.FREQ.ALL.NONCOOP[i,j]*multALLwordsnoncoop[1,j]
                }
              }
            #Create Average Columns
              DF.WP1000.ALL.COOP$average<-apply(DF.WP1000.ALL.COOP,1,mean)
              DF.WP1000.ALL.NONCOOP$average<-apply(DF.WP1000.ALL.NONCOOP,1,mean)
            #Create column of Words
              DF.WP1000.ALL.COOP$words<-rownames(DF.WP1000.ALL.COOP)
              DF.WP1000.ALL.NONCOOP$words<-rownames(DF.WP1000.ALL.NONCOOP)
            

  # PART 2) PRINCIPLE ANALYSIS

    #2a) Creating Co-op Dictionary and Coop words
      #Create Principle Dictionary
        principle1<-c("voluntary","open","non-discriminatory","accept","admissible","accessible","welcoming","unrestricted","optional","unforced","volitional")
        principle2<-c("democratic","vote","socialist","communal","representation","contribute","participate","partake","self-governing")
        principle3<-c("capital","benefits","fair","equitable","egalitarian","proportional")
        principle4<-c("autonomous","independent","self-sufficient","self-determining","self-reliant","self-supporting","self-sustaining")
        principle5<-c("education","training","inform","knowledge-transfer","skill improvements","mentor","teach","instruct","coach","tutor","guide")
        principle6<-c("together","partner","serve","mutual","cooperate","collaborate","coact")
        principle7<-c("sustainable","communities","society","livable","viable","social","environmental")
        allprinciples<-c(principle1,principle2,principle3,principle4,principle5,principle6,principle7)
        allprinciples<-stemDocument(allprinciples)
        allprinciples<-data.frame(allprinciples)
        colnames(allprinciples)<-c("word")
        
        
      #Merge Coop words and Non coop words with the principles
        PRINC.COOP<-merge(DF.WP1000.ALL.COOP,allprinciples,by="word")
        PRINC.NONCOOP<-merge(DF.WP1000.ALL.NONCOOP,allprinciples,by="word")
        tot.principles.coop<-sum(PRINC.COOP[,2])
        tot.principles.noncoop<-sum(PRINC.NONCOOP[,2])
      #Create a bar plot of Principles in both Coop and Non Coop
        par(mfrow=c(1,1))
        barplot(c(tot.principles.coop,tot.principles.noncoop),names.arg=c("Number of Co-op Principle words used in Coop","Number of Co-op Principle words used in Non Co-op"),las=0.5,main="Number of Co-operative Principle keywords used in Co-op and Non Co-op",col=ifelse(POSORNEG.TOT[1:2,1]=="positive",'red','green'),ylim=c(0,40))
        mtext("Frequency of Co-op Principle Keywords",side=2,line=2)
     
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
        colnames(allprinciples)<-c("words","principle")



      # 2b) Merge Principles and Frequency Data Frames
        #Create Word Columns
          DF.FREQ.AG.COOP$words<-rownames(DF.FREQ.AG.COOP)
          DF.FREQ.FIN.COOP$words<-rownames(DF.FREQ.FIN.COOP)
          DF.FREQ.SERV.COOP$words<-rownames(DF.FREQ.SERV.COOP)
          DF.FREQ.OTHER.COOP$words<-rownames(DF.FREQ.OTHER.COOP)
          DF.FREQ.ALL.COOP$words<-rownames(DF.FREQ.ALL.COOP)
          DF.FREQ.ALL.NONCOOP$words<-rownames(DF.FREQ.ALL.NONCOOP)
        #Create Neccessary Principles and Sector Data frames
          FREQBYPRINC.AG<-merge(DF.FREQ.AG.COOP,allprinciples,by="words")
          FREQBYPRINC.FIN<-merge(DF.FREQ.FIN.COOP,allprinciples,by="words")
          FREQBYPRINC.SERV<-merge(DF.FREQ.SERV.COOP,allprinciples,by="words")
          FREQBYPRINC.OTHER<-merge(DF.FREQ.OTHER.COOP,allprinciples,by="words")
          FREQBYPRINC.ALL.COOP<-merge(DF.FREQ.ALL.COOP,allprinciples,by="words")
          FREQBYPRINC.ALL.NONCOOP<-merge(DF.FREQ.ALL.NONCOOP,allprinciples,by="words")
      
      #2c) Create Lists with each Data Frame from each company in it
          #AGRICULTURE
                list.agg<-vector("list",(ncol(FREQBYPRINC.AG)-2))
                for (i in 1:(ncol(FREQBYPRINC.AG)-2)){
                attach(FREQBYPRINC.AG)
                list.agg[[i]]<-aggregate(FREQBYPRINC.AG[,i+1],by=list(Type=principle),FUN=sum,na.rm=TRUE)
                colnames(list.agg[[i]])<-c("principle","freq")
                if (i==1){FREQBYPRINC.AG.SUM<-list.agg[[1]]}
                else {FREQBYPRINC.AG.SUM<-cbind(FREQBYPRINC.AG.SUM,list.agg[[i]][[2]]) }}
                #Loop to create words per 1000
                for (i in 1:(nrow(FREQBYPRINC.AG.SUM))){
                  for (j in 2:(ncol(FREQBYPRINC.AG.SUM))){
                FREQBYPRINC.AG.SUM[i,j]<-FREQBYPRINC.AG.SUM[i,j]*multagwordscoop[1,j-1]}}
                #Create Average Word Frequency in Agriculture
                FREQBYPRINC.AG.SUM$agavg<-apply(FREQBYPRINC.AG.SUM[,2:(ncol(FREQBYPRINC.AG.SUM))],1,mean)
        #FINANCE
                list.fin<-vector("list",(ncol(FREQBYPRINC.FIN)-2))
                for (i in 1: (ncol(FREQBYPRINC.FIN)-2)){
                attach(FREQBYPRINC.FIN)
                list.fin[[i]]<-aggregate(FREQBYPRINC.FIN[,i+1],by=list(Type=principle),FUN=sum,na.rm=TRUE)
                colnames(list.fin[[i]])<-c("principle","freq")
                if (i==1){FREQBYPRINC.FIN.SUM<-list.fin[[1]]}
                else {FREQBYPRINC.FIN.SUM<-cbind(FREQBYPRINC.FIN.SUM,list.fin[[i]][[2]]) }}
                #Loop to create words per 1000
                for (i in 1:(nrow(FREQBYPRINC.FIN.SUM))){
                  for (j in 2:(ncol(FREQBYPRINC.FIN.SUM))){
                    FREQBYPRINC.FIN.SUM[i,j]<-FREQBYPRINC.FIN.SUM[i,j]*multfinwordscoop[1,j-1]}}
                #Create Average Word Frequency in Finance
                FREQBYPRINC.FIN.SUM$finavg<-apply(FREQBYPRINC.FIN.SUM[,2:(ncol(FREQBYPRINC.FIN.SUM))],1,mean)
  
        #Services    
                list.serv<-vector("list",(ncol(FREQBYPRINC.SERV)-2))
                for (i in 1: (ncol(FREQBYPRINC.SERV)-2)){
                attach(FREQBYPRINC.SERV)
                list.serv[[i]]<-aggregate(FREQBYPRINC.SERV[,i+1],by=list(Type=principle),FUN=sum,na.rm=TRUE)
                colnames(list.serv[[i]])<-c("principle","freq")
                if (i==1){
                FREQBYPRINC.SERV.SUM<-list.serv[[1]]
                }
                else {FREQBYPRINC.SERV.SUM<-cbind(FREQBYPRINC.SERV.SUM,list.serv[[i]][[2]]) }}
                #Loop to create words per 1000
                for (i in 1:(nrow(FREQBYPRINC.SERV.SUM))){
                  for (j in 2:(ncol(FREQBYPRINC.SERV.SUM))){
                    FREQBYPRINC.SERV.SUM[i,j]<-FREQBYPRINC.SERV.SUM[i,j]*multSERVwordscoop[1,j-1]}}
                #Create Average Word Frequency in Finance
                FREQBYPRINC.SERV.SUM$servavg<-apply(FREQBYPRINC.SERV.SUM[,2:(ncol(FREQBYPRINC.SERV.SUM))],1,mean)
                
        #Other 
              list.other<-vector("list",(ncol(FREQBYPRINC.OTHER)-2))
              for (i in 1: (ncol(FREQBYPRINC.OTHER)-2)){
                attach(FREQBYPRINC.OTHER)
                list.other[[i]]<-aggregate(FREQBYPRINC.OTHER[,i+1],by=list(Type=principle),FUN=sum,na.rm=TRUE)
                colnames(list.other[[i]])<-c("principle","freq")
                if (i==1){
                  FREQBYPRINC.OTHER.SUM<-list.other[[1]]
                }
                else {FREQBYPRINC.OTHER.SUM<-cbind(FREQBYPRINC.OTHER.SUM,list.other[[i]][[2]]) }}
              #Loop to create words per 1000
              for (i in 1:(nrow(FREQBYPRINC.OTHER.SUM))){
                for (j in 2:(ncol(FREQBYPRINC.OTHER.SUM))){
                  FREQBYPRINC.OTHER.SUM[i,j]<-FREQBYPRINC.OTHER.SUM[i,j]*multOTHERwordscoop[1,j-1]}}
              #Create Average Word Frequency in Finance
              FREQBYPRINC.OTHER.SUM$otheravg<-apply(FREQBYPRINC.OTHER.SUM[,2:(ncol(FREQBYPRINC.OTHER.SUM))],1,mean)
        #All Coop
            
              list.all.coop<-vector("list",(ncol(FREQBYPRINC.ALL.COOP)-2))
              for (i in 1: (ncol(FREQBYPRINC.ALL.COOP)-2)){
              attach(FREQBYPRINC.ALL.COOP)
              list.all.coop[[i]]<-aggregate(FREQBYPRINC.ALL.COOP[,i+1],by=list(Type=principle),FUN=sum,na.rm=TRUE)
              colnames(list.all.coop[[i]])<-c("principle","freq")
              if (i==1){FREQBYPRINC.ALL.COOP.SUM<-list.all.coop[[1]]}
              else {FREQBYPRINC.ALL.COOP.SUM<-cbind(FREQBYPRINC.ALL.COOP.SUM,list.all.coop[[i]][[2]]) }}
              #Loop to create words per 1000
              for (i in 1:(nrow(FREQBYPRINC.ALL.COOP.SUM))){
                for (j in 2:(ncol(FREQBYPRINC.ALL.COOP.SUM))){
                  FREQBYPRINC.ALL.COOP.SUM[i,j]<-FREQBYPRINC.ALL.COOP.SUM[i,j]*multALLwordscoop[1,j-1]}}
              #Create Average Word Frequency in Finance
              FREQBYPRINC.ALL.COOP.SUM$allcoopavg<-apply(FREQBYPRINC.ALL.COOP.SUM[,2:(ncol(FREQBYPRINC.ALL.COOP.SUM))],1,mean)
            
              
              
              
           #All NonCoop
              list.all.noncoop<-vector("list",(ncol(FREQBYPRINC.ALL.NONCOOP)-2))
              for (i in 1: (ncol(FREQBYPRINC.ALL.NONCOOP)-2)){
              attach(FREQBYPRINC.ALL.NONCOOP)
              list.all.noncoop[[i]]<-aggregate(FREQBYPRINC.ALL.NONCOOP[,i+1],by=list(Type=principle),FUN=sum,na.rm=TRUE)
              colnames(list.all.noncoop[[i]])<-c("principle","freq")
              if (i==1){FREQBYPRINC.ALL.NONCOOP.SUM<-list.all.noncoop[[1]]}
              else {FREQBYPRINC.ALL.NONCOOP.SUM<-cbind(FREQBYPRINC.ALL.NONCOOP.SUM,list.all.noncoop[[i]][[2]]) }}
              #Loop to create words per 1000
              for (i in 1:(nrow(FREQBYPRINC.ALL.NONCOOP.SUM))){
                for (j in 2:(ncol(FREQBYPRINC.ALL.NONCOOP.SUM))){
                  FREQBYPRINC.ALL.NONCOOP.SUM[i,j]<-FREQBYPRINC.ALL.NONCOOP.SUM[i,j]*multALLwordsnoncoop[1,j-1]}}
              #Create Average Word Frequency in Finance
              FREQBYPRINC.ALL.NONCOOP.SUM$allnoncoopavg<-apply(FREQBYPRINC.ALL.NONCOOP.SUM[,2:(ncol(FREQBYPRINC.ALL.NONCOOP.SUM))],1,mean)

            
        #2d) Compare Principle Words in Coop and Non Coop
          #Creating New Dataframe and appropriate columns
            COOPNONCOOP.PRINC<-merge(FREQBYPRINC.ALL.COOP.SUM,FREQBYPRINC.ALL.NONCOOP.SUM,by="principle")
            COOPNONCOOP.PRINC$differences<-COOPNONCOOP.PRINC$allcoopavg -COOPNONCOOP.PRINC$allnoncoopavg
            COOPNONCOOP.PRINC$pctchange<-100*(COOPNONCOOP.PRINC$differences)/(COOPNONCOOP.PRINC$allnoncoopavg)
            COOPNONCOOP.PRINC$ratio<-COOPNONCOOP.PRINC$allcoopavg/COOPNONCOOP.PRINC$allnoncoopavg
          #Create Barplots
            #Barplot of Differences
              ggplot(COOPNONCOOP.PRINC,aes(principle))+geom_bar(aes(weight = differences),fill=ifelse(COOPNONCOOP.PRINC$differences < 0,'red','green'),col="black",position = "dodge") +
              xlab("Principle") + ylab("Co-op Word Freq - Non Co-op Word Freq (per 1000 words)") +
              ggtitle("Differences of Word Frequency/1000 by 7 Co-op Principles between Co-op and Non Co-op")+geom_hline(yintercept = 0) +
              theme(plot.title=element_text(hjust=0.5))
            #Barplot of Percent Difference
              ggplot(COOPNONCOOP.PRINC,aes(principle))+geom_bar(aes(weight = pctchange),fill=ifelse(COOPNONCOOP.PRINC$pctchange < 0,'red','green'),col="black",position = "dodge") +
              xlab("Principle") + ylab("% Difference of Word Frequency/1000 from Co-op to Non-Co-op") +
              ggtitle("Co-operatives talk about their Principles more than Non Co-operatives")+geom_hline(yintercept = 0) +
              theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill=NA, size=1))
              makeFootnote(footnote)
            #Barplot of Ratios
              ggplot(COOPNONCOOP.PRINC,aes(principle))+geom_bar(aes(weight = ratio),fill=ifelse(COOPNONCOOP.PRINC$pctchange < 0,'red','green'),col="black",position = "dodge") +
              xlab("Principle") + ylab("Ratio of Coop Frequency to Non Coop Frequency") +
              ggtitle("Ratios of Word Frequency/1000 by 7 Co-op Principles between Co-op and Non Co-op")+geom_hline(yintercept = 1) +
              theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill=NA, size=1))

      #2e Comparing Principle Words in Sectors with Overall
              
          #Agriculture
            #Create New Columns for Agriculture
              COOP.AG.PRINC<-merge(FREQBYPRINC.ALL.COOP.SUM,FREQBYPRINC.AG.SUM,by="principle")
              COOP.AG.PRINC$differences<-COOP.AG.PRINC$agavg-COOP.AG.PRINC$allcoopavg
              COOP.AG.PRINC$pctchange<-100*(COOP.AG.PRINC$differences)/(COOP.AG.PRINC$allcoopavg)
              COOP.AG.PRINC$ratio<-COOP.AG.PRINC$agavg/COOP.AG.PRINC$allcoopavg
              
              #Barplots
                  #Barplot of Differences
                    ggplot(COOP.AG.PRINC,aes(principle))+geom_bar(aes(weight = differences),fill=ifelse(COOP.AG.PRINC$differences < 0,'green4','green'),col="black",position = "dodge") +
                    xlab("Principle") + ylab("Agriculture Sector Word Frequency - Overall Coop Word Frequency (per 1000)") +
                    ggtitle("Differences of Word Frequency/1000 by 7 Co-op Principles between Agriculture Sector and Overall Co-ops")+geom_hline(yintercept = 0) +
                    theme(plot.title=element_text(hjust=0.5))
                  #Barplot of Percent Differences
                    ggplot(COOP.AG.PRINC,aes(principle))+geom_bar(aes(weight = pctchange),fill=ifelse(COOP.AG.PRINC$pctchange < 0,'green4','green'),col="black",position = "dodge") +
                    xlab("Principle") + ylab("% Difference of Word Frequency/1000 from Agriculture to Overall Co-op") +
                    ggtitle("Agriculture Sector Talks Mainly about 2nd and 3rd Co-operative Principles ")+geom_hline(yintercept = 0) +
                    theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill=NA, size=1))
                    makeFootnote(footnote)
                  #Barplot of Ratios
                    ggplot(COOP.AG.PRINC,aes(principle))+geom_bar(aes(weight = ratio),fill=ifelse(COOP.AG.PRINC$ratio < 1,'green4','green'),col="black",position = "dodge") +
                    xlab("Principle") + ylab("Ratio of Agriculture Frequency to Overall Co-op Frequency") +
                    ggtitle("Ratios of Word Frequency/1000 by 7 Co-op Principles between Agriculture Sector and Overall Co-op")+geom_hline(yintercept = 1) +
                    theme(plot.title=element_text(hjust=0.5))

            #Finance 
              #Create New Columns for Finance
                  COOP.FIN.PRINC<-merge(FREQBYPRINC.ALL.COOP.SUM,FREQBYPRINC.FIN.SUM,by="principle")
                  COOP.FIN.PRINC$differences<-COOP.FIN.PRINC$finavg -COOP.FIN.PRINC$allcoopavg
                  COOP.FIN.PRINC$pctchange<-100*(COOP.FIN.PRINC$differences)/(COOP.FIN.PRINC$allcoopavg)
                  COOP.FIN.PRINC$ratio<-COOP.FIN.PRINC$finavg/COOP.FIN.PRINC$allcoopavg
                  
                #Barplots
                  #Barplot of Differences
                    ggplot(COOP.FIN.PRINC,aes(principle))+geom_bar(aes(weight = differences),fill=ifelse(COOP.FIN.PRINC$differences < 0,'blue4','blue'),col="black",position = "dodge") +
                    xlab("Principle") + ylab("Finance Sector Word Frequency - Overall Coop Word Frequency (per 1000)") +
                    ggtitle("Differences of Word Frequency/1000 by 7 Co-op Principles between Finance Sector and Overall Co-ops")+geom_hline(yintercept = 0) +
                    theme(plot.title=element_text(hjust=0.5))
                  #Barplot of PercentDifference
                    ggplot(COOP.FIN.PRINC,aes(principle))+geom_bar(aes(weight = pctchange),fill=ifelse(COOP.FIN.PRINC$pctchange < 0,'blue4','blue'),col="black",position = "dodge") +
                    xlab("Principle") + ylab("% Difference of Word Frequency/1000 from Finance to Overall Co-op") +
                    ggtitle("Finance Sector Talks Mainly about the 6th and 7th Co-operative Principles ")+geom_hline(yintercept = 0) +
                    theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill=NA, size=1))
                    makeFootnote(footnote)
                  #Barplot of Ratios
                    ggplot(COOP.FIN.PRINC,aes(principle))+geom_bar(aes(weight = ratio),fill=ifelse(COOP.FIN.PRINC$ratio < 1,'blue4','blue'),col="black",position = "dodge") +
                    xlab("Principle") + ylab("Ratio of Finance Frequency to Overall Co-op Frequency") +
                    ggtitle("Ratios of Word Frequency/1000 by 7 Co-op Principles between Finance Sector and Overall Co-op")+geom_hline(yintercept = 1) +
                    theme(plot.title=element_text(hjust=0.5))
        
          
            #Service
              #Create New Columns for Service
                COOP.SERV.PRINC<-merge(FREQBYPRINC.ALL.COOP.SUM,FREQBYPRINC.SERV.SUM,by="principle")
                COOP.SERV.PRINC$differences<-COOP.SERV.PRINC$servavg-COOP.SERV.PRINC$allcoopavg
                COOP.SERV.PRINC$pctchange<-100*(COOP.SERV.PRINC$differences)/(COOP.SERV.PRINC$allcoopavg)
                COOP.SERV.PRINC$ratio<-COOP.SERV.PRINC$servavg/COOP.SERV.PRINC$allcoopavg
          
              #Barplots for Service
                  #Barplot of Differences
                  ggplot(COOP.SERV.PRINC,aes(principle))+geom_bar(aes(weight = differences),fill=ifelse(COOP.SERV.PRINC$differences < 0,'orange4','orange'),col="black",position = "dodge") +
                  xlab("Principle") + ylab("Service Sector Word Frequency - Overall Coop Word Frequency (per 1000)") +
                  ggtitle("Differences of Word Frequency/1000 by 7 Co-op Principles between Service Sector and Overall Co-op")+geom_hline(yintercept = 0) +
                  theme(plot.title=element_text(hjust=0.5))
              
                  #Barplot of Percent change
                  ggplot(COOP.SERV.PRINC,aes(principle))+geom_bar(aes(weight = pctchange),fill=ifelse(COOP.SERV.PRINC$pctchange < 0,'orange4','orange'),col="black",position = "dodge") +
                  xlab("Principle") + ylab("% Difference of Word Frequency/1000 from Service to Overall Co-op") +
                  ggtitle("Service Sector Discusses Most of the Co-operative Principles")+geom_hline(yintercept = 0) +
                  theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill=NA, size=1))
                  makeFootnote(footnote)
              
                  #Barplot of Ratios
                  ggplot(COOP.SERV.PRINC,aes(principle))+geom_bar(aes(weight = ratio),fill=ifelse(COOP.SERV.PRINC$ratio < 1,'orange4','orange'),col="black",position = "dodge") +
                  xlab("Principle") + ylab("Ratio of Retail Frequency to Overall Co-op Frequency") +
                  ggtitle("Ratios of Word Frequency/1000 by 7 Co-op Principles between Service Sector and Overall Co-op")+geom_hline(yintercept = 1) +
                  theme(plot.title=element_text(hjust=0.5))
                  
            #Other
                  #Create New Columns for Other
                  COOP.OTHER.PRINC<-merge(FREQBYPRINC.ALL.COOP.SUM,FREQBYPRINC.OTHER.SUM,by="principle")
                  COOP.OTHER.PRINC$differences<-COOP.OTHER.PRINC$otheravg-COOP.OTHER.PRINC$allcoopavg
                  COOP.OTHER.PRINC$pctchange<-100*(COOP.OTHER.PRINC$differences)/(COOP.OTHER.PRINC$allcoopavg)
                  COOP.OTHER.PRINC$ratio<-COOP.OTHER.PRINC$servavg/COOP.OTHER.PRINC$allcoopavg
                  
                  #Barplots for Other
                    #Barplot of Differences
                      ggplot(COOP.OTHER.PRINC,aes(principle))+geom_bar(aes(weight = differences),fill=ifelse(COOP.SERV.PRINC$differences < 0,'yellow4','yellow'),col="black",position = "dodge") +
                        xlab("Principle") + ylab("Other Sector Word Frequency - Overall Coop Word Frequency (per 1000)") +
                        ggtitle("Differences of Word Frequency/1000 by 7 Co-op Principles between Other Sector and Overall Co-op")+geom_hline(yintercept = 0) +
                        theme(plot.title=element_text(hjust=0.5))
                    #Barplot of Percent Difference
                      ggplot(COOP.OTHER.PRINC,aes(principle))+geom_bar(aes(weight = pctchange),fill=ifelse(COOP.OTHER.PRINC$pctchange < 0,'yellow4','yellow'),col="black",position = "dodge") +
                        xlab("Principle") + ylab("% Difference of Word Frequency/1000 from Other to Overall Co-op") +
                        ggtitle("Other Sectors Discuss Most Co-operative Principles")+geom_hline(yintercept = 0) +
                        theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill=NA, size=1))
                         makeFootnote(footnote)
                  
                  #Barplot of Ratios
                  ggplot(COOP.SERV.PRINC,aes(principle))+geom_bar(aes(weight = ratio),fill=ifelse(COOP.SERV.PRINC$ratio < 1,'yellow4','yellow'),col="black",position = "dodge") +
                    xlab("Principle") + ylab("Ratio of Other Frequency to Overall Co-op Frequency") +
                    ggtitle("Ratios of Word Frequency/1000 by 7 Co-op Principles between Other Sector and Overall Co-op")+geom_hline(yintercept = 1) +
                    theme(plot.title=element_text(hjust=0.5))

                  
  #Create Summary Data Frames
                  
    SUMMARY.COOP.AG.PRINC<-as.data.frame(cbind(COOP.AG.PRINC$principle,COOP.AG.PRINC$pctchange))
    SUMMARY.COOP.AG.PRINC$sector<-"AG"
    
    SUMMARY.COOP.FIN.PRINC<-as.data.frame(cbind(COOP.FIN.PRINC$principle,COOP.FIN.PRINC$pctchange))
    SUMMARY.COOP.FIN.PRINC$sector<-"FIN"
    
    SUMMARY.COOP.SERV.PRINC<-as.data.frame(cbind(COOP.SERV.PRINC$principle,COOP.SERV.PRINC$pctchange))
    SUMMARY.COOP.SERV.PRINC$sector<-"SERV"
    
    SUMMARY.COOP.OTHER.PRINC<-as.data.frame(cbind(COOP.OTHER.PRINC$principle,COOP.OTHER.PRINC$pctchange))
    SUMMARY.COOP.OTHER.PRINC$sector<-"OTHER"
    
    #Create Massive Data frame
      SUMMARY.PCTDIFF<-rbind(SUMMARY.COOP.AG.PRINC,SUMMARY.COOP.FIN.PRINC,SUMMARY.COOP.SERV.PRINC,SUMMARY.COOP.OTHER.PRINC)

    #Create Proper Row names
      colnames(SUMMARY.PCTDIFF)<-c("principle","pctdiff","Sector")
    #Make Numeric
      SUMMARY.PCTDIFF$pctdiff <- as.numeric(as.character(SUMMARY.PCTDIFF$pctdiff))
    #Create Color Pallette
      #cbPalette <- c("green", "blue", "yellow", "red")
    #Create Barplot
      ggplot(SUMMARY.PCTDIFF, aes(principle, pctdiff)) +xlab("Principle")+ylab("% Difference of Word Frequency/1000 from Sectors to Overall Co-op")+
        geom_bar(aes(fill = Sector),position = "dodge", stat="identity",col="black")+ggtitle("How Differing Sectors Talk about Different Co-operative Principles")+theme(plot.title=element_text(hjust=0.5),panel.border = element_rect(colour = "black", fill=NA, size=1),legend.background = element_rect(color = "black")+# length slightly shrinks the size of the arrow head; lwd makes the line bolder
                                                                                                                                                                           arrows(x0=0, y0=0, x1=1, y1=20, col='blue', length=0.1, lwd=3)) #+scale_fill_manual(values=cbPalette)+
    
      makeFootnote(footnote)
    

    