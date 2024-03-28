
loadPackages<-function(){
  # load packages
  #list of packages
  packages<-c("tidyverse", "tidytext", "sentimentr", "gt", "gtsummary", "kableExtra")
  
  # install packages not yet installed
  installed_packages<-packages %in% rownames(installed.packages())
  if(any(installed_packages==FALSE)){
    install.packages(packages[!installed_packages])
  }
  
  # Packages loading, with library function
  invisible(lapply(packages, library, character.only=TRUE))
  
  
}


frequencyQuestion<-function(data, columnName, label, fileName, casOnly=FALSE, gradOnly=FALSE){
  
  #remove na rows for selected column:
  dataNaRemoved<-data %>% drop_na(columnName)
  
  if(casOnly==TRUE){
    dataNaRemoved<- dataNaRemoved %>%
      filter(campus=="CAS")
  }
  if(gradOnly==TRUE){
    dataNaRemoved<- dataNaRemoved %>%
      filter(campus=="GRAD")
  } 
  
  total<-nrow(dataNaRemoved)
  dataNaRemoved
  
  all<-dataNaRemoved %>%
    group_by(!!sym(columnName)) %>%
    summarize(count=n(), percent=round(n()/total*100, digits=1)) %>%
    arrange(match(!!sym(columnName), c("Daily", "Weekly", "Monthly", "Once a semester", "Never")))
  
  # turn this into factors, so it preserves the order on the charts
  all[[columnName]] <- factor(all[[columnName]], levels = all[[columnName]])
  #return (all)
  
  chart<-all %>% 
    ggplot(mapping=aes(x=.data[[columnName]], y=percent, fill=.data[[columnName]]))+
    geom_bar(stat="identity")+
    geom_text(aes(label = paste(percent, "%")), vjust = -0.5, size = 3) + 
    labs(title=paste("Frequency of Watzek Visits for",label), y="Percent of respondants", x="", fill=paste("Frequency"), caption=paste("n=", total, sep = ""))
  
  print(chart)
  ggsave(chart, file=paste("../output/",fileName))
  
  
}

frequencyQuestionCampus<-function(data, columnName, label, fileName){
  
  #remove na rows for selected column:
  dataNaRemoved<-data %>% drop_na(columnName)
  

  
  total<-nrow(dataNaRemoved)
  dataNaRemoved
  
  all<-dataNaRemoved %>%
    group_by(onCampus, !!sym(columnName)) %>%
    summarize(count=n(), percent=round(n()/total*100, digits=1)) %>%
    arrange(match(!!sym(columnName), c("Daily", "Weekly", "Monthly", "Once a semester", "Never")))
  
  # turn this into factors, so it preserves the order on the charts
  all[[columnName]] <- factor(all[[columnName]], levels = all[[columnName]])
  #return (all)
  
  chart<-all %>% 
    ggplot(mapping=aes(x=.data[[columnName]], y=percent, fill=.data[[columnName]]))+
    geom_bar(stat="identity")+
    geom_text(aes(label = paste(percent, "%")), vjust = -0.5, size = 3) + 
    labs(title=paste("Frequency of Watzek Visits for",label), y="Percent of respondants", x="", fill=paste("Frequency"), caption=paste("n=", total, sep = ""))
  
  print(chart)
  ggsave(chart, file=paste("output/",fileName))
  
  
}


needsQuestion<-function(data, columnName, label, fileName, casOnly=FALSE, gradOnly=FALSE){
  
  
  dataNaRemoved<-data %>% drop_na(columnName)

  dataNaRemoved
  if(casOnly==TRUE){
    dataNaRemoved<- dataNaRemoved %>%
      filter(campus=="CAS")
  }
  if(gradOnly==TRUE){
    dataNaRemoved<- dataNaRemoved %>%
      filter(campus=="GRAD")
  } 
  total<-nrow(dataNaRemoved)
  
  all<-dataNaRemoved %>%
    group_by(!!sym(columnName)) %>%
    summarize(count=n(), percent=round(n()/total*100, digits=1)) %>%
    arrange(match(!!sym(columnName), c("Meets my needs", "Does not meet my needs", "I don't have a need for this", "I'm not familiar with this")))
  
  all[[columnName]] <- factor(all[[columnName]], levels = all[[columnName]])
  #return (all)
  
  chart<-all %>% 
    ggplot(mapping=aes(x=.data[[columnName]], y=percent, fill=.data[[columnName]]))+
    geom_bar(stat="identity")+
    geom_text(aes(label = paste(percent, "%")), vjust = -0.5, size = 3) +
    theme(axis.text.x=element_blank())+
    labs(title=paste("Meets needs for",label), y="Percent of respondants", x="", fill=paste("Extent of Meeting Needs"), caption=paste("n=", total, sep = ""))
  
  print(chart)
  ggsave(chart, file=paste("../output/",fileName))
  
}

usefulQuestion<-function(data, columnName, label, fileName, casOnly=FALSE, gradOnly=FALSE){
  
  
  dataNaRemoved<-data %>% drop_na(columnName)
  
  dataNaRemoved
  if(casOnly==TRUE){
    dataNaRemoved<- dataNaRemoved %>%
      filter(campus=="CAS")
  }
  if(gradOnly==TRUE){
    dataNaRemoved<- dataNaRemoved %>%
      filter(campus=="GRAD")
  } 
  total<-nrow(dataNaRemoved)
  
  all<-dataNaRemoved %>%
    group_by(!!sym(columnName)) %>%
    summarize(count=n(), percent=round(n()/total*100, digits=1)) %>%
    arrange(match(!!sym(columnName), c("Very useful", "Somewhat useful", "Not useful at all","I haven't used this", "I'm not familiar with this")))
  
  all[[columnName]] <- factor(all[[columnName]], levels = all[[columnName]])
  #return (all)
  
  chart<-all %>% 
    ggplot(mapping=aes(x=.data[[columnName]], y=percent, fill=.data[[columnName]]))+
    geom_bar(stat="identity")+
    geom_text(aes(label = paste(percent, "%")), vjust = -0.5, size = 3) +
    theme(axis.text.x=element_blank())+
    labs(title=paste("Usefulness of",label), y="Percent of respondants", x="", fill=paste("Degree of Usefulness"), caption=paste("n=", total, sep = ""))
  
  print(chart)
  ggsave(chart, file=paste("../output/",fileName))
  
}
