
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
  
  colors <- c("Daily" = "#e60049", 
              "Weekly" = "#0bb4ff", 
              "Monthly" = "#50e991", 
              "Once a semester" = "#e6d800", 
              "Never"="#9b19f5")
  
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
    scale_fill_manual(values = colors) +
    geom_text(aes(label = paste(percent, "%")), vjust = -0.5, size = 3) + 
    labs(title=paste("Frequency of Watzek Visits for",label), y="Percent of respondants", x="", fill=paste("Frequency"), caption=paste("n=", total, sep = ""))
  
  print(chart)
  ggsave(chart, file=paste("../output/",fileName))
  
  
}

bookQuestion<-function(data, columnName, label, fileName, casOnly=FALSE, gradOnly=FALSE){
  
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
    arrange(match(!!sym(columnName), c("Printed books", "Ebooks", "No preference", "It depends")))
  
  # turn this into factors, so it preserves the order on the charts
  all[[columnName]] <- factor(all[[columnName]], levels = all[[columnName]])
  #return (all)
  
  chart<-all %>% 
    ggplot(mapping=aes(x=.data[[columnName]], y=percent, fill=.data[[columnName]]))+
    geom_bar(stat="identity")+
    geom_text(aes(label = paste(percent, "%")), vjust = -0.5, size = 3) + 
    labs(title=paste("Book format preference for",label), y="Percent of respondants", x="", fill=paste("Format preference"), caption=paste("n=", total, sep = ""))
  
  print(chart)
  ggsave(chart, file=paste("../output/",fileName))
  
  
}

libClassQuestion<-function(data, columnName, label, fileName, casOnly=FALSE, gradOnly=FALSE){
  
  
  colors <- c("Not often enough" = "#e60049", 
              "Often enough" = "#0bb4ff", 
              "Too often" = "#50e991", 
              "No librarians have ever met with any of my classes"="#9b19f5")
  
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
    arrange(match(!!sym(columnName), c("Not often enough", "Often enough", "Too often", "No librarians have ever met with any of my classes")))
  
  # turn this into factors, so it preserves the order on the charts
  all[[columnName]] <- factor(all[[columnName]], levels = all[[columnName]])
  #return (all)
  
  chart<-all %>% 
    ggplot(mapping=aes(x=.data[[columnName]], y=percent, fill=.data[[columnName]]))+
    geom_bar(stat="identity")+
    scale_fill_manual(values = colors) +
    geom_text(aes(label = paste(percent, "%")), vjust = -0.5, size = 3) + 
    labs(title=paste("Frequency of librarian class visits, ",label), y="Percent of respondants", x="", fill=paste("Frequency of librarian class visits"), caption=paste("n=", total, sep = ""))+
    theme(axis.text.x = element_blank())
  
  print(chart)
  ggsave(chart, file=paste("../output/",fileName))
  
  
}


comfortQuestion<-function(data, columnName, label, fileName, casOnly=FALSE, gradOnly=FALSE){
  
  
  colors <- c("Strongly agree" = "#e60049", 
              "Somewhat agree" = "#0bb4ff", 
              "Disagree" = "#50e991")
  
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
    arrange(match(!!sym(columnName), c("Strongly agree", "Somewhat agree", "Disagree")))
  
  # turn this into factors, so it preserves the order on the charts
  all[[columnName]] <- factor(all[[columnName]], levels = all[[columnName]])
  #return (all)
  
  chart<-all %>% 
    ggplot(mapping=aes(x=.data[[columnName]], y=percent, fill=.data[[columnName]]))+
    geom_bar(stat="identity")+
    scale_fill_manual(values = colors) +
    geom_text(aes(label = paste(percent, "%")), vjust = -0.5, size = 3) + 
    labs(title=paste("Feel welcome at Watzek, ",label), y="Percent of respondants", x="", fill=paste("Feel welcome at Watzek"), caption=paste("n=", total, sep = ""))+
    theme(axis.text.x = element_blank())
  
  print(chart)
  ggsave(chart, file=paste("../output/",fileName))
  
  
}


comfortAskingQuestions<-function(data, columnName, label, fileName, casOnly=FALSE, gradOnly=FALSE){
  
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
    arrange(match(!!sym(columnName), c("Strongly agree", "Somewhat agree", "Disagree")))
  
  # turn this into factors, so it preserves the order on the charts
  all[[columnName]] <- factor(all[[columnName]], levels = all[[columnName]])
  #return (all)
  
  chart<-all %>% 
    ggplot(mapping=aes(x=.data[[columnName]], y=percent, fill=.data[[columnName]]))+
    geom_bar(stat="identity")+
    geom_text(aes(label = paste(percent, "%")), vjust = -0.5, size = 3) + 
    labs(title=paste("Feel comfortable asking questions, ",label), y="Percent of respondants", x="", fill=paste("Feel comfortable asking questions"), caption=paste("n=", total, sep = ""))+
    theme(axis.text.x = element_blank())
  
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
  
  colors <- c("Meets my needs" = "#e60049", 
              "Does not meet my needs" = "#0bb4ff", 
              "I don't have a need for this" = "#50e991", 
              "I'm not familiar with this" = "#e6d800")
  
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
    scale_fill_manual(values = colors) +
    geom_text(aes(label = paste(percent, "%")), vjust = -0.5, size = 3) +
    theme(axis.text.x=element_blank())+
    labs(title=paste("Meets needs for",label), y="Percent of respondants", x="", fill=paste("Extent of Meeting Needs"), caption=paste("n=", total, sep = ""))
  
  print(chart)
  ggsave(chart, file=paste("../output/",fileName))
  
}

usefulQuestion<-function(data, columnName, label, fileName, casOnly=FALSE, gradOnly=FALSE){
  
  colors <- c("Very useful" = "#e60049", 
              "Somewhat useful" = "#0bb4ff", 
              "I'm not familiar with this" = "#50e991", 
              "Have not used" = "#e6d800", 
              "Not at all useful"="#9b19f5")
  
  
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
    scale_fill_manual(values = colors) +
    geom_text(aes(label = paste(percent, "%")), vjust = -0.5, size = 3) +
    theme(axis.text.x=element_blank())+
    labs(title=paste("Usefulness of",label), y="Percent of respondants", x="", fill=paste("Degree of Usefulness"), caption=paste("n=", total, sep = ""))
  
  print(chart)
  ggsave(chart, file=paste("../output/",fileName))
  
}


usefulQuestion2<-function(data, columnName, label, fileName, casOnly=FALSE, gradOnly=FALSE){
  
  colors <- c("Very useful" = "#e60049", 
              "Somewhat useful" = "#0bb4ff", 
              "I'm not familiar with this" = "#50e991", 
              "I haven't used this" = "#e6d800", 
              "Not useful at all"="#9b19f5")
  
  
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
    scale_fill_manual(values = colors) +
    geom_text(aes(label = paste(percent, "%")), vjust = -0.5, size = 3) +
    theme(axis.text.x=element_blank())+
    labs(title=paste("Usefulness of",label), y="Percent of respondants", x="", fill=paste("Degree of Usefulness"), caption=paste("n=", total, sep = ""))
  
  print(chart)
  ggsave(chart, file=paste("../output/",fileName))
  
}

