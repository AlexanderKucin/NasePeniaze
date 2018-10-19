library(readr)
library(XML)

# downloading XML files from CRZ and storing them as a list
getCRZ<- function(last_date, download_method = 'auto', verbose = T){
  
  dates<- as.numeric(Sys.Date()-1-as.Date(last_date))
  
  crz<- list()
  
  for(i in 1:dates)
  {
    date<- as.character(as.Date(last_date)+i)
    
    URL<- paste0("http://www.crz.gov.sk/export/",date,".zip")
    
    dl_file<- try(download.file(URL,"temp", quiet = T, method = download_method))
    
    if(class(dl_file) == "try-error")
    {
      if(verbose == T) cat('\n ', date, '\n')
      
      system("rm temp")
      
      next 
    }
    
    # unzip downloaded file in the current directory and remove it
    system("unzip temp && rm temp")
    
    # read unzipped XML file
    p<- read_file(paste0(date,".xml"))
    
    # leave only printable characters
    p<- gsub("[^[:print:]]", "", p)
    
    # parse XML
    data<- xmlInternalTreeParse(p)
    
    # remove XML file
    system(paste0("rm ",date,".xml"))
    
    crz[[i]]<- data
    
    if(verbose == T) cat('\n ', i,'/',dates, '\n')
  }

  return(crz)
}

#parsing XML files from CRZ list to DF
parseCRZ<- function(crz_list, verbose = T)
{
  data<- data.frame()
  
  for(n in 1:length(crz_list))
  {
    if(length(crz_list[[n]])==0)
    {
      next
    }else{
      xml<- xmlToList(crz_list[[n]])  
    }
    
    if(length(xml)==0)
    {
      next
    }
    
    df<- as.data.frame(matrix(nrow=length(xml), ncol=length(names(xml[[1]]))))
    names(df)<- names(xml[[1]])
    
    for(i in 1:length(xml))
    {
      for(j in 1:(length(names(xml[[1]]))-1))
      {
        if(length(xml[[i]][[j]])==0)
        {
          df[i,j]<- NA
        }else{
          df[i,j]<- xml[[i]][[j]]  
        }
        
      }
      
      df[i,length(names(xml[[1]]))]<- length(xml[[i]]$prilohy)
    }
    data<- rbind(data, df)
    
    if(verbose == T) cat('\n ',n,'\n')
  }
  
  return(data)  
}

