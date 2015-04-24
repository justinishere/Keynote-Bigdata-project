rm(list=ls())       #clear all data

#read data
ip_tagged<-read.csv("C:/Users/ahbbxx92/Desktop/R/IP_tagged.csv",
                header=TRUE,
                stringsAsFactors= FALSE,
                numerals="no.loss")

#import cdn list
cdn_list<-c("Akamai","Amazon","Bitgravity","Cachefly","CDN77","CDNetworks","CDNify",
            "ChinaCache","ChinaNetCenter","EdgeCast","Fastly","Highwinds","Incapsula","Internap",
            "KeyCDN","Leaseweb","Level 3","Limelight","MaxCDN","NGENIX","SkyparkCDN","Telefónica","XCDN")

cdn_tag<-rep(0, times=length(ip_tagged$registrant))   # default cdn's tag as 0

is.integer0 <- function(x)            #use this function for the condition in 'if'
{
  is.integer(x) && length(x) == 0L
}


# check if registrant [i] is in the cdn list, if yes, tag=3
for (i in 1:length(ip_tagged$registrant)) {
  if (ip_tagged$tag[i]==0)
  {for(j in 1:length(cdn_list) )
    {test<-grep(cdn_list[j], ip_tagged$registrant[i], value=F)  # grep is checking if string pattern is similar
    if( is.integer0(test) == F)  {   
      cdn_tag[i]<-3    
      j=j+1             }     
    else j=j+1
     }
  i=i+1
  }
  else i=i+1
}

temp_out = cbind(ip_tagged, data.frame(cdn_tag)) # stack the temp 
temp_out_cdn<-temp_out[temp_out$cdn_tag==3, ]    # export the cdn list     
write.csv(temp_out, file="CDN_tagged.csv")


# Background Information
# first party client list
first_list<-c("google",   # Assuming google is one of our clients
              "honda", "ncl", "ford", "quil", "pbimgs", "weimgs", "subaru", "combank", "pkimgs", "skype", "lincoln", "bjs", "williams","commsec", "burberry", "discover", "accessories", "21st", "dinersclubgv","vodafone")



# Identify the origin of cdn
# If the cdn is working with first parties, cdn_origin_tag = 1
# If the cdn is working with third parties, cdn_origin_tag = 2
# else (AKA: Not cdn)  cdn_origin_tag = 0

cdn_origin_tag<-rep(0, times=length(temp_out$registrant))   # default cdn's origin tag as 0


for (i in 1:length(temp_out$registrant)) {
  if (temp_out$cdn_tag[i] == 3)
    {cdn_origin_tag[i]<-2
     for(j in 1:length(first_list) )
      {test<-grep(first_list[j], temp_out$domain[i], value=F)  # grep is checking if string pattern is similar
       if( is.integer0(test) == F)  {   
         cdn_origin_tag[i]<-1    
         j=j+1             }     
       else j=j+1
       }
     i=i+1
     }
  else i=i+1
}


temp_out_origin = cbind(temp_out, data.frame(cdn_origin_tag)) # stack the temp  
write.csv(temp_out_origin, file="CDN_origin_tagged.csv")




