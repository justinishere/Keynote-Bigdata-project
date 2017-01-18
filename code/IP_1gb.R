rm(list=ls())   #clear all data


# install.packages("data.table")
library(data.table)
main_dir <- "~/Desktop/Big data/R" # setting data folder

# GET PATHS -----------------
assignments <- list.files(file.path(main_dir), pattern = "^slotinfo_IP_1gb.*csv$", full.names = T)

# IMPORT DATA ----------------------------------------

## assignment data
dt_assign <- fread(assignments, sep = ',')
setnames(dt_assign, names(dt_assign), c('slotid', 'slotname','page_num','page_name','sequence','protocol','domain', 'url','total_request','ip_address')) # rename data
len0<- nrow(dt_assign)



## import known first party and 3rd party list
first_list <- read.csv("~/Desktop/Big data/R/first_party_domain.csv", head=F)
third_list <- read.csv("~/Desktop/Big data/R/3rd_party_list.csv", head=F)


#  Number of occurence for each uni_domain---------------------


uni_domain<-unique(dt_assign$domain, incomparables = FALSE);  # unique domains
uni_ID<-unique(dt_assign$slotid, incomparables = FALSE);


count<-NULL;
m1<-NULL
for (i in 1:length(uni_domain)-1) {
	
m1<-dt_assign[dt_assign$domain==uni_domain[i], ]; 
count[i]<-nrow(unique(m1,by='slotid'));      # number of occurance for domain
count<-c(count,count[i]);
i=i+1;

}

sum<-data.frame(count,uni_domain);
write.csv(sum, file="~/Desktop/full_3rd.csv"); # Excel 1

#sub<-setdiff(sub$uni_domain,image);  sub<-setdiff(sub$uni_domain,metric)  # exclude image and metrix


# coding methods-------------------------Indicate your code here---------------------------
## Seperate id patterns: 000-xxx, xxx:000,xxxx000-xxx
## Or we can subtract the string: 15 characters in the middle



# Remove first party and third party--------------------------------------------------



is.integer0 <- function(x)            #use this function for the condition in 'if'
{
  is.integer(x) && length(x) == 0L
}





## check if domain is in the 1st party list
test<-NULL
dt_rm_1st <- dt_assign
#first_rm<- NULL

for(j in 1:nrow(first_list)){
    	  test<-grep(first_list$V1[j], dt_rm_1st$domain,value=F)  # grep is checking if string pattern is similar
             
           # first_rm<-rbind(first_rm,dt_assign[k])
            if (is.integer0(test)== F) {
             dt_rm_1st<-dt_rm_1st[-test, ]  # remove all the rows in 'test'
              j=j+1}
            else j=j+1
          
                  }     
       
len1<-nrow(dt_rm_1st)
len1-len0
len1_percent <-len1/len0



## Remove 3rd party 
test<-NULL
dt_rm_3rd <- dt_rm_1st

     
 for(i in 1:nrow(third_list)){
    	  test<-grep(third_list$V1[i], dt_rm_3rd$domain,value=F)  # grep is checking if string pattern is similar
             
            #third_rm<-rbind(first_rm,dt_assign[k])
            if (is.integer0(test)== F) {
             dt_rm_3rd<-dt_rm_3rd[-test, ]
              i=i+1}
            else i=i+1
          
                  }     
       
len2<-nrow(dt_rm_3rd)
len2-len1
len2_percent <-len2/len0





# remove cdn from the original dt_assign-----------------------------------------
## create cdn list

cdn_list<-c("Akamai","Amazon","Bitgravity","Cachefly","CDN77","CDNetworks","CDNify",
            "ChinaCache","ChinaNetCenter","EdgeCast","Fastly","Highwinds","Incapsula","Internap",
            "KeyCDN","Leaseweb","Level 3","Limelight","MaxCDN","NGENIX","SkyparkCDN","Telefónica","XCDN","CND")


test<-NULL
dt_rm_cdn <- dt_assign
 
for(i in 1:length(cdn_list)){
    	  test<-grep(tolower(cdn_list[i]), dt_rm_cdn$domain,value=F)  # grep is checking if string pattern is similar
             
            #third_rm<-rbind(first_rm,dt_assign[k])
            if (is.integer0(test)== F) {
             dt_rm_cdn<-dt_rm_cdn[-test, ]
              i=i+1}
            else i=i+1
          
                  }     
       
len3<-nrow(dt_rm_cdn)
len3-len0
len3_percent <-len3/len0
    
         
         
#temp_out = cbind(ip_assign, data.frame(tag)) # stack the temp 
#temp_out_first<-temp_out[temp_out$tag==1, ]         
#write.csv(temp_out, file.path(main_dir, "IP_tagged.csv"))

write.csv(dt_rm_3rd, file="~/Desktop/1st_3rd_cdn removed.csv"); 








