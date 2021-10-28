library(tidyverse)
library('rvest')
library('xml2')
library("base")
library("data.table")

allurls<- c("NCT00528879","NCT01032629","NCT01177813")

Names <- paste0("https://clinicaltrials.gov/ct2/show/results/", allurls)
len <- 100
empty_list <- vector(mode = "list", length = len)


#read the html page
for (i in seq_along(Names)){
  empty_list[[i]] <- read_html(Names[[i]])
}


#extract all tables present

res <- vector('list', length(empty_list))

for(i in seq_along(empty_list)){
  res[[i]] <- html_table(empty_list[[i]],fill = TRUE)
}

## extract only the relevent table from each of the list of lists of tables
new <- vector('list', length(res))
for(i in seq_along(res)){
  new[[i]] <- res[[i]][length(res[[i]])-2]
  
}


#jettison the nulls

new2 <- Filter(Negate(is.null), new)


#filter by partial match
b<- vector('list', length(10))

for(i in 1:3){
  b[[i]] <-  dplyr::filter(new2[[i]][[1]],str_detect(X1, "Urinary")| str_detect(X1, "Arm/Group Title")| str_detect(X1, "Total")| str_detect(X1, "Serious Adverse Events")| str_detect(X1, "Other"))
  
}


##add the NCTS as columns
for(i in 1:3){
  b[[i]]<- as.data.frame(b[[i]]) %>% dplyr::mutate(newcol = allurls[i])
}


*
  
  ### remove weird symbols 
  for(i in 1:3){
    b[[i]]$X1<- str_replace_all(b[[i]]$X1, "[[:punct:]]", " ")  
  }

## remove digits (only 1 atm)
for(i in 1:3){
  b[[i]]$X1<- gsub('[[:digit:]]+', '', b[[i]]$X1)
}


##get relative position of 'serious adverse effects' line
onset_ind <- vector()
for(i in 1:3){
  onset_ind[i] <- which(b[[i]]$X1 == b[[i]] %>% filter(str_detect(X1,"Serious Adverse Events")))
}


##get relative position of 'Other' line

offset_ind <- vector()
for(i in 1:3){
  offset_ind[i] <- grep("Other", b[[i]]$X1)
}

## add 'serious' marker to column X1 (had to use different format than for non-serious because original was displacing one cell!)

for(i in 1:3){
  b[[i]]$X1[onset_ind[i]:(offset_ind[i]-1)]<- paste0("Serious", b[[i]]$X1[onset_ind[i]:(offset_ind[i]-1)])
}


## add 'non-serious'marker to column X1

for(i in 1:3){
  b[[i]]$X1[offset_ind[i]:length(b[[i]]$X1)] <- lapply(b[[i]]$X1[offset_ind[i]:length(b[[i]]$X1)], function(x) paste("Non-Serious", x, sep="_"))
}


##remove the two extra columns
for(i in c(1,2,3)){
  b[[i]] <- b[[i]][-c(onset_ind[i],offset_ind[i]),]
}

# keep the first column 

names <- vector('list', length(10))
for(i in 1:3){
  names[[i]]<- b[[i]][,1]
}

# Transpose everything other than the first column

try <- vector('list', length(10))
for(i in 1:3){
  try[[i]] <- as.data.frame(as.matrix(t(b[[i]][,-1]))) 
}



# Assign first column as the column names of the transposed dataframe

for(i in 1:3){
  colnames(try[[i]]) <- names[[i]]
}

for(i in 1:3){
  try[[i]] <- try[[i]][-nrow(try[[i]]),]
}



##add the NCTS as back as columns (had to add and remove them earlier just becasue that code supported later code )
for(i in 1:3){
  try[[i]]<- as.data.frame(try[[i]]) %>% dplyr::mutate(newcol = allurls[i])
}




###make all headings start with capital letters
for(i in 1:3){
  names(try[[i]]) <- toupper(names(try[[i]]))
}
###bind dataframes

a <- dplyr::bind_rows(try[[1]],try[[2]],try[[3]])

##automatic version of binding 
this <- dplyr::bind_rows(try[[1]],try[[2]])

try[[4]] <- try[[3]]

for(i in 3:4){
  this<- dplyr::bind_rows(this,try[[i]])
}

#remove na rows 
a<- a[!is.na(a$`ARM GROUP TITLE`),]


###arbitrary change
###second arbitrary change to test                                                       
