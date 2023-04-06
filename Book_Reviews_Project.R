#loading packages
library(tidyverse)

#Books Reviews Dataquest Project

#We will be acting as a data analyst for a company that sells books for learning programming. The company has produced multiple books, and each has received many reviews. The company wants us to check out the sales data and see if we can extract any useful information from it

table<-read.csv("book_reviews.csv",header=T) #loading data

# How big is the dataset?
glimpse(table) #Dataset has 2000 rows and 4 columns. 

# What are the column names?
for(i in 1:ncol(table)){
  
  print(colnames(table[i]))
  
}

#Columns IDs: book, review, state, price. Book - name of the sold publication; Review - book quality ("Excellent", "Great", "Good", "Fair", "Poor"); State - where book was sold; Price - ranges from 15.99 till 50.00

# What are the column types?
for(i in 1:ncol(table)){
  
  print(class(table[[i]]))
  
}

#Book, review and state: character
#Price: numeric

#What are the unique values are present in each of the columns? 
#1st solution
for(i in 1:ncol(table)){
  
  print(unique(table[[i]]))
  
}

#I tried this previous solution first, but as you can see, all results get mixed and very confusing. That is why it is reasonable to print the the i index and also a "" separator, so that you can see things clearer

#2nd solution
for(i in 1:ncol(table)){
  
  print("Unique values in the column")
  print(i)
  print(unique(table[[i]]))
  print("")
  
}

#3rd solution - you may replace the i index by the name of the column itself!
for(i in 1:ncol(table)){
  
  print("Unique values in the column")
  print(colnames((table[i])))
  print(unique(table[[i]]))
  print("")
  
}

#Examine the data and get an understanding of which columns have data missing

for(i in 1:ncol(table)){
    
  print(names(which(colSums(is.na(table[i]))>0)))
       
}

#Create a new copy of the dataset that removes all of the rows that have missing data

#classical way
valid_lines<-which(!is.na(table$review))
new_dataset<-table[valid_lines,]

unique(new_dataset$review) #checking whether removing NAs really worked

#pipelines
new_dataset <- table %>%
  filter(!is.na(table$review))

#How much data was removed by taking out the rows with missing data? 
2000-length(valid_lines) #206 lines removed, or app. 10% of the original data

#What are all the states that are present in the dataset? 

#"TX","NY","FL","Texas","California","Florida","CA","New York". Now, replace "California" by "CA"

#classical way
new_dataset$state[new_dataset$state[]=="California"]<-"CA"
new_dataset$state[new_dataset$state[]=="Texas"]<-"TX"
new_dataset$state[new_dataset$state[]=="Florida"]<-"FL"
new_dataset$state[new_dataset$state[]=="New York"]<-"NY"
unique(new_dataset$state) #checking whether replacement worked

#pipeline
new_dataset<-new_dataset %>%
  mutate(
    state=case_when(
      state=="California"~"CA",
      state=="Texas"~"TX",
      state=="Florida"~"FL",
      state=="New York"~"NY",
      TRUE~state
      )
  )
unique(new_dataset$state) #checking whether replacement worked

# Now our goal is to evaluate the ratings of each of the textbooks, but there's not much we can do with text versions of the review scores. It would be better if we were to convert the reviews into a numerical form

#classical way
new_dataset$review_num<-new_dataset$review #creates new column

#loop for replacing values
for(i in 1:nrow(new_dataset)){
  
  if(new_dataset$review_num[i]=="Poor"){
    
    new_dataset$review_num[i]<-"1"
    
  }else if(new_dataset$review_num[i]=="Fair"){
    
    new_dataset$review_num[i]<-"2"
    
  }else if(new_dataset$review_num[i]=="Good"){
    
    new_dataset$review_num[i]<-"3"
    
  }else if(new_dataset$review_num[i]=="Great"){
    
    new_dataset$review_num[i]<-"4"
    
  }else{
    
    new_dataset$review_num[i]<-"5"
    
  }
  
}

new_dataset$is_high_review<-new_dataset$review_num #creates new column

#loop for replacing values
for(i in 1:nrow(new_dataset)){
  
  if(new_dataset$is_high_review[i]>= 4){
    
    new_dataset$is_high_review[i]<-"TRUE"
    
  }else{
    
    new_dataset$is_high_review[i]<-"FALSE"
    
  }
  
}

#pipelines
new_dataset<-new_dataset %>%
  mutate(
    review_num=case_when(
      review=="Poor"~"1",
      review=="Fair"~"2",
      review=="Good"~"3",
      review=="Great"~"4",
      review=="Excellent"~"5"
    ),
    is_high_review = if_else(review_num >= 4, TRUE, FALSE)
  )

#Now our main goal is to figure out what book is the most profitable. How will we judge what the "most profitable" book is though? Our dataset represents customer purchases. One way to define "most profitable" might be to just choose the book that's purchased the most. Another way to define it would be to see how much money each book generates overall. We will consider that "most profitable" is a combination between the number of sold books and its price (n books X price)

dataset_1<-new_dataset %>%
  distinct(book, price)

dataset_2<-new_dataset %>%  
  group_by(book)%>%
  summarize("purchased"=n())

final_dataset<-left_join(dataset_1,dataset_2,by="book")

final_dataset%>%
  mutate(
    
    profitable=price*purchased
    
  )%>%
  arrange(-profitable)

#Secrets Of R For Advanced Students is the most profitable of all books!