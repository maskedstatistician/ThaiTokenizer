# Sys.setlocale(locale = "thai")
# Sys.setlocale("LC_TIME", "thai")
# 

############
# read the file  from interbest 2009
###########
read_data <- function(path) {
  #get the file name
  temp = list.files(path = path ,
                    pattern="*")
  print(paste0('there is ',length(temp), ' files'))
  
  #loop through all the file name 
  all_files <- list()
  for(file in temp){
    print(file)
    words <- fread(input=paste0(path,file),sep="\n",header=FALSE,encoding='UTF-8')
    words <- words %>% 
      mutate(V1 = strsplit(as.character(V1), "\\|")) %>% 
      unnest(V1)
    words <- words %>% filter(V1 !=' ') 
    words <- words %>% filter(!grepl("<NE>",V1) ) %>% 
      filter(!grepl("<AB>",V1) ) %>% 
      filter(!grepl("<POEM>",V1) )  
    words <- words %>% mutate(filename=file)
    all_files[[file]] <- words
  }
  #concatenate the list into one dataframe 
  words <- do.call(rbind,all_files)
  rownames(words) <- NULL
  #return the dataframe
  print("finished")
  return(words)
}




############
# split a string into character
###########


split_to_char <- function(words) {
  lst <- strsplit(words$V1, "")
  words <- data.frame(char = unlist(lst), idx = sequence(lengths(lst)))
  return(words)
}



############
# create a dataframe with that affect each character to a class 
###########
char_to_class <- function() {
  CHAR_TYPE = list(c = 'กขฃคฆงจชซญฎฏฐฑฒณดตถทธนบปพฟภมยรลวศษสฬอ',
                   n = 'ฅฉผฌหฮ',
                   v = 'ะาำิีืึุู',# า ะ ำ ิ ี ึ ื ั ู ุ
                   w = 'เแโใไ',
                   t = '่้๊๋',
                   s = '์ๆฯ.', 
                   d = '0123456789๑๒๓๔๕๖๗๘๙', 
                   q = "‘",
                   q = '"',
                   q = "’", 
                   q = "'",
                   p = ' ', 
                   s_e = 'abcdefghijklmnopqrstuvwxyz',
                   b_e = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  )
  class_df <- NULL
  for( i in 1:length(CHAR_TYPE)){ 
    class_df <- rbind(class_df , 
                      class_df <- cbind(
                        data.frame(
                          type = names(CHAR_TYPE)[[i]],
                          char = substring(CHAR_TYPE[[i]],1:nchar(CHAR_TYPE[[i]]),1:nchar(CHAR_TYPE[[i]]))
                        ) 
                      )
    )
  }
  return(class_df)
}


########
# Apply the class of character to the main dataframe 
#######
df_char_to_class <- function(a,b) {
  df <-sqldf(sprintf("select a.char,
                     coalesce(b.type,'o') as type ,
                     a.target
                     from %s a 
                     left join %s b on a.char = b.char",a,b))
  return(df)
}


###############
# Apply a padding 
#################

padding_fn <- function(df,n_gram) { 
  padding <- (n_gram-1)/2  
  X_var <- df %>% select(char,type)
  padding_df <- data.frame(char = NA, type=NA)
  temp <- NULL 
  for(i in 1:5){
    temp <- rbind(temp,padding_df)
  }
  padding_df <- temp 
  X_var_padding <- rbind(padding_df , X_var,padding_df)
  X_var_padding <- X_var_padding %>% mutate(char=as.numeric(char),
                                            type = as.numeric(type))
  
  return(X_var_padding)
}


###############
# Apply N gram  
#################
ngram_fn <- function(X_var_padding,padding){
  char_ <- shift(X_var_padding$char, n=1:padding, fill='', type="lag")
  char_ <- data.frame(do.call(cbind,char_))
  names(char_)[grep("^X", names(char_))] <- 
    paste("charlag", names(char_)[grep("^X", names(char_))], sep="")
  type_ <- shift(X_var_padding$type, n=1:padding, fill='', type="lag")
  type_ <- data.frame(do.call(cbind,type_))
  names(type_)[grep("^X", names(type_))] <- 
    paste("typelag", names(type_)[grep("^X", names(type_))], sep="")
  char <- shift(X_var_padding$char, n=1:padding, fill='', type="lead")
  char <- data.frame(do.call(cbind,char))
  names(char)[grep("^X", names(char))] <- 
    paste("charlead", names(char)[grep("^X", names(char))], sep="")
  type <- shift(X_var_padding$type, n=1:padding, fill='', type="lead")
  type <- data.frame(do.call(cbind,type))
  names(type)[grep("^X", names(type))] <- 
    paste("typelead", names(type)[grep("^X", names(type))], sep="")
  X_var_ngram <- cbind(X_var_padding,char_,type_,char,type)
  X_var_ngram <- X_var_ngram %>% filter(char!='')
}

############
# Apply the missing colums 
###########

missing_colum_fn <- function(X_var_dummy,all_column_names){
  nms <- all_column_names[,1]   # Vector of columns you want in this data.frame
  Missing <- setdiff(nms, names(X_var_dummy))  # Find names of missing columns
  if( length(Missing) == 0) {
    print('no column missing')
    X_var_dummy <- X_var_dummy[nms]
  }else{
    X_var_dummy[Missing] <- 0                    # Add them, filled with '0's
    X_var_dummy <- X_var_dummy[nms]              # Put columns in desired order
  }
  return(X_var_dummy)
}





##############
# APPLY the pat cut 
##############

ThaiTokenizer <- function(whatdoyouwanttocut){
  training <- data.frame(V1=whatdoyouwanttocut) %>% mutate(V1=as.character(V1))
  char_df_training <- split_to_char(training)
  char_df_training <- char_df_training %>% mutate(target=ifelse(idx==1,1,0))
  main_df <- char_df_training
  ####create the classification of character based on the pdf 
  ###: https://pdfs.semanticscholar.org/3c8a/b284ca6e315be0def549b7d64c28af678861.pdf
  class_df <- char_to_class()
  class_df$number <- 1
  class_df$number <- cumsum(class_df$number) 
  unique_type <- data.frame(type = unique(class_df$type),
                            number = 1 ) 
  unique_type$number <- cumsum(unique_type$number)
  ###Apply the classification to our main dataframe
  char_df_training <-sqldf("select a.char,
                           coalesce(b.number,143) as char_nb , 
                           coalesce(b.type,'o') as type ,
                           a.target
                           from char_df_training a 
                           left join class_df b on a.char = b.char")
  
  char_df_training <- sqldf("select 
                            a.char_nb as char,
                            coalesce(b.number,12) as type
                            from 
                            char_df_training a
                            left join unique_type b
                            on a.type = b.type ")
  char_df_training <- char_df_training %>% mutate(char = as.numeric(char))
  #APPLY PADDING depending on the ngram 
  n_gram <- 11 
  X_padding_training <- padding_fn(char_df_training,n_gram)
  #Apply n-gram
  padding <- (n_gram-1)/2  
  x_train <- ngram_fn(X_padding_training,padding)
  #transform into matrix
  x_train[is.na(x_train)]<- 0 
  x_train <- as.matrix(x_train)
  #prediction
  my_prediction <- data.frame(prediction = predict(model,x_train))
  my_prediction <- my_prediction %>% mutate(prediction_round=ifelse(prediction>0.5,1,0))
  final_df <- data.frame(cbind(as.character(main_df[,1]),my_prediction$prediction_round)) %>%
    mutate(X1=as.character(X1),X2=as.character(X2))
  final_df <- final_df %>%
    group_by(cumsum(X2))%>%
    summarise(words = paste(X1, collapse="")) %>%
    select(words ) %>% data.frame()
  return(final_df)
}
