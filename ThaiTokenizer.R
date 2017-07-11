rm(list=ls())
Sys.setlocale(locale = "thai")
Sys.setlocale("LC_TIME", "thai")
Sys.setlocale("LC_ALL", "thai")

#########
#Import libraries
#########

library("data.table")
library("dplyr")
library("tidyr")
library("stringr")
library("sqldf")
library("keras")

#starts here:
path <- 'Input your repertory'
source(paste0(path,'/my_function.R'))
output_model <- paste0(path,'/model/')
#load model
model <- load_model_hdf5(filepath=paste0(output_model,'rnn'), custom_objects = NULL)


#Apply one function 
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

#Here is the magic 

whatdoyouwanttocut <- 'คุณชื่ออะไร'
my_cut <- ThaiTokenizer(whatdoyouwanttocut)
my_cut %>% View() 