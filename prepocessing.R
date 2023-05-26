read.pcibex <- function(filepath, auto.colnames=TRUE, fun.col=function(col,cols){cols[cols==col]<-paste(col,"Ibex",sep=".");return(cols)}) {
  n.cols <- max(count.fields(filepath,sep=",",quote=NULL),na.rm=TRUE)
  if (auto.colnames){
    cols <- c()
    con <- file(filepath, "r")
    while ( TRUE ) {
      line <- readLines(con, n = 1, warn=FALSE)
      if ( length(line) == 0) {
        break
      }
      m <- regmatches(line,regexec("^# (\\d+)\\. (.+)\\.$",line))[[1]]
      if (length(m) == 3) {
        index <- as.numeric(m[2])
        value <- m[3]
        if (is.function(fun.col)){
          cols <- fun.col(value,cols)
        }
        cols[index] <- value
        if (index == n.cols){
          break
        }
      }
    }
    close(con)
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=cols))
  }
  else{
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=seq(1:n.cols)))
  }
}

data = read.pcibex("results.csv")


library(tidyverse)
library(stringi)


data = tibble(data)

#filter useless rows
data %>% 
  filter(!id == "пробный") %>% 
  filter(!Parameter %in% c("_Trial_", "_Header_")) -> filtered_data
  
#filter unnecessary columns
filtered_data %>% 
  select(-c(Results.reception.time, MD5.hash.of.participant.s.IP.address, 
            Controller.name, Inner.element.number, Latin.Square.Group, 
            PennElementName, Comments, Newline.)) %>% 
  mutate(age = str_extract(age, "[0-9]+"))  -> filtered_data


#check for accuracy in answers
filtered_data %>% 
  filter(PennElementType == "Selector") %>% 
  mutate(answer_value = ifelse(Value == "answer1", answer_1, answer_2)) %>% 
  mutate(answer_correct = answer_value == correct_answer) %>% 
  group_by(id) %>% 
  summarise(prop_correct = sum(answer_correct/n())) -> check_data


#get data with accuracy
left_join(filtered_data, check_data, by = "id") -> filtered_data

#filter results with accuracy lower than 0.8 and delete unnecessary columns
filtered_data %>% 
  filter(prop_correct > 0.8) %>% 
  filter(PennElementType != "Selector") %>% 
  select(
    -c(prop_correct, Sentence..or.sentence.MD5., correct_answer, answer_1,
       answer_2, EventTime, Parameter, PennElementType, Label, 
       Order.number.of.item)) -> filtered_data


#filter non-target rows
filtered_data %>% 
  filter(Value == word) -> filtered_data

filtered_data %>% view()

write_csv(filtered_data, "processed_results.csv")
