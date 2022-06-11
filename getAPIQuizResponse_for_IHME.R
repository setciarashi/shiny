# This R script runs as a daily cron job to get students' quiz response data from Canvas API

library(odbc)
library(DBI)
library(jsonlite)
library(dplyr)
library(zoo)
library(tidyr)

#### Set up a log file to save messages and errors ####
log.path<-paste0('/path/to/log/getAPIQuizResponse_',format(Sys.time(),"%Y%m%d%H%M%S"),".log")
zz <- file(log.path, open="w")
sink(zz, type=c("message","output"))

#### Database: Get A List of Quizzes to Show on Dashboard ####
# canvas API
info<-read.csv("/opt/ola/itools-cron/itools_info.csv",header=F,sep=',')
canvas_token_ll<-Sys.getenv("canvas_token")
canvas_api<-'https://institution.instructure.com/api/v1/courses/'

# connect to data warehouse to get a quizlist shown on the dashboard with assignment_id for API calls
con<-dbConnect(odbc::odbc(),Driver="Oracle 21 ODBC driver", DNS = "OracleODBC-21")
print("Connected to database.")

q1<-"SELECT COURSE_DESCRIPTION,
       COURSE_ID,
       QUIZ_ID,
       QUIZ_ID_SHORT,
       ASSIGNMENT_ID,
       QUIZ_NAME,
       QUIZ_CREATED
FROM RDS_LEARNING_ANALYTICS.OLA_QUIZ_SUBMISSION_MONITOR
WHERE API_ACCESS = 'Y' AND DASHBOARD_ACTIVE='Y'
ORDER BY COURSE_DESCRIPTION, QUIZ_NAME;"
quizlist<-dbGetQuery(con, q1)

if(nrow(quizlist)==0){
  print("Found 0 quiz. Script is stopped.")
  quit(save="no")
}else{
  print(paste("Getting response data of",nrow(quizlist),"quiz(zes) from",length(unique(quizlist$COURSE_ID)),"courses."))
  writeLines(unique(quizlist$COURSE_DESCRIPTION))
}

#### API: Get Quiz Response Data ####
# API Call 1
api_assignment<-paste(canvas_api,quizlist$COURSE_ID,'/assignments/',quizlist$ASSIGNMENT_ID,'/submissions?include[]=submission_history&per_page=999&',canvas_token_ll,sep = "")
sub_data_raw<-data.frame()
#sub_data_raw stores quiz submissions in the raw format. Quiz responses are nested in the submission_history column
for (i in 1:length(api_assignment)){
  new_sub_history <- fromJSON(api_assignment[i])
  if(nrow(new_sub_history)>0){
    new_sub_history$COURSE_ID<-quizlist$COURSE_ID[i]
    sub_data_raw<-rbind(sub_data_raw,new_sub_history)
  }
}
print(paste("Found",nrow(sub_data_raw),"raw quiz submission records."))

#sub_data obtains the quiz response data from submission_history column
sub_data<-data.frame()
for (i in 1:nrow(sub_data_raw)){
  if(!is.na(sub_data_raw[i,c("body")]) &&
     !is.na(sub_data_raw[i,c("submission_history")]) &&
     !is.null(sub_data_raw[["submission_history"]][[i]][["submission_data"]][[1]])){#equals to workflow_state !="unsubmitted"
    new_sub_data <- data.frame(sub_data_raw[["submission_history"]][[i]][["submission_data"]][[1]]) %>%
    mutate(across(everything(),zoo::na.locf,na.rm = FALSE)) %>% filter(row_number()==n())  #combine two rows using na.locf and save the filled row (last row), only one questio_id is kept. Others are dropped.
    new_sub_data$COURSE_ID<-sub_data_raw$COURSE_ID[i]
    new_sub_data$assignment_id_short<-sub_data_raw$assignment_id[i]
    new_sub_data$id<-sub_data_raw$id[i]
    new_sub_data$user_id<-sub_data_raw$user_id[i]
    new_sub_data$QUIZ_ID_SHORT<-sub("(?i).*quiz:.*?(\\d+).*", "\\1", sub_data_raw$body[i]) #get the short quiz id, (?i): ignore case,  Any length of characters up to "quiz:", "\\1": return the capture group
    new_sub_data$submitted_at <- sub_data_raw$submitted_at[i]
    new_sub_data$workflow_state <- sub_data_raw$workflow_state[i]
    new_sub_data$late <- sub_data_raw$late[i]
    new_sub_data$preview_url <- sub_data_raw$preview_url[i]
    new_sub_data <- lapply(new_sub_data,as.character)
    sub_data<- bind_rows(sub_data, new_sub_data)
  }
}

sub_data2<-sub_data %>% select (COURSE_ID, QUIZ_ID_SHORT, assignment_id_short,id,user_id,submitted_at,workflow_state,
                                late,preview_url,everything(),-text) %>%
  mutate(QUIZ_ID_SHORT=as.numeric(QUIZ_ID_SHORT)) %>% #re-order columns
  arrange(COURSE_ID, QUIZ_ID_SHORT,user_id) #sort rows
print(paste("Retrieved",nrow(sub_data2),"quiz submissions"))

#### Database: Get Quiz Answer Text ####
# sub_data2 now only has numeric answer IDs. Needs to be replaced by the actual answer text.

# Get Quiz Answer ID from data warehouse
q2<-paste0("SELECT DISTINCT oqm.QUIZ_ID_SHORT, oqm.QQ_ANSWER_CANVAS_ID, to_char(oqm.ANSWER_TEXT) AS TEXT
FROM RDS_LEARNING_ANALYTICS.OLA_QUIZ_METAINFO oqm
WHERE oqm.QUIZ_ID_SHORT  IN ('",paste(quizlist$QUIZ_ID_SHORT,collapse = "','"),"');")

# currently only use QUIZ_ID_SHORT, QQ_ANSWER_CANVAS_ID, TEXT, but potentials are using of BLANK_ID to reduce text replacement time
quiz_answer_lookup<-dbGetQuery(con, q2)
print(paste("Retrieved",nrow(quiz_answer_lookup),"pairs of answer id and answer text."))

#replace answer id in sub_data2 by answer text in quiz_answer_lookup, link is quiz_id
vars<-grep("answer_for",names(sub_data2))
cnt_missing<-0
for(i in 1:nrow(sub_data2)){
  for(j in vars){
    if(!is.na(sub_data2[i,j])){
      current_row<- quiz_answer_lookup %>% filter(QQ_ANSWER_CANVAS_ID==sub_data2[i,j]&
                                               QUIZ_ID_SHORT==sub_data2$QUIZ_ID_SHORT[i])
        if(nrow(answr_txt)!=0){
          sub_data2[i,j]<-current_row$TEXT
        }else{cnt_missing<-cnt_missing+1}
    }
  }
}
print(paste("Replaced the answer IDs by answer text with",cnt_missing,"ID(s) not replaced."))

#### API: Add student names ####

# API Call 2
user_id_list<-unique(sub_data2[,c("COURSE_ID","user_id")])
user_info_list<-data.frame()
for (i in 1:nrow(user_id_list)){
  api_user<-paste(canvas_api,user_id_list$COURSE_ID[i],'/users/',user_id_list$user_id[i],'?per_page=999&',canvas_token_ll,sep = "")
  jsn<-fromJSON(api_user)
  for (j in 1:length(jsn) ){
    if(is.null(jsn[[j]])){jsn[[j]]<-NA} #data.frame doens't work with NULL in json
  }
  new_user_info <- data.frame(jsn)
  new_user_info <- new_user_info %>% select(id, name,short_name,sortable_name) %>% # depends on the token permission, the available columns can be different. integration_id is not available as course designer
    rename(user_id=id, user_name=name,user_short_name=short_name,user_sortable_name=sortable_name)
  user_info_list <- rbind(user_info_list,new_user_info)
}
print(paste("Retrieved information of",nrow(user_info_list),"students."))

# Merge names into sub_data3
sub_data3<-merge(sub_data2,user_info_list,by="user_id", all.x = T)


#### Final Cleaning ####

# Add other course information ####
sub_data3<-sub_data3 %>% select(-COURSE_ID) %>% left_join(quizlist,by="QUIZ_ID_SHORT",keep=F)%>%
  select (COURSE_ID,COURSE_DESCRIPTION, QUIZ_ID, QUIZ_ID_SHORT,
          ASSIGNMENT_ID, assignment_id_short, QUIZ_NAME,QUIZ_CREATED, everything()) %>%
  arrange(COURSE_ID, QUIZ_NAME,user_id)
print(paste("Combined dataset has",nrow(sub_data3),"rows."))

#### Write to Database ####
if(nrow(sub_data3)>0){
  if(DBI::dbExistsTable(con,"TEST_QUIZ_RESPONSE")){
    DBI::dbRemoveTable(con,"TEST_QUIZ_RESPONSE")
  }
  DBI::dbWriteTable(con, "TEST_QUIZ_RESPONSE", sub_data3)
  final_row_cnt<-dbGetQuery(con, "SELECT COUNT(*) AS ROW_CNT FROM TEST_QUIZ_RESPONSE")
  print(paste("Wrote", final_row_cnt$ROW_CNT, "rows to database."))
}

dbDisconnect(con)

sink(type=c("message","output"))
close(zz)
