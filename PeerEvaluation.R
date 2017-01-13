## Peer Evaluation Output Tool ( in progress )
#
# The aim of this piese of script is to build a GUI application that can take raw data 
# from Peer Evaluation survey of students in BIOL1121, and output the score of the evaluation
# of each student. 
#
# Each studetn evaluat his teammates (2 or 3) and is evaluated by his team members via 
# an online survey. In the survey, they give the names of their group members and then assign
# a score to them.
#
#.	Raw data of survey can be downloaded from Canvas. It is a CSV file that contains information
#   of surveys of all students that participate the evaluation. 
#
#.	The name input from students are not exactly accurate. Some are missing family name, some have
#   misspelling and some provide nicky name etc. Text match methed need to be used to match those
#   input names to true names on the class roster. Levenshtein distance is used to find the best match.
#
#.	The peer score is calculated by averaging scores got from every team member who participates the
#   evaluation.
#


package_needed <- c("dplyr","RecordLinkage","gWidgets","gWidgetsRGtk2")  # packages used in this script
pack_install_idx <- which(package_needed %in% rownames(installed.packages()) == F)
# find the package or packages needed but not installed

if(length(pack_install_idx) > 0){  
  install.packages(package_needed[pack_install_idx]) 
  # Install the pakage(s) needed but not installed
}

library(dplyr)
library(RecordLinkage)
library(gWidgets)
library(gWidgetsRGtk2)
options ( guiToolkit="RGtk2" )

# raw_data <- read.csv(
#   "Peer Evaluation 3 - Inv D Survey Student Analysis Report.csv",
#   header = T, sep = ",", na.strings = c("","NA","N/A","na","Na"))  #import quize file

data_ognz <- function(rawdata){
  # function of data reorganization
  
  emptyCol = seq(12,30,2)  #column idx of empty columns
  whole_data <- rawdata[,-c(2,3,5,6,7,8,9,10,31,32,33, emptyCol)]  
  #delete unnecessary info from the dataset 
  
  col_teamate <- paste(c("Teamate","Score","Reason"), rep(1:3, each = 3))
  #create column names for the part of the dataset
  colnames(whole_data) <- c(colnames(whole_data)[1:2], col_teamate, "Self")
  #define column names for the whole dataset
  whole_data[] <- lapply(whole_data, as.character) #convert factor variable to character
  
  whole_data[whole_data$name == "Micaiah Bradford", "section"] <- "BIOL-1121-002"
  whole_data[whole_data$name == "Cheyenne Roche", "section"] <- "BIOL-1121-905"
  whole_data[whole_data$name == "Tylar Williams", "section"] <- "BIOL-1121-011"
  whole_data[whole_data$name == "Alexandra Sabados", "section"] <- "BIOL-1121-904"
  # correct wrong info of students
  
  Name_evaluator <- whole_data$name  #names of evaluators
  Name_evaluator_rep <- rep( Name_evaluator, 3)  #3 peers per student
  
  team_name_raw <- as.character(c(whole_data$`Teamate 1`,whole_data$`Teamate 2`,
                                  whole_data$`Teamate 3`))
  #All names of evaluated students from quiz
  team_score_raw <- as.numeric(c(whole_data$`Score 1`, whole_data$`Score 2`, 
                                 whole_data$`Score 3`))
  #All scores assigned to evaluated students from quiz (numeric)
  team_section_raw <- as.factor(rep(whole_data$section,3))
  #All section info of evaluated students from quiz
  name_score_raw <- data.frame(Name_evaluator_rep, team_name_raw, team_score_raw, 
                               team_section_raw)
  #combine names with scores and sections
  name_score_raw <- name_score_raw[ name_score_raw$team_name_raw != "", ]
  #remove rows with empty teamate names
  idx_na_score <- which(is.na(name_score_raw$team_score_raw)) #index of NAs in score
  
  if ( length(idx_na_score)>0 )  name_score_raw <- name_score_raw[-idx_na_score,]
  #remove rows with NA socres 
  
  idx_na_name <- which(is.na(name_score_raw$team_name_raw)) #index of nas in name
  
  if ( length(idx_na_name)>0 )  name_score_raw <- name_score_raw[-idx_na_name,]
  #remove rows with na socres 
  
  name_input_total <- as.character(name_score_raw$team_name_raw)  
  # character input of name input
  # name_string_len <- as.numeric(lapply(name_input_total, nchar ))  #length of name charater
  name_string_com <- lapply(name_input_total, grepl,pattern = " ")
  # check if name input is complete ( contains both Last and First names ) by checking space
  idx_incom_name <- which(name_string_com == T) #idx of complete name input
  name_score_raw <- name_score_raw[idx_incom_name, ] #remove incomplete name input
  
  pattern_to_check <- c("&", " and ", "," ,"Do not", "Cannot", "Can not", "can't","Don't","no ")
  pattern_reco <- c()
  for ( i in 1:length(pattern_to_check) ){
    
    #loop that removes observations with inappropriate patterns 
    pattern_check <- lapply( as.character(name_score_raw$team_name_raw), grepl, 
                             pattern = pattern_to_check[i], ignore.case = TRUE)
    # check if name input contain any of the pattern listed above
    pattern_checked <- which(pattern_check == T)  #idx of incorrect name input
    pattern_reco <- c(pattern_checked, pattern_checked)   
    #append idx of each pattern into one vector
    idx_pattern_reco <- unique(pattern_reco) #remove duplicate elements
    if ( length(idx_pattern_reco)>0 ) name_score_raw <- name_score_raw[-idx_pattern_reco,]
    #remove names with incorrect pattern
    
  }
  
  whole_data$section <- as.factor(whole_data$section)
  #convert section back to factor
  
  return(name_score_raw)
}


# roster <- read.csv( "roster.csv", header = T, sep = ",")
# #import roster data as a standard of names
# roster <- roster[-1,c(1,5)]  # select name and section and delete the first row of possible points
# roster <- roster[ roster$Student != "Test Student", ]  # delete test student
# 
# # correct wrong info of students
# roster[roster$Student == "Micaiah Bradford", "Section"] <- "BIOL-1121-002"
# roster[roster$Student == "Cheyenne Roche", "Section"] <- "BIOL-1121-905"
# roster[roster$Student == "Tylar Williams", "Section"] <- "BIOL-1121-011"
# roster[roster$Student == "Alexandra Sabados", "Section"] <- "BIOL-1121-904"
# roster$Section <- factor(roster$Section)
# roster$Student <- as.character(roster$Student)


ClosestMatch = function(string, stringVector){  
  #define a function to find best match based on Levenshtein distance
  
  distance = levenshteinSim(string, stringVector);
  best_match <- stringVector[distance == max(distance)]  #best match in the vector
  output <- best_match[1]
  return(output)
  
}

#num_sections <- nlevels(name_score_raw$team_section_raw)
#sections_level <- levels(name_score_raw$team_section_raw)


score_compute <- function(name_score_raw,roster,save_dir,output_filename){
  #browser()
  # function to calculate peer score
  num_sections <- nlevels(roster$Section)   #total number of section categories
  sections_level <- levels(roster$Section)   #all possible levels of section
  
  data_by_section <- list()  #create empty list to hold different subsets
  name_standard_by_section <- list()  #create empty list to hold different name subset,
  #(of different section)
  name_matched_by_section <- list()  #create empty list to hold different name subset,
  #(of different section)
  num_score_by_sec <- list()  #create empty list to hold num of scores of each name
  idx_abnorm <- list()   #create empty list to hold idx of abnormal score number (>3)
  
  
  for (i in 1:num_sections ){
    # loop through all sections
    data_by_section[[i]] <- name_score_raw[ name_score_raw$team_section_raw == sections_level[i], ]
    name_standard_by_section[[i]] <- roster[ roster$Section == sections_level[i], "Student"]
    ######change the standard to names from rostar with all students, 
    ######quiz data don't have info of students who did not take PeerEvaluation
    
    name_matched <- character( length = dim(data_by_section[[i]])[1]    )
    # empty character ector to hold matched manes
    
    for (j in 1:dim(data_by_section[[i]])[1]){
      # loop through all observations in each section
      temp_data <- data_by_section[[i]]
      name_matched[j] <- ClosestMatch( as.character(temp_data$team_name_raw[j] ), 
                                       name_standard_by_section[[i]]  )  # matched names
    }
    name_matched_by_section[[i]] <- name_matched 
    data_by_section[[i]] <- data.frame(data_by_section[[i]], 
                                       name_match = name_matched_by_section[[i]])
    # add a new variable with info of matched names into the data set
    # can be used to compare between original input and matched result
    
    num_score_by_sec[[i]] <- data_by_section[[i]] %>% 
      group_by(name_match) %>%
      summarise(num_score = length(name_match))  
    # group matched data by names and check num of inputs of each evaluated students
    
    idx_abnorm[[i]] <- which(num_score_by_sec[[i]]$num_score > 3)
    # mark evaluated students with more than 3 evaluation inputs as abnormal results
    More_than_4_evaluator <- rep(0,dim(num_score_by_sec[[i]])[1] )
    if ( length(idx_abnorm[[i]]) > 0 )  More_than_4_evaluator[idx_abnorm[[i]]] <-1
    
    section_info <- rep(sections_level[i],dim(num_score_by_sec[[i]])[1] ) 
    num_score_by_sec[[i]] <- cbind(num_score_by_sec[[i]],section_info,More_than_4_evaluator)
    # add a new variable with info of if each name is matched more than 4 times 
  }
  
  output_data_by_section <- list( ) 
  
  for ( i in 1:num_sections ){
    # loop through all sections
    num_names_by_section <- nlevels(data_by_section[[i]]$name_match) # levels of names
    section <- rep(sections_level[i], num_names_by_section) # section info of each observation
    name_output <- character(length = num_names_by_section ) # vector to hold matched names
    score_match <- numeric(length = num_names_by_section)  # vector to hold computed scores
    score_match_round <- numeric(length = num_names_by_section) # vector to hold rounded scores
    #outlier_idx <- c( )
    
    for ( j in 1:num_names_by_section ){
      # loop through all evaluated names of each section
      name_output[j] <- levels(data_by_section[[i]]$name_match)[j] # different names from levels
      score_match[j] <- mean(data_by_section[[i]][data_by_section[[i]]$name_match == name_output[j],
                                                  "team_score_raw"])
      #peer scores computed as mean of input scores from all evaluators
      
      score_match_round[j] <- round( score_match[j], digit = 2 ) # round score to 2 decimal places
      #if ( score_match[j] < 7 | score_match[j] > 12 ) outlier_idx <- c(outlier_idx, j)  
    }   
    
    output_data_by_section[[i]] <- data.frame(name_output, section, score_match_round)
    #if ( length(outlier_idx) >0 ) outlier_by_section[[i]] <- output_data_by_section[[i]][outlier_idx, ]
  }
  
  caculated_score_wholeClass <- do.call(rbind, output_data_by_section)
  colnames( caculated_score_wholeClass ) <- c("Name", "Section", "PeerScore")
  
  result_check_wholeClass <- do.call(rbind, num_score_by_sec)
  colnames( result_check_wholeClass ) <- c("Name", "Number of evaluator", "Section","More than 4 evaluator")
  
  result_filename <- paste(save_dir,"//", output_filename,"score",".csv",sep = "")
  check_filename <- paste(save_dir,"//",output_filename,"check",".csv",sep = "")
  
  write.csv(caculated_score_wholeClass, file = result_filename)
  write.csv(result_check_wholeClass, file = check_filename)
  
  list(score = caculated_score_wholeClass, check = result_check_wholeClass )
  
}

mainFunc <- function(rawdata,roster,save_dir,output_filename){
  ognzed_data <- data_ognz(rawdata) # call data_ognz to reorganize survey data
  roster <- roster[-1,c(1,5)]  # select name and section and delete the first row of possible points
  roster <- roster[ roster$Student != "Test Student", ]  # delete test student
  # correct wrong info of students
  roster[roster$Student == "Micaiah Bradford", "Section"] <- "BIOL-1121-002"
  roster[roster$Student == "Cheyenne Roche", "Section"] <- "BIOL-1121-905"
  roster[roster$Student == "Tylar Williams", "Section"] <- "BIOL-1121-011"
  roster[roster$Student == "Alexandra Sabados", "Section"] <- "BIOL-1121-904"
  roster$Section <- factor(roster$Section)
  roster$Student <- as.character(roster$Student)
  
  data_score_raw <- data_ognz(Survey_Data)
  output <- score_compute(ognzed_data, roster, save_dir,output_filename)
  
}



############Build GUI################### 
w = gwindow("Simple IRR GUI",width = 300 , height = 300, visible = F) # creat a window
gg<- ggroup(container = w,spacing = 20, horizontal = F)  # widget container
g1 <- ggroup(container = gg,spacing = 10, horizontal = T) # secondary widget container

lbl_data_name <- glabel(
  "Survey Raw Data, renamed as: ",
  container = g1
)
addSpring(g1)
txt_data_frame_name1 <- gedit("Survey_Data", cont = g1)


g2 <- ggroup(container = gg,spacing = 10, horizontal = T)
# secondary widget container

lbl_data_name2 <- glabel(
  "Roster Data, renamed as: ",
  container = g2
)
addSpring(g2)
txt_data_frame_name2 <- gedit("Roster", cont = g2)
status_bar <- gstatusbar("", container = w)

gp1 <-ggroup(container = gg,spacing = 20, horizontal = T)
# secondary widget container

btn_upload1 <- gbutton(   #push button to upload raw data file (only accept csv)
  text      = "Survey Raw Data",
  container = gp1,
  handler   = function(h, ...)
  {
    gfile(
      text    = "Upload data from peer evaluation survey",
      type    = "open",
      action = "read.csv",
      handler = function(h, ...)
      {
        tryCatch(
          {
            data_frame_name <- make.names(svalue(txt_data_frame_name1))
            the_data <- do.call(h$action, list(h$file, colClasses = "factor"))
            assign(data_frame_name, the_data, envir = globalenv())
            svalue(status_bar) <- "Data from Survey uploaded"
          },
          error = function(e) svalue(status_bar) <- "Cannot upload data"
        )
      },
      
      filter = list(
        "Comma delimited" = list(patterns = c("*.csv","*.xls")),
        "All files" = list(patterns = c("*"))
      )
    )
  }
)

addSpring(gp1)

btn_upload2 <- gbutton(   #push button to upload raw data files (only accept csv)
  text      = "Roster Data",
  container = gp1,
  handler   = function(h, ...)
  {
    gfile(
      text    = "Upload data from Roster",
      type    = "open",
      action = "read.csv",
      handler = function(h, ...)
      {
        tryCatch(
          {
            data_frame_name <- make.names(svalue(txt_data_frame_name2))
            the_data <- do.call(h$action, list(h$file, colClasses = "factor"))
            assign(data_frame_name, the_data, envir = globalenv())
            svalue(status_bar) <- "Data from Roster uploaded"
          },
          error = function(e) svalue(status_bar) <- "Cannot upload data"
        )
      },
      
      filter = list(
        "Comma delimited" = list(patterns = c("*.csv","*.xls")),
        "All files" = list(patterns = c("*"))
      )
    )
  }
)


g3 <- ggroup(container = gg,spacing = 10, horizontal = T) # secondary widget container

lbl_data_name <- glabel(
  "Name result as: ",
  container = g3
)
addSpring(g3)
txt_data_frame_name3 <- gedit("Peer3D", cont = g3, anchor = c ( 5 ,0))


## label and file selection widget
group <- ggroup ( container = gg , horizontal = FALSE )
save_dir <- gfilebrowse ( text = "Select a directory to save ..." ,
                           quote = FALSE ,type = "selectdir" , cont = group )



gp2 <-ggroup(container = gg,spacing = 10, horizontal = F)


Calculate <- gbutton(   #push button to start calculataion
  text      = "Compute Peer Evaluation Score",
  container = gp2,
  expand = T,
  handler = function(h,...){
    tryCatch(
      {
        data_frame_name <- make.names(svalue(txt_data_frame_name3))
        output<- mainFunc(Survey_Data,Roster,svalue(save_dir),data_frame_name)
        svalue(status_bar) <- "Calculation succeeded"
      },
      error = function(e) svalue(status_bar) <- "Cannot calculate"
    )
    
    assign("myOutput", output, envir = globalenv())
    
  }
  
)

visible(w) <- T

