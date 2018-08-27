source("IMSDB/Read Generic Version.R")
#script_lines <- read_imsdb_file_into_lines("Screenplays/Beauty and the Beast Script at IMSDb..html")
script_lines <- read_imsdb_file_into_lines("Screenplays/Guardians of the Galaxy Vol 2 Script at IMSDb..html")
#script_df <-create_script_df(script_lines,"Beauty and The Beast")
script_df <-create_script_df(script_lines,"Gaurdians of the Galaxy")
View(sqldf("select current_title,current_character, current_dialogue 
           from script_df 
           where length(current_character) > 1 and length(current_dialogue) > 1")
)
