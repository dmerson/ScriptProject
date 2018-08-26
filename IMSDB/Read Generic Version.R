library("devtools")
if (require("RegWhy")==FALSE){
  install_github("dmerson/RegWhyMultiLanguage/R/RegWhy")
}
library(RegWhy)
#install.packages("rvest")
library(rvest)
library(readr)
library(stringi)
library(stringr)
read_imsdb_file_into_lines <- function(path){
  script <- read_html(path) %>%
    html_node("pre") %>%
    html_text()
  script_lines <-read_lines(script)
  return (script_lines)
}
View(read_imsdb_file_into_lines("Screenplays/War for the Planet of the Apes Script at IMSDb..html"))

#scene_example="1   EXT./INT.   TAXI CAB - MORNING                                        1"
SCENE <-RegWhy.Statement(c(
  RegWhy.Where.StartOfString,
  RegWhy.Group.StartNonCapturing,
  RegWhy.CharacterType.Digit,
  RegWhy.Count.OneOrMore,
  RegWhy.Group.End,
  RegWhy.CharacterType.Space,
  RegWhy.Count.ZeroOrMore,
  RegWhy.Group.StartNonCapturing,
  RegWhy.Literal("INT"),
  RegWhy.OrMarker,
  RegWhy.Literal("EXT"),
  RegWhy.OrMarker,
  RegWhy.Literal("EXT./INT."),
  RegWhy.Group.End,
  RegWhy.Period,
  RegWhy.Count.Optional,
  RegWhy.CharacterType.BlankSpace,
  RegWhy.Count.ZeroOrMore,
  RegWhy.Group.StartCapturing,
  RegWhy.CharacterType.CharacterRange("A-Z0-9 -."),
  RegWhy.Count.ZeroOrMore,
  RegWhy.Group.End,
  RegWhy.CharacterType.Space,
  RegWhy.Count.ZeroOrMore,
  RegWhy.Group.StartNonCapturing,
  RegWhy.CharacterType.Digit,
  RegWhy.Count.ZeroOrMore,
  RegWhy.Group.End,
  RegWhy.Count.ZeroOrMore,
  RegWhy.Where.EndOfString
))

PAGE_END <-RegWhy.Literal("                   Deadpool    Final Shooting Script   11/16/15")
CONTINUED_PAGE <-RegWhy.Statement(c(
  RegWhy.Group.StartNonCapturing,
  RegWhy.CharacterType.Digit,
  RegWhy.Count.RangeOfTimes(1,3),
  RegWhy.Group.End,
  RegWhy.Count.Optional,
  RegWhy.CharacterType.Space,
  RegWhy.Count.ZeroOrMore,
  RegWhy.LeftParenthesis,
  RegWhy.Count.Optional,
  RegWhy.Literal("CONTINUED"),
  RegWhy.RightParenthesis,
  RegWhy.Count.Optional,
  RegWhy.Literal(":"),
  RegWhy.Count.Optional
))
#RegWhy.Do.Detect("                                                                  (CONTINUED)",CONTINUED_PAGE)
#RegWhy.Do.Detect("10   CONTINUED: (2)                                                         10",CONTINUED_PAGE)
PAGE_NUMBER <- RegWhy.Statement(c(
  RegWhy.Where.StartOfString,
  RegWhy.CharacterType.Space,
  RegWhy.Count.ZeroOrMore,
  RegWhy.CharacterType.Digit,
  RegWhy.Count.OneOrMore,
  RegWhy.Period,
  RegWhy.Where.EndOfString
))
#RegWhy.Do.Detect("                                                                      17.",PAGE_NUMBER)
#RegWhy.Do.Detect("10   INT.   FOYER, TOWNHOUSE - NIGHT - PAST                                  10",SCENE)
#RegWhy.Do.ExtractCapturedGroup("10   INT.   FOYER, TOWNHOUSE - NIGHT - PAST                                  10",SCENE, 1)
#run_script <- function(title, path) {
#  script_lines <-readLines(path)
  start_of_script=FALSE
 
 
 
  
  FADE_IN_SCRIPT =RegWhy.Statement(c(
    RegWhy.CharacterType.AnyCharacter,
    RegWhy.Count.ZeroOrMore,
    RegWhy.Literal("FADE IN"),
    RegWhy.CharacterType.AnyCharacter,
    RegWhy.Count.ZeroOrMore)
  )
  
  
  
  BLANK_LINE=""
  PRECEDING_SPACE <- RegWhy.Statement(c(
    RegWhy.Where.StartOfString,
    RegWhy.CharacterType.Whitespace,
    RegWhy.Count.OneOrMore
    
  ))
  CHARACTER <- RegWhy.Statement(c(
    RegWhy.Where.StartOfString,
    RegWhy.Group.StartCapturing,
    RegWhy.CharacterType.CharacterRange("A-Z0-9# .'"),
    RegWhy.Count.OneOrMore,
    RegWhy.Group.End,
    RegWhy.Group.StartNonCapturing,
    RegWhy.CharacterType.Whitespace,
    RegWhy.Count.ZeroOrMore,
    RegWhy.Group.End,
    RegWhy.Group.StartCapturing,
    RegWhy.Literal("(V.O.)"),
    RegWhy.Group.EndOptional,
    RegWhy.Group.StartNonCapturing,
    RegWhy.Literal("(O.S.)"),
    RegWhy.Group.EndOptional,
    RegWhy.Group.StartNonCapturing,
    RegWhy.Literal("(CONT'D)"),
    RegWhy.Group.EndOptional,
    RegWhy.Where.EndOfString
    
    
  ))
  PARENTHETICAL <- RegWhy.Statement(c(
    PRECEDING_SPACE,
    RegWhy.LeftParenthesis,
    RegWhy.CharacterType.AnyCharacter,
    RegWhy.Count.OneOrMore,
    RegWhy.RightParenthesis,
    RegWhy.Where.EndOfString
    
  ))
  CUTS <- RegWhy.Statement(c(
    PRECEDING_SPACE,
    RegWhy.Group.StartNonCapturing,
    RegWhy.Literal("HARD CUT TO"),
    RegWhy.OrMarker,
    RegWhy.Literal("CUT TO"),
    RegWhy.OrMarker,
    RegWhy.Literal("BACK TO"),
    RegWhy.Group.End,
    RegWhy.Literal(":"),
    RegWhy.Count.Optional,
    RegWhy.CharacterType.AnyCharacter,
    RegWhy.Count.OneOrMore,
    RegWhy.Where.EndOfString
    
  ))
  #RegWhy.Do.Detect("    CUT TO: test",CUTS)
  CAMERADIRECTION <- RegWhy.Statement(c(
    RegWhy.Where.StartOfString,
    RegWhy.Literal("CROSSFADE TO"),
    RegWhy.CharacterType.AnyCharacter,
    RegWhy.Count.OneOrMore,
    RegWhy.Where.EndOfString
    
  ))
  TRANSISTION <- RegWhy.Statement(c(
    RegWhy.Where.StartOfString,
    RegWhy.Literal("TRANSISTION "),
    RegWhy.CharacterType.AnyCharacter,
    RegWhy.Count.OneOrMore,
    RegWhy.OrMarker,
    RegWhy.Literal("MATCH "),
    RegWhy.CharacterType.AnyCharacter,
    RegWhy.Count.OneOrMore,
    RegWhy.Where.EndOfString
    
  ))
  
create_script_df <-function(){  
  last_line_type = ""
  current_title = "War for the Planet of the Apes"
  current_scene = ""
  current_stage_direction = ""
  current_character = ""
  current_character_plus_direction = ""
  current_dialogue = ""
  script_df <- data.frame(
    title = character(),
    scene = character(),
    stage_direction = character(),
    charactername = character(),
    characterPlusDirection = character(),
    dialogue = character(),
    stringsAsFactors = FALSE
  )
  last_line_of_title_card="FADE IN..."
  len_of_script = length(script_lines)
  start_of_script=FALSE
  for (i in 1:len_of_script) {
    #print("Current line")
   # print(script_lines[i])
     
    # if (RegWhy.Do.Detect(script_lines[i],"FADE IN")) {
    #   start_of_script = TRUE
    #   last_line_type = "START"
    #   #print("Start!")
    # }    #if script is started
    start_of_script=TRUE
    if ((start_of_script == TRUE) &&
        (RegWhy.Do.Detect(script_lines[i], "FADE IN") == FALSE)) {
      current_line <- script_lines[i]
      
      #look for Blank line
      if (current_line == BLANK_LINE  ) {
        last_line_type = "BLANK_LINE"
        
      }
      else{
        if (
            RegWhy.Do.Detect(current_line, CUTS) ||
            RegWhy.Do.Detect(current_line, CAMERADIRECTION) ||
            RegWhy.Do.Detect(current_line, TRANSISTION) ||
            RegWhy.Do.Detect(current_line,CONTINUED_PAGE)  ||
            RegWhy.Do.Detect(current_line,PAGE_NUMBER)
                             ) {
          last_line_type = "BLANK_LINE"
          #print("JUNK")
          #print(current_line)
        }
        else{
          #look for SCENE
          if (RegWhy.Do.Detect(current_line, SCENE)) {
            #print("SCENE")
            #print(current_line)
            last_line_type = "SCENE"
            if (current_scene != current_line && current_scene != "") {
              current_frame <-
                data.frame(
                  current_title,
                  current_scene,
                  current_stage_direction,
                  current_character,
                  current_character_plus_direction,
                  current_dialogue,
                  stringsAsFactors = FALSE
                )
              
              script_df <- rbind(script_df, current_frame)
              current_stage_direction = ""
              current_character = ""
              current_dialogue = ""
              current_character_plus_direction = ""
            }
            current_scene <- current_line
          }
          else{
            #Look for Character
            if (RegWhy.Do.Detect(current_line, CHARACTER)) {
              #print("CHARACTER")
              #print(RegWhy.Do.ExtractCapturedGroup(current_line,CHARACTER,1))
              #print(current_line)
              last_line_type = "CHARACTER"
              if (current_character != RegWhy.Do.ExtractCapturedGroup(current_line, CHARACTER, 1)) {
                current_frame <-
                  data.frame(
                    current_title,
                    current_scene,
                    current_stage_direction,
                    current_character,
                    current_character_plus_direction,
                    current_dialogue,
                    stringsAsFactors = FALSE
                  )
                 
                script_df <- rbind(script_df, current_frame)
                current_stage_direction = ""
                current_character = ""
                current_dialogue = ""
                current_character_plus_direction = ""
              }
              
              
              current_character = RegWhy.Do.ExtractCapturedGroup(current_line, CHARACTER, 1)
              if (!is.na(RegWhy.Do.ExtractCapturedGroup(current_line, CHARACTER, 2))){
                current_character_plus_direction = RegWhy.Do.ExtractCapturedGroup(current_line, CHARACTER, 2)
              }
              else{
                current_character_plus_direction=""
              }
              
              
            }
            else{
              if (RegWhy.Do.Detect(current_line, PARENTHETICAL) == TRUE) {
                last_line_type = "PARENTHETICAL"
              }
              else{
                if (last_line_type == "CHARACTER" ||
                    last_line_type == "PARENTHETICAL") {
                  last_line_type = "DIALOGUE"
                  dialogue=RegWhy.Do.ReplaceFirst(current_line, PRECEDING_SPACE,"")
                  current_dialogue = dialogue
                  
                }
                else
                {
                  if (last_line_type == "DIALOGUE") {
                    last_line_type = "DIALOGUE"
                    dialogue=RegWhy.Do.ReplaceFirst(current_line, PRECEDING_SPACE,"")
                    current_dialogue = paste(current_dialogue," ", dialogue)
                    current_dialogue =RegWhy.Do.ReplaceAll(current_dialogue,RegWhy.Literal("  ")," ")
                    
                    
                  }
                  else # We arrive at it must be Stage Direction
                  {
                    
                    if (last_line_type=="DIALOGUE"){
                      last_line_type="STAGE_DIRECTION"
                      current_frame <-
                        data.frame(
                          current_title,
                          current_scene,
                          current_stage_direction,
                          current_character,
                          current_character_plus_direction,
                          current_dialogue
                        )
                      
                      script_df <- rbind(script_df, current_frame)
                      fixed_line=RegWhy.Do.ReplaceFirst(current_line, PRECEDING_SPACE,"")
                      current_stage_direction <- fixed_line
                    }
                    
                    if (last_line_type == "BLANK_LINE") {
                      last_line_type = "STAGE_DIRECTION"
                      fixed_line=RegWhy.Do.ReplaceFirst(current_line, PRECEDING_SPACE,"")
                      current_stage_direction <- fixed_line
                      current_stage_direction <- RegWhy.Do.ReplaceAll(current_stage_direction,RegWhy.Literal("  ")," ")
                      
                    }
                    if (last_line_type == "STAGE_DIRECTION") {
                      last_line_type = "STAGE_DIRECTION"
                      fixed_line=RegWhy.Do.ReplaceFirst(current_line, PRECEDING_SPACE,"")
                      current_stage_direction <- paste(current_stage_direction,fixed_line)
                      current_stage_direction <- RegWhy.Do.ReplaceAll(current_stage_direction,RegWhy.Literal("  ")," ")
                      
                    }
                  }
                }
              }
              
              #print(current_line)
            }
          }
        }
        
      }
      
      
    }
    script_df$current_title <- as.character(script_df$current_title)
    script_df$current_scene <- as.character(script_df$current_scene)
    script_df$current_stage_direction <- as.character(script_df$current_stage_direction)
    script_df$current_character <- as.character(script_df$current_character)
    script_df$current_character_plus_direction <- as.character(script_df$current_character_plus_direction)
    script_df$current_dialogue <- as.character(script_df$current_dialogue)
  }
  return (script_df)
}


