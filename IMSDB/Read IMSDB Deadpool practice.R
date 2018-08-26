library("devtools")
if (require("RegWhy")){
  install_github("dmerson/RegWhyMultiLanguage/R/RegWhy")
}
library(RegWhy)
#install.packages("rvest")
library(rvest)
library(readr)
library(stringi)
library(stringr)

rawHTML <- paste(readLines("Screenplays/Deadpool Script at IMSDb..html"), collapse="\n")
deadpool_script <- read_html("Screenplays/Deadpool Script at IMSDb..html") %>%
              html_node("pre") %>%
              html_text()
deadpool_script_lines <-read_lines(deadpool_script)

scene_example="1   EXT./INT.   TAXI CAB - MORNING                                        1"
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
  RegWhy.CharacterType.CharacterRange("A-Z -."),
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
  RegWhy.CharacterType.Space,
  RegWhy.Count.ZeroOrMore,
  RegWhy.Literal("(CONTINUED)")
))
PAGE_NUMBER <- RegWhy.Statement(c(
  RegWhy.Where.StartOfString,
  RegWhy.CharacterType.Space,
  RegWhy.Count.ZeroOrMore,
  RegWhy.CharacterType.Digit,
  RegWhy.Count.OneOrMore,
  RegWhy.Period,
  RegWhy.Where.EndOfString
))
RegWhy.Do.Detect("                                                                      17.",PAGE_NUMBER)
#run_script <- function(title, path) {
#  script_lines <-readLines(path)
  start_of_script=FALSE
  HAS_A_TAB =RegWhy.Statement(c(
    RegWhy.Where.StartOfString,
    RegWhy.Tab,
    RegWhy.Count.OneOrMore,
    RegWhy.CharacterType.AlphaNumeric,
    RegWhy.Count.OneOrMore,
    RegWhy.Where.EndOfString
  ))
  SIX_SPACES =RegWhy.Statement(c(
    RegWhy.Where.StartOfString,
    RegWhy.CharacterType.Space,
    RegWhy.Count.ExactNumber(6),
    RegWhy.CharacterType.AnyCharacter,
    RegWhy.Count.OneOrMore,
    RegWhy.Where.EndOfString
    
  ))
 
  
  FADE_IN_SCRIPT =RegWhy.Statement(c(
    RegWhy.CharacterType.AnyCharacter,
    RegWhy.Count.ZeroOrMore,
    RegWhy.Literal("FADE IN"),
    RegWhy.CharacterType.AnyCharacter,
    RegWhy.Count.ZeroOrMore)
  )
  
  SCENE =RegWhy.Statement(c(
    RegWhy.Group.StartNonCapturing,
    RegWhy.Literal("INT"),
    RegWhy.OrMarker,
    RegWhy.Literal("EXT"),
    RegWhy.Group.End,
    RegWhy.Period,
    RegWhy.Count.ZeroOrMore,
    RegWhy.CharacterType.Space,
    RegWhy.Count.OneOrMore,
    RegWhy.Group.StartCapturing,
    RegWhy.CharacterType.AlphaNumeric,
    RegWhy.Count.OneOrMore,
    RegWhy.Group.End
    
  ))
  
  BLANK_LINE=""
  
  CHARACTER <- RegWhy.Statement(c(
    RegWhy.Where.StartOfString,
    RegWhy.Group.StartCapturing,
    RegWhy.CharacterType.CharacterRange("A-Z0-9 .'"),
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
    RegWhy.Where.StartOfString,
    RegWhy.LeftParenthesis,
    RegWhy.CharacterType.AnyCharacter,
    RegWhy.Count.OneOrMore,
    RegWhy.RightParenthesis,
    RegWhy.Where.EndOfString
    
  ))
  CUTS <- RegWhy.Statement(c(
    RegWhy.Where.StartOfString,
    RegWhy.Literal("CUT TO"),
    RegWhy.CharacterType.AnyCharacter,
    RegWhy.Count.OneOrMore,
    RegWhy.Where.EndOfString
    
  ))
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
  last_line_type = ""
  current_title = title
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
  #script_df <-c("","","","","")
  len_of_script = length(script_lines)
  for (i in 1:len_of_script) {
    #print(script_lines[i])
    if (RegWhy.Do.Detect(script_lines[i], FADE_IN_SCRIPT) == TRUE) {
      start_of_script = TRUE
      last_line_type = "START"
      #print("Start!")
    }
    #if script is started
    if ((start_of_script == TRUE) &&
        (RegWhy.Do.Detect(script_lines[i], FADE_IN_SCRIPT) == FALSE)) {
      current_line <- script_lines[i]
      
      #look for Blank line
      if (current_line == BLANK_LINE) {
        last_line_type = "BLANK_LINE"
        
      }
      else{
        if (RegWhy.Do.Detect(current_line, CUTS) ||
            RegWhy.Do.Detect(current_line, CAMERADIRECTION) ||
            RegWhy.Do.Detect(current_line, TRANSISTION)) {
          last_line_type = "STAGE_DIRECTION"
          #print("Camera")
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
                  current_dialogue
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
                    current_dialogue
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
                  
                  current_dialogue = current_line
                  
                }
                else
                {
                  if (last_line_type == "DIALOGUE") {
                    last_line_type = "DIALOGUE"
                    current_dialogue = paste(current_dialogue, current_line)
                    
                    
                  }
                  else
                  {
                    if (last_line_type == "BLANK_LINE") {
                      last_line_type = "STAGE_DIRECTION"
                      
                      current_stage_direction <- current_line
                      
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
  }
#  return (script_df)
#}

#View(run_script("Big Fish","Screenplays/Big-Fish.fountain.txt"))
