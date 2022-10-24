setwd("C:/Users/kevin/OneDrive/Desktop/Masters/MSC2011H/A3")

#read from keys.txt and store words in a library 
library = scan("keys.txt", character(), sep = "\n")
#to add to the library, add new lines to keys.txt

#display strings will be stored in a vector that uses
#game state to modulate its selected index, must use
#cat to get newline characters to print properly
displays = c("-------\n|     |\n|\n|\n|\n|\n|\n|\n",
             "-------\n|     |\n|     O\n|\n|\n|\n|\n|\n",
             "-------\n|     |\n|     O\n|     |\n|\n|\n|\n|\n",
             "-------\n|     |\n|     O\n|    /|\n|\n|\n|\n|\n",
             "-------\n|     |\n|     O\n|    /|\\\n|\n|\n|\n|\n",
             "-------\n|     |\n|     O\n|    /|\\\n|    /\n|\n|\n|\n",
             "-------\n|     |\n|     O\n|    /|\\\n|    / \\\n|\n|\n| GAME OVER\n")

#set outer engine to on and wrap the entire game
engine = TRUE
while(engine){
  #pick a random word from the library, uppercase it in advance
  answer_string = toupper(library[sample(1:length(library), 1)])
  #create a display string that represents the 
  #solution with spaces and blanks
  current_string = ""
  character_count = 0
  #temp variables for holding and comparing string variables
  current_string_split = vector()
  answer_split = strsplit(answer_string, "")[[1]]
  for (char in answer_split){
    if (char == " "){
      current_string = paste(current_string, " ")
      current_string_split = append(current_string_split, " ")
    } else {
      current_string = paste(current_string, "_")
      character_count = character_count + 1
      current_string_split = append(current_string_split, "_")
    }
  }
  
  #set game state (and victory flag)
  #since the body has 1 head, 1 torso+pelvis, 
  #2 arms, & 2 legs the game state will max out 
  #at 7 and then GAME-OVER if the engine loop is 
  #broken before the state hits 7 the player has 
  #won the game
  state = 1
  #create holder for user input and input history
  user_input = ""
  input_history = vector()
  
  #above assets should be enough to run this game now

  #display instructions
  cat("------------------Welcome to Hangman------------------\nInstructions: A hidden phrase/word has been selected.\nTo guess one character at a time, input one character.\nTo guess the entire puzzle, input a string of multiple \ncharacters. Cases of letters don't matter (A==a).\nEach time you guess wrong, a body part will be added.\nIf you end up fully drawn into the gallows, GAME OVER.")
  #inform user of non-blank characters in the puzzle
  cat(sprintf("\n\nNumber of non-blank characters: %i\n", character_count))
  #display starting game-state
  cat(displays[state])
  cat(current_string)
  
  #initialize the inner game engine and run the game
  while (state < 7){
    #get input, uppercase it in advance
    user_input = toupper(readline(prompt = "Enter a character ... or dare to guess the phrase: "))
    #assess input
    if (grepl("^\\s*$", user_input)){
      #reprompt next iteration for pure whitespace input
      #do nothing effectively
    } else if(user_input %in% input_history) {
      #input has already been attempted, reprompt
      cat("\nYou already tried that buddy\n")
    }
    else if (nchar(user_input) == 1){
      input_history = append(input_history, user_input)
      #single character input
      if (grepl(user_input, answer_string, fixed = TRUE)){
        cat("\nYup, the solution contains that character\n")
        #update current_string_split and remaining char counter
        current_string = ""
        for (index in 1:nchar(answer_string)){
          if (user_input == answer_split[index]){
            current_string_split[index] = user_input
            character_count = character_count - 1
          }
        }
        #reconstruct current_string
        for (char in current_string_split){
          current_string = paste(current_string, char)
        }      
      } else {
        #let user know if they guessed wrong
        cat("\nSorry buddy, there are none of those in the answer")
        state = state + 1
      }
    } else {
      input_history = append(input_history, user_input)
      #user is guessing the entire puzzle
      if (user_input == answer_string){
        break
      } else {
        #let user know if they guessed wrong
        cat("\nSorry buddy, that isn't the answer\n")
        state = state + 1
      }
    }
    
    #display current state
    cat(sprintf("\nNumber of non-blank characters remaining: %i\n", character_count))
    cat(displays[state])
    cat(current_string)
    #break for if all letters are filled
    if (character_count == 0){
      break
    }
  }#inner wrap for game engine
  
  #determine game outcome
  if (state < 7){
    cat("\n-----------------------VICTORY!-----------------------")
  } else {
    #game over state has been reached, give answer
    cat(paste("\nAnswer was: ", answer_string))
  }
  
  #ask user if they want to continue
  engine = ("Y" == toupper(readline(prompt = "Play again?(Y/N) ")))
}#outer wrap of main engine

