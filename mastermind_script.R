# A code written by Sam Siljee based on the game Mastermind
# created 2nd December 2020
# last modified 9th February 2023

# Things to be improved still:
# 1. Scoring logic is not yet entirely compatible with some combinations of colour repitition
# 2. Give an option to change the number of holes

# load appropriate packages
library(tidyverse)

# Create the function
mastermind <- function(n = 6, x = FALSE){
  # define the colours
  colours <- c("yellow", "red", "blue", "purple", "green", "orange", "black", "white", "brown", "grey", "pink")

  # create a named colours vector for graphical board
  namedcolours <- setNames(colours, colours)
 
  # create playing board
  round <- 0
  board <- data.frame(first = NA,
                      second = NA,
                      third = NA,
                      fourth = NA,
                      black = NA,
                      white = NA,
                      round = round,
                      clue_1_black_size = NA,
                      clue_2_black_size = NA,
                      clue_3_black_size = NA,
                      clue_4_black_size = NA,
                      clue_1_white_size = NA,
                      clue_2_white_size = NA,
                      clue_3_white_size = NA,
                      clue_4_white_size = NA)
  ans = NA
  # set the solution
  solution <- paste(sample(colours[1:n], 4, replace = x))
  cat("Welcome to this game of Mastermind, enter quit to exit the game or show to reveal the answer.\n
  The number of colours can be chosen with a numerical value for the first argument when starting mastermind, maximum of eleven.\n
      Repetition of colours can be chosen as an option by entering TRUE as the second argument. default is FALSE.\n")
  
  # show the colour options
  cat("These are the available colours:", colours[1:n], "\b.\nNote that the spelling has to be exact.\n")
 
  # start while loop for incorrect answer
  while(!identical(ans, solution)){
      
      # ask for an answer, with break if quit entered
      ans <- scan(what = character(), quiet = TRUE, nlines = 1)
      
      # provision to quit if quit entered
      if(ans[1] %in% "quit" | ans[1] %in% "Quit"){
          break
          }
      
      # provision to show answer
      if(ans[1] %in% "show" | ans[1] %in% "Show"){
          print(solution)
          }
      
      # check for total number of correct colours
      total_correct <- min(sum(solution %in% ans), sum(ans %in% solution))
      
      # check for number of blacks
      black <- sum(solution == ans)
      
      # calculate number of whites
      white <- total_correct - black
      
      # create sizes for the black clues
      black_sizes <- c(rep(5, black), rep(0, 4 - black))
      
      # create sizes for the white clues
      white_sizes <- c(rep(0, black), rep(5, white), rep(0, 4 - total_correct))
      
      # add to round counter
      round <- round + 1
      
      # create vector of latest guess with scoring
      guess_vector <- c(ans, black, white, round, black_sizes, white_sizes)
      
      # add vector to current board and convert to numeric where appropriate
      board <- rbind(board, guess_vector)
      
      # print current board (table format)
      print(board[-1, 1:7])
      
      # print current board (graphic format)
      graphic_board <- board[-1, ] %>% ggplot() +
          scale_color_manual(values = namedcolours) +
          geom_point(aes(x = 1, y = round, colour = first), size = 15) +
          geom_point(aes(x = 2, y = round, colour = second), size = 15) +
          geom_point(aes(x = 3, y = round, colour = third), size = 15) +
          geom_point(aes(x = 4, y = round, colour = fourth), size = 15) +
          geom_point(aes(x = 4.7, y = round, size = clue_1_black_size), colour = "black", alpha = 0.5) +
          geom_point(aes(x = 4.9, y = round, size = clue_2_black_size), colour = "black", alpha = 0.5) +
          geom_point(aes(x = 5.1, y = round, size = clue_3_black_size), colour = "black", alpha = 0.5) +
          geom_point(aes(x = 5.3, y = round, size = clue_4_black_size), colour = "black", alpha = 0.5) +
          geom_point(aes(x = 4.7, y = round, size = clue_1_white_size), colour = "white", alpha = 0.5) +
          geom_point(aes(x = 4.9, y = round, size = clue_2_white_size), colour = "white", alpha = 0.5) +
          geom_point(aes(x = 5.1, y = round, size = clue_3_white_size), colour = "white", alpha = 0.5) +
          geom_point(aes(x = 5.3, y = round, size = clue_4_white_size), colour = "white", alpha = 0.5) +
          xlim(0.7, 5.5) +
          xlab("Position") +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")
      
      print(graphic_board)
      
      }
  
  # print the exit statement
  ifelse(ans == solution, print("\nCongratulations, you have guessed the code!"), print("\nGame quit"))

}

