mastermind <- function(n = 6, x = FALSE)
{
    # A code written by Sam Siljee based on the game Mastermind
    # 2nd December 2020
    
    # Things to be improved still:
    # 1. scoring not yet entirely compatible with some combinations of repeating colours
    # 2. give an option to change the number of holes
    
    # load appropriate packages
    library(tidyverse)
    
    # define the colours
    colours <- c("yellow", "red", "blue", "purple", "green", "orange", "black", "white", "brown", "grey", "pink")
    
    # create a named colours vector for grapihcal board
    namedcolours <- setNames(colours, colours)
    
    # create playing board
    r = 0
    board <- data.frame(first = NA, second = NA, third = NA, fourth = NA, B = NA, W = NA, R = r)
    ans = NA
    
    # set the solution
    solution <- paste(sample(colours[1:n], 4, replace = x))
    print("Welcome to this game of Mastermind, enter quit to exit the game or show to reveal the answer. The number of colours can be chosen with a numerical value for the first argument when starting mastermind, maximum of eleven. Repetition of colours can be chosen as an option by entering TRUE as the second argument. default is FALSE.")
    
    # show the colour options
    print("These are the available colours. Note that spelling has to be exact.")
    print(colours[1:n])
    
    # start while loop for incorrect answer
    while(!identical(ans, solution))
    {
        # ask for an answer, with break if quit entered
        ans <- scan(what = character(), quiet = TRUE, nlines = 1)
        
        # provision to quit if quit entered
        if(ans[1] %in% "quit"){
            break
        }
        
        # provision to show answer
        if(ans[1] %in% "show"){
            print(solution)
        }
        
        # check for total number of correct colours
        total_correct <- min(sum(solution %in% ans), sum(ans %in% solution))
        
        # check for number of blacks
        black <- sum(solution == ans)
        
        # check for number of whites
        white <- total_correct - black
        
        # add to round counter
        r <- r + 1
        
        # create vector of latest guess with scoring
        guess_vector <- c(ans, black, white, r)
        
        # add vector to current board
        board <- rbind(board, guess_vector)
        
        # print current board (table format)
        print(board[- 1, ])
        
        # print current board (graphic format)
        graphic_board <- board[- 1, ] %>% ggplot() + 
            scale_color_manual(values = namedcolours) + 
            geom_point(aes(x = 1, y = R, colour = first), size = 10) + 
            geom_point(aes(x = 2, y = R, colour = second), size = 10) + 
            geom_point(aes(x = 3, y = R, colour = third), size = 10) + 
            geom_point(aes(x = 4, y = R, colour = fourth), size = 10) +
            theme(legend.position = "none")
        print(graphic_board)
        
    }
    # print statement if solution correct
    print("Congratulations, you have guessed the code!")
    
}
