# A script based on the game Mastermind
# 2nd December 2020
# Sam Siljee

# define the colours
colours <- c("yellow", "orange", "red", "purple", "blue", "green")

# set the solution
solution <- paste(sample(colours, 4))
print(solution)

# ask for an answer
g1 <- readline("What is your first colour? ")
g2 <- readline("What is your second colour? ")
g3 <- readline("What is your third colour? ")
g4 <- readline("What is your fourth colour? ")
ans <- c(g1, g2,g3,g4)
print(ans)

# trial code to test checking for one answer
colour <- sample(colours, 1)
guess <- readline("Now try guessing for one colour: ")

# evaluate the answer
ifelse(identical(colour, guess), "Correct!", "Not correct")