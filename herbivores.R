#set.up.herbivore takes 3 arguments
#eat: probability a herbavore will eat if its in a cell with food
#kill: numeric of length one, probability that when the herbavore eats, it will kill the thing its eating
#reproduce: probability it will reproduce if they eat
#herbavore move, eat, reproduce, kill plant.  Need a function to put them all together
#move: function potential new location given its row and its column
#use runif to make a random draw - use runif trick for kill
#herbavore.timestep does all ofthis once, make another function that calls this over and over to move through time
#Eating:
#matrix tracks itL the number is time to dies, zero is a dead animal
#subset by matrix: look up matrix value in eat vector, if you eat reset to sated, if you eat runif trick to reproduce
#Movement: new.loc(row,col, herbavore)
#Kills: Runif trick on probability of kill.  Would then need to make a change to the plant matrix
#if it passes the runif, needs to make change to plant matrix.  =Non-linear simulation
