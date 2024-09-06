#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return 
#'   The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   Game player selects a door from the three doors 
#'   
#' @description 
#'   `select_door()` chooses one of the three doors created in `create_game()` at random
#'   
#' @details
#'   The function randomly selects one of three doors, mimicking the initial choice 
#'   made by a contestant in the Monty Hall game. This selection represents the first 
#'   step in the game show scenario, where the contestant must choose a door without 
#'   any prior information about what lies behind it. The function uses a random number 
#'   generator to ensure an unbiased selection among the three available options, 
#'   labeled as doors 1, 2, and 3. This random selection is crucial for simulating the 
#'   contestant's initial guess in the Monty Hall problem, allowing for further 
#'   exploration of the game's probability dynamics and the effectiveness of different 
#'   strategies (staying with the initial choice or switching) when given the opportunity 
#'   to change doors.
#'   
#' @param ... no arguments are used by the function.
#' 
#' @return 
#'   This function returns a number between 1 and 3, representing the selected door.
#'   
#' @examples
#'   select_door()
#'   
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Host opens a door that displays a goat
#' 
#' @description
#'   `open_goat_door()` chooses one of the three doors not selected by participants 
#'   to open and show a goat.
#'   
#' @details
#'   This function simulates the host's action of opening a door to reveal a goat in 
#'   the Monty Hall game. After the contestant's initial selection, the host must open 
#'   one of the two remaining doors, adhering to two crucial rules: the opened door 
#'   must not be the contestant's choice, and it must not contain the prize (typically 
#'   a car). The function takes into account the contestant's selection and the actual 
#'   location of the prize, then determines which of the remaining doors can be safely 
#'   opened to reveal a goat. This step is critical in the game as it provides 
#'   additional information to the contestant before they make their final decision to 
#'   stay with their original choice or switch doors. The function's behavior mirrors 
#'   the host's knowledge of the prize's location and their deliberate action to 
#'   maintain the game's suspense while adhering to the established rules.
#'   
#' @param game A character vector of length 3 representing the 
#'   distribution of prizes behind the doors (e.g., c("goat", "car", "goat")).
#' 
#' @param a.pick An integer 1, 2, or 3 which represents the choice selected (or 
#'   in this case, simulated) by the player.
#' 
#' @return 
#'   This function returns the number (1 through 3) of the door that the host opens.
#'   
#' @examples
#'   open_goat_door( my.game, 2 )
#' 
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   Change the contestant's door choice in the Monty Hall Problem.
#'
#' @description
#'   `change_door()` determines the contestant's final door choice based on 
#'   whether they decide to switch, which door was opened by the host, and 
#'   their original choice.
#'
#' @details
#'   In the Monty Hall Problem, after the contestant's initial choice and 
#'   the host reveals a goat behind one of the unchosen doors, the contestant 
#'   is given the option to switch their choice to the remaining unopened door. 
#'   This function simulates that decision process, taking into account whether 
#'   the contestant chooses to switch, which door the host opened, and the 
#'   contestant's original choice. It then returns the contestant's final door 
#'   selection.
#'
#' @param switch A logical value indicating whether the contestant decides to 
#'   switch doors (TRUE) or stay with their original choice (FALSE).
#'   
#' @param opened An integer (1, 2, or 3) indicating which door the host opened 
#'   to reveal a goat.
#' @param original_choice An integer (1, 2, or 3) representing the contestant's 
#'   original door choice.
#' 
#' @return 
#'   The function returns an integer (1, 2, or 3) representing the contestant's 
#'   final door choice.
#'
#' @examples
#'   change_door(switch = TRUE, opened = 2, original_choice = 1)
#'   change_door(switch = FALSE, opened = 3, original_choice = 2)
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determine the outcome of a Monty Hall Problem game.
#'
#' @description
#'   `determine_winner()` evaluates whether the contestant has won the car or 
#'   a goat based on their final door choice and the actual game setup.
#'
#' @details
#'   In the Monty Hall Problem, the game concludes when the contestant makes 
#'   their final door selection. This function compares the contestant's final 
#'   choice with the actual distribution of prizes behind the doors to determine 
#'   if the contestant has won the car or a goat. It's a crucial part of the 
#'   simulation as it allows for the analysis of different strategies (switching 
#'   or staying) over multiple game iterations.
#'
#' @param final_pick An integer (1, 2, or 3) representing the contestant's 
#'   final door choice.
#' @param game_setup A character vector of length 3 representing the 
#'   distribution of prizes behind the doors (e.g., c("goat", "car", "goat")).
#' 
#' @return 
#'   The function returns a character string, either "car" if the contestant 
#'   won the car, or "goat" if they did not.
#'
#' @examples
#'   determine_winner(final_pick = 2, game_setup = c("goat", "car", "goat"))
#'   determine_winner(final_pick = 1, game_setup = c("goat", "goat", "car"))
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   Simulate one complete Monty Hall Problem game.
#'
#' @description
#'   `play_game()` simulates a full round of the Monty Hall Problem, including 
#'   both "stay" and "switch" strategies, and returns the outcomes.
#'
#' @details
#'   This function orchestrates a complete simulation of the Monty Hall Problem 
#'   by calling other functions to create the game setup, select the contestant's 
#'   initial door, open a goat door, and determine the outcomes for both "stay" 
#'   and "switch" strategies. It encapsulates the entire game process, from 
#'   initial setup to final results, allowing for easy simulation of multiple 
#'   games to analyze the probabilities associated with each strategy.
#'
#'   The function creates a new game setup, selects an initial door for the 
#'   contestant, opens a door revealing a goat, determines the final pick for both 
#'   "stay" and "switch" strategies, evaluates the outcome for both strategies, 
#'   returns the results in a data frame.
#'
#' @param ... no arguments are used by the function
#' 
#' @return 
#'   The function returns a data frame with two rows and two columns:
#'   - 'strategy': a character vector with values "stay" and "switch"
#'   - 'outcome': a character vector with the result ("car" or "goat") for each strategy
#'
#' @examples
#'   play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   Simulate one complete Monty Hall Problem game.
#'
#' @description
#'   `play_game()` simulates a full round of the Monty Hall Problem, including 
#'   both "stay" and "switch" strategies, and returns the outcomes.
#'
#' @details
#'   This function orchestrates a complete simulation of the Monty Hall Problem 
#'   by calling other functions to create the game setup, select the contestant's 
#'   initial door, open a goat door, and determine the outcomes for both "stay" 
#'   and "switch" strategies. It encapsulates the entire game process, from 
#'   initial setup to final results, allowing for easy simulation of multiple 
#'   games to analyze the probabilities associated with each strategy.
#'
#'   The function creates a new game setup, selects an initial door for the 
#'   contestant, opens a door revealing a goat, determines the final pick for both 
#'   "stay" and "switch" strategies, evaluates the outcome for both strategies, 
#'   returns the results in a data frame.
#'
#' @param n An integer which represents the number of simulated games to complete
#' 
#' @return 
#'   The function returns a data frame with two rows and two columns:
#'   - 'strategy': a character vector with values "stay" and "switch"
#'   - 'outcome': a character vector with the result ("car" or "goat") for each strategy
#'
#' @examples
#'   play_game()
#'
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
