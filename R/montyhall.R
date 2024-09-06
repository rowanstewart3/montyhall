
# step 1: create a vector of 3 doors: 2 goats and 1 car
# step 2: randomize the position of the car for a new game
# step 3: return the new game vector 

create_game <- function( ) {
  a.game <- sample( x = c("🐐 goat","🐐 goat","🚗 car"), size = 3, replace = F )
  return( a.game )
} 

# step 1: create a vector of doors numbered 1,2,3
# step 2: randomly select ONE of the doors 
# step 3: return the selection 

select_door <- function() {
  doors <- c( 1, 2, 3 )
  a.pick <- sample( doors, size = 1, replace = F )
  return( a.pick )
}

# step 1: check if the door that was chosen was a goat
# step 2: if it was, choose the other door that's a goat and reveal it
# step 3: if it was the car, choose either one of the goat doors to reveal

open_goat <- function( game, a.pick ) {
  if ( game[a.pick] == "🐐 goat") {
    if ( a.pick != 1 & game[1] == "🐐 goat" ) {
      opened.door <- 1
    } else if ( a.pick != 2 & game[2] == "🐐 goat" ) {
      opened.door <- 2
    } else {
      opened.door <- 3
    }
  } else {
    choices <- c( 1, 2, 3 )
    a.open.pick <- sample( choices[ choices != a.pick ], size = 1, replace = F )
    opened.door <- a.open.pick
  }
  
  return( opened.door )
}

# if stay = true, a.pick stays the same 
# if stay = false, a.pick switches from one closed door to the other 
# remove the open door from the choices and make a.pick = the new choice 

change_door <- function( stay = TRUE, opened.door, a.pick ) {
  if ( stay == TRUE ) {
    final.pick <- a.pick
  } else if ( stay == FALSE ) {
    choices <- c( 1, 2, 3 ) 
    final.pick <- choices[ choices != a.pick & choices != opened.door ]
  }
  return( final.pick )
}

# check to see if the final.pick is equal to "car"
# if so, return "WIN", if not return "LOSE"

determine_winner <- function( final.pick, game ) {
  if ( game[ final.pick ] == "🚗 car" ) {
    return( "✨ WIN ✨" )
  } else {
    return( "LOSE" )
  }
}

create_challenge_game <- function( ) {
  a.hard.game <- sample( x = c("🐐", "🐐", "🐐", "🚗", "🚗"), size = 5, replace = F )
  return( a.hard.game )
} 

select_new_door <- function() {
  new.doors <- c( 1, 2, 3, 4, 5 )
  a.new.pick <- sample( new.doors, size = 1, replace = F )
  return( a.new.pick )
}

open_doors <- function( a.hard.game, a.new.pick ) {
  choices <- c( 1, 2, 3, 4, 5 )
  open.a.car <- sample( choices[ choices != a.new.pick & a.hard.game[ choices ] == "🚗" ], size = 1, replace = F )
  open.a.goat <- sample( choices[ choices != a.new.pick & a.hard.game[ choices ] == "🐐" ], size = 1, replace = F )
  opened.doors <- c( open.a.car, open.a.goat )
  return( opened.doors )
}

switch_doors <- function( a.hard.game, a.new.pick, opened.doors, switch = TRUE ) {
  new_choices <- c( 1, 2, 3, 4, 5 )
  if ( switch == TRUE ) {
    final.pick <- a.new.pick
  } else {
    final.pick <- sample( new_choices[ new_choices != opened.doors[1] & new_choices != opened.doors[2] & new_choices != a.new.pick ], size = 1, replace = F)
  }
  return( final.pick )
}

determine_chal_winner <- function( final.pick, game ) {
  if ( game[ final.pick ] == "🚗" ) {
    return( "✨ WIN ✨" )
  } else {
    return( "LOSE" )
  }
}

build_game <- function( num.doors, num.cars, strategy ) {
  if ( num.cars > num.doors - 2 ) {
    result <- "can't create a game with these conditions"
  } else {
    options <- c( rep( "🚗", num.cars ), rep( "🐐", num.doors - num.cars ))
    game <- sample( options, size = num.doors, replace = F )
    choices <- c( 1:num.doors )
    choice <- sample( choices, size = 1, replace = F )
    
    reveal <- sample( choices[ choices != choice & game[ choices ] != "🚗" ], size = 1, replace = F )
    
    if ( strategy == "stay" ) {
      final.choice <- choice
      if ( game[ final.choice ] == "🚗" ) {
        outcome <- "✨ WIN ✨"
      } else {
        outcome <- "LOSE"
      }
    } else if ( strategy == "switch" ) {
      final.choice <- sample( choices[ choices != reveal & choices != choice ], size = 1, replace = F )
      
      if ( game[ final.choice ] == "🚗" ) {
        outcome <- "✨ WIN ✨"
      } else {
        outcome <- "LOSE"
      }
    } else {
      result <- "please choose either 'stay' or 'switch' as your strategy"
    }
    
    game_string <- toString( game )
    
    result <- paste( "the original game setup was ", game_string, ". your first choice was ", choice, " and you chose to ", strategy, ", so your final choice was ", final.choice, ". YOU", outcome, "!" )
  }
  
  return( result )
}

build_game( 5, 2, 'switch' )

create_game <- function( ) {
  a.game <- sample( x = c("🐐 goat","🐐 goat","🚗 car"), size = 3, replace = F )
  return( a.game )
} 

select_door <- function() {
  doors <- c( 1, 2, 3 )
  a.pick <- sample( doors, size = 1, replace = F )
  return( a.pick )
}

open_goat <- function( game, a.pick ) {
  if ( game[a.pick] == "🐐 goat") {
    if ( a.pick != 1 & game[1] == "🐐 goat" ) {
      opened.door <- 1
    } else if ( a.pick != 2 & game[2] == "🐐 goat" ) {
      opened.door <- 2
    } else {
      opened.door <- 3
    }
  } else {
    choices <- c( 1, 2, 3 )
    a.open.pick <- sample( choices[ choices != a.pick ], size = 1, replace = F )
    opened.door <- a.open.pick
  }
  
  return( opened.door )
}

change_door <- function( stay = TRUE, opened.door, a.pick ) {
  if ( stay == TRUE ) {
    final.pick <- a.pick
  } else if ( stay == FALSE ) {
    choices <- c( 1, 2, 3 ) 
    final.pick <- choices[ choices != a.pick & choices != opened.door ]
  }
  return( final.pick )
}

determine_winner <- function( final.pick, game ) {
  if ( game[ final.pick ] == "🚗 car" ) {
    return( "✨ WIN ✨" )
  } else {
    return( "LOSE" )
  }
}

play_game <- function( ) {
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat( new.game, first.pick )
  final.pick.stay <- change_door( stay = T, opened.door, first.pick )
  final.pick.switch <- change_door( stay = F, opened.door, first.pick )
  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c( "stay", "switch" )
  outcome <- c( outcome.stay, outcome.switch )
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors = F )
  return( game.results )
}


create_challenge_game <- function( ) {
  a.hard.game <- sample( x = c("🐐", "🐐", "🐐", "🚗", "🚗"), size = 5, replace = F )
  return( a.hard.game )
} 

select_new_door <- function() {
  new.doors <- c( 1, 2, 3, 4, 5 )
  a.new.pick <- sample( new.doors, size = 1, replace = F )
  return( a.new.pick )
}

open_doors <- function( a.hard.game, a.new.pick ) {
  choices <- c( 1, 2, 3, 4, 5 )
  open.a.car <- sample( choices[ choices != a.new.pick & a.hard.game[ choices ] == "🚗" ], size = 1, replace = F )
  open.a.goat <- sample( choices[ choices != a.new.pick & a.hard.game[ choices ] == "🐐" ], size = 1, replace = F )
  opened.doors <- c( open.a.car, open.a.goat )
  return( opened.doors )
}

switch_doors <- function( a.hard.game, a.new.pick, opened.doors, switch = TRUE ) {
  new_choices <- c( 1, 2, 3, 4, 5 )
  if ( switch == TRUE ) {
    final.pick <- a.new.pick
  } else {
    final.pick <- sample( new_choices[ new_choices != opened.doors[1] & new_choices != opened.doors[2] & new_choices != a.new.pick ], size = 1, replace = F)
  }
  return( final.pick )
}


determine_chal_winner <- function( final.pick, game ) {
  if ( game[ final.pick ] == "🚗" ) {
    return( "✨ WIN ✨" )
  } else {
    return( "LOSE" )
  }
}


play_bigger_game <- function( ) {
  new.game <- create_challenge_game()
  first.pick <- select_new_door()
  opened.doors <- open_doors( new.game, first.pick )
  final.pick.stay <- switch_doors( new.game, first.pick, opened.doors, switch = FALSE )
  final.pick.switch <- switch_doors( new.game, first.pick, opened.doors, switch = TRUE )
  outcome.stay <- determine_chal_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_chal_winner( final.pick.switch, new.game )
  
  strategy <- c( "stay", "switch" )
  outcome <- c( outcome.stay, outcome.switch )
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors = F )
  return( game.results )
}

build_doors <- function( n ){ return( 1:n ) }

create_live_game <- function( num.goats, num.cars ) {
  a.game <- sample( x = rep( c( "goat", "car" ), c( num.goats, num.cars ) ), 
                    size = ( num.goats + num.cars ), replace = F )
  return( a.game )
}

select_live_door <- function( game ) {
  doors <- build_doors( n = length( game ) ) 
  a.pick <- sample( doors, size = 1 )
  return( a.pick )  # number between 1 and N
}

open_live_goat_door <- function( game, a.pick ) {
  doors <- build_doors( n = length( game ) )
  doors.that.can.be.opened <- doors[ ! ( game == "car" | doors == a.pick ) ]
  opened.door <- sample( doors.that.can.be.opened, size = 1 )
  return( opened.door ) # number between 1 and N
}

change_live_door <- function( stay = T, game, opened.door, a.pick ) {
  doors <- build_doors( length( game ) )
  if( stay ) {
    final.pick <- a.pick
  }
  if( ! stay ) {
    available.doors <- doors[ doors != opened.door & doors != a.pick ]
    final.pick  <- sample( available.doors, size=1 ) 
  }
  
  return( final.pick )  # number between 1 and N
}

determine_live_winner <- function( final.pick, game ) {
  if( game[ final.pick ] == "car" ) {
    return( "✨WIN✨" )
  }
  
  if( game[ final.pick ] == "goat" ) {
    return( "LOSE" )
  }
}

play_live_game <- function( num.goats, num.cars ) {
  new.game <- create_live_game( num.goats, num.cars ) 
  first.pick <- select_live_door( new.game )
  opened.door <- open_live_goat_door( new.game, first.pick )
  final.pick.stay <- change_live_door( stay = T, new.game, opened.door, first.pick )
  final.pick.switch <- change_live_door( stay = F, new.game, opened.door, first.pick )
  outcome.stay <- determine_live_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_live_winner( final.pick.switch, new.game )
  
  strategy <- c( "stay", "switch" )
  outcome <- c( outcome.stay, outcome.switch )
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors = F )
  return( game.results )
}


