library(tidyverse)
library(rlist)

sample_size = 15  # choose number of tiles in hand

all_dominoes = readLines("domino_list.txt")        # load list of all tiles
dominoes <- list.sample(all_dominoes,sample_size)  # select random hand of tiles

tile_seq <- list(dominoes[1])     # initialize tile sequence
tile_rem <- list(dominoes[2:15])  # initialize remaining tile list

# loop through each tile sequence until no more possible new tile additions
# Note:  this algorithm does not handle sequence 'splits' from double tiles
repeat {
  end_of_tiles <- TRUE  # flag to break loop if no new additions
  new_tile_seq <- NULL  # new set of tile sequences
  new_tile_rem <- NULL  # new set of remaining tiles
  for (i in 1:length(tile_seq)) {
    # select current tile sequence and remaining tiles
    tile <- tile_seq[[i]]            
    remaining_tiles <- tile_rem[[i]]
    
    # select last tile number and subset all remaining tiles that can be added (one of the two numbers matches the last tile number)
    last_tile <- unlist(str_split(tile[length(tile)],'-'))[2]
    possible_tiles <- remaining_tiles[str_detect(remaining_tiles,regex(paste0('^',last_tile,'-|-',last_tile,'$')))]

    # loop through all possible tiles and add to end of tile sequence and remove from remaining tile.
    for (next_tile in possible_tiles) {
      end_of_tiles <- FALSE                                             # adding at least one new tile, so continue loop
      split_tile = unlist(str_split(next_tile,"-"))                     # take current tile and split into half
      if (split_tile[2] == last_tile) { split_tile <- rev(split_tile) } # flip tile if matching number is on right side
      cur_rem <- remaining_tiles[!remaining_tiles %in% next_tile]       # remove the current tile from remaining tile list
      next_tile <- paste0(split_tile[1],"-",split_tile[2])              # paste current tile halves back together
      cur_seq <- c(tile, next_tile)                                     # add current tile to current sequence
      new_tile_seq <- c(new_tile_seq,list(cur_seq))                     # add current sequence to list
      new_tile_rem <- c(new_tile_rem,list(cur_rem))                     # add remaining tile set to list
    } 
  }
  if (end_of_tiles == TRUE) { break }  # if no new tiles added then end loop
  tile_seq <- new_tile_seq             # replace existing tile sequence with latest one
  tile_rem <- new_tile_rem             # replace remaining tile list with latest one
}

# number of longest sequences and length
print(paste0("Number of longest sequences: ",length(tile_seq),".  Longest Sequnce: ",length(tile_seq[[1]])," tiles."))
