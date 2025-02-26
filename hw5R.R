#load all necessary libraries

library(tidyverse)
library(jsonlite)

#load the csv files provided
essentia.model.csv = read_csv("EssentiaOutput/EssentiaModelOutput.csv")
liwc.csv = read_csv("LIWCOutput/LIWCOutput.csv")

#gets all the indices of JSON files
essentia.files <- list.files(path = "EssentiaOutput")
check.json <- str_count(essentia.files, pattern=".json")
songnames <- essentia.files[which(check.json == 1)]

song.info <- tibble(
  artist = character(),
  album = character(),
  track = character(),
  overall.loudness = numeric(),
  spectral.energy = numeric(),
  dissonance = numeric(),
  pitch.salience = numeric(),
  bpm = numeric(),
  beats.loudness = numeric(),
  danceability = numeric(),
  tuning.frequency = numeric()
)

essentia.files <- list.files(path = "EssentiaOutput")

for(i in 1:length(songnames)){
  current.filename <- songnames[i]
  #current.filename <- str_sub(current.filename, start = 0, end = -6)
  track.info <- str_split_1(current.filename, "-")
  load.song.json <- fromJSON(paste("EssentiaOutput", current.filename, 
                                   sep = "/"))
  
  song.info <- bind_rows(song.info, tibble(
    artist = track.info[1],
    album = track.info[2],
    track = track.info[3],
    overall.loudness = load.song.json$lowlevel$loudness_ebu128$integrated,
    spectral.energy = load.song.json$lowlevel$spectral_energy$mean,
    dissonance = load.song.json$lowlevel$dissonance$mean,
    pitch.sailence = load.song.json$lowlevel$pitch_salience$mean,
    tempo.bpm = load.song.json$rhythm$bpm,
    beat.loudness = load.song.json$rhythm$beats_loudness$mean,
    danceability = load.song.json$rhythm$danceability,
    tuning.freq = load.song.json$tonal$tuning_frequency
  ))
}

view(song.info)