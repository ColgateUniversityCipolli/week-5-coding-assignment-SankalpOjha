#load all necessary libraries

library(tidyverse)
library(jsonlite)

#load the csv files provided
essentia.csv = read_csv(paste("EssentiaOutput", "EssentiaModelOutput.csv", 
                              sep = "/"))
LIWC.csv = read_csv(paste("LIWCOutput", "LIWCOutput.csv", 
                          sep = "/"))

#gets all the indices of JSON files
essentia.files <- list.files(path = "EssentiaOutput")
check.json <- str_count(essentia.files, pattern=".json")
songnames <- essentia.files[which(check.json == 1)]

#make an empty tibble
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

#load all the files in the directory
essentia.files <- list.files(path = "EssentiaOutput")

#gather all the data from the files in the directory
for(i in 1:length(songnames)){
  current.filename <- songnames[i]
  track.info <- str_split_1(current.filename, "-")
  load.song.json <- fromJSON(paste("EssentiaOutput", current.filename, 
                                   sep = "/"))
  
  #add to the tibble by row
  song.info <- bind_rows(song.info, tibble(
    artist = track.info[1],
    album = track.info[2],
    track = str_sub(track.info[3], start = 0, end = -6),
    overall.loudness = load.song.json$lowlevel$loudness_ebu128$integrated,
    spectral.energy = load.song.json$lowlevel$spectral_energy$mean,
    dissonance = load.song.json$lowlevel$dissonance$mean,
    pitch.salience = load.song.json$lowlevel$pitch_salience$mean,
    bpm = load.song.json$rhythm$bpm,
    beats.loudness = load.song.json$rhythm$beats_loudness$mean,
    danceability = load.song.json$rhythm$danceability,
    tuning.frequency = load.song.json$tonal$tuning_frequency
  ))
}

#make a new tibble which has all the mean values
all.data.csv <- essentia.csv|>
  rowwise()|>
  mutate(valence = mean(c(deam_valence, 
                          emo_valence, 
                          muse_valence)),
         
         arousal = mean(c(deam_arousal,
                          emo_arousal,
                          muse_arousal)),
         
         agressive = mean(c(eff_aggressive,
                            nn_aggressive)),
         
         happy = mean(c(eff_happy,
                        nn_happy)),
         
         party = mean(c(eff_party,
                        nn_party)),
         
         relaxed = mean(c(eff_relax,
                          nn_relax)),
         
         sad = mean(c(eff_sad,
                      nn_sad)),
         
         acoustic = mean(c(eff_acoustic,
                           nn_acoustic)),
         
         electric = mean(c(eff_electronic,
                           nn_electronic)),
         
         instrumental = mean(c(eff_instrumental,
                               nn_instrumental))
         )|>
  
  ungroup()|>
  
  rename(timbreBright = eff_timbre_bright) |>
  
  #select all the relevant columns for the final tibble
  select("artist",
         "album",
         "track",
         "valence",
         "arousal",
         "agressive",
         "happy",
         "party",
         "relaxed",
         "sad",
         "acoustic",
         "electric",
         "instrumental",
         "timbreBright") %>%
  
  #join the orignal tibble with the tibble we made
  left_join(as_tibble(song.info), by = c("album", "track")) %>%
  
  #join the merged tibble with the LIWC csv
  left_join(LIWC.csv, by = c("album", "track")) |>
  
  #remove the extra artist columns
  select(-artist, -artist.y) |>
  
  #rename the first artist column to the correct name
  rename(funct = "function") |>
  rename(artist = artist.x) 
  
  
  
#view all out tibbles
view(song.info)
view(LIWC.csv)
view(all.data.csv)
