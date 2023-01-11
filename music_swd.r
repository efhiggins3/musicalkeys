library(ggplot2)
library(dplyr)
library(tidyverse)

prepped_data <- read.csv("./music.csv")


prepped_data %>%
  #filter(song.id == 'SOMZWCG12A8C13C480') %>%
  filter(song.id == 'SOIAZJW12AB01853F1') %>%
  select(artist.name, song.key, song.tempo, song.mode)

# https://songbpm.com/@casual/i-didn-t-mean-to (F♯/G♭)
#   artist.name song.key song.tempo
# 1      Casual        1     92.198

# https://songbpm.com/@planet-p-project/pink-world-MO2ceC_ot1 (E)
#        artist.name song.key song.tempo
# 1 Planet P Project        4     86.643

# Description of fields: http://selkie.macalester.edu/csinparallel/modules/WMRExemplar_LastFM/build/html/0-Introduction/Introduction.html

# Song names: https://raw.githubusercontent.com/bgmartins/geocoding-music/master/MillionSongSubset/AdditionalFiles/subset_unique_tracks.txt


prepped_data %>%
  filter(song.key > 0 & song.key < 12) %>%
  group_by(song.key, song.mode) %>%
  summarize(n()) %>%
  arrange(song.key, song.mode) %>%
  print(n=50)

  #    song.key song.mode `n()`
  #       <dbl>     <int> <int>
  #  1        1         0   197
  #  2        1         1   624
  #  3        2         0   221
  #  4        2         1   908
  #  5        3         0    84
  #  6        3         1   226
  #  7        4         0   380
  #  8        4         1   430
  #  9        5         0   264
  # 10        5         1   531
  # 11        6         0   265
  # 12        6         1   312
  # 13        7         0   216
  # 14        7         1  1123
  # 15        8         0   119
  # 16        8         1   409
  # 17        9         0   358
  # 18        9         1   682
  # 19       10         0   354
  # 20       10         1   346
  # 21       11         0   416
  # 22       11         1   322



prepped_data %>%
  filter(song.key >= 0 & song.key < 12) %>%
  group_by(artist.terms) %>%
  summarize(cnt = n()) %>%
  arrange(desc(cnt)) %>%
  print(n=50)
#    artist.terms         cnt
#    <chr>              <int>
#  1 blues-rock           346
#  2 hip hop              346
#  3 ccm                  255
#  4 chanson              208
#  5 country rock         156
#  6 latin jazz           150
#  7 post-grunge          146
#  8 dance pop            141
#  9 gangster rap         134
# 10 roots reggae         131
# 11 pop rock             130
# 12 progressive house    119
# 13 dancehall             97
# 14 heavy metal           97


df <- prepped_data %>%
  filter(song.key >= 0 & song.key < 12 & song.tempo > 0) %>%
  mutate(
    tempo_bucket = case_when(
      song.tempo < 85 ~ "<85",
      song.tempo < 90 ~ "085-90",
      song.tempo < 100 ~ "090-100",
      song.tempo < 110 ~ "100-110",
      song.tempo < 120 ~ "110-120",
      song.tempo < 125 ~ "120-125",
      song.tempo < 135 ~ "125-135",
      song.tempo < 150 ~ "135-150",
      song.tempo < 165 ~ "150-165",
      song.tempo >= 165 ~ "165+"
    ),
    key_mode = case_when(
      song.mode == 0 ~ "Minor",
      song.mode == 1 ~ "Major"
    ),
    key_translated = case_when(
      song.key == 0 ~ "C",
      song.key == 1 ~ "C#",
      song.key == 2 ~ "D",
      song.key == 3 ~ "D#",
      song.key == 4 ~ "E",
      song.key == 5 ~ "F",
      song.key == 6 ~ "F#",
      song.key == 7 ~ "G",
      song.key == 8 ~ "G#",
      song.key == 9 ~ "A",
      song.key == 10 ~ "A#",
      song.key == 11 ~ "B"
    ),
    is_electronic = case_when(
      artist.terms %in% c('hip hop', 'dance pop', 'gangster rap', 'progressive house', 'dancehall') ~ TRUE,
      !artist.terms %in% c('hip hop', 'dance pop', 'gangster rap', 'progressive house', 'dancehall') ~ FALSE
    )
  ) %>%
  filter(is_electronic == FALSE) %>%
  group_by(tempo_bucket, key_translated, key_mode) %>%
  summarize(cnt = n(), rnk_tempo = min(song.tempo)) %>%
  arrange(desc(rnk_tempo), key_translated)
  # %>% print(n=50)

df_major <- df %>% filter(key_mode == 'Major') %>% arrange(desc(rnk_tempo))
df_minor <- df %>% filter(key_mode == 'Minor') %>% arrange(desc(rnk_tempo))

ggplot(df_major, aes(tempo_bucket, key_translated, fill=cnt)) +
  geom_tile(color="white", size=1) +
  #geom_text(aes(label = cnt)) + 
  scale_fill_gradient(low = "#FFFFFF",
                      high = "#1C53C7",
                      guide = "colorbar")
