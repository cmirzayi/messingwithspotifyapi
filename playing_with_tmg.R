library(spotifyr)
library(ggplot2)

Sys.setenv(SPOTIFY_CLIENT_ID = 'YOURIDHERE')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'YOURSECRETHERE')
access_token <- get_spotify_access_token()

tmg <- get_artist_audio_features('the mountain goats')

summary(lm(danceability~album_release_year, data=tmg))

tmg[order(tmg$danceability),]$track_name[1:10]
library(dplyr)
tmg_album_dance<-tmg %>%
    group_by(album_name) %>%
    summarize(mean_dance=mean(danceability),
              med_dance=median(danceability))

tmg_album_dance$diff <- tmg_album_dance$mean_dance-tmg_album_dance$med_dance

p <- ggplot(tmg, aes(x=album_name, y=danceability)) +
    geom_boxplot()
p + coord_flip()

tmg_album_stats<-tmg %>%
    group_by(album_name) %>%
    summarize(mean_tempo=mean(tempo), albumyear=first(album_release_year),
              mean_energy=mean(energy), mean_loud=mean(loudness))

tmg_key_stats<-tmg %>%
    group_by(key_name) %>%
    summarize(total=n(), mean_tempo=mean(tempo),
              mean_energy=mean(energy), mean_loud=mean(loudness))

tmg_mode_stats<-tmg %>%
    group_by(mode_name) %>%
    summarize(total=n(), mean_tempo=mean(tempo),
              mean_energy=mean(energy), mean_loud=mean(loudness))


tmg_album_stats[order(tmg_album_stats$mean_loud),][,c(1,5)]

plot(tempo~album_release_year, data=tmg)
