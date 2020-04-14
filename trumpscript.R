library(tidyverse)
library(tidytext)
data(stop_words)
stop_words_found <- tibble(word = c("the","we're", "it's", "that's", "you're", "i'm", "00", "crosstalk", "01", "they're", "he's", "we'll"))


#import speeches from raw transcript
speeches_init <- tibble(X1 = character())
for(i in c(23:27,29:31)){
        var <- read_delim(paste(".\\data\\",i, ".txt", sep = ""), 
                          delim ="\n", 
                          col_names = FALSE, 
                          col_types = cols(col_character()))
        speeches_init <- bind_rows(speeches_init,var)
}


#create data frame with speaker and speech from the speaker 
lines <- nrow(speeches_init)
speaker <- speeches_init[seq(1, lines - 1, by = 2),1]
words <- speeches_init[seq(2, lines, by = 2),1]

speeches <- tibble(speaker = speaker$X1, words = words$X1)

#remove colons and time stamps
speeches$speaker <- sapply(strsplit(speeches$speaker,":"), function(x) x[1])

#clean duplicate speaker titles
speeches$speaker[grep("President Trump",speeches$speaker)] <- "Donald Trump"
speeches$speaker[grep("Attorney General Barr",speeches$speaker)] <- "Bill Barr"
speeches$speaker[grep("Dr. Deborah Birx",speeches$speaker)] <- "Deborah Birx"
speeches$speaker[grep("Vice President Pence",speeches$speaker)] <- "Mike Pence"
speeches$speaker[grep("Dr. Fauci",speeches$speaker)] <- "Anthony Fauci"
speeches$speaker[grep("Dr. Tony Fauci",speeches$speaker)] <- "Anthony Fauci"
speeches$speaker[grep("Sec. DeVos",speeches$speaker)] <- "Betsy DeVos"

pressnames <- c("John", "Speaker 1", "Speaker 2", "Jeff", "Jim", "Kristin", "Reporter", 
                        "Male Speaker", "Speaker 3", "Speaker 4", "Speaker 5", "Speaker 6", 
                        "Speaker 7", "Speaker 8", "Speaker 9", "Press", "Press John", 
                        "Press John2", "Press Steve", "Speaker 10", "Speaker 11", "Speaker 12", 
                        "Speaker 13", "Johnny", "Speaker 14", "Speaker 15", "Speaker 16", 
                        "Speaker 17", "Speaker 18", "Peter Navarro", "Peter", "Owen Jensen",  
                        "Audience", "Moderator", "Steve")
for(name in pressnames){
        speeches$speaker[grep(name,speeches$speaker)] <- "Press"
}

#create version of data set grouped into role: pres, whitehouse officials, drs, press
speeches <- add_column(speeches, position = NA)

cabinet <- c("Betsy DeVos", "Bill Bar", "Larry Kudlow", "Mike Pence", "Sonny Perdue")
for(name in cabinet){
        speeches$position[grep(name,speeches$speaker)] <- "whitehouse"
}
doctors <- c("Anthony Fauci", "Deborah Birx")
for(name in doctors){
        speeches$position[grep(name,speeches$speaker)] <- "doctor"
}
business <- c("Niren Chaudhary", "Denton McLane")
for(name in business){
        speeches$position[grep(name,speeches$speaker)] <- "business"
}
speeches$position[grep("Donald Trump", speeches$speaker)] <- "president"

#tidy the data file
speech_words <- speeches %>%
        unnest_tokens(word,words) %>%
        count(speaker, word, sort = TRUE)

total_words <- speech_words %>%
        group_by(speaker) %>%
        summarize(total = sum(n))

speech_words <- left_join(speech_words, total_words)
        
#examine word frequencies without stop words
speech_words_stop <-speeches %>%
        unnest_tokens(word,words) %>%
        anti_join(stop_words) %>%
        anti_join(stop_words_found, by = "word") %>%
        count(speaker, word, sort = TRUE)
                
#examine word frequency using tf-idf 

speech_words <- speech_words %>%
        bind_tf_idf(word, speaker, n)

speech_words %>%
        select(-total) %>%
        arrange(desc(tf_idf)) %>%

speech_words %>%
        arrange(desc(tf_idf)) %>%
        mutate(word = factor(word, levels = rev(unique(word)))) %>% 
        group_by(speaker) %>% 
        top_n(15) %>% 
        ungroup() %>%
        ggplot(aes(word, tf_idf, fill = speaker)) +
        geom_col(show.legend = FALSE) +
        labs(x = NULL, y = "tf-idf") +
        facet_wrap(~speaker, ncol = 2, scales = "free") +
        coord_flip()

speech_words_stop %>%
        arrange(desc(n)) %>%
        mutate(word = factor(word, levels = rev(unique(word)))) %>% 
        group_by(speaker) %>% 
        top_n(15) %>% 
        ungroup() %>%
        ggplot(aes(word, n, fill = speaker)) +
        geom_col(show.legend = FALSE) +
        labs(x = NULL, y = "n") +
        facet_wrap(~speaker, ncol = 2, scales = "free") +
        coord_flip()
