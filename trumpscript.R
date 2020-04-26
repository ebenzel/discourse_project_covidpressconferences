library(tidyverse)
library(tidytext)
library(lubridate)
library(jtools)
library(gt)

data(stop_words)


#import and clean data
importandclean <- function(){
        #import speeches from raw transcript
        speeches_init <- tibble(X1 = character(), date = character())
        filenames <- list.files(".\\data")
        for(f in filenames){
                vars <- read_delim(paste(".\\data\\",f, sep = ""), 
                                   delim ="\n", 
                                   col_names = FALSE, 
                                   col_types = cols(col_character()),
                                   locale=locale(encoding = "windows-1252"))
                date <- c(rep(f,length(vars$X1)))
                speech <- tibble(X1 = vars$X1, date = date)
                speeches_init <- bind_rows(speeches_init,speech)
        }
        
        
        #create data frame with speaker and speech from the speaker and clean date
        lines <- nrow(speeches_init)
        speaker <- speeches_init[seq(1, lines - 1, by = 2),1]
        words <- speeches_init[seq(2, lines, by = 2),1]
        words$X1 <- sapply(words$X1, tolower)
        
        dates <- speeches_init[seq(2, lines, by = 2),2]
        dates <- sapply(strsplit(dates$date,".t"), function(x) x[1])
        for(i in 1:length(dates)){
                dates[i] <- gsub("_"," ", dates[i])
                dates[i] <- paste(dates[i]," 2020")
        }
        dates <- sapply(dates, mdy)
        dates <- tibble(date = as_date(dates,origin = lubridate::origin))
        
        speeches <- tibble(date = dates$date,speaker = speaker$X1, words = words$X1)
        
        #remove colons and time stamps
        speeches$speaker <- sapply(strsplit(speeches$speaker,":"), function(x) x[1])
        
        #clean duplicate speaker titles
        speeches$speaker[grep("President Trump",speeches$speaker)] <- "Donald Trump"
        speeches$speaker[grep("President Donald Trump",speeches$speaker)] <- "Donald Trump"
        speeches$speaker[grep("Attorney General Barr",speeches$speaker)] <- "Bill Barr"
        speeches$speaker[grep("William Barr",speeches$speaker)] <- "Bill Barr"
        speeches$speaker[grep("Dr. Deborah Birx",speeches$speaker)] <- "Deborah Birx"
        speeches$speaker[grep("Dr. Berks",speeches$speaker)] <- "Deborah Birx"
        speeches$speaker[grep("Doctor Birx",speeches$speaker)] <- "Deborah Birx"
        speeches$speaker[grep("Debbie Birx",speeches$speaker)] <- "Deborah Birx"
        speeches$speaker[grep("Dr. Birch",speeches$speaker)] <- "Deborah Birx"
        speeches$speaker[grep("Dr. Burks",speeches$speaker)] <- "Deborah Birx"
        speeches$speaker[grep("Dr. Birx",speeches$speaker)] <- "Deborah Birx"
        speeches$speaker[grep("Vice President Pence",speeches$speaker)] <- "Mike Pence"
        speeches$speaker[grep("Vice President Mike Pence",speeches$speaker)] <- "Mike Pence"
        speeches$speaker[grep("Michael Pence",speeches$speaker)] <- "Mike Pence"
        speeches$speaker[grep("Pence",speeches$speaker)] <- "Mike Pence"
        speeches$speaker[grep("Dr. Stephen Hahn",speeches$speaker)] <- "Stephen Hahn"
        speeches$speaker[grep("Dr. Fauci",speeches$speaker)] <- "Anthony Fauci"
        speeches$speaker[grep("Dr. Tony Fauci",speeches$speaker)] <- "Anthony Fauci"
        speeches$speaker[grep("Tony Fauci",speeches$speaker)] <- "Anthony Fauci"
        speeches$speaker[grep("Dr. Anthony Fauci",speeches$speaker)] <- "Anthony Fauci"
        speeches$speaker[grep("Dr Fauci",speeches$speaker)] <- "Anthony Fauci"
        speeches$speaker[grep("Sec. DeVos",speeches$speaker)] <- "Betsy DeVos"
        speeches$speaker[grep("Secretary Alex Azar",speeches$speaker)] <- "Alex Azar"
        speeches$speaker[grep("Admiral Giroir",speeches$speaker)] <- "Brett Giroir"
        speeches$speaker[grep("Dr. Anne Schuchat",speeches$speaker)] <- "Anne Schuchat"
        speeches$speaker[grep("Dr. Robert Kadlec",speeches$speaker)] <- "Robert Kadlec"
        speeches$speaker[grep("Dr. Kadlecik",speeches$speaker)] <- "Robert Kadlec"
        speeches$speaker[grep("Secretary Azar",speeches$speaker)] <- "Alex Azar"
        speeches$speaker[grep("Secretary Cuccinelli",speeches$speaker)] <- "Ken Cuccinelli"
        speeches$speaker[grep("Secretary Joel Szabat",speeches$speaker)] <- "Joel Szabat"
        speeches$speaker[grep("Dr. Hahn",speeches$speaker)] <- "Stephen Hahn"
        speeches$speaker[grep("Dr Redfield",speeches$speaker)] <- "Robert Redfield"
        speeches$speaker[grep("Dr. Redfield",speeches$speaker)] <- "Robert Redfield"
        speeches$speaker[grep("Admiral Polowczyk ",speeches$speaker)] <- "John Polowczyk"
        speeches$speaker[grep("Admiral",speeches$speaker)] <- "John Polowczyk"	
        speeches$speaker[grep("Admiral Polowczyk",speeches$speaker)] <- "John Polowczyk"
        speeches$speaker[grep("Secretary Mnuchin",speeches$speaker)] <- "Steven Mnuchin"
        speeches$speaker[grep("Dr. Ben Carson",speeches$speaker)] <- "Ben Carson"
        speeches$speaker[grep("Dr. Carson",speeches$speaker)] <- "Ben Carson"	
        speeches$speaker[grep("Sema Verma",speeches$speaker)] <- "Seema Verma"
        speeches$speaker[grep("Sema",speeches$speaker)] <- "Seema Verma"
        speeches$speaker[grep("Sec. Pompeo",speeches$speaker)] <- "Mike Pompeo"
        speeches$speaker[grep("Pete",speeches$speaker)] <- "Pete Gaynor"
        speeches$speaker[grep("Surgeon General",speeches$speaker)] <- "Jerome Adams"
        speeches$speaker[grep("General Milley",speeches$speaker)] <- "Mark Milley"
        
        
        
        pressnames <- c("John", "Speaker 1", "Speaker 2", "Jeff", "Jim", "Kristin", "Reporter", 
                        "Male Speaker", "Speaker 3", "Speaker 4", "Speaker 5", "Speaker 6", 
                        "Speaker 7", "Speaker 8", "Speaker 9", "Press", "Press John", 
                        "Press John2", "Press Steve", "Speaker 10", "Speaker 11", "Speaker 12", 
                        "Speaker 13", "Johnny", "Speaker 14", "Speaker 15", "Speaker 16", 
                        "Speaker 17", "Speaker 18", "Peter Navarro", "Peter", "Owen Jensen",  
                        "Audience", "Moderator", "Steve", "Yamiche Alcindor", "Jeremy", "Jenn Pellegrino",
                        "Kelly", "Journalist 1", "Journalist 2", "Journalist 3", "Journalist 4",
                        "Journalist 5", "Journalist 6", "Journalist 7", "Journalist 8", "Journalist 9",
                        "Journalist 10", "Brianna", "Sophie Tatum", "Carol Pearson", "Bob", "Jeannie Baton",
                        "Anne Flaherty", "Dan Vergano", "Philip Weidman", "Sue", "George", "Jonathan Karl",
                        "Tony", "Kristen", "Tony", "Deborah Saunders", "Gordon Lubold", "Wolf", "Ross Palombo",
                        "OAN", "Yamiche A.", "Vanessa", "Cordelia L.", "Doug", "Jennifer", "Group", "Kaitlin", "Katelyn",
                        "Dan", "Hallie Jackson", "Ben", "Ashley"
        )
        for(name in pressnames){
                speeches$speaker[grep(name,speeches$speaker)] <- "Press"
        }
        
        #create data table of speakers and role: pres, whitehouse and military, drs, press
        positions <- tibble(speaker = unique(speeches$speaker), position = NA)
        
        whitehouse <- c("Betsy DeVos", "Bill Bar", "Larry Kudlow", "Mike Pence", "Sonny Perdue","Seema Verma",
                        "Alex Azar", "Stephen Biegun", "Ken Cuccinelli", "Joel Szabat", "Eugene Salia","John Polowczyk",
                        "Kevin McCarthy", "Eugene Scalia", "Jared Kushner", "Jovita Carranza", "Robert O’Brien",
                        "Steven Mnuchin", "Mark Esper", "Robert Wilkie", "Mike Pompeo", "Pete Gaynor", "Mark Milley")
        for(name in whitehouse){
                positions$position[grep(name,positions$speaker)] <- "Whitehouse"
        }
        doctors <- c("Anthony Fauci", "Deborah Birx","Anne Schuchat","Robert Kadlec", "Stephen Hahn", "Brett Giroir",
                     "Robert Redfield","Jerome Adams", "Ben Carson")
        for(name in doctors){
                positions$position[grep(name,positions$speaker)] <- "Doctor"
        }
        business <- c("Niren Chaudhary", "Denton McLane","Greg Hayes", "David Taylor", "Debra Waller", "Darius Adamczyk", "Mike Lindell", "Doug McMillon",
                      "Richard Ashworth", "Brian Cornell", "Tom Polen", "Matt Sause", "David Pierre", "Adam Schechter", "Thomas Moriarty", "Bruce Greenstein")
        for(name in business){
                positions$position[grep(name,positions$speaker)] <- "Business"
        }
        positions$position[grep("Donald Trump", positions$speaker)] <- "Trump"
        positions$position[grep("Press", positions$speaker)] <- "Press"
        
        speeches <- left_join(speeches, positions, by = "speaker") 
        speeches <- select(speeches, date, speaker, position, words)
        return(speeches)
}
speeches <- importandclean()

#tidy text data

word_data<- speeches %>%
        unnest_tokens(word,words)

word_data$word <- sapply(word_data$word, function(x) gsub("’", "'", x = x))

#word count by day by position plot
speech_words_day <- word_data %>%
        group_by(position, date) %>%
        count(word, sort = TRUE)

total_words_day <- speech_words_day %>%
        group_by(position, date) %>%
        summarize(total = sum(n))

#summarize data by frequency of words per day
total_words_day %>%
        filter(position)

total_words_day %>%
        ggplot(aes(x = date, y = total)) +
        geom_col() +
        geom_smooth(method = "lm", se = FALSE, color = "grey", size = 1) +
        facet_grid(rows = vars(position), margins = TRUE) +
        scale_x_date() +
        theme_apa() +
        scale_fill_brewer(palette = "Set1") +
        labs(x = "Press Conference Date", y = "Total Words Spoken")
dev.copy(png,"Total_words_by_date.png")
dev.off()

#create word frequency frames by speaker
speech_words_speaker <- word_data %>%
        group_by(speaker) %>%
        count(word) 

total_words_speaker <- speech_words_speaker %>%
        group_by(speaker) %>%
        summarize(total = sum(n))

speech_words_speaker <- left_join(speech_words_speaker, total_words_speaker, by = "speaker")

#create word frequency frame by position
speech_words_position <- word_data %>%
        group_by(position) %>%
        count(word, sort = TRUE) 

total_words_position <- speech_words_position %>%
        group_by(position) %>%
        summarize(total = sum(n))

speech_words_position <- left_join(speech_words_position, total_words_position, by = "position")

# Create graph of speaker and position frequencies
speech_words_speaker %>%
        group_by(speaker) %>%
        summarise(total = sum(n)) %>%
        filter(total > 100) %>%
        arrange(desc(total)) %>%
        ggplot(aes(x = reorder(speaker, total), y = total)) +
        geom_col() +
        coord_flip() +
        labs(x = "Speaker", y = "Total words spoken") +
        theme_apa()
dev.copy(png,"Total_words_by_speaker.png")
dev.off()

speech_words_position %>%
        group_by(position) %>%
        summarise(total = sum(n)) %>%
        ggplot(aes(x = reorder(position, total), y = total)) +
        geom_col() +
        coord_flip() +
        labs(x = "Position", y = "Total words spoken") +
        theme_apa()
dev.copy(png,"Total_words_by_position.png")
dev.off()


#examine word frequencies without stop words
speech_words_position_sr <-filter(speech_words_position, !word %in% stop_words$word)
my_stop_words = c("crosstalk", "01", "00", "sir", "15", "30")
speech_words_position_sr <- filter(speech_words_position_sr, !word %in% my_stop_words)

speech_words_speaker_sr <-filter(speech_words_speaker, !word %in% stop_words$word)
my_stop_words = c("crosstalk", "01", "00", "sir", "15", "30")
speech_words_speaker_sr <- filter(speech_words_speaker_sr, !word %in% my_stop_words)

# create word proportion graph by position
gd <- speech_words_position_sr %>%
        arrange(desc(n)) %>%
        group_by(position) %>%
        top_n(50,n) %>%
        filter(n>3) %>%
        ungroup() %>%
        arrange(position, n) %>%
        mutate(order = row_number()) 
        
gd %>%
        ggplot(aes(order,n,fill = position)) +
        geom_col(show.legend = FALSE) +
        labs(x = NULL, y = "word frequency") +
        facet_wrap( ~ position, ncol = 5, scales = "free") +
        scale_x_continuous(
                breaks = gd$order,
                labels = gd$word,
                expand = c(0,0)
        ) +
        theme_apa() +
        coord_flip()

# bigram frequency
word_bigrams <- speeches %>%
        unnest_tokens(bigram,words,token = "ngrams", n = 2)

bigrams_separated <- word_bigrams %>%
        separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
        filter(!word1 %in% c(stop_words$word,my_stop_words)) %>%
        filter(!word2 %in% c(stop_words$word,my_stop_words))

bigram_counts <- bigrams_filtered %>% 
        count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
        unite(bigram, word1, word2, sep = " ") %>%
        group_by(position) %>%
        count(bigram)

gd <- bigrams_united %>%
        filter(position != "Business") %>%
        arrange(desc(n)) %>%
        group_by(position) %>%
        top_n(50,n) %>%
        ungroup() %>%
        arrange(position, n) %>%
        mutate(order = row_number()) 

gd %>%
        ggplot(aes(order,n,fill = position)) +
        geom_col(show.legend = FALSE) +
        labs(x = NULL, y = "word frequency") +
        facet_wrap( ~ position, ncol = 4, scales = "free") +
        scale_x_continuous(
                breaks = gd$order,
                labels = gd$bigram,
                expand = c(0,0)
        ) +
        theme_apa() +
        coord_flip()

# Create frequency table w/ position data 
table <- speech_words_speaker %>% 
        left_join(positions, by = "speaker")

table <- distinct(select(table,speaker,position, total))
table <- arrange(table, desc(total))
write.table(table,file = "freqtable.txt",sep = ",", quote = F, col.names = F)
