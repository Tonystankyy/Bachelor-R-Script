#### Heading ####



#This R-script was constructed in relation to the bachelor's thesis: "The linkage between defensive realism and EU foreign policy"



#Candidate number: X



#Date; 01.06.2023



#### Acquisition of data ####



### packages:



library(rvest)



library(pdftools)



library(stringr)



library(tidyverse)



library(tibble)



library(lubridate)



### Saving base URL - Ukraine:


UKRAINE_URL <- "https://ukraine.europarl.europa.eu/en/documents/ep-resolutions"



### Data scrape - Ukraine: 



## Download the HTML file to local directory:



UKRAINE <- download.file("https://ukraine.europarl.europa.eu/en/documents/ep-resolutions", 
              destfile = "./HTML/UKRAINE.html") 



## Construct framework for scraping, where I removed a few documents because of relevance: (UKRAINE) 



UKRAINE_links_all <- read_html("./HTML/UKRAINE.html") %>%
  html_node("#website-body > div > div > div > div > div > div > div > div > div > div > div > div > ul") %>%
  html_elements("a") %>%
  html_attr("href") %>% 
  na.omit() 



paste0(UKRAINE_links_all)



UKRAINE_links_all <- UKRAINE_links_all[-c(21,22,23,24)]



paste0(UKRAINE_links_all)



UKRAINE_links_PDF <- UKRAINE_links_all[14:20]



paste0(UKRAINE_links_PDF)



UKRAINE_links_html <- UKRAINE_links_all[1:13]



paste0(UKRAINE_links_html)



remove(UKRAINE_links_all)



## automating the scraping process: html - links first



for(i in 1:length(UKRAINE_links_html)) { 
  download.file(UKRAINE_links_html[[i]],
                destfile = str_c("./links-html/", i, ".html"))
}



UKRAINE_list_html <- list.files("./links-html/", full.names = TRUE) 



paste0(UKRAINE_list_html)



## Automating the scraping process: PDF - links second



for(i in 1:length(UKRAINE_links_PDF)) { 
  download.file(UKRAINE_links_PDF[[i]],
                destfile = str_c("./links-pdf/", i, ".pdf"), mode = "wb")
}



UKRAINE_list_PDF <- list.files("./links-pdf/", full.names = TRUE)



paste0(UKRAINE_list_PDF)



## Organizing data through the creation of empty "list" - objects



ta_number_html <- list()



content_html <- list()



date_html <- list()



## For-loop (automating procedure) to extract the necessary information (metadata) with respect to the variables I have selected 
## and the creation of a data-frame: 



for (i in seq_along(UKRAINE_list_html)) {
  
  UKRAINE_html_file <- read_html(UKRAINE_list_html[i])
  
  if (i == 2) {
    ell <- UKRAINE_html_file %>% 
      html_nodes("td[nowrap='nowrap']") %>%
      html_text()
    ta_number_html[i] <- ell
  } else if (i == 13) {
    twe <- UKRAINE_html_file %>% 
      html_nodes("td[nowrap='nowrap']") %>%
      html_text()
    ta_number_html[i] <- twe
  } else {
    ta_number_html[i] <- UKRAINE_html_file %>% 
      html_node("a[class='ring_ref_selected']") %>% 
      html_text()
  }
  
  date_html[[i]] <- UKRAINE_html_file %>% 
    html_node("td[class='doc_title']") %>% 
    html_text() %>% 
    str_extract("\\d{1,2}\\s[A-Z][a-z]{2,8}\\s\\d{4}") # extract date in "dd MMM yyyy" format
  
  content_html[[i]] <- UKRAINE_html_file %>%
    html_nodes("tr[class='contents']") %>% 
    html_text() %>%
    str_c(., collapse = "\n\n")
}



UKRAINE_html_df <- tibble(Date = unlist(date_html),
                  TA_number = ta_number_html,
                  Content = content_html) %>% 
  unnest(cols = c(Date, TA_number, Content)) 


## PDF section because the previous section was only the HTML - links



# Extract text from the first PDF document in the "links-pdf" folder within your working directory



pdf_file <- "links-pdf/1.pdf"



pdf_text <- pdf_text(pdf_file)



# Split the text into lines and extract the first 25 rows



pdf_lines <- strsplit(pdf_text, "\r\n")[[1]]



pdf_rows <- head(pdf_lines, n = 25)



# Print each row separately



for (i in 1:length(pdf_rows)) {
  print(pdf_rows[i])
} # it is page one where the import metadata is with respect to date and ta-number.



# Set the path to the PDF folder within your working directory



pdf_folder <- "links-pdf"



# Get a list of all PDF files in the folder



pdf_files <- list.files(pdf_folder, full.names = TRUE)



# initialize lists



date_pdf <- list()



ta_number_pdf <- list()



content_pdf <- list()



# loop through each PDF file



for (pdf_file in pdf_files) {
  
  pdf_text <- pdf_text(pdf_file)
  pdf_date <- NA
  pdf_ta_number <- NA
  
  # Find the date
  for (i in seq_along(pdf_text)) {
    if (grepl("European Parliament resolution of\\s+([0-9]{1,2}\\s+[[:alpha:]]+\\s+[0-9]{4})", pdf_text[i], perl=TRUE)) {
      pdf_date <- gsub(".*European Parliament resolution of\\s+([0-9]{1,2}\\s+[[:alpha:]]+\\s+[0-9]{4}).*", "\\1", pdf_text[i])
      break
    }
  }
  
  # Find the TA number
  for (i in seq_along(pdf_text)) {
    if (grepl("P9_TA\\(\\d{4}\\)\\d{4}", pdf_text[i])) {
      pdf_ta_number <- gsub(".*?(P9_TA\\(\\d{4}\\)\\d{4}).*", "\\1", pdf_text[i])
      break
    }
  }
  
  # Get the content
  pdf_content <- paste(pdf_text, collapse = "\n")
  
  # Add to the corresponding lists
  if (is.na(pdf_date) || is.na(pdf_ta_number)) {
    next
  } else {
    if (pdf_ta_number %in% names(content_pdf)) {
      content_pdf[[pdf_ta_number]] <- paste(content_pdf[[pdf_ta_number]], pdf_content, sep = "\n\n")
    } else {
      date_pdf[[pdf_ta_number]] <- pdf_date
      ta_number_pdf[[pdf_ta_number]] <- pdf_ta_number
      content_pdf[[pdf_ta_number]] <- pdf_content
    }
  }
}



# Print the first date, TA number, and content for each PDF document



for (i in 1:length(ta_number_pdf)) {
  print(paste("TA number:", ta_number_pdf[[i]][1]))
  print(paste("Date:", date_pdf[[i]][1]))
  print(paste("Content:", paste(content_pdf[[i]][1:25], collapse = "\n")))
}



UKRAINE_pdf_df <- tibble(
  Date = unlist(date_pdf),
  TA_number = unlist(ta_number_pdf),
  Content = unlist(content_pdf)
)



### combining data-frames: 



Ukraine <- rbind(UKRAINE_html_df, UKRAINE_pdf_df)



remove(ta_number_html, ta_number_pdf, content_html, content_pdf, date_html, date_pdf, content, date, ta_number, 
       UKRAINE_pdf_df, UKRAINE_html_file, UKRAINE_html_df, eleven, twelve, PDF_metadata)



library(dplyr)



library(lubridate)



Ukraine <- Ukraine %>% mutate(Date = dmy(Date))



Ukraine <- Ukraine %>% arrange(Date)



Ukraine



write.csv(Ukraine, "Ukraine.csv", row.names = FALSE)



### creating the Iraq data-frame



## Because there is not that many PR's provided by the EU with respect to Iraq, 
## we do not need to take as many steps to construct a data-frame. 



link1 <- "https://web.archive.org/web/20070213035323/http://www.europarl.europa.eu/omk/omnsapir.so/pv2?PRG=CALDOC&FILE=030130&LANGUE=EN&TPV=PROV&LASTCHAP=10&SDOCTA=5&TXTLST=1&Type_Doc=FIRST&POS=1&textMode=on" 



link2 <- "https://www.europarl.europa.eu/doceo/document/TA-5-2003-0222_EN.html?redirect"



link3 <- "https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=uriserv:OJ.L_.2003.046.01.0001.01.ENG"



# list objects



Date <- list()



Content <- list()



TA_number <- list()



# Extract the HTML content - link1



IRAQ_html <- read_html(link1)



# Extract date from the table



Date <- IRAQ_html %>%
  html_nodes("body > h3") %>%
  html_text()



# Define the character string with the date



date_string <- "European ParliamentTexts Adopted by ParliamentProvisional Edition : 30/01/2003"



# Extract the date using substring



date <- substring(date_string, nchar(date_string)-9, nchar(date_string))



# Convert the extracted string to a Date object



Date <- as.Date(date, format = "%d/%m/%Y")



# Print the date



Date



# Extract text from the <pre> element



Content <- IRAQ_html %>%
  html_nodes("body > form") %>%
  html_text()




# Extract TA number from the table



TA_number <- IRAQ_html %>%
  html_nodes("body > form > p.lien") %>%
  html_text()



Iraq1 <- tibble(
  Date = unlist(Date),
  TA_number = unlist(TA_number),
  Content = unlist(Content)
)



## Extract the HTML content - link2



# extract metadata from link2



page <- read_html(link2)



Date <- page %>% 
  html_nodes("td[class='doc_title']") %>% 
  html_text() %>% 
  str_extract("[0-9]{2}\\s[a-zA-Z]{3,}\\s[0-9]{4}") %>% 
  as.Date(format = "%d %B %Y")



TA_number <- page %>% 
  html_nodes("a[class='ring_ref_selected']") %>% 
  html_text()



Content <- page %>% 
  html_nodes("tr[class='contents']") %>% 
  html_text()



## add metadata to existing tibble called Iraq



Iraq2 <- tibble(Date = Date, TA_number = TA_number, Content = Content)



Iraq2 <- Iraq2 %>% slice(-2)



# extract metadata from link3



page <- read_html(link3)



# find the first occurrence of the date string



text <- page %>% html_text()



date_string <- str_extract(text, "\\d{1,2}\\s\\w+\\s\\d{4}")



Date <- dmy(date_string)



# extract the content



Content <- page %>% 
  html_nodes("#TexteOnly") %>% 
  html_text() %>% 
  trimws()



# replace metadata in existing tibble called Iraq



Iraq3 <- tibble(Date = Date, TA_number = "CR", Content = Content)



Iraq3 <- Iraq3 %>% slice(-2)



# combining all the data-frames: 



Iraq <- bind_rows(Iraq1, Iraq2, Iraq3)



remove(Iraq1, Iraq2, Iraq3)



Iraq <- Iraq %>% arrange(Date)



Iraq


# download the CSV file to my directory



write.csv(Iraq, "Iraq.csv", row.names = FALSE)



#### Pre-processing ####


### Packages



library(tidyverse)



library(quanteda)



library(tidytext)



library(ggdark)



library(quanteda.textplots)



library(quanteda.textstats)



library(ggthemes)



library(tm)


### Read-in my dataframes



Iraq <- read.csv("Iraq.csv")



Ukraine <- read.csv("Ukraine.csv")



### I will conduct a dual-pre-processing of the information within my corpora: Iraq and Ukraine. 



## visualizations of each data-frame before pre-processing:



# Iraq



Iraq_tokens <- Iraq %>% 
  unnest_tokens(output = token,
                input = Content)



Iraq_tokens %>% 
  count(token) %>% 
  slice_max(order_by = n,
            n = 50,
            with_ties = FALSE) # clear indication that the top 50 words are stopwords. 



Iraq_tokens <- Iraq %>%
  unnest_tokens(word, Content) %>% 
  count(word) %>% 
  arrange(desc(n))



Iraq_tokens %>% 
  head(20) %>% 
  arrange(desc(n)) %>% 
  ggplot(., aes(x = 1:20, y = n)) +
  geom_point() +
  geom_line(aes(group = 1)) +
  scale_y_continuous(trans = "log") +
  scale_x_continuous(trans = "log") +
  geom_smooth(method = "lm", se = FALSE) +
  ggrepel::geom_label_repel(aes(label = word)) +
  ggdark::dark_theme_classic() +
  labs(x = "Rankings (log)", y = "Frequency (log)", title = "Pre-preprocessing") # figure 3.1.1



# Ukraine



Ukraine_tokens <- Ukraine %>% 
  unnest_tokens(output = token,
                input = Content)



Ukraine_tokens %>% 
  count(token) %>% 
  slice_max(order_by = n,
            n = 50,
            with_ties = FALSE) # clear indication that the top 50 words are stopwords. 



Ukraine_tokens <- Ukraine %>%
  unnest_tokens(word, Content) %>% 
  count(word) %>% 
  arrange(desc(n))



Ukraine_tokens %>% 
  head(20) %>% 
  arrange(desc(n)) %>% 
  ggplot(., aes(x = 1:20, y = n)) +
  geom_point() +
  geom_line(aes(group = 1)) +
  scale_y_continuous(trans = "log") +
  scale_x_continuous(trans = "log") +
  geom_smooth(method = "lm", se = FALSE) +
  ggrepel::geom_label_repel(aes(label = word)) +
  ggdark::dark_theme_classic() +
  labs(x = "Rankings (log)", y = "Frequency (log)", title = "Pre-preprocessing") # equivalent to figure 3.1.1 but in the Ukraine case



# Creating a data-feature-matrix (DFM)



stopwords(kind = "en") #list of stopwords provided by quanteda



Stopwords1 <- c(
  "a", "about", "above", "after", "again", "against", "all", "am", "an", 
  "and", "any", "are", "aren't", "as", "at", "be", "because", "been", 
  "before", "being", "below", "between", "both", "but", "by", "can't", 
  "cannot", "could", "couldn't", "did", "didn't", "do", "does", "doesn't", 
  "doing", "don't", "down", "during", "each", "few", "for", "from", 
  "further", "had", "hadn't", "has", "hasn't", "have", "haven't", "having", 
  "he", "he'd", "he'll", "he's", "her", "here", "here's", "hers", "herself", 
  "him", "himself", "his", "how", "how's", "i", "i'd", "i'll", "i'm", 
  "i've", "if", "in", "into", "is", "isn't", "it", "it's", "its", "itself", 
  "let's", "me", "more", "most", "mustn't", "my", "myself", "no", "nor", 
  "not", "of", "off", "on", "once", "only", "or", "other", "ought", 
  "our", "ours", "ourselves", "out", "over", "own", "same", "shan't", 
  "she", "she'd", "she'll", "she's", "should", "shouldn't", "so", "some", 
  "such", "than", "that", "that's", "the", "their", "theirs", "them", 
  "themselves", "then", "there", "there's", "these", "they", "they'd", 
  "they'll", "they're", "they've", "this", "those", "through", "to", "too", 
  "under", "until", "up", "very", "was", "wasn't", "we", "we'd", "we'll", 
  "we're", "we've", "were", "weren't", "what", "what's", "when", "when's", 
  "where", "where's", "which", "while", "who", "who's", "whom", "why", 
  "why's", "with", "won't", "would", "wouldn't", "you", "you'd", "you'll", 
  "you're", "you've", "your", "yours", "yourself", "yourselves", "whereas", "eu", "including", 
  "eu's", "due", "since", "set", "can", "p", "c", "b", "oj", "european", "parliament", 
  "call", "food", "use", "calls", "inform", "shall", "ukraine", "ukrainian", "russia",
  "russian"
) # stopwords and other words I have found to be necessary to remove. 



ukraine_corpus <- corpus(Ukraine, text_field = "Content") 



iraq_corpus <- corpus(Iraq, text_field = "Content")



# Filtering out components with respect to filler, stopwords, punctuations etc. 



ukraine_tokens <- tokens(ukraine_corpus,
                      remove_numbers = TRUE,
                      remove_punct = TRUE,
                      remove_symbols = TRUE,
                      remove_separators = TRUE,
                      remove_url = TRUE,
                      verbose = TRUE
)



iraq_tokens <- tokens(iraq_corpus,
                         remove_numbers = TRUE,
                         remove_punct = TRUE,
                         remove_symbols = TRUE,
                         remove_separators = TRUE,
                         remove_url = TRUE,
                         verbose = TRUE
)



# in addition to removing the aforementioned elements, I have also filtered out words in relation to my objects stopwords and stopwords1


ukraine_tokens <- tokens_remove(ukraine_tokens, pattern = c(stopwords('en')))



ukraine_tokens <- tokens_remove(ukraine_tokens, pattern = Stopwords1)



iraq_tokens <- tokens_remove(iraq_tokens, pattern = c(stopwords('en')))



iraq_tokens <- tokens_remove(iraq_tokens, pattern = Stopwords1)



# creation of DFMs



ukraine_dfm <- dfm(ukraine_tokens)



iraq_dfm <- dfm(iraq_tokens)



textplot_wordcloud(ukraine_dfm) #wordcloud visualizations



textplot_wordcloud(iraq_dfm) #wordcloud visualizations



# Stemming 



ukraine_stem <- dfm_wordstem(ukraine_dfm, language = "en")



textplot_wordcloud(ukraine_stem)



ukraine_stem %>% tidy() %>%
  arrange(desc(count)) %>%
  head(20) %>%
  ggplot(aes(x = 1:20, y = count)) +
  geom_point() +
  scale_y_continuous(trans = "log") +
  scale_x_continuous(trans = "log")+
  geom_line(aes(group = 1)) +
  ggrepel::geom_label_repel(aes(label = term), max.overlaps = 100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggdark::dark_theme_classic() +
  labs(x = "Rankings (log)", y = "Frequency (log)", title = "Post-preprocessing") # equivalent to figure 3.1.2



iraq_stem <- dfm_wordstem(iraq_dfm, language = "en")



textplot_wordcloud(iraq_stem)



iraq_stem %>% tidy() %>%
  arrange(desc(count)) %>%
  head(20) %>%
  ggplot(aes(x = 1:20, y = count)) +
  geom_point() +
  scale_y_continuous(trans = "log") +
  scale_x_continuous(trans = "log") +
  geom_line(aes(group = 1)) +
  ggrepel::geom_label_repel(aes(label = term), max.overlaps = 100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggdark::dark_theme_classic() +
  labs(x = "Rankings (log)", y = "Frequency (log)", title = "Post-preprocessing") # figure 3.1.2



#### Thematic modelling ####



library(readr)      # Read data through C++



library(dplyr)      # Parse manipulation of data through C++



library(pbmcapply)  # Parallellization with progress bar



library(quanteda)



library(tidyverse)



library(tidytext)



library(rvest)


library(stm)



library(geometry)



library(wordcloud)



metadata_ukraine <- Ukraine[1:20, c("Date", "TA_number", "Content")]  #metadata with the aforementioned variables of interest



metadata_iraq <- Iraq[1:3, c("Date", "TA_number", "Content")] #metadata with the aforementioned variables of interest



### Iraq - four topics (decision based off of size)



iraq_stm <- stm(iraq_stem,
                K = 4, # assigned value
                prevalence = ~ factor(Date),
                data = Iraq,
                init.type = "Spectral",
                max.em.its = 50,  
                gamma.prior='L1') # running a topic model analysis where the decided number of topics is four




## downloading the topic model's object to directory



save(iraq_stm, file = "./iraq_stm") 



## loading in the object to avoid having to rerun the topic modelling x number of times



load("./iraq_stm") 



## distribution of topics according to size and proportion (figure 4.1.1)



plot(iraq_stm, labeltype = "frex") 



## Get the top words associated with each topic


top_words_iraq <- labelTopics(iraq_stm, n=10)



### Ukraine



# running a topic model analysis where the decided number of topics is 4



ukraine_stm <- stm(ukraine_stem,
                   K = 4, # assigned value
                   prevalence = ~ factor(Date),
                   data = Ukraine,
                   init.type = "Spectral",
                   max.em.its = 50,  
                   gamma.prior='L1') 



# downloading the topic model's object to directory



save(ukraine_stm, file = "./ukraine_stm") 



#loading in the object to avoid having to rerun the topic modelling x number of times



load("./ukraine_stm") 



# distribution of topics according to size and proportion (figure 4.1.2)



plot(ukraine_stm, labeltype = "frex") 



# Get the top words associated with each topic



top_words_ukraine <- labelTopics(ukraine_stm, n=10)



#### Text - Analysis ####



### identifying response-related words for the invasions of Ukraine and Iraq under economic and diplomatic means (foreign policy)




response_related_words <- c("militari", "action", "support", "sanction", "secur", "war",
                            "defenc", "aid",
                            "law", "respons", "polici", "condemn", "denounc",
                            "cooper")




### I am trying to define a function to get the frequency of response-related words in my stemmed DFMs



get_response_freq <- function(dfm, response_words) {
  dfm_response <- dfm_select(dfm, pattern = response_words, valuetype = "fixed")
  response_freq <- colSums(as.matrix(dfm_response))
  return(response_freq)
}



## getting the frequency of response-related words in each stemmed DFM



response_freq_iraq <- get_response_freq(iraq_stem, response_related_words)



response_freq_ukraine <- get_response_freq(ukraine_stem, response_related_words)



## Creating a matrix of zeros with the same number of rows as the longest response frequency vector



max_rows <- max(length(response_freq_iraq), length(response_freq_ukraine))



zero_matrix <- matrix(0, nrow = max_rows, ncol = 2)



## Replacing the corresponding rows in the zero matrix with the response frequency vectors



zero_matrix[1:length(response_freq_iraq), 2] <- response_freq_iraq



zero_matrix[1:length(response_freq_ukraine), 1] <- response_freq_ukraine



## creating a data frame with the combined response frequencies



response_freq_combined <- data.frame( word= response_related_words,
  ukraine_freq = zero_matrix[, 1],
  iraq_freq = zero_matrix[, 2]
)



## calculating the difference in frequencies



response_freq_combined$difference <- response_freq_combined$iraq_freq - response_freq_combined$ukraine_freq



## View the response frequency comparison



response_freq_combined



ggplot(response_freq_combined, aes(x = reorder(word, -ukraine_freq), y = ukraine_freq, fill = "ukraine_freq")) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_bar(aes(y = iraq_freq, fill = "iraq_freq"), stat = "identity", position = "dodge") +
  labs(x = "Word", y = "Frequency") +
  scale_fill_manual(values = c("ukraine_freq" = "blue", "iraq_freq" = "red")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # figure 4.1.3



### key words extraction



## Extract sentences containing any form of the word "sanction" - ukraine



sentences_ukraine_sanction <- str_extract_all(Ukraine$Content, "(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?sanction[^\\.\\?]*?(?=\\.|\\?|$)|(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?sanctions[^\\.\\?]*?(?=\\.|\\?|$)|(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?sanctioned[^\\.\\?]*?(?=\\.|\\?|$)")



sentences_iraq_sanction <- str_extract_all(Iraq$Content, "(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?sanction[^\\.\\?]*?(?=\\.|\\?|$)|(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?sanctions[^\\.\\?]*?(?=\\.|\\?|$)|(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?sanctioned[^\\.\\?]*?(?=\\.|\\?|$)")



## Extract sentences containing anything related to the word 'action' - Iraq & Ukraine



sentences_iraq_action <- str_extract_all(Iraq$Content, "(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?action[^\\.\\?]*?(?=\\.|\\?|$)|(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?actions[^\\.\\?]*?(?=\\.|\\?|$)|(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?sanctioned[^\\.\\?]*?(?=\\.|\\?|$)")
    # here the EU does make it clear that the war in Iraq was against international law



sentences_ukraine_action <- str_extract_all(Ukraine$Content, "(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?action[^\\.\\?]*?(?=\\.|\\?|$)|(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?actions[^\\.\\?]*?(?=\\.|\\?|$)|(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?sanctioned[^\\.\\?]*?(?=\\.|\\?|$)")
    # here the EU makes an exhaustive list on how it will 'act' in response to the Russian invasion of Ukraine



## Extract sentences containing anything related to the word 'law' - Iraq & Ukraine



sentences_ukraine_law <- str_extract_all(Ukraine$Content, "(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?law[^\\.\\?]*?(?=\\.|\\?|$)|(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?lawful[^\\.\\?]*?(?=\\.|\\?|$)|(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?unlawful[^\\.\\?]*?(?=\\.|\\?|$)")



sentences_iraq_law <- str_extract_all(Iraq$Content, "(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?law[^\\.\\?]*?(?=\\.|\\?|$)|(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?lawful[^\\.\\?]*?(?=\\.|\\?|$)|(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?unlawful[^\\.\\?]*?(?=\\.|\\?|$)")



## Extract sentences containing anything related to the word 'defense' - Iraq & Ukraine



sentences_ukraine_defence <- str_extract_all(Ukraine$Content, "(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?defense[^\\.\\?]*?(?=\\.|\\?|$)|(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?defending[^\\.\\?]*?(?=\\.|\\?|$)|(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?defend[^\\.\\?]*?(?=\\.|\\?|$)")



sentences_Iraq_defence <- str_extract_all(Iraq$Content, "(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?deferral[^\\.\\?]*?(?=\\.|\\?|$)|(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?defending[^\\.\\?]*?(?=\\.|\\?|$)|(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?defender[^\\.\\?]*?(?=\\.|\\?|$)")



## Extract sentences containing anything related to the word 'aid' - Iraq & Ukraine



sentences_ukraine_aid <- str_extract_all(Ukraine$Content, "(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?aid[^\\.\\?]*?(?=\\.|\\?|$)|(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?aiding[^\\.\\?]*?(?=\\.|\\?|$)|(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?aided[^\\.\\?]*?(?=\\.|\\?|$)")



sentences_iraq_aid <- str_extract_all(Iraq$Content, "(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?aid[^\\.\\?]*?(?=\\.|\\?|$)|(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?aiding[^\\.\\?]*?(?=\\.|\\?|$)|(?<=^|\\.|\\?)\\s*[A-Z][^\\.\\?]*?aided[^\\.\\?]*?(?=\\.|\\?|$)")



#### Sentiment modelling #### 



### Packages



library(tidytext)



library(dplyr)



library(quanteda)



library(ggplot2)



library(remotes)



library(qdapDictionaries)



library(syuzhet)


### converting the stem DFMs into data-frames



ukraine_df <- as.data.frame(as.matrix(ukraine_stem))



iraq_df <- as.data.frame(as.matrix(iraq_stem))



### Convert row names to a column



ukraine_df$word <- rownames(ukraine_df)



iraq_df$word <- rownames(iraq_df)



### Join with Bing lexicon



bing_lex <- get_sentiments("bing")



ukraine_sentiment <- inner_join(ukraine_df, bing_lex, by = "word")



iraq_sentiment <- inner_join(iraq_df, bing_lex, by = "word")


### Calculate sentiment scores



ukraine_score <- sum(ukraine_sentiment$sentiment == "positive") - sum(ukraine_sentiment$sentiment == "negative")



iraq_score <- sum(iraq_sentiment$sentiment == "positive") - sum(iraq_sentiment$sentiment == "negative")



response_sentiment <- inner_join(response_freq_combined, bing_lex, by = "word")



response_sentiment_score <- sum(response_sentiment$sentiment == "positive") - sum(response_sentiment$sentiment == "negative")



### Print results



ukraine_sentiment_score <- cat("Ukraine sentiment score:", ukraine_score, "\n")



iraq_sentiment_score <- cat("Iraq sentiment score:", iraq_score, "\n")
















