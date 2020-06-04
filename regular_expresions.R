x <- c("apple","banana","pear")
str_extract(x,"ap")
str_detect(x,"ap")
str_extract(x,".a.")
str_extract(x,"a.")

str_extract(c("abc","a.c","bef"),"a\\.c")

x <- "a\\b"
writeLines(x)

x <- c("a.b.c.d","aeb")
empieza_con <- "a.b"
paste0("^",empieza_con)
str_detect(x,paste0("^",empieza_con))
paste0("^\\Q",empieza_con,"\\E")
str_detect(x,paste0("^\\Q",empieza_con,"\\E"))

str

x <- "a\u0301"
writeLines(x)

x <- c("a.b.c.d","aeb","a\u0301")
x
str_c(x,"__")
x <- append(x,NA)
x
x <- str_c(x,"__")
x



x <- c("Apple", "Banana", "Pear")
str_sub(x,1,3)

str_view(x,"an")
str_view(x,".a.")
str_match(stringr::words,"^y") # words that start with y
str_detect(stringr::words,"^y")

# grouping and backreferences
# ===========================
str_view(fruit,"(^co)\\1")
str_view(fruit,"(.)[a-z](.)\\2")
str_view(fruit,"(.).\\1.\\1")

#subsetting a vector using a regexp
# =================================
words # vector of commong words
fruit # vector of fruits
words[str_detect(words,"^y")]
fruit[!str_detect(fruit,"[aeiou]$")] #fruits that do not end in vowel

#filtering a dataframe with a string variable
# ============================================
df <- tibble(word=words,i=seq_along(word))
df%>% filter(str_detect(word,"x$"))


# counting words
# ==============
df%>% mutate(
        vowels=str_count(word,"[aeiou]"),
        consonants=str_count(word,"[^aeiou]")
      )

# extracting actual matches
# =========================
sentences # a vector of 720 sentences
# we want to find all sentences that include a conlour in the list
colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colour_match <- str_c(colours, collapse = "|")
colour_match

# now we can select the sentence that has a colour
has_colour <- str_subset(sentences,colour_match)
has_colour # sentences that have colour
# and the colors
matches <- str_extract(has_colour,colour_match)
matches

# however note that str_extract ONLY extracts the first match
# there are some sentences that include more than one match
more <- sentences[str_count(sentences, colour_match) > 1]
str_view_all(more, colour_match)
# to extract all then we need to use str_extract_all
str_extract_all(more, colour_match)
str_extract_all(more, colour_match, simplify = TRUE) # now we obtain the same info as a matrix

# grouped matches
# =========================

# imagine we want to extract nouns from the sentences. As a heuristic, we’ll look 
# for any word that comes after “a” or “the”.

noun <- "(a|the) ([^ ]+)" 
has_noun <- sentences %>% 
  str_subset(noun)%>% head(10)

has_noun
has_noun%>%str_extract(noun) # str_extract gives us the complete match
has_noun%>%str_match(noun) # str_match gives each individual component in a matrix

# if data is in a tibble it is easier to use tidyr::extract()
tibble(sentence=sentences) %>%
  tidyr::extract(
    sentence, c("article","noun"), "(a|the) ([^ ]+)", 
    remove=F
  )

# Replacing matches
# =========================

x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")
str_replace_all(x, "[aeiou]", "-")


# With str_replace_all() you can perform multiple replacements by supplying a named 
# vector:

x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))

head(sentences)
sentences %>% 
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>% 
  head(5)

# Splitting
# =========================

sentences %>%
  head(5) %>% 
  str_split(" ")

# to rearrange in matrix form
sentences %>%
  head(5) %>% 
  str_split(" ",simplify = T)

#limiting the number of items the matrix returns
sentences %>%
  head(5) %>% 
  str_split(" ", n=2, simplify = T)

#limiting the number of items when fields are defined

fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
fields %>% str_split(": ", n = 2, simplify = TRUE)

# Instead of splitting up strings by patterns, 
# you can also split up by character, line, sentence and word boundary()s:

x <- sentences[1]
str_extract_all(x, boundary("word"))

# working with a length 1 vector to extract the first element  
"a|b|c|d" %>% 
  str_split("\\|")

# Find matches
#================

#str_locate() and str_locate_all() give you the starting and ending 
#positions of each match. These are particularly useful when none 
#of the other functions does exactly what you want. You can use 
#str_locate() to find the matching pattern, str_sub() to extract and/or modify them.


# Other types of patterns
#========================

# any pattern that is a string is wrapped into a call to regex()
# 
str_view(fruit,"ana")
str_view(fruit,regex("ana"))

# regex has many options
?regex

# one is fixed that operates at low level and runs faster than regex
microbenchmark::microbenchmark(
  fixed = str_detect(sentences, fixed("the")),
  regex = str_detect(sentences, "the"),
  times = 20
)

# Other uses of regex
#========================

# apropos searches objects abvailable form teh global environment
apropos("replace")

# dir lists all files in a directory

