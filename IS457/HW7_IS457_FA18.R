# Do not remove any of the comments. These are marked by #

# HW 7 - Due Monday Nov 5, 2018 in moodle and hardcopy in class. 

# (1) Upload R file to Moodle with filename: HW7_IS457_YOURCLASSID.R
# (2) Do not remove any of the comments. These are marked by #

## For this assignment we will work with regular expressions. 
## First, we will have some warmup questions, then proceed
## to learn some interesting things about Aesop's fables.

# Part 1.  Regular Expressions Warmup (12 pt)

## Basic Regex

## Q1. Find words in test vector test_1 which start with lowercase 'e' using grep. What does grep return? (2 pt)

test_1 = c("wireless", "car", "energy", "2020", "elation", "alabaster", "Endoscope")

## Your code here
grep("^e", test_1)

## Your explanation here
#Grep returned a vector of length which was the number of items which began with e, and the two numbers tell us the position of the two objects that begin with e. 


## Q2. Find characters which can be a password with ONLY letters and numbers. (1 pt)

test_2 = c("bb1l9093jak", "jackBlack3", "the password", "!h8p4$$w0rds", "wiblewoble", "ASimpleP4ss", "d0nt_use_this")

## Your code here
grep("^[[:alnum:]]*$", test_2)


## Q3. Find Email addresses of the form letters@letters.xxx (1 pt)
## Here "xxx" means any alpha numeric characters with length of 3.
## Letters can be any alpha numeric characters of any length. Letters before "@" can also be along with the underscore.

test_3 = c("wolf@gmail.com", "little_red_riding_hood@comcast.net", "spooky woods5@swamp.us", "grandma@is.eaten", "the_ax@sbcglobal.net")

## Your code here
grep("[[:alpha:]]+@[[:alpha:]]+.[[:alpha:]]{,3}$", test_3)


## Capture Groups
## This is a method to grab specific text within a given match.
## This is a very useful technique to get specific bits of text quickly.
## We will use a series of steps to extract the domain names from properly formatted email addresses.

## Q4. Use regexec() to execute a regular expression to find properly formatted email addresses in test_3. Save it as test_3_reg_exec. 
## This time, we will allow domain names of the form letters.letters. i.e. addresses like 'test.us' are now allowed.(1 pt)

## Your code here
test_3_reg_exec<-regexec("[[:alpha:]]+@[[:alpha:]]+.[[:alpha:]]{1,}$", test_3)
test_3_reg_exec

## Q5. What type of object is test_3_reg_exec? What type of information does it contain? (2 pt)

## Your code here
class(test_3_reg_exec)

## Your explanation here
#"test_3_reg_exec" is a list. It contains information on which of the items in the vector match the pattern specified, and information of the starting position and length. 


## Q6. Use regmatches() to get a list of the text matches from test_3_reg_exec. Call this 'reg_match_list'.
## What is the class of reg_match_list? and what is the format? (4 pt)

## Your code here
reg_match_list <- regmatches(test_3, test_3_reg_exec)
class(reg_match_list)
reg_match_list

## Your explanation here
#reg_match_list is a list which contains the original objects that we were trying to identify with regexec. Each resonse it the nth item in the list that it was in the original character vector, and the one response that doesn't match is "character(0)".


## Q7. Use reg_match_list() to get a vector of matched domain names in Q6. Name this vector 'domain names'. (3 pt)

## Your code here
`domain names`<-unlist(reg_match_list, use.names = F)
`domain names`<-gsub(".*@|\\..*", "\\1", `domain names`)
`domain names`

# Part 2.  Aesop's Fables 

## We will now look at a text file of aesop's fables. We will first need to process the data to get it into a form we can use.
## We can then look at interesting properties like the number of words in each fable.

## Q8. Use readLines() to load the aesop fable data from the aesop-fables.txt file you can find in moodle. 
## Name it aesop_data. MAKE SURE to use the encoding 'UTF-8'. (1 pt)

## Your code here
setwd("~/IS457")
aesop_data<-readLines("aesop-fables.txt", encoding = 'UTF-8')


## Q9. What is the format of aesop_data? How is the book formatted? How might we use this formatting to our advantage? (3 pt)

## Your explanation here
#The format of aesop_data is that each fable is organized under a chapter name. We could use these individual chapter names to extract individual fables or chunks of them. 


## Q10. Let's take a look of fables using the table of contents.
## First, find the start point and end point of the table of content using grep() and specific header names in the file.
## Then subset only those lines which are from the table of contents.
## Save the fable titles in a character vector.
## Finally, count the number of non-empty lines in your subset. Print out the number.(5 pt)

## Your code here
grep("CONTENTS", aesop_data)
grep("LIST OF ILLUSTRATIONS", aesop_data)
table_of_contents<-aesop_data[196:770]
fable_titles<-table_of_contents[table_of_contents!=""&table_of_contents!="LIST OF ILLUSTRATIONS"&table_of_contents!="CONTENTS"]
length(fable_titles)

## Q11. Separate out all the fables in the file.
## The process is similar to Q10, find the start point and end point. (3 pt)
## Call this fable_data. Here do not remove the titles or empty lines.

## NOTE: Notice that, in this text file, "AESOP'S FABLES" is sometimes shown as "ÆSOP'S FABLES", after you find the lines
## you want to extract information from, make sure you read the text carefully. 
## (if you need to use it, just a simple copy or paste will work).

## Your code here



## Q12. How do you know when a new fable is starting? (1 pt)

## Your explanation here
#You know a new fable is starting because there are four blank lines, the title of the new fable, two blank lines, and then the fable occurs. 


## Q13.
## We will now transform this data to be a bit easier to work with.
## Fables always consist of a body which contains consequtive non-empty lines which are the text of the fable.
## This is sometimes followed by a 'lesson' (summary) statement whose lines are consecutive but indented by four spaces.
## We will create a list object which contains information about each fable.

## Get the start positions of each fable in fable_data (how you answer Q12?). (3 pt) 
## Hint: Look at the title vector you created in Q10, what does it include (other than letters?)

## Your code here



## Q14. Transform the fables into an easy-to-reference format (data structure).
## First create a new list object named 'fables'. 
## Each element of the list is a sublist that contains two elements ('text' and 'lesson').

## For each fable, merge together the separate lines of text into a single character element.
## That is, one charactor vector (contains all sentences) for that fable.
## This will be the 'text' element in the sublist for that fable.

## If the fable has a lesson, extract the statement into a character vector (also remove indentation).
## This will be the 'lesson' element in the sublist for that fable. (10 pt)

## Your code here



## Q15. How many fables have lessons? (2 pt)

## Your code here



## Q16. Add a character count element named 'chars' and a word count element named 'words' to each fable's list. (3 pt)
## Use the following function to count words:

word_count = function(x) {
  return(lengths(gregexpr("\\W+", x)) + 1)  # words separated by space(s)
}

## Your code here


## Q17. Create separate histograms of the number of characters and words in the fables. (10 pt)
## Recall the graphics techniques you learned before.
## Your code here



## Q18. Lets compare the fables with lessons to those without.
## Extract the text of the fables (from your fables list) into two vectors. One for fables with lessons and one for those without. (4 pt)

## Your code here



## Q19. Remove all non alphabetic characters (except spaces) and change all characters to lowercase. (3 pt)

## Your code here



## Q20. Split the fables from Q19 by blanks and drop empty words. Save all the split words into a single list for each type of fable.
## Name them token_with_lessons and token_without_lessons. Print out their lengths. (5 pt)

## Your code here:



## Q21. Calculate the token frequency for each type of fable. (2 pt)

## Your code here:



## Q22. Carry out some exploratory analysis of the data and token frequencies.
## For example, find the words associated fables with lessons. What are distribution patterns for term frequencies?

## Use wordcloud function in wordcloud package to plot your result. What are your observations? (10 pt)

## Hint: you'll want to include important words but not stopwords (we provided a list below) into your plot.
## What are important words? we have token_with(out)_lessons from Q20, think relative high frequency (use quantile() to help you decide).
## so, start by creating a table of token frequency; filter out low frequency words and stopwords.

mask_word = c("by", "as", "a","their", "which", "have", "with", "are", "been", "will", "we", "not",
                "has", "this", "or", "from", "on", "i", "the","is","it","in","my","of","to",
                "and","be","that","for","you","but","its","was")
library(wordcloud)

## Your code here:



## Your explanation here:

