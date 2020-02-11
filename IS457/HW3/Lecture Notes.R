#### Lecture Notes ####

# Data Frame- Ordered Collection of vectors *all same length*
# List- Ordered collection of objects (vector, matrix, data frame)

# Type 1 Paradigm
#   1. Research Question
#   2. Collect Data to Answer Question
#   3. Answer Question
# Statistical Methodology Designed Here

# Type 2 Paradigm
#   1. Start with dataset(s)
#   2. Look in the data to discover research question(s)
#   3. Use the same data again to answer question
# Statistical Methodology designed for Type I can fail if applied in Type II
# If you develop research question from data, you have to be careful

#[ ] for data frame
#[[ ]] index one more layer within 

load(url("http://www.stanford.edu/~vcs/StatData/rainfallCO.rda"))
rain[[1]]
rain[1:2]
sapply(rain, mean) #output vector
lapply(rain, mean) #output list
#apply is for matrices
#tapply() for "tables", i.e. ragged arrays as vectors, analyze conditionally

# Functions
InToCm = function(x) x*2.54
# or
InToCm = function(x) 
{
  x*2.54
}
y=c(1,2,3)
InToCm(y)


numYears = function(y) {
  length(unique(floor(y)))
}
sapply(rain, numYears)