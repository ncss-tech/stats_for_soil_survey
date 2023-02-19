# implicit vector creation from a sequence from 1:10
1:10

# numeric vector example: clay percent values
clay <- c(10, 12, 15, 26, 30)

# 'c()' is the concatenate function
# values are concatenated into an object we've called 'clay' by assigning the concatenate result to 'clay' using '<-' or '='
# print the values of 'clay' by typing it into the R console followed by enter
clay

# character vector: taxonomic subgroup
subgroup <- c("typic haplocryepts","andic haplocryepts","typic dystrocryepts")  
subgroup

# logical vector: diagnostic feature presence/absence
# note that TRUE and FALSE must be capitalized
andic <- c(FALSE,TRUE,FALSE) 
andic

# 2nd and 4th elements of vector 'clay' from above
clay[c(2, 4)] 

# 1st and 3rd elements of vector 'subgroup' from above
subgroup[c(1, 3)]

# everything but the first element of vector 'andic'
andic[-1]

# re-order clay values using a sequence from 5 to 1
clay[5:1]

# clay values from above
# divide by 100, notice that iteration is over all the elements of 'clay' in the result
clay / 100

# search for the text 'dyst' in elements of 'subgroup'...more on this pattern matching process in chapter 2a!
grepl('dyst', subgroup)

# multiply two vectors of the same length
c(5, 5) * c(1, 2)

# be careful, operations on vectors of different length results in "recycling"!
# this is helpful at times, but can be a common source of confusion
10 * c(1, 2)
c(1, 10, 100) + c(1, 2)

# what is actually happening here in this recycling of vectors?
# taking the following elements from above, first+first, second+second, third+first
# 1+1=2, 10+2=12, 100+1=101

# Take our two character and logical vectors we created above and convert them into a more useful dataframe.
# we'll use the data.frame() function to glue these two vectors together into object 'd'
d <- data.frame(subgroup, andic)
d

# get the column names of a dataframe
names(d)
# we can use 'names()' and 'c()' to rename the columns in a dataframe
names(d) <- c('tax_subgroup', 'andic.soil.properties')
d

# format: dataframe_name[rows, columns]
d[1, ] # first row of dataframe
d[, 1] # first column of dataframe
d[2, 2] # second row, second column

# In dataframes we can also use the '$' symbol to reference vector columns within a specific dataframe object
# format: dataframe_name$column_name
d$tax_subgroup

# Other useful functions for checking objects and working with dataframes
# the 'str()' function will show you the structure of an object and the data types of the vectors within it
str(d)
# 'class()' will tell you the object type or data type
class(d)
class(d$tax_subgroup)
# use 'colnames()' to get a vector of column names from a dataframe
colnames(d)
# ncol and nrow provide column and row dimensions
ncol(d)
nrow(d)

# building on what we've learned above, we can use the square bracket notation on a dataframe to re-order columns
d <- d[ ,c('andic.soil.properties', 'tax_subgroup')]
d
# another way we could do this is to use the column indexes within the concatenate function
# although the column indexes are not visible each column has an index number assigned to it
d <- d[ , c(2,1)]

# generate a factor representation of the characters contained in the word 'pedology'
# also setting the range of possibilities to the letters of the alphabet
x <- factor(substring("pedology", 1:8, 1:8), levels = letters)
# note that the object 'x' knows that there are 26 possible levels
x
# another way to do the same thing - the substring() is parsing the string 'pedology' for us in the above example
#x <- factor(c('p', 'e', 'd', 'o', 'l', 'o', 'g', 'y'), levels=letters)

# what happens when the levels are not specified?
factor(substring("mississippi", 1:11, 1:11))
# the string collapses to repeating levels only

# make a list with named elements
l <- list('favorite shovels'=c('sharpshooter', 'gibbs digger', 'auger', 'rock bar', 'backhoe!'),
          'food'=c('apples', 'bread', 'cheese', 'vienna sausages', 'lutefisk'),
          'numbers I like'=c(12, 1, 5, 16, 25, 68),
          'chips I like' =c('plantain', 'tortilla', 'potato', '10YR 3/2'),
          'email messages deleted'=c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE))
# check the list
l

# access the first element of the list, note the double square brackets
l[[1]]

# access the a specific element of the list, note the double square brackets denote which element in the list plus additional single brackets for the position within the element
l[[1]][4]

# access the element by name, for example 'food'
l[['food']]

# convert a dataframe into a list
as.list(d)

# a list of lists
list.of.lists <- list('pedon_1'=list('top'=c(0,10,25,55), 'bottom'=c(10,25,55,76), 'pH'=c(6.8,6.6,6.5,6.4)))
list.of.lists

# convert list of elements with equal length into a data.frame
as.data.frame(list.of.lists)

# make a 5x5 matrix of 0's
m <- matrix(0, nrow=5, ncol=5)
m

# operations by scalar (single value) are vectorized
m <- m + 1
m * 5

# notice that the result of m * 5 is displayed but because the output wasn't assigned to 'm' it remains unchanged from m + 1
m

# now use the square bracket notation to get / set values within the matrix: m[row, col]
# set row 1, col 1 to 0
m[1,1] <- 0
m

# access diagonal and upper/lower triangles, useful for chapter 5
m[upper.tri(m)] <- 'U'
m[lower.tri(m)] <- 'L'
diag(m) <- 'D'
m

# many functions return matrix objects
# create a matrix of the sequences 1:10 and 1:10, then multiply every combination by applying a function
outer(1:10, 1:10, FUN='*')
