
#!/usr/bin/env Rscript

#Title: Webscraping - IMDb Movie Title Ratings
#Student: Joel Gayle
#University: University of the Cumberlands
#Course name:  Analyzing and Visualizing Data
#Course number: ITS-530-02
#Professor: Dr. Kathy McClure  
#Date: 1/11/2020


args = commandArgs(trailingOnly=TRUE)

#function getMovieRating
#input: movie title number String
#output: The IMDb numeric rating for the film

getMovieRating = function(movieTitleNumber) {
  baseUrl = "http://www.imdb.com"
  # Store web page
  url <- paste(baseUrl, movieTitleNumber, "/", sep="")
  #print(url)
  moviePage <- read_html(url)
  #Scrape the website for the movie rating
  rating <- moviePage %>% 
    html_nodes("strong span") %>%
    html_text() %>%
    as.numeric()
  #Print the rating if it exists
  if (length(rating))
    print(rating)
  else print("No Rating Available")
}

#function getMovieTitleNumber
#input: a movie title String
#output: Results of the IMDb search for the movie title and the ratings for each film

getMovieTitleNumber = function(movieTitle) {
  
  #base url of the imdb search function
  baseUrl = "https://www.imdb.com/find?q="
  
  #combine the base url with the movie title to search
  #save the results page
  url <- paste(baseUrl, movieTitle, sep="")
  
  #find the links within the results section of the page
  searchPage <- read_html(url)
  titleNumbers <- searchPage %>%
    html_nodes(".result_text") %>%
    html_nodes(xpath = "./a") %>% 
    html_attr("href")
  
  #find the title names within the link elements
  titleNames = searchPage %>%
    html_nodes(".result_text") %>%
    html_nodes(xpath = "./a") %>%
    html_text() 
  
  #filter the results to only the links containing the searched title
  numbers <- grep("title", titleNumbers, value=TRUE)
  x = 1
  
  #loop through the results and print the title and ratings
  for(i in numbers)
  {
    #print(x)
    print(titleNames[x])
    getMovieRating(i)
    x = x + 1
  }
}


library(rvest)

# Entry point for the script, expects one argument for the movie name 
# test if there is one argument: if not, return an error
if (length(args)!=1) {
  stop("One argument must be supplied (Movie Name)", call.=FALSE)
} else if (length(args)==1) {
  # do the work
  movieTitle = args[1]
  print(paste("Looking up IMDb ratings for: ", movieTitle, sep=""))
  movieTitle <- gsub(" ", "+", movieTitle, fixed = TRUE)
  getMovieTitleNumber(movieTitle)
  
}

