# Do not remove any of the comments. These are marked by #

# HW 8 

# (1). Please upload R code and report to Moodle 
#      with filename: HW8_IS457_YourClassID.


## Important: Make sure there is no identifying information on your printout, including name, username etc. 
## Only include your class ID on there.

### ClassID:

### For this assignment, you will extract useful information from HTML and use Google Earth for data visualization. 
### The LatLon.rda file containing the country geographic coordinate is uploaded to Moodle.
### Look at detail instructions for the assignment in hw8_Intro.pdf.


### Part I. Create the data frame from HTML file (20 pts)

### Q1. Load LatLon.rda,
### install and load XML and RCurl libraries.

### Your code here
install.packages("XML")
install.packages("RCurl")
library(XML)
library(RCurl)
setwd("~/IS457")
load("LatLon.rda")

### Q2. Download the html file from the url below, and parse it to html_text. (2 pts)
### Open the website and read it before coding.
### We will be working on the data from one of the tables: United Nations 2016-2017 global population data

url = "https://en.m.wikipedia.org/wiki/List_of_countries_by_population_(United_Nations)"

### Your code here
url_data<-getURL(url)
UNhtml<-htmlParse(url_data, asText = TRUE)

### Q3. Read the tables in html_text with the readHTMLTable() function, set the 1st row as header.
### Now you should have a list object of 3 data frames, of which one is the table of population data.
### Coerce the table of population data to a data frame named "population". (2 pts)

### Your code here
UNhtml_tables <- readHTMLTable(UNhtml, header = TRUE)
population<-UNhtml_tables[[2]]

### Q4. Let's simplify the data frame.
### Remove all other columns except country name, 2016 population, and 2017 population.
### Rename the 3 columns "Country", "Population_2016" and "Population_2017". (2 pts)

### Your code here
poptable<-population[c(2,5,6)]
names(poptable)<-c("Country", "Population_2016","Population_2017")

### Q5. Recall our regular expression lessons.
### In the population data frame, some country names have annotations at the end.
### Using regular expressions, remove all the annotations in country names. (e.g. change "China[a]" to "China") (4 pts)
### (Hint: Combine apply family functions and string split methods), then convert the country names to uppercase. (1 pt)
### Show the first 5 rows of your new population data frame. (1 pt)

### Your code here
poptable$Country<-sapply(poptable$Country, function(x){gsub(pattern = "\\[.*",replacement = "", x)})
poptable$Country<-toupper(poptable$Country)

### Q6. Now merge LatLon with population by country to create a data frame named AllData. (2 pts)
### It should have 6 columns: Country, Code, Latitude, Longitude, Population_2016 and Population_2017.

### Your code here
AllData <- merge(poptable, LatLon, by="Country")


### Q7. Finally, convert the population data to numeric values, and
### calculate the 2016-2017 growth rate percentage of population by country,
### and add the growth rate to AllData as a new column named "Growth". (3 pts)
### (Hint: growth rate percentage = (population in 2017 / population in 2016 - 1)*100)
### Show the last 5 rows of your AllData data frame. (1 pt)

### Your code here
AllData$Population_2016 <- as.numeric(gsub(",","", as.character(AllData$Population_2016)))
AllData$Population_2017 <- as.numeric(gsub(",","", as.character(AllData$Population_2017)))
AllData$Growth <- (AllData$Population_2017 / AllData$Population_2016 - 1)*100

### Part II.  Create a KML document for google earth visualization 

### First take a look at the file on moodle: HW8_Intro.pdf
### It shows the structure of the KML file which we will create next.

### Q8. Let's start with creating the base of the KML document.
### Create a base document named doc1. (1 pt)
### Then create nodes "kml" and "document". (2 pts)
### (Hint: Check arguments "doc" and "parent" to make the nodes connected)

### Your code here
doc1 <- newXMLDoc()
kmlnode <- newXMLNode("kml", doc=doc1)
documentnode <- newXMLNode("document", parent = kmlnode)


### Q9. According to the KML tree in HW8_Intro.pdf, you can add many placemark nodes with parent "Document".
### The addPlacemark() function below can be used to add one placemark to your file in each call.
### Explain what each line does. (4 pts)

addPlacemark = function(lat,lon,country,code,pop16,pop17,growth,parent){
  pm = newXMLNode("Placemark",attrs=c(id=code),parent=parent)
  newXMLNode("name",country,parent=pm)
  newXMLNode("description",paste(country,"\n population_2016: ",pop16,"\n population_2017: ",pop17,"\n growth: ",growth,sep =""),parent=pm)
  newXMLNode("Point",newXMLNode("coordinates",paste(c(lon,lat,0),collapse=",")),parent=pm)
}

### Your answer here
#Line one creates the function with all of the inputs. The next line makes a node that contains the country code as an ID. The next line makes a child node of PM that holds information on the country name. The next line makes a child node of PM that countains country information, and the final line makes a child node of PM that contains the coordinates of the country. 

### Q10. Now let's create the KML file.
### Take doc1 as your base, then use addPlacemark() to create a KML file. (5 pts)
### Save it as "Part2.kml" using the saveXML() function. (1 pt)
### (Hint: First find the root of your base, then add placemark nodes to it in a for loop)
### Open the KML document in Google Earth. (You will need to install Google Earth.) 
### If you are doing correctly, it should have pushpins for all the countries.

### Your code here
root <- xmlRoot(doc1)
root2 <- root[["document"]]
for(i in 1:length(AllData$Country)){
  addPlacemark(AllData$Latitude[i], AllData$Longitude[i], AllData$Country[i], AllData$Code[i], AllData$Population_2016[i], AllData$Population_2017[i], AllData$Growth[i], root2)
}
saveXML(doc1, file = "Part2.kml")


### Part III.  Add Style to your KML file (16 pts)

### Now you are going to make the visualization a bit fancier.
### Instead of pushpins, we want different labels for countries with different population sizes in 2017,
### and we will use different colors to represent different levels of population growth rate.
### Code is given to you below to create style elements.
### Here, you just need to figure out what it all does.

### Q11. The following code is an example of how to create cut points for different categories of population in 2016.
pop16Cut = as.numeric(cut(AllData$Population_2016, breaks=5))

### (But this example contains too many 1's, which is not suitable for visualization)
### So find suitable cut points for 2017 population and growth rate,
### and create your categories named pop17Cut and growCut. (3 pts)

### Hint: take a look at their distribution first. You may want to perform some simple transformations
### before finding the cuts.
### Explain the transformation you chose and why; also explain how you chose your cuts. (3 pt)

### Your code here
AllData$pop17Cut = as.numeric(cut(log10(AllData$Population_2017), breaks=c(0, 5, 7, 7.5, 8.5, 10)))
AllData$growCut = as.numeric(cut(AllData$Growth, breaks=c(-3, 0, 1, 2, 3.5, 10)))

### Your answer here
#I took the logarithm of Populaiton to make it a more normal distribution and pick cut points. I picked cuts by looking at histograms and trying to roughly estimate even groupings, although they are not very even. 


### Q12. We modify the addPlacemark() function in Q8, so it can add both placemark and style information.
### It has 3 new arguments: pop17cut, growcut, and style.
### Explain what the new line of code does. (2 pts)

addPlacemark = function(lat,lon,country,code,pop16,pop17,growth,parent,pop17cut,growcut,style=TRUE){
  pm = newXMLNode("Placemark",newXMLNode("name",country),attrs=c(id=code),parent=parent)
  newXMLNode("description",paste(country,"\n population_2016: ",pop16,"\n population_2017: ",pop17,"\n growth: ",growth,sep =""),parent=pm)
  newXMLNode("Point",newXMLNode("coordinates",paste(c(lon,lat,0),collapse=",")),parent=pm)
  if(style){newXMLNode("styleUrl",paste("#YOR",growcut,"-",pop17cut,sep=''),parent=pm)}
}

### Your answer here
#The new line of code creates information based on the style of the placemarker. 


### Q11. Here is another function addStyle(), by which we can add style information to KML file.
### Figure out what the arguments "scales" and "colors" should be, and
### create two objects scale_label and color_label that you can input into this function. (5 pts)
### (Hint: For growth rate from low to high, you want to use this order of color: blue-green-yellow-orange-red)
### (Hint2: make a bigger symbol for country with larger population)

addStyle = function(parent,scales,colors){
  for(j in 1:5){
    for(k in 1:5){
      st = newXMLNode("Style",attrs=c("id"=paste("YOR",j,"-",k,sep="")),parent=parent)
      newXMLNode("IconStyle",newXMLNode("Icon",paste("color_label/label_",colors[j],".png",sep="")),newXMLNode("scale",scales[k]),parent=st)
    }
  }
}

### Your code here
scale_label <- c(1, 2, 3, 5, 8)
colors_label <- c("blue","green","yellow","orange","red")

### Q12. Let's build a tree properly, that contains both country and style information. (6 pts)

### You can complete this by following steps:
### 1) Create a base KML document named "doc2", similar to what you did in Q8
### 2) Add style information by addStyle() in nested for loops
### 3) Add placemarks by addPlacemark()

### Your code here
doc2 <- newXMLDoc()
kmlnode2 <- newXMLNode("kml", doc=doc2)
documentnode2 <- newXMLNode("document", parent = kmlnode2)
addStyle(documentnode2, scale_label, colors_label)
for(l in 1:length(AllData$Country)){
  addPlacemark(AllData$Latitude[l], AllData$Longitude[l], AllData$Country[l], AllData$Code[l], AllData$Population_2016[l], AllData$Population_2017[l], AllData$Growth[l], parent=documentnode2, AllData$pop17Cut[l], AllData$growCut[l], style = TRUE)
}

### Q13. Output your KML document, call it "Part3.kml". (1 pt)
### Open it in Google Earth to verify that it works.
### Explain your findings about the world population from the results you get. (2 pts)

### Your code here
saveXML(doc2, file = "Part3.kml")

### Your answer here
##I was never able to get my kml file to properly open in google earth, and I am not sure what is wrong with the kml file. 

### For this assignment, you only need to submit your R code and "Part3.kml", the PDF report is not required.
