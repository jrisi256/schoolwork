## Retrieving the text from an article in Nexis JSON format
##
## Just retrieves metadata (DocID, title. date. byline) and 
## the text from a one-page result set -- written to a .tsv
## You would probably (a) want other information and 
## (b) have multiple json files, with 50 results each
## which you would need to loop over.
##
## The key is that the result set is in JSON and the text of
## the article is in an HTML/XML string within the JSON.

library(jsonlite)
library(rvest)
library(readr)


#####################

json_list<-jsonlite::fromJSON("corona.FOXNews.2020-01-31.json")

## This reads the json file into a list
## The json has metadata, then a list of results, up to 50,
## stored in the attribute "value"

json_value<-json_list$value


## This is a (nested) data.frame with (up to) 50 observations
## of 35 variables:

# > names(json_value)
# [1] "ResultId"
# [2] "Jurisdiction"
# [3] "Location"
# [4] "ContentType"
# [5] "Byline"
# [6] "WordLength"
# [7] "WebNewsUrl"
# [8] "Geography"
# [9] "NegativeNews"
# [10] "Language"
# [11] "Industry"
# [12] "People"
# [13] "Subject"
# [14] "Section"
# [15] "Company"
# [16] "PublicationType"
# [17] "Publisher"
# [18] "GroupDuplicates"
# [19] "InternationalLocation"
# [20] "LEI"
# [21] "CompanyName"
# [22] "LNGI"
# [23] "SearchWithinResults"
# [24] "SearchType"
# [25] "Date"
# [26] "Keyword"
# [27] "Title"
# [28] "DocumentContent@odata.mediaReadLink"
# [29] "DocumentContent@odata.mediaContentType"
# [30] "Overview"
# [31] "Extracts"
# [32] "IsCitationMatch"
# [33] "SourcePath"
# [34] "Document"
# [35] "Source"

## You presumably want to keep some of these

# > json_value$ResultId[1]
# [1] "urn:contentItem:5Y3X-49K1-DXH2-61B9-00000-00"
# > json_value$Title[1]
# [1] "Sen. Mike Lee (R-UT) And Sen. Kelly Loeffler (R-GA) Is Being Interviewed About the Donald Trump Impeachment Trial; Trump Could Be Acquitted by Senate On Wednesday; Trump Hits back At Senate Dems AS Witness Vote Flops; Top Al Qaeda Leader \"Likely Killed\" in Yemen; White House Preparing State Of The Union Address; Bernie Sanders And Joe Biden Lead In New Poll; Ongoing Frustration Over Corruption In Ukraine, Burisma; Questioning Hunter Biden's Association With Burisma; Trump Administration Declares Public Health Emergency; W.H., DOJ, Address Human Trafficking Ahead Of Super Bowl"
# > json_value$Date[1]
# [1] "2020-01-31T00:00:00Z"

## The documents themselves are in the Document "column"

## The Document "column" is itself a data.frame consisting
## of (up to) 50 observations of 4 variables

# > names(json_documents)
# [1] "DocumentId"     "DocumentIdType"
# [3] "Content"        "Citation"

## The "contents" of the documents are in the Context column
## This is a vector of (up to) 50 strings (character)

results_df <- data.frame(
  DocumentId = json_value$Document$DocumentId,
  Title = json_value$Title,
  Date = json_value$Date,
  Byline = json_value$Byline,
  Content = json_value$Document$Content)

nresults <- nrow(results_df)

ActualContent <- rep("",nresults)

for (i in 1:nresults) {

## Each of the strings in Content is an html/xml "file"

## Read the each of these into an rvest html_document object

  html_article <- rvest::read_html(results_df$Content[i])

# > html_article
# {html_document}
# <html>
#  [1] <body><entry xmlns="http://www.w3.or ...


## Find the elements/nodes of the html labeled "body"
## There are two -- a "nodeset" -- you want the second

  html_body <- html_elements(html_article,"body")[2]

# > html_elements(html_article,"body")
# {xml_nodeset (2)}
# [1] <body><entry xmlns="http://www.w3.or ...
# [2] <body xmlns:nitf="http://iptc.org/st ...
#
# > html_body
# {xml_nodeset (1)}
# [1] <body xmlns:nitf="http://iptc.org/st ...

## This body nodeset has two "children", one containing the
## header information (head) and one containing
## the "content" of the article (content).
## You want the second one.

  html_content <- html_children(html_body)[2]

# > html_children(html_body)
# {xml_nodeset (2)}
# [1] <body.head><hedline><hl1>Interview:  ...
# [2] <body.content><bodytext><p nitf:lede ...
# > html_content
# {xml_nodeset (1)}
# [1] <body.content><bodytext><p nitf:lede ...

## To avoid formatting errors, extract each of the
## paragraphs in the content

  html_pars <- html_elements(html_content,"p")

# > html_pars
# {xml_nodeset (206)}
# [1] <p nitf:lede="true">LAURA INGRAHAM, ...
# [2] <p nitf:lede="true">We'll see you o ...
# [3] <p nitf:lede="true">SHANNON BREAM,  ...
# [4] <p nitf:lede="true">We begin with t ...
# [5] <p>Senator Mike Lee is here live wi ...
# [6] <p>What about the democratic senato ...
# [7] <p>But now, we want to head over to ...
# [8] <p>CHAD PERGRAM, FOX NEWS SENIOR CA ...
# [9] <p>Here's what happens. The senator ...
# [10] <p>Now in 1999, the late senator fr ...
# [11] <p>I would start to look at some po ...
# [12] <p>On the Republican side certainly ...
# [13] <p>BREAM: All right, Chad. So what  ...
# [14] <p>PERGRAM: Yes. That's right. You  ...
# [15] <p>And that said it, you know, some ...
# [16] <p>Now, the White House director of ...
# [17] <p>And Ueland says it's a strong on ...
# [18] <p>BREAM: All right, Chad. So there ...
# [19] <p>PERGRAM: That's right. We have a ...
# [20] <p>And a lot of people said, you kn ...
# ...


## Use the html_text2 command to give you a list of paragraphs in roughly the form you would read it in the browser. With proper spacing, etc.

  text_pars <- sapply(html_pars,html_text2)

# > text_pars[1]
# [1] "LAURA INGRAHAM, FOX NEWS HOST: Brexit is official, or closer to an end. Britain freed itself from its 47-year membership in the E.U. Very satisfying moment. Sorry, Angela Merkel. She remarked, this is a deep cut for us all. Happy Brexit. Enjoy your freedom, Britain."

# Note: SOME paragraphs STILL not text of article.
# eg., NYT example includes
# "[Sign up for our daily newsletter about news from California.]"
# "PHOTO: Airline employees wore face masks to protect against the spread of the Wuhan coronavirus at Los Angeles International Airport last week. (PHOTOGRAPH BY Mark Ralston/Agence France-Presse — Getty Images FOR THE NEW YORK TIMES)"
#
# Fox News example includes
#     Speaker identification like above.
#     Notes, like
#  > text_pars[205]
# [1] "(END VIDEOTAPE)"
  
# Sew that list back together with endofline characters
  # You would probably want to do something more sophisticated
  ActualContent[i] <- paste(text_pars,collapse=" \\n ")
  
}

results_df$Content <- ActualContent

write_tsv(results_df,file="Nexis_results.tsv")
