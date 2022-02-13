# Read a Nexis JSON File
import json
from bs4 import BeautifulSoup
import pandas as pd

# Read JSON for results packet of up to 50
# would probably loop over files or call from data store
f = open('corona.FOXNews.2020-01-31.json','r')
articlegroup_json = json.load(f)

# Extract JSON for one article
# just the first, would presumably loop over articles

docid = []
title = []
date = []
byline =[]
content = []

for i in range(len(articlegroup_json['value'])):

    articlei_json = articlegroup_json['value'][i]

    # Extract some metadata
    docid.append(articlei_json['Document']['DocumentId'])
    title.append(articlei_json['Title'])
    date.append(articlei_json['Date'])
    #overview = article0_json['Overview']
    byline.append(articlei_json['Byline'])

    # Print metadata ... would presumably do something else with it
    #print(docid)
    #print(title)
    #print(date)
    #print(overview)
    #print(byline)

    # Isolate the content (stored as XML/HTML)
    # Parse it with Beautiful Soup
    # Within that isolate the HTML with just the body text
    # Separate that into a list of paragraph tags
    #     Each reads <p>Yada yada yada possibly with other tags</p>
    # Loop over paragraph tags and extract just the text
    #     May want to use other internal tags
    # Print each paragraph # would presumably do something else
    #     Note: SOME paragraphs still not text of article. eg.
    #     "[Sign up for our daily newsletter about news from California.]"
    #     "PHOTO: Airline employees wore face masks to protect against the spread of the Wuhan coronavirus at Los Angeles International Airport last week. (PHOTOGRAPH BY Mark Ralston/Agence France-Presse — Getty Images FOR THE NEW YORK TIMES)"
    #     (TV Transcripts even moreso -- Identify who's talking, etc.)
    contenti = articlei_json['Document']['Content']
    contentsoup = BeautifulSoup(contenti,'lxml')
    bodytextxml = contentsoup.find('nitf:body.content')
    bodytextpars = bodytextxml.find_all('p')
    actualcontent = ''
    for par in bodytextpars:
        actualcontent = actualcontent + ' \\n ' + par.string
    content.append(actualcontent)


results_df = pd.DataFrame()

results_df['DocumentID'] = docid
results_df['Date'] = date
results_df['Title'] = title
results_df['Byline'] = byline
results_df['Content'] = content

results_df.to_csv('Nexis_results_py.tsv',sep='\t')
