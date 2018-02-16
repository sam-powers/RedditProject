# RedditProject
School Project Analyzing Reddit Response to Local Issues

Official Project Description: National Conversations on Local Crises.

How do local crises resonate on a national stage? Why do some tragedies linger within the national consciousness while others barely make national news? With the rise of social media and hashtag activism, people from all over the country weigh in on issues unique to a particular locale. I aim to measure national response to local crises by analyzing Twitter data mentioning certain locations before, during, and after key events. Measures will include the volume, location, and sentiment of American tweets regarding certain locales. I will seek to gain baseline measures for each of these values and note the time it takes for those measures to return to baseline following a crisis. I will also note key characteristics including relevant political issues, the location of the event, and chief causes (i.e. natural disaster, crime, social unrest) to train machine learning/regression algorithms and gain a sense for which issues resonate most on a national stage. (Note: If twitter data isn't accessible, I think my goal could be accomplished by analyzing reddit postings/conversations. It is definitely a more fringe platform, but I believe it would provide insight into some of the things people invested in internet conversation care about.

What is in this Github right now.

- BigQueries contains a list of the SQL code run to retrieve the Reddit data from Google BigQuery Public Data sources  
- RPackages.txt file contains a list of R packages to be imported before running the R codes
- RedditDataAnalysis.R contains the model code for extracting data, cleaning it, running sentiment analysis and beginning basic graphs. It also contains the start of the modelling process.
- Orlando.R contains a duplicate of the RedditDataAnalysis.R script but run with the data from the Orlando Shooting.
- All data is stored in Google Drive and is retrieveable through publicly readable share links encoded within the R script. 

Where are we going with this? 

- draft statistically defendable cut points for when sentiment and rate of posting return to "normal" (Most likely variance of sentiment and t-stat of post/hour rate compared to two weeks before baseline, Or perhaps when concavity changes/levels out)

- Conduct analyses on 50 (aiming for 100 if possible) total crises events and extract the above statistics to create final dataset

- model "crisis measurements" by relevant characteristics i.e. bias-related, number of deaths, number of wounded, fire-arms, children involved. Discuss with polisci prof connection before proceeding.

- Determine which characteristics of crises are most significant in terms of prolonged national response on reddit.