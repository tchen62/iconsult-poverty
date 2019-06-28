# README

The script "Poverty Rate Script.r" has the code that the group can use to pull data from the American Community Survey (ACS). 
We can do checks on the data they pull here https://factfinder.census.gov/faces/nav/jsf/pages/searchresults.xhtml?refresh=t.

 

The basic elements of the script are
- Load libraries  
-  Get_acs function   
  - Geography – the level of data to putt. We want it at the tract level.  
  - Table – where the data comes from. This will be one of the elements of the loop. You’ll want the different table names to cycle through here when pulling the data for comparison.
  - Key – Census API key
  - State – our state
  - County – our county
  - Year –As we discussed, there are several years of data. We want the most recent, which is 2017.
  - Survey – As we discussed, there are 5-year estimates and 1-year estimates. We want to use the 5-year. There will potentially be tables where there are only 1-year estimates available. For these, we either want to exclude them, or find a way to capture that we’re using 1-year estimates.

Those are the most important elements. 
If you run the script, the GEOIDs object and ID_list object will give you the 55 GEOIDs for the 55 Census Tracts in the City that we’re interested in. 
We want to filter everything else out in all the other data we’ll be pulling.

 

There is the potential that some tables won’t have data available at the Census Tract level. 
We’ll want to exclude those and potentially find a way to capture that they aren’t tables we can use for this analysis.

The last part of the script calculates the poverty rate by tract. This is what you’ll want to compare everything else against.

Again, the basic tasks that we’ll need to complete are:

- Find a list of all of the “B” tables
- Pull all the data (looping through)
- Filter down to the tracts we care about using the GEOIDs
- Drop the Margin of Error
- Mutate the raw data into whatever format we need it in
- Divide everything by the “Total” row
- Loop through one row of data at a time and compare it to the poverty rate
- Calculate the correlation coefficient between the two datasets (each with 55 data points – one for each tract)
- If it’s above a certain threshold, write that table name and row number to a separate table. If it is below, exclude.
- We should consider writing everything to a table with the correlation coefficients. That way we have a complete dataset and we can filter on whatever we want and look at it.
