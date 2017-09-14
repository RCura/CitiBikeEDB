# CitiBikeEDB
## Interactive exploration of CitiBike trips

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.891241.svg)](https://doi.org/10.5281/zenodo.891241)

### Dependencies

Those packages are needed for the data handling : `tidyverse`, `lubridate`, and `rio`.

### Data

#### Download

The data can be downloaded on the official CitiBike data repository : https://www.citibikenyc.com/system-data
This app is designed to analyse the data of a whole year, and it was based on 2016 data that can be found here : https://s3.amazonaws.com/tripdata/index.html

Run the `src/0_Data_downloading.R` script for downloading data, after setting the parameters to the year and months that you want to explore.

#### File reducing

The `csv` files take a large space as each row contains all of the informations about the starting and the ending citibike station.

The first part of the data preparation is then to reduce those files

Run the script `src/1_Data_minifying.R` that will :  

- Remove trips longer than 1h (less that 2% of the trips)
- Convert all time to a unique format (for 2016, January to September are MMDDYYYY, but October to December are YYYYMMDD).
- Rename column names (from `start station id`,`start station name` *etc.* to more formatted names like `startID`, `startName` *etc.*)
- Extract station informations (`ID`, `Name`, `Lat`, `Long`) to other csv (one for each month of data)
- Remove those informations (keeping `startID` and `endID`) from the trip datasets
- Save all the newly formatted files into `data/2016mm-citibike-tripdata_mini.csv` files

#### Data compressing and filtering

The last script, `src/3_Data_compressing.R`, will read all the minified trip files and gather those inside a single `.Rdata` compressed file, so that it can be directly loaded into `R`.

It will also filter the trips according to these steps :

- Remove trips where stations are incorrectly located (`NULL island`, or too far away from the main area)
- Remove trips from/where inexistant stations
- Remove short (<2 min) and long (> 1h) trips

It will then create new columns for easier exploration :
- `Date`: contains the Date, without timestamp
- `Month` : a factor of the english name of each month
- `WDay` : a factor of the english name of each day of the week, starting on Monday
- `Day` : the number of the day
- `DHour` : The decimal hour (eg. 5h45 is 5.75)
- `Hour` : The plain hour (eg. 5h45 is 5)
- `userGender` : a factor of the user's gender : `0` is unknown, `1` is male, `2` is female


### Shiny Application

Once all of this is done, you can run the Shiny App with `shiny::runApp()`
You'll need those packages :
`shiny`, `scales`, `tidyverse`, `leaflet` ([Development version](http://rstudio.github.io/leaflet/)), `leaflet.extras` ([GitHub version](https://github.com/bhaskarvk/leaflet.extras))

The running app can also be used here : http://shiny.parisgeo.cnrs.fr/CitiBikeEDB/


### Licencing

Those scripts are under `GPLv3` licence, and the shiny app is licensed under the `GNU-AGPLv3` licence.
