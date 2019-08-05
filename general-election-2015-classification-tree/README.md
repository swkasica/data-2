# General Election 2015 Classification Tree

This branch is where I performed open coding on the version of the R script I got to work in the up-and-running branch. I can't guarantee that this is the correct data, but using historical data is "correct enough" for my purposes. 

## Steve's notes

* `read.xls` was really finicky. I had to convert the `.xlsx` file to `.xls` in order for `read.xls()` and specify the path to a Perl executable in order for it to work. It may be a Windows thing.

* The `url` variable in the `R` script for getting election results wasn't defined, so I just replaced that commented out section with actual 2015 general election results [data from Parliament](http://www.data.parliament.uk/dataset/general-election-2015), which is in `hocl-ge2015-results-summary.csv`.