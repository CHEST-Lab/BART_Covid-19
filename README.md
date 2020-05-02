# BART_Covid-19
Here we provide a brief overview of our procedure with the key methods used in our analysis.
Note, that we do not provide the processing of the OSM data. 


### Age_Estimation.R
The age groups from the official census data (INKAR) were different from those of the RKI. Therefore, a probabilistic age estimation has been applied. 

A empirical distribution function has been applied to estimate the probability of the RKI age groups:
![ECDF](https://github.com/CHEST-Lab/BART_Covid-19/blob/master/ecdf_p.jpeg)


The comparison for whole Germany between the original and the estimated distributions is shown here:
![Age](https://github.com/CHEST-Lab/BART_Covid-19/blob/master/AGe_Germany.jpeg)



### BART_machine.R
Here we provide a brief overview of our key analysis method used in our analysis.
Due to random seeds, results may vary to a limited degree.
Users can set serialize=TRUE in the bartMachine() function to save a specific model for a new session.

A full explanation is provided within the script.



#### Authors: 
Blake Byron Walker, Sebastian Brinkmann, Tim Große

Institute of Geography

Friedrich-Alexander-Universitaet Erlangen-Nuernberg

Erlangen, Germany
                             
e-mail: bastibrinkmann94@gmail.com
