# NOAA Weather  
This folder contains the NOAA weather data from 05-2007 to 08-2015 obtained using:  
<code>wget -r -l1 --no-parent -A.zip http://www.ncdc.noaa.gov/orders/qclcd/</code>  

The build_report.py processes the bulk download to build weather data into single files corresponding to their "type".

Please note that the NOAA data exceeds the github file size and will have to be uploaded as compressed to this repo.