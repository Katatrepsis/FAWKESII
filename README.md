# FAWKESII
This Github Repository contains an archive of code for the FAWKES II Workshop. The idea is to have all scripts work platform independent and do not require any "hard" data to be present on anyone's computer (everything will be downloaded once). And new bits of the analysis can be added as new scripts with a new leading number. In the long run it will hopefully help to organize the growing number of scripts. Particular files are as follows:

00_initialize_directories_and_scripts.R: This is basically the launchpad. 
This will first define the working directories for scripts and data. New users should adjust section 00.1 to include their local folders. Two folders will be specified: path2temp (where the data is stored and outputs will be created) and path2wd (the FAWKESII git folder). Use Sys.info() to identify username or nodename (username works better on macOS).
Secondly, all other R files can be triggered from this section 00.2. So if you source 00 all other scripts will be run as well.

01_load_libraries_and_functions.R:
It loads all libraries needed for subsequent analysis and automatically installs if libraries are missing. Please add all libraries needed here.
Secondly, this will take all self build functions needed for later scripts.

02_load_data.R:
Loads all data from the web. I usually place a file somewhere in my dropbox and copy the share link here. You need to change dl=0 to dl=1 at the end of the link to make it work.
Data will be downloaded only once to your path2temp.

03_Species_trends_IUCN_script.R:
a script for downloading IUCN data for species that are focal species in the Natura2000 network. The data includes the IUCN "Population" field, which denotes the trends in the species.

04_Natura2000_Pressures.R: contains a script for harmonising and summarising impacts on Natura2000 data based on the standard form data.

05_Text_mining_Natura2000.R: 
an R script that provides basic text mining capabilities from folders full of PDFs. Intended to be used to look for patterns in the management plans for Natura2000 sites.
