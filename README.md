# Turbidity Cleaner
 
This program is designed to automatically detect anomalous turbidity data from high-frequency measurements, allow manual verification of detected anomalies, and gap-fill missing data using a multivariate imputation model. The processing of turbidity data is split into three steps:
* Level 1 to Level 2: This step applies an automated anomaly detection algorithm to raw turbidity data
* Level 2 to Level 3: This step allows the user to manually verify the detected anamolies (reintroduce incorrectly removed data and/or remove anamolies that were not detected)
* Level 3 to Level 4: This final step applies a multivariate imputation model for gap-filling of missing data

# Installation and Setup
Prior to installation of this program, the R software environment (https://www.r-project.org/) needs to be installed. This program was developed using R version 3.5.3, so it is recommended that this version of R is installed to ensure compatibility. To install this program, file path locations need to be adjusted:
1.	Extract the folder from the zip file to any preferred location
2.	Edit the “run.R” file, located in the “shiny” folder, and change the path in line 8 to the path of where this program is located (i.e., the path to the extracted folder)
3.	Right-click and edit “RunProgram.cmd”. Change the paths in quotations to the paths of “Rscript.exe” (located where R is installed) and “run.R” (located in the “shiny” folder of this program) respectively
4.	Save “RunProgram.cmd”

The correct use of backslashes (\) and forward slashes (/) in these file paths are needed for the program to work correctly, so please follow the same formatting as the placeholder file paths.

# Data Structure
This program uses turbidity, water level, and precipitation data. These data must be split into three separate files. All three data files need to be structured as follows:
* Each needs to be a .csv text file with two columns
*	First column needs to be named “DateTime” and the date-time values should have this structure: YYYY-mm-dd HH:MM:SS
*	Second column should be named “DataValue” and contain the respective measurements (turbidity, water level, or precipitation)

There are no requirements for the naming and location of these files.

# Running the Program
To start the program, open “RunProgram.cmd”. This should open a new window in your default internet browser. The initial startup screen will allow you to choose which level of data will be worked on (ex: Level 1 == raw data, which will lead to applying the automated detection algorithm). The program can apply one step per run. Therefore, it needs to be restarted after each step.
