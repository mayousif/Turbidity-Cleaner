# Turbidity Cleaner
 
This program is designed to automatically detect anomalous turbidity data from high-frequency measurements, allow manual verification of detected anomalies, and gap-fill missing data using a multivariate imputation model.

# Installation and Setup
Prior to installation of this program, the R software environment (https://www.r-project.org/) needs to be installed. This program was developed using R version 3.5.3, so it is recommended that this version of R is installed to ensure compatibility.
1.	After downloading, extract the folder from the zip file to any preferred location
2.	Open “run.R” in the “shiny” folder and change the path in line 8 to the path where this program is located (i.e., the path to the extracted folder)
3.	Right-click and Edit “RunProgram.cmd”. Change the paths in quotations to the paths of “Rscript.exe” (located where R was installed) and “run.R” (located in the “shiny” folder of this program)
4.	Save “RunProgram.cmd”

# Data Structure
This program uses turbidity data, water level data, and precipitation data. All three data files need to be structured similarly:
* Each needs to be a .csv text file with two columns
*	First column needs to be named “DateTime” and the date-time values should have this structure: YYYY-mm-dd HH:MM:SS
*	Second column should be named “DataValue” and contain the respective measurements (turbidity, water level, or precipitation)

There are no requirements for the naming and location of these three .csv files.

# Running the Program
