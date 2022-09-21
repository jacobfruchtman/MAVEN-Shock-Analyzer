# MAVEN-Shock-Analyzer
This is the IDL spagetti codebase involved in attempting to detect and analyze the MAVEN bow shock crossings

Save the idlbatch files to home folder, other files anywhere that you've included in your IDL path. Save "dayLoaded.txt" to your documents folder, or otherwise edit the filepath that iterLoader.pro opens it from.

A single [MAVEN] day's script is split into four sections

 1. "startupsetup.idlbatch" loads the MAVEN data, checks if the day (dayLoaded=line number of chosen date in "dayLoaded.txt") is valid and goes to the next day if not
 2. "routineLoaderPart1.idlbatch" prepares the loaded data for fitting the shocks (interpolates moment data, cleans up REGID, etc). Attached shock profile plots were made pre-SWE recalibration. As such we multiplied our ion densities by .7 to uncalibrate those as well, before later recalibrating everything once pointed out we could just multiply electron density moments by 1/0.7 and recalculate upstream derived quantities. To get the same results shown in figures (pre-SWE recalibration), multiply ion densities by .7 in "ionsetup.pro". Post-SWE-Recalibration, do the same to SWE densities in electronsetup.pro. If correct calibrated values wanted, comment out "dat=paramrecalc(dat)" in overmachplotter.pro (called later to aggragate results together)
 3. "Bfitting.idlbatch" refines our boundary regions, then attempts to fit the shocks by a number of methods. We then pick the method at each shock with lowest chi^2. Next we make a first pass at measuring the overshoot and detecting a downstream interval to measure
 4. "routineLoaderPart2.idlbatch" analyzes the results of these fits. It makes a second pass at measuring the overshoot and detecting a downstream interval to measure, identifies an upstream measuring interval, measures the shock normals and other upstream quantities, and then plots the shock profiles and saves the data quantities as tplots to be aggragated - and saves the timestamps of crossings to "Documents/savedPoints.txt".

"routineLoader.idlbatch" calls 2,3,4
"StartupScript.idlbatch" goes through all of the above
"StartupScript.idlbatch" goes through all of the above then goes on to the next day
"PreBfitting.idlbatch" goes through 1 and 2

============


overmachplotter aggragates the 
