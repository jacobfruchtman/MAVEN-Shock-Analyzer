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


overmachplotter aggragates the data together. We can then plot things with shockaggragateplotter, or compare aggragated data using other procedures in the "Aggragate" directory

The raw quantities (plus M_crit which takes a while to calculate) for our aggragated data are given in shockdata.csv. The quantities given in this file are

-time of crossing

-L_S (Mars Solar Longitude)

-MAVEN position at time of crossing (in km)

-MAVEN latitude at time of crossing

-automatically measured Bmax of the shock (in nT)


-TIME AVERAGES of

--upstream electron number density (in 1/cm^3)

--upstream ion number density (in 1/cm^3)

--upstream electron temperature (in eV)

--upstream proton temperature (in eV)

--upstream ion velocity moments from SWIFS and SWICS (Component wise averages ,in km/s)

--downstream ion velocity moment from SWICS (Component wise average ,in km/s)

--Upstream and downstream B_field vectors (Component wise averages , in nT)

--Upstream and downstream |B| (in nT)



-FIT parameters:m0,m1,m2,m3 (in nT,Hz,sec,nT)

-FIT B_down and B_up (in nT)

-time stddev of upstream and downstream |B| in measuring intervals (nT)

-distance of position of crossing (in cylindrical coordinates. Technically the projection of such) from closest point on Trotigen conic surface

-$\hat{n}_{AVG}$, the component wise average of different methods of calculating the shock normal vector

-conic normal vector components of conic surface at closest point to the crossing -in cylindrical coordinates 

-dot product of the normal vectors (after shifting n_AVG to cylindrical coords)

-Mcrit, as calculated from above quantities

