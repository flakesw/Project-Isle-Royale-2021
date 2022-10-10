                          USCRN/USRCRN MONTHLY FILES 
                          
                            UPDATED: 2017-07-06

README CONTENTS:                          
    1. GENERAL INFORMATION 
    2. DATA VERSION / STATUS UPDATES
    3. DIRECTORY / FILE ORGANIZATION
    4. DATA FIELDS / FORMATS / IMPORTANT NOTES

********************************************************************************

1. GENERAL INFORMATION

NCDC provides access to monthly data from the U.S. Climate Reference Network / 
U.S. Regional Climate Reference Network (USCRN/USRCRN) via anonymous ftp at:

        ftp://ftp.ncdc.noaa.gov/pub/data/uscrn/products/monthly01

and an identical web interface at:

        http://www1.ncdc.noaa.gov/pub/data/uscrn/products/monthly01

Before using these data, be sure to review this document carefully, as well
as any announcements within the main (monthly01) directory. 

********************************************************************************

2. DATA VERSION / STATUS UPDATES

Status and Version Information for U.S. Climate Reference Network Data

  ##########################
  
Version change:     2.1 to 2.1.1
Commencement Date:  2017-06-20
Completion Date:    2017-06-30
Variables impacted: Precipitation

    Effective June 30, 2017, recalculation using the Official Algorithm
for Precipitation (OAP) 2.1.1 has been completed. The purpose of this 
recalculation is described in Appendix A of the OAP 2.1 documentation:
https://www1.ncdc.noaa.gov/pub/data/uscrn/documentation/maintenance/2016-05/USCRN_OAP2.1.Description_PrecipChanges.pdf

  ##########################
  
Status change:      Correction
Correction Applied: 2017-05-01
Variables impacted: Precipitation for 3 stations
	
    From July 2016 until May 1, 2017, the precipitation values for 
the following stations and time periods were mistakenly missing on 
the website and in the FTP product files:
		NC Asheville 8 SSW          2004-05-01 to 2004-11-01 
		SC McClellanville 7 NE      2005-05-01 to 2005-11-01
		CA Stovepipe Wells 1 SW     2004-05-01 to 2005-06-01
Precipitation data for these stations/time periods that were downloaded
during the affected time period should be re-acquired. 

  ##########################
  
Version change:     2.0 to 2.1 
Commencement Date:  2016-05-12 
Completion Date:    2016-06-13
Variables impacted: Precipitation
                    Relative Humidity (RH)
                    Temperatures from RH Sensor
                    Thermometer Shield Aspiration Fan Speeds
                    Air Temperature (when fan speeds are low)
                      
    Beginning May 12, 2016, USCRN changed the current data set to 
version 2.1 from v2.0. This version change was retroactively applied to 
all USCRN/USRCRN stations for their period-of-record 5-minute measurements 
from 2016-05-12 until 2016-06-13. [Note that during this period of 
reprocessing, data on the website and in these FTP products contained 
a mixture of v2.0 and v2.1 values.]
    Version 2.1 includes minor precipitation algorithm changes and 
changes/additions to the quality control ranges for acceptable 
relative humidity (RH) values, temperatures measured with the 
RH sensors, and for the speed of the fans which are used to 
aspirate the air temperature sensors. For precipitation, the Official 
Algorithm for Precipitation (OAP) v2.1 was implemented which  
addresses a minor correction to v2.0 that guards against
overally large (> 0.3 mm) precipitation residuals from one hour 
being transferred to the next hour. Further information can be found at 
http://www.ncdc.noaa.gov/crn/documentation.html.

  ##########################
  
Version change:     1.0 to 2.0 
Commencement Date:  2015-08-17
Completion Date:    2015-09-15
Variables impacted: Precipitation

    The original Official Algorithm for Precipitation 
(OAP) version 1.0 was operational until August 17, 2015 and used a 
pairwise comparison and moving reference depth to calculate 
precipitation. Precipitation data accessed and/or downloaded 
prior to this date were calculated using OAP v1.0. 
    Beginning August 17, 2015, all precipitation data were 
calculated using a new processing algorithm, OAP v2.0. In addition, 
the v2.0 algorithm was retroactively applied to all USCRN/USRCRN 
stations for their periods of record (PORs) starting when 5-minute 
data began being collected. The reprocessing took approximately four
weeks to recalculate all station's existing values from v1.0 to v2.0
for their PORs and was completed on September 15, 2015. 
    OAP v2.0 marked a fundamental shift in the procedures used to calculate 
precipitation. The new algorithm uses a weighted average approach 
based on each sensor's noise level. It has greatly improved the 
network's capacity to detect lighter precipitation with greater 
confidence. For details, see Leeper et. al., 2015, 
(http://journals.ametsoc.org/doi/abs/10.1175/JTECH-D-14-00185.1) and
http://www.ncdc.noaa.gov/crn/documentation.html.
********************************************************************************

3. DIRECTORY / FILE ORGANIZATION

The monthly01 directory contains a single ASCII text file for each USCRN/USRCRN 
station. Files list that station's monthly data for its period of record. Files 
are typically updated monthly and are named according to the following 
convention:

        CRNM01TT-${name}.txt
        
   CRNM01 = filename prefix to denote CRN Monthly01 data     
       TT = 2-character file format number (currently 02)
  ${name} = station name (state location vector) (e.g. AZ_Tucson_11_W)

    The 2-character sequence TT indicates the file format number and is updated 
    when the file format is changed.   
  
********************************************************************************

4. DATA FIELDS / FORMATS

Each station file contains fixed-width formatted fields with a single set of 
monthly data per line. A summary table of the fields and a detailed listing of 
field definitions/column formats are shown below. 

Fortran users will find the column widths and counts useful. 

The file "HEADERS.txt", found in the monthly01 directory, is designed to be 
prepended to the data for use with spreadsheet programs, data extraction tools 
(e.g. awk) or any other programming language. This file contains the following 
three lines:

    1. Field Number
    2. Field Name
    3. Unit of Measure

Please be sure to refer to the "Important Notes" section below for essential 
information.

All monthly data are calculated over the station's month in Local Standard 
Time.

Field#  Name                           Units
---------------------------------------------
   1    WBANNO                         XXXXX
   2    LST_YRMO                       YYYYMM
   3    CRX_VN_MONTHLY                 XXXXXX
   4    PRECISE_LONGITUDE              Decimal_degrees
   5    PRECISE_LATITUDE               Decimal_degrees
   6    T_MONTHLY_MAX                  Celsius
   7    T_MONTHLY_MIN                  Celsius
   8    T_MONTHLY_MEAN                 Celsius
   9    T_MONTHLY_AVG                  Celsius
   10   P_MONTHLY_CALC                 mm
   11   SOLRAD_MONTHLY_AVG             MJ/m^2
   12   SUR_TEMP_MONTHLY_TYPE          X
   13   SUR_TEMP_MONTHLY_MAX           Celsius
   14   SUR_TEMP_MONTHLY_MIN           Celsius
   15   SUR_TEMP_MONTHLY_AVG           Celsius

    1    WBANNO  [5 chars]  cols 1 -- 5
              The station WBAN number.

    2    LST_YRMO  [6 chars]  cols 7 -- 12
              The Local Standard Time (LST) year/month of the observation.

    3    CRX_VN_MONTHLY  [6 chars]  cols 14 -- 19
              The version number of the station datalogger program that was in 
          effect at the end of the month. Note: This field should be treated 
          as text (i.e. string).

    4    PRECISE_LONGITUDE  [9 chars]  cols 21 -- 29
              Station longitude, using WGS-84, with a precision of 4 decimal places.

    5    PRECISE_LATITUDE  [9 chars]  cols 31 -- 39
              Station latitude, using WGS-84, with a precision of 4 decimal places.

    6    T_MONTHLY_MAX  [7 chars]  cols 41 -- 47
              The maximum air temperature, in degrees C. See Note F.

    7    T_MONTHLY_MIN  [7 chars]  cols 49 -- 55
              The minimum air temperature, in degrees C. See Note F.

    8    T_MONTHLY_MEAN  [7 chars]  cols 57 -- 63
              The mean air temperature, in degrees C, calculated using the typical 
          historical approach of (T_MONTHLY_MAX + T_MONTHLY_MIN) / 2. See Note 
          F.

    9    T_MONTHLY_AVG  [7 chars]  cols 65 -- 71
              The average air temperature, in degrees C. See Note F.

    10   P_MONTHLY_CALC  [7 chars]  cols 73 -- 79
              The total amount of precipitation, in mm. See Note G.

    11   SOLRAD_MONTHLY_AVG  [7 chars]  cols 81 -- 87
              The average daily total solar energy received, in MJ/meter^2. See 
          Note H.

    12   SUR_TEMP_MONTHLY_TYPE  [1 chars]  cols 89 -- 89
              Type of infrared surface temperature measurement: 'R' denotes raw 
          (uncorrected), 'C' denotes corrected, and 'U' is unknown/missing. 
          See Note I.

    13   SUR_TEMP_MONTHLY_MAX  [7 chars]  cols 91 -- 97
              The maximum infrared surface temperature, in degrees C. See Note 
          J.

    14   SUR_TEMP_MONTHLY_MIN  [7 chars]  cols 99 -- 105
              The minimum infrared surface temperature, in degrees C. See Note 
          J.

    15   SUR_TEMP_MONTHLY_AVG  [7 chars]  cols 107 -- 113
              The average infrared surface temperature, in degrees C. See Note 
          J.

    IMPORTANT NOTES:
        A.  All fields are separated from adjacent fields by at least one space.
        B.  Leading zeros are omitted.
        C.  Missing data are indicated by the lowest possible integer for a 
            given column format, such as -9999.0 for 7-character fields with 
            one decimal place or -99.000 for 7-character fields with three
            decimal places.
        D.  Monthly data are calculated using the station's Local Standard Time. 
        E.  There are no quality flags for these derived quantities. When the 
            raw data are flagged as erroneous, these derived values are not 
            calculated, and are instead reported as missing. Therefore, these 
            fields may be assumed to always be good (unflagged) data, except 
            when they are reported as missing.
        F.  Monthly maximum/minimum/average temperatures are the average of all 
            available daily max/min/averages. To be considered valid, there 
            must be fewer than 4 consecutive daily values missing, and no more 
            than 5 total values missing.
        G.  For a monthly precipitation value to be considered valid, there 
            must be precipitation records for at least 97% of the hours in the 
            month.
        H.  Monthly solar radiation is calculated as the average of daily total
            global solar radiation. To be considered valid, there must be fewer 
            than 4 consecutive daily values missing, and no more than 5 total 
            values missing.
        I.  On 2013-01-07 at 1500 UTC, USCRN began reporting corrected surface 
            temperature measurements for some stations. These changes  
            impact previous users of the data because the corrected values 
            differ from uncorrected values. To distinguish between uncorrected 
            (raw) and corrected surface temperature measurements, a surface 
            temperature type field was added to the monthly01 product. The 
            possible values of the this field are "R" to denote raw surface 
            temperature measurements, "C" to denote corrected surface 
            temperature measurements, and "U" for unknown/missing. 
        J.  Monthly maximum/minimum/average surface temperatures are the 
            average of all available daily max/min/avg values. To be
            considered valid, there must be fewer than 4 consecutive values 
            missing, and no more than 5 total values missing.
        K.  USRCRN stations do not measure solar radiation or surface 
            temperature, so those fields are shown as missing data.
        L.  In accordance with Service Change Notice 14-25 from the National 
            Weather Service, NCDC stopped providing data from the 72 
            Southwest Regional Climate Reference Network (USRCRN) stations on 
            June 1, 2014. The historical data for these stations remain 
            available. 