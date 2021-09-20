
This is a test to add new ecoregions representing wetlands,
as well as an alder and herbaceous functional type.

9/13/2021: created new ecoregions with wetland types from NLCD data
9/16/2021: added alder and grass functional types TODO give these actual values
	   appears to be running! 
	   Error when wind or browse are run:
		2021-09-16 11:13:54,615 - Running Base Wind ...
		2021-09-16 11:13:54,618 - Processing landscape for wind events ...
		2021-09-16 11:13:54,893 - Internal error occurred within the program:
		2021-09-16 11:13:54,894 -   Object reference not set to an instance of an object.
		2021-09-16 11:13:54,895 - 
		2021-09-16 11:13:54,895 - Stack trace:
		2021-09-16 11:13:54,931 -    at Landis.Extension.BaseWind.Event.Initiate(ActiveSite site, Int32 windTimestep)
		   at Landis.Extension.BaseWind.PlugIn.Run()
		   at Landis.Model.Run(String scenarioPath, IUserInterface ui)
		   at Landis.App.Main(String[] args) in D:\CGAProjects\Rob\LANDIS_FINAL\GitHubRepo\Core-Model-v7\Tool-Console\src\App.cs:line 98

		
	    Added initial communities to the new ecoregions (just a draft, with some alder in one and grass in the other)
	    This did not fix the error.
            Removed any initial community that did NOT have an active ecoregion (i.e. sites that had previously been
              classified as forest which I changed to a "water" ecoregion based on the NLCD mask). 
	    Added ecoregions for wind -- fixed error with wind disturbance
	    Updated population zone map -- fixed error with browse disturbance. Pop zone map must overlap exactly with IC active zones