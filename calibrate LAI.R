# calibrate biomass - LAI relationship

biomass_lai <- readxl::read_xlsx("./Parameterization/necn_parameterize/biomass and lai/biomass_anpp_lai.xlsx")

#calibrate LAI ~ biomass relationship
plot(LAI ~ `Total biomass`, data = biomass_lai[biomass_lai$Species == "Populus tremuloides", ])



#calibrate lai_growth_limit ~ lai relationship
plot(`Annual production` ~ LAI, data = biomass_lai[biomass_lai$Species == "Populus tremuloides", ])

LAI_Growth_limit = 1.0 - Math.Exp(lai_to_growth * lai)

