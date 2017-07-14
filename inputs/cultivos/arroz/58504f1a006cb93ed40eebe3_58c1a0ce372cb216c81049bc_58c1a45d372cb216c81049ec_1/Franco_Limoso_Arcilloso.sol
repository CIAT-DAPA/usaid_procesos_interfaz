**********************************************************************
* Template soil data file for PADDY soil water balance model.        *
* File name   : PADDYIN.sol                                          *
* Soil        : IRRI lowland farm, Los Banos, Philippines            *
*                (IsohyperthermicTypicHapludalf)                    *
* Experiment  : Drought stress and well-watered control experiment.  *
*               Data was given by Dr. Tao Li with file name:         *
*                    "root growth simulation 2010DS" at              *
*                    E:\oryza 2000_DATA\Data\root growth.            *
*                    ORYZA2000. IRRI, Los Banos.                     *
**********************************************************************

* Give code name of soil data file to match the water balance PADDY:
SCODE = 'PADDY'

*---------------------------------------------------------------*
* 1. Various soil and management parameters
*---------------------------------------------------------------*
WL0MX = 100.  ! Bund height (mm)
NL = 7        ! Number of soil layers (maximum is 10) (-)
TKL = 0.1,0.1,0.1,0.2,0.2,0.3,0.3      ! Thickness of each soil layer (m)
ZRTMS = 1.0   ! Maximum rooting depth in the soil (m)

*---------------------------------------------------------------*
* 2. Puddling switch: 1=PUDDLED or 0=NON PUDDLED
*---------------------------------------------------------------*
SWITPD = 0  !Non puddled
*SWITPD = 1  ! Puddled

* If PUDDLED, supply parameters for puddled soil
NLPUD = 3 ! Number of puddled soil layers, including the plow sole (-)
! (NLPUD cannot exceed the total number of soil layers NL)

* Saturated volumetric water content of ripened (previously puddled)
* soil (m3 m-3), for each soil layer:
*WCSTRP = 3*0.52, 3*0.55, 3*0.61, 0.64
WCSTRP = 0.51779,0.51779,0.52601,0.44934,0.51259,0.49898,0.47412      

* Soil water tension of puddled soil layer at which cracks reach
* break through the plow sole (pF):
PFCR = 6.0

DPLOWPAN = 0.3   !* The depth of plow pan (m); if it does not appear, it is:
                 !* if SWITPN = 1, DPLOWPAN = sum(TKL(1:NPLUD))
                 !* if SWITPN = 0, DPLOWPAN = sum(TKL(1:NL))

*---------------------------------------------------------------*
* 3. Groundwater switch: 0=DEEP (i.e., not in profile), 1=DATA
* (supplied), 2=CALCULATE
*---------------------------------------------------------------*
*SWITGW = 0 ! Deep groundwater
*SWITGW = 2 ! Calculate groundwater
SWITGW = 1 ! Groundwater data

* If DATA, supply table of groundwater table depth (cm; Y-value)
* as function of calendar day (d; X value):
ZWTB =   1.,200.,
       366.,200.

* If CALCULATE, supply the following parameters:
ZWTBI = 100. ! Initial groundwater table depth (cm)
MINGW = 100. ! Minimum groundwater table depth (cm)
MAXGW = 100. ! Maximum groundwater table depth (cm)
ZWA   = 1.0  ! Receding rate of groundwater with no recharge (cm d-1)
ZWB   = 0.5  ! Sensitivity factor of groundwater recharge (-)

*---------------------------------------------------------------*
* 4. Percolation switch
* Value for SWITVP cannot be 1 (CALCULATE) for non-puddled soil
*---------------------------------------------------------------*
SWITVP = -1 ! Fixed percolation rate
*SWITVP = 0 ! Percolation as function of the groundwater depth
*SWITVP = 1 ! Calculate percolation
*SWITVP = 2 ! Fixed percolation rate as function of time

* If SWITVP = -1, supply fixed percolation rate (mm d-1):
FIXPERC = 0.2

* If SWITVP = 0, supply table of percolation rate (mm d-1; Y-value)
* as function of water table depth (cm; X value):
*PERTB =   0., 3.,
*        200., 3.

* If SWITVP = 2, give percolation rate (mm/d) as function of calendar day
PTABLE =
  1., 1.0,   ! First number is calendar day, second is percolation rate)
 50., 1.0,
100., 20.0,
366., 20.0

*---------------------------------------------------------------*
* 5. Conductivity switch: 0=NO DATA, 1=VAN GENUCHTEN or 2=POWER
*  OR 3= SPAW  function used
*---------------------------------------------------------------*
*SWITKH = 0 ! No data
*SWITKH = 2 ! Power
SWITKH  = 1 ! vanGenuchten
*SWITKH = 11 !Spaw function

*---------------------------------------------------------------*
* 6. Water retention switch: 0=DATA; 1=VAN GENUCHTEN. When DATA, data
* have to be supplied for saturation, field capacity,
* wilting point and at air dryness
*---------------------------------------------------------------*
*SWITPF = 0  ! Data
SWITPF  = 1 ! van Genuchten
*SWITPF = 11 ! SPAW FUNCTION

*---------------------------------------------------------------*
* 7.Soil physical properties, these parameters will be used when model
* runs under actual water or nitrogen condition, or even both. Otherwise
* these parameters will not be used.
*---------------------------------------------------------------*
CLAYX = 4*0.55,0.53,0.5,0.45      !soil clay content, fraction
SANDX = 4*0.08,0.1,0.13,0.18      !soil sand content, fraction
BD = 3*1.17,1.42,1.26,1.31,1.37   !Soil bulk density (g/cm3) 

SOC = 2*11831.64,8044.87,4529.56,2632.66,673.66,80.62     !soil organic carbon content (kg C/ha)
SON = 2*829.48,186.57,167.488,82.48,51.63,6.41            !Soil organic nitrogen content (kg N/ha)

SNH4X = 2*4.97,3.77,3.13,1.62,0.50,0.05      !* soil ammonium content (kg N/ha)
SNO3X = 2*2.49,0.628,0.52,0.27,0.083,0.0088  !*soil natre content (kg N/ha)

*FORGANC =200.0,1000.0, 5*0.0   !* Fresh organic carbon (kg C/ha)
*FORGANN = 10.0,100.0,5*0.0     !* Fresh organic nitrogen (kg N/ha)
*FCarboh = 0.54                !* Fraction of carbonhydrate in fresh organic matter (--)
*FCellulo = 0.38                !* Fraction of cellulos in fresh organic matter (--)

*---------------------------------------------------------------*
* 8. Soil hydrological properties. Required type of data input
* according to setting of conductivity and water retention switch
*---------------------------------------------------------------*
* Saturated hydraulic conductivity, for each soil layer
* (cm d-1) (always required!):
*KST = 2*255.850266, 297.858490, 114.549477, 0.789587, 1.244055, 2*74.991531
KST = 161.57672,161.57672,224.75884,22.81329,71.78426,51.08835,0.18755      !):

* Saturated volumetric water content, for each soil layer
* (m3 m-3)(always required!):
*WCST = 2*0.533142, 0.542527, 0.491697, 0.339206, 0.429186, 2*0.481078
WCST = 0.51779,0.51779,0.52601,0.44934,0.51259,0.49898,0.47412        !):

* Van Genuchten parameters, for each soil layer
* (needed if SWITKH = 1 and/or SWITPF = 1):
*VGA = 2*0.019890, 2*0.017293, 0.008320, 0.018045, 2*0.019957                  !* a parameter (cm-1)
VGA = 0.019574,0.019574,0.017704,0.014746,0.014521,0.018903,0.558184           !* a parameter (cm-1)
*VGL = 2*-2.560369, -1.701567, -2.569782, -4.899517, -1.787825, 2*-0.938208    !* l parameter (-)
VGL = -1.944751,-1.944751,-1.836101,-3.772868,-1.645805,-0.56346,1.268085      !* l parameter (-)
*VGN = 2*1.154652, 1.165071, 1.106051, 1.060753, 1.095501, 2*1.110456          !* n parameter (-)
VGN = 1.103996,1.103996,1.120194,1.062112,1.096456,1.097214,1.026042   
*VGR = 2*0.109187, 0.124051, 0.114157, 0.060378, 0.084557, 2*0.099269          !* residual water content (-)
VGR = 7*0.01      !0.041094,0.050921,0.035146,0.099286,0.114151,0.11415

* Power function parameters, for each soil layer (-)
* (needed if SWITKH = 2):
*PN = 3*-2.5, 3*-2.5, 2*-2.5, -2.5

*!* Volumetric water content at field capacity, for each soil layer
*!* (m3 m-3)(needed if SWITPF = 0):
*WCFC = 2*0.439149, 0.448714, 0.407265, 0.274259, 0.352447, 2*0.396666

*!* Volumetric water content at wilting point, for each soil layer
*!* (m3 m-3) (needed if SWITPF = 0):
*WCWP = 2*0.262450, 0.278898, 0.254430, 0.156693, 0.207871, 2*0.237632

*!* Volumetric water content at air dryness, for each soil layer
*!* (m3 m-3) (needed if SWITPF = 0):
*WCAD = 2*0.109187, 0.124051, 0.114157, 0.060378, 0.084557, 2*0.099269

*---------------------------------------------------------------*
* 9. Initialization conditions, and re-initialization
*---------------------------------------------------------------*
WL0I = 10.   ! Initial ponded water depth at start of simulation (mm)

* Initial volumetric water content at the start of simulation,
* for each soil layer (m3 m-3):  USE ALWAYS FIELD CAPACITY, OR 0.5 TIMES WCST
WCLI = 0.51779,0.51779,0.52601,0.44934,0.51259,0.49898,0.47412   
! Initial ponded water depth at start of simulation (mm)

* Initial ponded water depth and water contents may be reset:
* Ponded water depth: at minimum of WL0I and WL0MX
* Water contents in all soil layers: at saturation value
* For direct-seeded rice, this happens at sowing, for transplanted
* rice, this happens at transplanting
* Re-initialize switch RIWCLI is YES or NO
RIWCLI = 'NO'
*RIWCLI = 'YES'

*---------------------------------------------------------------*
* 10. Initialization of soil thermal conditions
*---------------------------------------------------------------*
SATAV = 18.0       ! Soil annual average temperature of the first layers
SOILT = 22.0, 17.0, 16.0, 15.0, 14.0, 2*15.0     
! Initial soil temperature in each layer
! Have to provide above either one or two of above parameter, otherwise,
! model start the calculation of soil temperature at 0 degree

*---------------------------------------------------------------*
* 11. Observations/measurements
*    Switches to force observed water content in water balance
*---------------------------------------------------------------*
*!* WCL1_OBS, WCL2_OBS,...WCL10_OBS: Observed soil water contents
*!* in layer 1, 2, ..., 10. Format: year, day number, water content
*!* Not obligatory to give data

*WCL1_OBS =
* TO BE FILLED-IN (OPTIONAL)

*!* Parameter to set forcing of observed water content yes (2) or no (0)
*!* during simulation (instead of using simulated values)
*WCL1_FRC = 0 ! No forcing
*WCL1_FRC = 2 ! Forcing

* Table for interpolation of water content between soil layers for
* those layers for which no observations were made: first number is
* the soil layer for which interpolation needs to be done, the second
* is the number of the underlying soil layer, the third is the number
* of the overlying soil layer. No interpolation is performed when all
* three numbers are the same:
WCLINT = 1,1,1,
         2,2,2,
         3,3,3,
         4,4,4,
         5,5,5,
         6,6,6,
         7,7,7

*!* MSKPA1_OBS, MSKPA2_OBS,...MSKPA10_OBS: Observed soil water contents
*!* in layer 1, 2, ..., 10. Format: year, day number, water content
*!* Not obligatory to give data

*MSKPA1_OBS =
* TO BE FILLED-IN (OPTIONAL)

****Any additional variable will be added after this line****