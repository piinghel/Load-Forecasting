from datetime import datetime as dt
from functions_data import get_data_fifthplay, get_data_fluvius
import os

# %%
# Some global parameters which are similar for the fifthplay and fluvius API
START = dt(2016, 1, 1)
END = dt(2020, 2, 6)
USER = 'pieterjan_inghelbrecht'
PSWD = 'KQnYV0jkGN7vuKTYh4vp'

# some global parameters specfic for the fifthplay API
URL = '/api/v0.1/buildings/energyville1/fifthplay/'
URL_Devices = '/api/v0.1/buildings/energyville1/fifthplay/devices/'
PV_GEN = 'AE2648BF-173E-47ED-8E33-AA4C0EED1A8E'  # PV generation
I_TR1 = '70CCA0AD-9A63-4919-AF71-D2E23DFD792C'  # injection TR1
I_TR2 = 'CB2A1B29-D527-411F-B593-246D51D27586'  # injection TR2
C_TR1 = 'aaab7f83-293f-41af-8416-7ca78d47990b'  # consumption TR1
C_TR2 = '1200c16b-ec82-4235-bbdd-a577a042d51b'  # #consumption TR2

DIRECTORY_FIFTH = os.path.join(os.getcwd(), 'Fifthplay/')
COLUMNS_FIFTH = ['DateTimeMeasurement', 'Value', 'Description']

# PV generation: AE2648BF-173E-47ED-8E33-AA4C0EED1A8E
# injection TR2 :CB2A1B29-D527-411F-B593-246D51D27586
# injection TR1: 70CCA0AD-9A63-4919-AF71-D2E23DFD792C
#consumption TR1: aaab7f83-293f-41af-8416-7ca78d47990b
#consumption TR2: 1200c16b-ec82-4235-bbdd-a577a042d5



# %%
# have a look at the devices
df_devices = get_data_fifthplay(user=USER,
                                pswd=PSWD,
                                url=URL_Devices)
df_devices.head()
# %%
# PV generation: AE2648BF-173E-47ED-8E33-AA4C0EED1A8E
df_pv = get_data_fifthplay(user=USER,
                           pswd=PSWD,
                           url=URL,
                           device_ids=PV_GEN,
                           start=START,
                           end=END,
                           columns=COLUMNS_FIFTH,
                           save_as='pvGeneration.csv',
                           directory=DIRECTORY_FIFTH)
# %%
# injection TR1: 70CCA0AD-9A63-4919-AF71-D2E23DFD792C
df_Itr1 = get_data_fifthplay(user=USER,
                             pswd=PSWD,
                             url=URL,
                             device_ids=I_TR1,
                             start=START,
                             end=END,
                             columns=COLUMNS_FIFTH,
                             save_as='injectionTR1.csv',
                             directory=DIRECTORY_FIFTH)
# %%
# injection TR2 :CB2A1B29-D527-411F-B593-246D51D27586
df_Itr2 = get_data_fifthplay(user=USER,
                             pswd=PSWD,
                             url=URL,
                             device_ids=I_TR2,
                             start=START,
                             end=END,
                             columns=COLUMNS_FIFTH,
                             save_as='injectionTR2.csv',
                             directory=DIRECTORY_FIFTH)
# %%
# consumption TR1: aaab7f83-293f-41af-8416-7ca78d47990b
df_Ctr1 = get_data_fifthplay(user=USER,
                             pswd=PSWD,
                             url=URL,
                             device_ids=C_TR1,
                             start=START,
                             end=END,
                             columns=COLUMNS_FIFTH,
                             save_as='consumptionTR1.csv',
                             directory=DIRECTORY_FIFTH)

# %%
# consumption TR2: 1200c16b-ec82-4235-bbdd-a577a042d51b
df_Ctr2 = get_data_fifthplay(user=USER,
                             pswd=PSWD,
                             url=URL,
                             device_ids=C_TR2,
                             start=START,
                             end=END,
                             columns=COLUMNS_FIFTH,
                             save_as='consumptionTR2.csv',
                             directory=DIRECTORY_FIFTH)


# %%
# Data from fluvius
# Note that consumption here means the consumption from the grid only;
# the building also consumes energy coming from the PV installation.
# So the total consumption can be calculated via the following formula
# Total consumption = consumption + PV - Injection

# Global parameters specific for the fluvius dataset
URL_Meter = "api/v0.1/buildings/energyville1/fluvius/meters/"
URL_FLUV = "api/v0.1/buildings/energyville1/fluvius/"
METER_CO = 541449200004157424  # consumption (electricity)
METER_CO_MO = 541449200004157431  # consumption monthly (gas)
METER_PV = 541449200004448997  # pv (electricity)
METER_IN = 541449200004449017  # injection (electricity)
DIRECTORY_FLUV = os.path.join(os.getcwd(), 'Fluvius/')
COLUMNS_FLUV = ['DateTimeMeasurement', 'Active','Capacitive', 'Inductive', 'Info']
#%%
# get an overview of the EAnNumbers
df_meters = get_data_fluvius(user=USER,
                             pswd=PSWD,
                             url=URL_Meter)


df_meters
# %%
# Consumption
df_cons_fluv = get_data_fluvius(user=USER,
                                pswd=PSWD,
                                url=URL_FLUV,
                                start=START,
                                end=END,
                                columns=COLUMNS_FLUV,
                                meter=METER_CO,
                                directory=DIRECTORY_FLUV,
                                save_as="consumption.csv")

# %%
# PV
df_pv_fluv = get_data_fluvius(user=USER,
                              pswd=PSWD,
                              url=URL_FLUV,
                              start=START,
                              end=END,
                              columns=COLUMNS_FLUV,
                              meter=METER_PV,
                              directory=DIRECTORY_FLUV,
                              save_as="pv.csv")

# %%
# Injection
df_in_fluv = get_data_fluvius(user=USER,
                              pswd=PSWD,
                              url=URL_FLUV,
                              start=START,
                              end=END,
                              columns=COLUMNS_FLUV,
                              meter=METER_IN,
                              directory=DIRECTORY_FLUV,
                              save_as="injection.csv")
