This is a program, which can be used to simulate energy production using green energy and storage as a solution for Energy Production. It can be used to calculate, how much energy storage one would need to have a reliable grid based on green energy.

The program is written to use UK's GridWatch data, which can be downloaded at http://www.gridwatch.templar.co.uk/download.php

A full dataset can be used as this program searches only columns:
- timestamp
- demand
- wind
- hydro

Solar is not available because the Gridwatch data does not contain it.

The program is written with Lazarus, which is a free IDE and available for a wide number of operating systems. So if you want to compile and change the code, download Lazarus at http://lazarus.freepascal.org

USAGE:

- Specify, how much Wind and/or Hydro you want to use. For example: 10 = 10 times the number of wind turbines in UK
  - Notice, that the default numbers of 1 are unrealistic.
- Specify, how much Pumped storage you want to use for Energy storage
  - 10 = 10 Pumped Storage Power Stations the size of Dinorwig.
  - Efficiency of the Energy Storage. 75% is from Dinorwig's specs.
  - How long will it take before running out of water running at full power. 6h is what Dinorwig has
  - How full are the Pumped Storage Power Stations at the start of the simulation
- Download the gridwatch csv data from http://www.gridwatch.templar.co.uk/download.php
- Specify the year you want test. If you leave this empty, the whole gridwatch data is used.
  - 2013 = Test the whole year
  - 2013-01 = Test January 2013
  - 2013-01-01 = Test January 1st 2013
- Shortage treshold: If Production is nn% under Demand, a shortage is logged.
- Click on Open gridwatch csv file
- Finally click on Simulate to calculate

RESULTS

Results are divided to two tabs:
- Results = Overview of the results
  - What values were used
  - Amount of stored electricity at the beginning and end in MWh
  - Amount of used electricity during the run in MWh
  - Amount of wasted electricity due to
    - not having enough pumped storage
    - energy loss while pumping water to the storage 
  - Number of shortages
  - Worst time during a shortage
- Shortages = Log of every shortage.
  - When the shortage started
  - When the shortage ended
  - How long the shortage was
  - Lowest = How little electricity was available at the worst time during the shortage

EXAMPLE

- Year 2014-01-01
- Shortage treshold 90.00%
- Wind 6 x current capacity
- Hydro 1 x current capacity
- Pumped storage 6 x Dinorwig 1728 MW
- Pumped storage efficiency 75%
- Stored electricity at start 31,104.00 MWh    <-- Started half full
- Stored electricity at end 613.56 MWh         <-- Storage almost empty at the end of the day
- Used electricity 794,042.09 MWh
- Wasted electricity 859.01 MWh
- Shortages: 1
- Worst shortage 76.67%                        <-- Amount of electricity during the worst time

- Shortage started: 01.01.2014 12:45:02
- Shortage ended: 01.01.2014 22:10:02
- Length of shortage: 9 hours, 24 minutes, 59 seconds

CHANGELOG

08.11.2014 V1.00 Initial version

12.11.2014 V1.01 Changes to pumped storage:
                 - Define how long it will take to run out of water.
                 - Make sure to not get more energy from pumped storage, than it can produce in dT.
                 Added more documentation to the code.
                 Added license

14.11.2014 V1.02 Changes to the code to improve accuracy.
                 Results-tab for an overview.
                 Made code more readable.
                 Added more documentation to the code.
                 Testing against calculations in OpenOffice.
                 Added an example to README

LICENSE

GNU General Public License v3

