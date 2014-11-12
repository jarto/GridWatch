This is a program, which can be used to simulate energy production, which only uses green energy and storage as a solution for Energy Production. It can be used to calculate, how much energy storage one would need to have a reliable grid based on green energy.

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
  - Efficiency of the Energy Storage.
  - How long will it take before running out of water running at full power. 6h is what Dinorwig has
  - How full are the Pumped Storage Power Stations at the start of the simulation
- Download the gridwatch csv data from http://www.gridwatch.templar.co.uk/download.php
- Specify the year you want test. If you leave this empty, the whole gridwatch data is used.
- Shortage treshold: If Production is nn% under Demand, a shortage is logged.
- Click on Open gridwatch csv file
- Finally click on Simulate to calculate

RESULTS

Any shortages are logged to the grid. Logged information contains:
- When the shortage started
- When the shortage ended
- How long the shortage was
- Lowest = How little electricity was available at the worst time during the shortage

CHANGELOG

08.11.2014 V1.00 Initial version
12.11.2014 V1.01 Changes to pumped storage:
                 - Define how long it will take to run out of water.
                 - Make sure to not get more energy from pumped storage, than it can produce in dT.
                 Added more documentation to the code.
                 Added license

LICENSE

GNU General Public License v3

