This is a program, which can be used to simulate energy production using green energy and storage as a solution for Energy Production. It can be used to calculate, how much energy storage one would need to have a reliable grid based on green energy.

The program is written to use UK's GridWatch data, which can be downloaded at http://www.gridwatch.templar.co.uk/download.php

A full dataset can be used as this program searches only columns:
- timestamp
- demand
- wind
- solar
- hydro
- biomass
- nuclear

The program is written with Lazarus, which is a free IDE and available for a wide number of operating systems. So if you want to compile and change the code, download Lazarus at http://lazarus.freepascal.org

USAGE:

- Specify, how much of different energy forms you want to use. For example: 10 = 10 times the number of wind turbines in UK
  - Notice, that the default numbers of 1 are unrealistic.
- Specify, how many Tesla Big Batteries you want to use for Energy storage
  - 1000 = 1000 batteries the size of Australia's 150 MW Hornsdale Power Reserve "GigaBattery"
  - Efficiency of the Energy Storage. 94% is probably optimistic. 80-90% is typical efficiency.
  - How long will it take for a Big Battery to run out of power if it's drained full power. 5h is theoretical maximum for Hornsdale.
  - How full are the batteries at the start of the simulation
- Download the gridwatch csv data from http://www.gridwatch.templar.co.uk/download.php
- Specify the year you want test. If you leave this empty, the whole gridwatch data is used.
  - 2022 = Test the whole year
  - 2022-01 = Test January 2022
  - 2022-01-01 = Test January 1st 2022
- Shortage treshold: If Production is lower than nn% of Demand, a shortage is logged.
- Click on Open gridwatch csv file
- Finally click on Simulate to calculate

RESULTS

Results are divided to two tabs:
- Results = Overview of the results
  - What values were used
  - Total consumption and production by energy form
  - Amount of stored electricity at the beginning and end in MWh
  - Amount of used electricity during the run in MWh
  - Amount of wasted electricity due to
    - not having enough storage
    - energy loss from using storage 
  - Number of shortages
  - Worst shortage of energy
  - Lowest storage level
- Shortages = Log of every shortage.
  - When the shortage started
  - When the shortage ended
  - How long the shortage was
  - Lowest = How little electricity was available at the worst time during the shortage

EXAMPLE
  
Year 2022  
Shortage treshold 100.00%  
Wind 5 x current capacity  
Solar 20 x current capacity  
Hydro 1 x current capacity  
Biomass 1 x current capacity  
Nuclear 1 x current capacity  
  
Total consumption 261,694,797.49 MWh  
Total production: 594,652,207.53 MWh  
  Wind production: 308,483,588.24 MWh  
  Solar production: 223,102,430.64 MWh  
  Hydro production: 3,347,595.54 MWh  
  Biomass production: 14,999,268.64 MWh  
  Nuclear production: 44,719,324.46 MWh  
  
Battery storage 1000 x Tesla Big Battery 150 MW  
Storage efficiency 94%  
Stored electricity at start 375,000.00 MWh  
Stored electricity at end 750,000.00 MWh  
Wasted electricity 333,754,049.84 MWh  
  
Shortages: 7  
Worst shortage 33.00%  
Lowest storage 0.00%  
  
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

24.04.2023 V1.03 Add solar, biomass and nuclear  
                 Change from pumped storage to Tesla battery storage  
                 Improve reports  
                 Bug fixes  
  
LICENSE  
  
GNU General Public License v3  

