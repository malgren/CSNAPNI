#anim_query_data

if(print_tags == 1){
  print("CreateInputsSubs/NASSpull/anim_query_data.R")
}

# inventory data for
#(1) fattened cattle (uses data from `CATTLE, ON FEED - SALES FOR SLAUGHTER, MEASURED IN HEAD`)
#(2) milk cows (uses data from `CATTLE, COWS, MILK - INVENTORY`)
#(3) hogs for breeding, (uses data from 'HOGS, BREEDING - INVENTORY')
#(4) hogs for slaughter, (uses data from 'HOGS - INVENTORY', 'HOGS, BREEDING - INVENTORY','HOGS - SALES, MEASURED IN HEAD')
#(5) chicken layers, (uses data from 'CHICKENS, LAYERS - INVENTORY')
#(6) breeding turkeys, (uses data from 'TURKEYS - INVENTORY','TURKEYS - SALES, MEASURED IN HEAD')
#(7) chicken pullets, (uses data from 'CHICKENS, PULLETS, REPLACEMENT - INVENTORY',"CHICKENS, PULLETS, REPLACEMENT - SALES, MEASURED IN HEAD")
#(8) chicken broilers, (uses data from 'CHICKENS, BROILERS - INVENTORY',"CHICKENS, BROILERS - SALES, MEASURED IN HEAD")
#(9) slaughter turkeys, (uses data from 'TURKEYS - INVENTORY','TURKEYS - SALES, MEASURED IN HEAD')
#(10) beef breeding herd, (uses data from `CATTLE, COWS, BEEF - INVENTORY`, `CATTLE, (EXCL COWS) - INVENTORY`)
#(11) beef calves, (uses data from `CATTLE, COWS, BEEF - INVENTORY`,'CATTLE, CALVES - SALES, MEASURED IN HEAD')
#(12) dairy calves,  (uses data from `CATTLE, COWS, MILK - INVENTORY`,'CATTLE, CALVES - SALES, MEASURED IN HEAD')
#(13) beef heifers, (uses data from `CATTLE, COWS, BEEF - INVENTORY`, `CATTLE, (EXCL COWS) - INVENTORY`)
#(14) dairy heifers, (uses data from `CATTLE, COWS, MILK - INVENTORY`, `CATTLE, (EXCL COWS) - INVENTORY`)
#(15) beef stockers, (uses data from `CATTLE, COWS, BEEF - INVENTORY`,`CATTLE, GE 500 LBS - SALES, MEASURED IN HEAD`,`CATTLE, ON FEED - SALES FOR SLAUGHTER, MEASURED IN HEAD`, `CATTLE, (EXCL COWS) - INVENTORY`)
#(16) dairy stockers, (uses data from `CATTLE, COWS, MILK - INVENTORY`,`CATTLE, GE 500 LBS - SALES, MEASURED IN HEAD`,`CATTLE, ON FEED - SALES FOR SLAUGHTER, MEASURED IN HEAD`, `CATTLE, (EXCL COWS) - INVENTORY`)
#(17) sheep, (need 'SHEEP, INCL LAMBS - INVENTORY')
#(18) horses, (need 'EQUINE, HORSES & PONIES - INVENTORY')
#(19) goats (need 'GOATS, ANGORA - INVENTORY')


anim_desc <- c('CATTLE, (EXCL COWS) - INVENTORY',
           'CATTLE, COWS, BEEF - INVENTORY',
           'CATTLE, COWS, MILK - INVENTORY',
           'CATTLE, GE 500 LBS - SALES, MEASURED IN HEAD',
           'CATTLE, ON FEED - SALES FOR SLAUGHTER, MEASURED IN HEAD',
           'CATTLE, CALVES - SALES, MEASURED IN HEAD',
           'HOGS - INVENTORY',
           'HOGS, BREEDING - INVENTORY',
           'HOGS - SALES, MEASURED IN HEAD',
           'CHICKENS, BROILERS - INVENTORY',
           'CHICKENS, BROILERS - SALES, MEASURED IN HEAD',
           'CHICKENS, LAYERS - INVENTORY',
           'CHICKENS, PULLETS, REPLACEMENT - INVENTORY',
           'CHICKENS, PULLETS, REPLACEMENT - SALES, MEASURED IN HEAD',
           'TURKEYS - INVENTORY',
           'TURKEYS - SALES, MEASURED IN HEAD',
           'SHEEP, INCL LAMBS - INVENTORY',
           'EQUINE, HORSES & PONIES - INVENTORY',
           'GOATS, MILK - INVENTORY',
           'GOATS, ANGORA - INVENTORY')


