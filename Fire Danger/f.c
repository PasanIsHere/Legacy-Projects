

sub R( ,WET, ISNOW,PRECIP,WIND,IHERP,DF,ADFM,GRASS.TIMBER.FLOAD )





 DF,FFM,ADFM,GRASS,TIMI_ER,FLOAD) DNGR 002 C IF FINE FUEL MOISTURE IS ONE OR LESS WE SET IT TO ONE
C ROUTINEFOR COMPUTINGNATIONALFIRE DANGERRATINGSAND FIRE LOAD INDEX 10 IF ( FFM-I. ) 11,12,12 DNGR 045
C DATANEEDEDFORTHE CALCULATIONASRE= 11 FFM=I. DNGR046
C . DRY', DRYBULBTEMPERATURE C ADD5 PERCENTFINE FUELMOISTUREFOREACHHERBSTAGEGREATERTHANONE
C WET, WETBULB TEMPERATURE 12 FFM = FFM + ( IHERB-I ) * 5. DNGR 047 m
c
C
ISNOW,
WIND,
S
THE
OME
CURRENT
POSITIV
WIND
ENON
SPEED
ZERO NUMBER
IN MILES
IF
PER
THERE
HOUR
IS SNOW ON THE GROUND C WE
IF
MU
(PRECI
ST AD
P
J
-.
US
I
T
)
THE
15,
B
1
U
5,
I F
13
OR PRECIPITATIONBEFORE ADDINGTHE DRYING F
DNGR
ACTOR
048 m
C BUO,- THE LAST VALUE OF THE BUILD UP INDEX .....:
C IHERB, THE CURRENTHERB STATE OF THE DISTRICTI=CURED,2=TRANSITION,3=GREEN
C DATA RETURNEDFROM THE SUBROUTINEARE C PRECIPITATIONEXCEEDED 0.10 INCHES WE MUST REDUCE THE
C DRYING FACTORAS DF C BUILD UP INDEX (BUO) BY AN AMOUNT EQUAL TO THE RAIN FALL
C FINE FUEL MOISTUREAS FFM 13 BUO=-50.*ALOG(I.-(I.-EXP(-BUO/50.))*EXP(-l.175*(PRECIP-.l))) DNGR 049
C ADJUSTED(10 DAY LAG) FUEL MOISTUREAS ADFM IF ( BUO ) 14,15,15 DNGR 050 F"
C .GRASSSPREAD INDEXWILL BE RETURNEDAS GRASS 14 BUO=O.O DNGR 051
C TIMBER SPREAD INDEXWILL BE RETURNEDAS TIMBER C AFTER CORRECTIONFOR RAIN, IF ANY, WE ARE READY TO ADD TODAY'S
C FIRE LOAD RATING (MAN-HOURBASE) AS FLOAD 

*/

int main(){
   
   
   
   
   
    return 0;
}