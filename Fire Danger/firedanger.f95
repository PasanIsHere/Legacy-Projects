!Program Refactored by Pasan Undugodage
!Student ID: 1037862
!Date:2/8/2021
!Original Code Can Be Found At https://www.nrs.fs.fed.us/pubs/rn/rn_nc079.pdf
program main
    implicit none
    real dryBulbTemp, wetBulbTemp, isSnow, windSpeed,  herbState, precipitation
    real fineFuelMoisture, adjustedFuelMoisture, fineFuelSpreadIndex, timberSpreadIndex, &
    fireLoadIndex, buildUpIndex
    fineFuelMoisture = 0
    adjustedFuelMoisture = 0
    fineFuelSpreadIndex = 0
    timberSpreadIndex = 0
    fireLoadIndex = 0
    print *,'-------------------------------------'
    call getInputs(dryBulbTemp, wetBulbTemp, isSnow, windSpeed, buildUpIndex, herbState, precipitation)
    call firedanger(dryBulbTemp, wetBulbTemp, isSnow, windSpeed,  herbState, precipitation, &
    fineFuelMoisture, adjustedFuelMoisture, fineFuelSpreadIndex, timberSpreadIndex, fireLoadIndex, buildUpIndex)
    call printOutput(fineFuelMoisture, adjustedFuelMoisture, fineFuelSpreadIndex, timberSpreadIndex,&
    fireLoadIndex, buildUpIndex)

end program main
    
    
subroutine getInputs(dryBulbTemp, wetBulbTemp, isSnow, windSpeed, buildUpIndex, herbState, precipitation)
    real, intent(out) :: dryBulbTemp, wetBulbTemp, isSnow, windSpeed, buildUpIndex, herbState, precipitation
    print *,'INPUTS:'
    print *, '  Dry bulb temperature = '
    read(*,*) dryBulbTemp
    print  *, '  Wet bulb temperature = '
    read(*,*) wetBulbTemp
    print  *, '  Is there snow? (Enter 0 for No and 1 for Yes) = '
    read(*,*) isSnow
    print *, '  Wind Speed mph? = '
    read(*,*) windSpeed
    print *, '  Build Up Index? = '
    read(*,*) buildUpIndex
    print *, '  Herb State (1 for Cured, 2 for Transition, 3 for Green) = '
    read(*,*) herbState
    print *, '  Precipitation = '
    read(*,*) precipitation   
end subroutine getInputs
    
subroutine printOutput(fineFuelMoisture, adjustedFuelMoisture, fineFuelSpreadIndex, timberSpreadIndex,&
    fireLoadIndex, buildUpIndex)
    real, intent(in) :: fineFuelMoisture, adjustedFuelMoisture, fineFuelSpreadIndex, timberSpreadIndex,&
    fireLoadIndex, buildUpIndex
    print *,'OUTPUTS:'
    print '("   Fine Fuel Moisture      =  ", F10.5)', fineFuelMoisture
    print '("   Adjusted Fuel Moisture  =  ", F10.5)', adjustedFuelMoisture
    print '("   Fine Fuel Spread        =  ", F10.5)', fineFuelSpreadIndex
    print '("   Timber Spread Index     =  ", F10.5)', timberSpreadIndex
    print '("   Fire Load Index         =  ", F10.5)', fireLoadIndex
    print '("   Build Up Index          =  ", F10.5)', buildUpIndex
end subroutine printOutput
    
subroutine firedanger(dryBulbTemp, wetBulbTemp, isSnow, windSpeed ,herbState, precipitation, &
    fineFuelMoisture, adjustedFuelMoisture, fineFuelSpreadIndex, timberSpreadIndex, fireLoadIndex, buildUpIndex)
    real, intent(in) :: dryBulbTemp, wetBulbTemp, isSnow, windSpeed, herbState, precipitation
    real, intent(out) :: fineFuelMoisture, adjustedFuelMoisture, fineFuelSpreadIndex, timberSpreadIndex, &
    fireLoadIndex, buildUpIndex
    real bulbTempDif, dryingFactor
    real :: A(4), B(4), C(3),D(6)  
    integer i , calcTimber

    !Table Values 
    A = [-0.185900,-0.85900,-0.059660,-0.077373]
    B = [30.0, 19.2, 13.8, 22.5]
    C = [4.5, 12.5, 27.5]
    D = [16.0, 10.0, 7.0, 5.0, 4.0, 3.0 ]

    if (isSnow /= 0) then
        !There is snow we set all spread indexes to zero

        fineFuelSpreadIndex = 0
        timberSpreadIndex = 0
        if (precipitation > 0.1) then
            buildUpIndex = -50.*ALOG(1.-(1-EXP (-buildUpIndex/50.0))*EXP (-1.175*(precipitation-.1)))     
        end if
        if (buildUpIndex < 0) then
            buildUpIndex = 3
        end if
        return 
    end if
 
    !Calculate Fine Fuel Moisture
    bulbTempDif = dryBulbTemp - wetBulbTemp
    i = 1
    do while(i<4 )
        if (bulbTempDif- C(i) <= 0) exit
        i = i + 1 
    end do

    !Calculate fine Fuel Moisture
    fineFuelMoisture = B(i)*EXP (A(i)*bulbTempDif)
    
    !Find Drying Factor
    i = 1
    dryingFactor = 0
    do while (i<7)
        if (fineFuelMoisture - D(i) > 0) then
            dryingFactor = i -1
            exit
        end if
        i = i + 1
    end do

    if (fineFuelMoisture < 1)  fineFuelMoisture = 1
    
    !Add 5 percent for each herb state  stage great than 1
    fineFuelMoisture = fineFuelMoisture + (herbState - 1) * 5.

    !Account for the precipitation  
    if (precipitation > 0.1) buildUpIndex = -50.*ALOG(1.-(1-EXP (-buildUpIndex/50.))*EXP (-1.175*(precipitation-.1)))     
    if (buildUpIndex < 0)   buildUpIndex = 0

    !Account for the drying factor
    buildUpIndex = buildUpIndex + dryingFactor
    
    adjustedFuelMoisture = 0.9 * fineFuelMoisture + 0.5 + 9.5*EXP (-buildUpIndex/50.)

    calcTimber = 1 !varaible to indicate if we calculate Timber Index 
    
    if (adjustedFuelMoisture >= 30.0) then
        calcTimber = 0
        timberSpreadIndex = 1  
        if(fineFuelMoisture > 30) then
            fineFuelSpreadIndex = 0
            return 
        end if 
    end if 
    
    !Account for windspeed
    if (windSpeed >= 14.0) then 
        fineFuelSpreadIndex   = 0.00918*(windSpeed+14.)* (33.0 -fineFuelMoisture)**1.65- 3.0
        if (calcTimber == 1) timberSpreadIndex = 0.00918*(windSpeed+14.)* (33.0 -adjustedFuelMoisture)**1.65- 3.0
        
        if ( fineFuelSpreadIndex  > 99 ) then
            fineFuelSpreadIndex  = 99
            if (timberSpreadIndex > 99) timberSpreadIndex = 99
        end if
    else 
        fineFuelSpreadIndex =  0.01312*(windSpeed+6.0)* (33.0- fineFuelMoisture)**1.65- 3.0
        if (calcTimber == 1) timberSpreadIndex = 0.01312*(windSpeed+6.0)* (33.0- adjustedFuelMoisture)**1.65- 3.0
        
    end if 
     !if buildUpIndex or fineFuelIndex is <= 0 we dont calculate fireloadIndex
    if (fineFuelSpreadIndex <= 0) return
    if (buildUpIndex <= 0) return

    !calculate fireLoadIndex
    fireLoadIndex = 1.75*ALOG10(timberSpreadIndex ) + 0.32*ALOG10(buildUpIndex ) - 1.640
    if (fireLoadIndex <= 0) then
        fireLoadIndex = 0
        return
    end if
    fireLoadIndex = 10.0 ** fireLoadIndex
    return
end subroutine firedanger
    
