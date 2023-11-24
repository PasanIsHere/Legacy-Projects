!Program Refactored by Pasan Undugodage
!Student ID: 1037862
!Date:2/8/2021
!Original Code Can Be Found At https://www.nrs.fs.fed.us/pubs/rn/rn_nc079.pdf

program main
    implicit none
    character (len=25) :: fname
    character (len = 1) :: curChar, prev1Char, prev2Char
    logical :: lexist, wordFlag
    integer :: EOF,counter, numWords, numSentences, wordSyllables, wordLen, totalSyllables
    real ::fRIndex, fKGLevel

    print *, 'Enter File Name (i.e file.txt): '
    read(*,*) fname
    wordLen = 0
    prev1Char = 'z'
    prev2Char = 'z'
    numSentences = 0
    numWords = 0
    EOF = 0
    counter = 1
    totalSyllables = 0

    wordFlag = .true.
    wordSyllables = 0 
    inquire(file=fname, exist=lexist)
    if (lexist) then
       open(unit=1,file=fname, recl = 1, access = "direct", form = "unformatted")
       do while(EOF ==0)
       read(1,rec = counter,  IOSTAT = EOF) curChar
        
        if(EOF == 0) then

            if (index(" .,;?!:", curChar) /= 0 .or. ichar(curChar) == 10 ) then

                if(wordFlag .eqv. .false.) then

                    !ignore "e" endings
                    
                    
                    if (index("eE", prev1Char) /= 0  ) then

                            !ignores the case where l comes before e
                            if(index("lL", prev2Char) == 0 ) then 
                                wordSyllables = wordSyllables -1
                                print *, prev1Char
                                print *, prev2Char
                            end if
                    end if
                   
                   
                    !ignore "ed" endings 
                    if (index("dD", prev1Char) /= 0 .and. index ("eE",prev2Char) /= 0) then
                        wordSyllables = wordSyllables -1
                    end if

                    !ignore "es" endings 
                    if (index("sS", prev1Char) /= 0 .and. index ("eE",prev2Char) /= 0) then
                        print *, prev1Char
                        print *, prev2Char
                        wordSyllables = wordSyllables -1
                    end if

                    numWords = numWords +1

                    prev1Char = 'z'
                    prev2Char ='z'

                    if(wordLen <= 3) then
                        wordSyllables = 1
                    end if

                    if (wordSyllables <= 0) then
                        totalSyllables = totalSyllables + 1
                        wordLen = 0
                    else
                        totalSyllables = totalSyllables + wordSyllables
                    end if
                endif 

                wordFlag = .true.
                if (index(".;?!:", curChar) /= 0 ) then

                    numSentences = numSentences + 1
 
                endif
                


                wordLen = 0
                wordSyllables = 0
                

            else 
                wordFlag = .false.
                wordLen = wordLen +1

            endif
            
            if (index("aAeEiIoOuUyY", curChar) /= 0 ) then

                if (index("aAeEiIoOuUyY", prev1Char) == 0 ) then
                    wordSyllables = wordSyllables + 1
                end if
            endif 

            counter = counter + 1
        
        end if
        prev2Char = prev1Char
        prev1Char = curChar
       
        end do
       close(1)
    else

       print *, 'File does not exist. Exiting...!'

    end if


    if(numWords == 0) then
        numWords = 1
    end if
    if(numSentences == 0) then
        numSentences = 1
    end if
    !calculate Flesch Readability Index

    
    !fKGLevel = 
    call calcFlesch (totalSyllables,numWords, numSentences ,fRIndex, fKGLevel )
    call printOutput(totalSyllables,numWords, numSentences ,fRIndex, fKGLevel )


end program main
    
 
subroutine calcFlesch (totalSyllables,numWords, numSentences , fRIndex, fKGLevel )
    integer, intent(in) :: totalSyllables,numWords, numSentences 
    real, intent(out) :: frIndex, fkGLevel
    fRIndex =  206.835 - (1.015* (real(numWords)/real(numSentences)) + 84.6 *  (real(totalSyllables) / real(numWords)))
    fKGLevel = (0.39 * (real(numWords) / real(numSentences)) + 11.8 * (real(totalSyllables) /real(numWords)))
    fkGLevel = fkGLevel - 15.59
end subroutine calcFlesch


subroutine printOutput(totalSyllables,numWords, numSentences ,frIndex, fKGLevel )
    integer, intent(in) :: totalSyllables,numWords, numSentences 
    real, intent(in) :: frIndex, fKGLevel

    print *,'Flesch Analysis:'
    print '("   Syllable Count              =      ", I0)', totalSyllables
    print '("   Word Count                  =      ", I0)', numWords
    print '("   Sentence Count              =      ", I0)', numSentences

    print  *,'----------------------------------------------------------'
    print '("   Flesch Readability Index    =  ", F10.2)', (fRIndex)
    print '("      -Rounded To Nearest Int  :      ", I0)', nint(fRIndex)
    print '("   Flesch-Kincaid Grade Level  =  ", F10.2)', fKGLevel
    print '("      -Rounded To Nearest Int  :      ", I0)', nint(fKGLevel)
end subroutine printOutput