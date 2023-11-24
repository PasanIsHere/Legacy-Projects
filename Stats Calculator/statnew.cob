identification division.
program-id. statnew.

environment division.
input-output section.
file-control.
select input-file assign to inFileName
   organization is line sequential.
select output-file assign to outFileName
   organization is line sequential.       
data division.
file section.
fd input-file.
   01 number-info.
   05 num pic 9(14)V9(4).
fd output-file.
   01 output-line pic X(80).


working-storage section.
01 inFileName   pic x(60). *>file name for input
01 outFileName  pic x(60). *>file name for output
77 i pic 999 value 1. *> counter variable
77 j pic 999 value 1. *> counter varaible
77 numData pic S9999. *>number of data points
77 temp pic S9(14)V9(4). *> temp varaible to hold thinks
77 midpoint pic 999 value 0. *>sorted array midpoint index 
77 tempMean pic S9(14)V9(14). *> temporary mean varaible
77 sumData pic S9(14)V9(4) value 0. *>holds sum  
77 mean pic S9(14)V9(4) value 0. *>holds mean 
77 median pic S9(14)V9(4) value 0. *>holds median
77 stdDev pic S9(14)V9(4) value 0. *>holds standard deviation
77 geoMean pic S9(14)V9(4) value 0. *>holds geometric mean
77 harMean pic S9(14)V9(4) value 0. *>holds harmonic mean
77 variance pic S9(14)V9(14) value 0.  *> holds variance
77 feof pic A(1). *>end of file checker

*>strings structs for output
01 print-dpTitle.
    02 filler pic X(44) value  '            Sorted Data Values              '.
01 print-title.
    02 filler pic X(44) value  '               Data Statistics     '.
01 print-lineBreak.
    02 filler pic X(44) value  '--------------------------------------------'.
01 print-dataValue. 
    02 filler pic X(24) VALUE  '                        ' .
    02 out-dataValue pic -(14)9.9(4).
01 print-mean. 
    02 filler pic X(24) VALUE  ' Mean                = ' .
    02 out-mean pic -(14)9.9(4).
01 print-geoMean.
   02 filler pic X(24) VALUE  ' Geometric Mean      = '.
   02 out-geoMean pic -(14)9.9(4).
01 print-harMean.
   02 filler pic X(24) VALUE ' Harmonic Mean       =  '.
   02 out-harMean pic -(14)9.9(4).
01 print-median.
   02 filler pic X(24) VALUE ' Median              =  '.
   02 out-median pic -(14)9.9(4).
01 print-var.
   02 filler pic X(24) VALUE ' Variance            =  '.
   02 out-Var pic -(14)9.9(4).
01 print-stdDev.
   02 filler pic X(24) VALUE ' Standard Deviation  =  '.
   02 out-stdDev pic -(14)9.9(4). 

01 arr.
   02 dataArray  pic S9(14)V9(4) occurs 1000 times. *> holds all data values

procedure division.
   *>get input and output file names from user
   display 'Enter filename for input: '
   accept inFileName 
   display 'Enter file name for output: '
   accept outFileName

   open input input-file, output output-file.
   perform read-num until feof='T' *>reads numbers until the end of the file
   compute numData = i - 1 
   compute j = i 
   perform calc-mean
   perform calc-stdDev
   perform calc-geoMean
   perform calc-harMean
   perform bubble-sort
   perform calc-median
   perform write-output
   close input-file, output-file.
   display 'The output has been saved to the specified file'
stop run.

*>reads in values from file 
read-num.
   read input-file
        at end move 'T' to feof *> if at the end of the file set feof to true
        not at end perform store-element
   end-read.

*>stores number into array
store-element.
   compute sumData = sumData + num
   move num to dataArray(i).
   compute i = i + 1.

*>writes each data value to file
print-nums.
   perform varying i from 1 by 1 until i > numData
      move dataArray(i) to out-dataValue
      write output-line from print-dataValue after advancing 1 lines
   end-perform.

*>sorts array in ascending using bubble sort algorithm
bubble-sort.
   perform varying i from 1 by 1 until i > numData
      perform varying j from 1 by 1 until j > numData - i
          if dataArray(j) > dataArray(j + 1)
             move dataArray(j) to temp
             move dataArray(j + 1) to dataArray(j)
             move temp to dataArray(j + 1)
          end-if
      end-perform
   end-perform.

*>calculates mean of the data
*>formula: summation of every value divided by total number of values
calc-mean.
    compute mean = sumData / numData
    move mean to out-mean.

*>calculates standard deviation of the data
*>note that variance is standard deviation squared
*> formula: https://www.mathsisfun.com/data/standard-deviation-formulas.html
calc-stdDev.
    compute temp  = 0.
     *> subtract the mean from each  value and then square
     *> sum all those new values
     perform varying i from 1 by 1 until i > numData
         compute variance  = variance + ( dataArray(i) - mean ) **2
         compute temp  = dataArray(i) - mean
      end-perform   
     *> divide by the total number of Data values
     compute variance  = variance /numData
     *>get the square root of the variance to calculate standard deviation 
     compute stdDev  = variance ** 0.5 
     
     move variance to out-Var.
     move stdDev to out-stdDev.

*>calculates median of the data
*>formula: https://en.wikipedia.org/wiki/Median
calc-median.
     compute temp = function mod (numData 2). *>determines if there are odd/even number of values
     compute midpoint = (numData + 1) / 2
     compute median = dataArray(midpoint)
     if temp = 0  *> if even there are "2" middle points, take the average of those
        compute median = (median + dataArray(midpoint + 1 )) / 2 
     end-if
     move median to out-median.

*>calculates geometric mean of the data
*>formula: https://en.wikipedia.org/wiki/Geometric_mean
calc-geoMean.
     compute temp = 1
     *>multiply all the values together
     perform varying i from 1 by 1 until i > numData
         compute temp = temp * dataArray(i)
     end-perform
     *>take the nth root where n is the total number of values
     compute geoMean = temp ** (1 / numData)
     move geoMean to out-geoMean.

*>calculates harmonic mean of the data
*>formula: https://en.wikipedia.org/wiki/Harmonic_mean
calc-harMean.
     compute tempMean = 0
     *>sum up the reciprocal of each number 
     perform varying i from 1 by 1 until i > numData
         compute tempMean = tempMean + (1 / dataArray(i)) 
     end-perform
     *> divide the total number of values by the sum the previous sum
     compute harMean = numData / tempMean
     move harMean to out-harMean.

*>writes output to file
write-output.
   write output-line from print-lineBreak after advancing 0 lines
   write output-line from print-dpTitle after advancing 1 lines
   write output-line from print-lineBreak after advancing 0 lines
   perform print-nums *> writes each value 
   write output-line from print-lineBreak after advancing 1 lines
   write output-line from print-lineBreak after advancing 2 lines
   write output-line from print-title after advancing 1 lines
   write output-line from print-lineBreak after advancing 1 lines
   write output-line from print-mean after advancing 1 lines
   write output-line from print-geoMean after advancing 1 lines
   write output-line from print-harMean after advancing 1 lines   
   write output-line from print-median after advancing 1 lines
   write output-line from print-var after advancing 1 lines
   write output-line from print-stdDev after advancing 1 lines     
   write output-line from print-lineBreak after advancing 1 lines.
