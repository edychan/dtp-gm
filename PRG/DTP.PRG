* ===========================================================================
*
* DTP - Direct Transaction Processing -  main program
*
* year 2000
* laser printing forms
* 
* 09.28.98
*    special GM interface (rdtp1)
* 10.11.98
*    print signature for tr11
* 10.28.98
*    add interface for delete transactions (rdtp2)
* 11.10.98
*    process branch trans into central (801)
*    implement eod report to print all branches
* 01.03.07: skip [U] & use [V] for year 2007 (jdate)
* ---------------------------------------------------------------------------
* 07.13.09: D-date for GM name change to [GENERAL MOTOTS COMPANY]
*           All transaction starting from 07.13.09 will be sent to SOS using
*           [GENERAL MOTOTS COMPANY] without exception.
*           Lien = [GELCO CORPORATION], default = [NONE]
* ---------------------------------------------------------------------------
* 05.09.10: 1. remove US Department of Treasury as 1st secured party
*           2. change default lien holder to STATE ST BK&TRST ... 
*           3. retain <F3> to de-select lien holder if necessary
* ===========================================================================
do while .t.
   begin sequence
      do rinit
      restore from (gmempath + "RAPATH.MEM") additive

      do while .t.
         if .not. usrlgin ()
            loop
         endif
         do while .t.
            xret0 = f_pushmenu ("MAIN MENU")
            if xret0 = 0
               if usrlgout (.t.)
                  exit
               else
                  loop
               endif
            endif
            xoption0 = substr (goption [xret0], 4)
            bliovlclr ()
            do case
            case xret0 = 1        && D.T.P. transactions
               do rdtp

            case xret0 = 2        && VEHICLE RECORD
               do rinv

            case xret0 = 3        && D.T.P. communication
               do rcom

            case xret0 = 4        && REPORT
               do rrpt

            case xret0 = 5        && SYSTEM MAINT.
               do rsys

            endcase
         enddo
      enddo
   end
   set key 28 to
   set key -1 to
   set key -2 to
   set key -3 to
   set key -4 to
   set key -5 to
   set key -6 to               && calculator
   set key -7 to               && phone book
   set key -8 to               && reminder
   set key -9 to               && calendar
   usrlgout (.f.)
enddo

*****************************
function f_goodvin
parameter xvin
private yvin

p1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
p2 = "123456780123450709234567890123456789"
wf = "8765432X098765432"

if len(alltrim(xvin)) <> 17
   f_valid (.f., "Invalid VIN #...")
   return .f.
endif

yvin = upper(xvin)
y3 = 0
for i = 1 to 17
   y1 = substr(yvin,i,1)
   y2 = val(substr(p2,at(y1,p1),1)) *    ;
        if(substr(wf,i,1)="X",10,val(substr(wf,i,1)))
   y3 = y3 + y2
next i
y4 = y3 - int(y3/11) * 11
cd = if(y4 = 10, "X", str(y4,1)) 

if cd <> substr(yvin,9,1) 
   f_valid (.f., "Please re-check VIN #....")
   if f_confirm ("[R]etype    [C]ontinue ", "RC") = "R"
      return .f.
   endif
endif

return .t.

**********************************************************************
function jdate

parameter xdate
private ydate, y1, y2, y3, yday, yyr, y4

ydate = dtoc (xdate)

y1 = substr(ydate,4,2)
y2 = substr(ydate,1,2)
y3 = substr(ydate,7,2)
y4 = substr(dtos(xdate),1,4)   && 01.04.00

yday = ctod (y2+"/01/"+y4) - ctod ("01/01/"+y4) + val(y1)

do case
   case y3 = "96"
     yyr = "G"
   case y3 = "97"
     yyr = "H"
   case y3 = "98"
     yyr = "J"
   case y3 = "99"
     yyr = "K"
   case y3 = "00"
     yyr = "L"
   case y3 = "01"
     yyr = "M"
   case y3 = "02"
     yyr = "N"
   case y3 = "03"
     yyr = "P"
   case y3 = "04"
     yyr = "R"
   case y3 = "05"
     yyr = "S"
   case y3 = "06"
     yyr = "T"
*   case y3 = "07"
*     yyr = "U"
   case y3 = "07"
     yyr = "V"
   case y3 = "08"
     yyr = "W"
   case y3 = "09"
     yyr = "X"
   case y3 = "10"
     yyr = "Y"
   case y3 = "11"
     yyr = "Z"
   otherwise
     yyr = " "
endcase
return (yyr+strtran(str(yday,3)," ","0"))

