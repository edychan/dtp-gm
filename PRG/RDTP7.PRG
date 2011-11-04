*
* end of month report
*
* 
f_clrscn ("End of Month Report")

f_box (01, 02, 05, 78)
@ 02, 04 say "Branch #......"
@ 03, 04 say "Date from....."
@ 04, 04 say "Date to......."
xbranch = space(3)
xdate1 = ctod ("  /  /  ")
xdate2 = ctod ("  /  /  ")

do while .t.
   @ 02, 19 get xbranch pict "!!!" valid f_valid (rdtp7a())
   @ 03, 19 get xdate1 valid f_valid(.not.empty(xdate1).and.f_y2k(@xdate1))
   @ 04, 19 get xdate2 valid f_valid(f_y2k(@xdate2).and.xdate2 > xdate1)
   if f_rd () = 27
      close data
      return
   endif
   yret = f_confirm ("[C]ontinue  [E]dit  [Q]uit ","CEQ")
   if yret = "C"
      exit
   elseif yret = "Q"
      close data
      return
   endif
enddo

f_use ("ravin")
f_use ("radtrh")
select radtrh
set softseek on
seek xbranch+dtos(xdate1)
set softseek off
declare y3 [4], y4 [4]
y3 [1] = 0
y3 [2] = 0
y3 [3] = 0
y3 [4] = 0
y4 [1] = 0
y4 [2] = 0
y4 [3] = 0
y4 [4] = 0

f_popup ("Please Wait While Processing ...")
private ytrans, ysum
ytrans = 0
ysum = 0
do while .not. eof () .and. radtrh->fbranch = xbranch .and.   ;
   radtrh->fdate >= xdate1 .and. radtrh->fdate <= xdate2
   ytrans = ytrans + 1
   if radtrh->ftitle > 0
      y3 [3] = y3 [3] + 1
      y4 [3] = y4 [3] + radtrh->ftitle
   endif
   if radtrh->ftfee > 0
      y3 [4] = y3 [4] + 1
      y4 [4] = y4 [4] + radtrh->ftfee
   endif
   if radtrh->freg > 0
      select ravin
      seek substr(radtrh->fvin,1,8)+substr(radtrh->fvin,10,1)
      if ravin->ffee = "01"
         y3 [1] = y3 [1] + 1
         y4 [1] = y4 [1] + radtrh->freg
      else
         y3 [2] = y3 [2] + 1
         y4 [2] = y4 [2] + radtrh->freg
      endif
   endif 
   select radtrh
   skip
enddo
ysum = y4 [1] + y4 [2] + y4 [3] + y4 [4]

* start printing here
if f_popup ("Press <Enter> to Begin or <Esc> to Stop Printing...", .t.) = 27
   close data
   return
endif

l_formlen = 80
l_date = dtoc(date())
l_time = time()

set console off
set print on
set device to print
set printer to lpt1

@ 02, 02 say "DATE: "+dtoc(date())
@ 02, 28 say gtitle
@ 03, 02 say "TIME: "+time()
@ 03, 22 say "END OF MONTH REPORT - FEE DISTRIBUTION"
@ 04, 27 say "BRANCH #: "+xbranch+"   "+"DATE: "+dtoc(xdate1)+" - "+dtoc(xdate2)
@ 06, 01 say " TRANS CODE     DESCRIPTION                          # TRANS    TRANS TOTAL"
@ 07, 01 say " ----------     ----------------------------------   ---------  -----------"
@ 09, 01 say "    01          PASSENGER PLATES "
@ 09, 57 say str(y3[1],4)
@ 09, 66 say str(y4[1],10,2)
@ 11, 01 say "    03          COMMERCIAL PLATES "
@ 11, 57 say str(y3[2],4)
@ 11, 66 say str(y4[2],10,2)
@ 13, 01 say "    21          TITLE APPLICATION FEE "
@ 13, 57 say str(y3[3],4)
@ 13, 66 say str(y4[3],10,2)
@ 15, 01 say "    22          PLATE TRANSFER "
@ 15, 57 say str(y3[4],4)
@ 15, 66 say str(y4[4],10,2)
@ 17, 66 say str(ysum,10,2)
@ 18, 66 say "=========="
@ 20, 01 say "TOTAL TRANSACTIONS: "+str(ytrans, 6)
eject

set printer to
set console on
set device to screen

close data

******************************
function rdtp7a

f_use ("ractr")
locate for fbranch = xbranch
if eof ()
   return .f.
else
   return .t.
endif

