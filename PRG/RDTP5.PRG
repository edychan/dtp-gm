* ========================================================================
* end of day processing
*
* 09/11/96: add 10th digit (model year) to vin chart
* ------------------------------------------------------------------------
* 11.03.08: post date updated without process SOS send file (rdtp5a)
* ------------------------------------------------------------------------
f_clrscn ("End of Day Processing")

close data            && 02/05/97

f_use ("rasys")
xpost = fpostdate                 && last posted date
use

f_box (01, 02, 10, 78)
@ 02, 04 say "Branch #......"
@ 02, 50 say "Last Posted on.... " + dtoc(xpost)
@ 03, 04 say "Date.........."

f_use ("ravin")
f_use ("radtrh")
xbranch = gloc
xdate = date()

do while .t.
   @ 02, 19 get xbranch pict "!!!" valid f_valid (rdtp5b())
   @ 03, 19 get xdate valid f_valid(f_y2k(@xdate).and.xdate > xpost,"Last posted on "+dtoc(xpost)) .and. ;
       f_valid (dow(xdate)>=2.and.dow(xdate)<=6, "Must be Monday thru Friday only...")
   if f_rd () = 27
      close data
      return
   endif
   yret = f_confirm ("[C]ontinue  [E]dit  [Q]uit ","CEQ")
   if yret = "C"
      select radtrh
      set softseek on
      seek xbranch + dtos(xdate)
      set softseek off     
      if eof () 
         f_valid (.f., "No transaction to be processed...") 
         loop
      elseif radtrh->fbranch <> xbranch .or. radtrh->fdate <> xdate    && 11.03.08: bug??
         f_valid (.f., "No transaction to be processed for "+xbranch)
         loop
      else                             
         if f_confirm ("Are you sure [Y/N] ? ","YN") = "Y"
            exit
         else
            loop
         endif
      endif
   elseif yret = "Q"
      close data
      return
   endif
enddo

* confirm user to run eod processing

* step 1
@ 05, 04 say "Step 1. Reset Counter "
f_use ("ractr")
locate for fbranch = xbranch
if .not. eof ()
   reclock ()
   replace fseq1 with 1
   commit
   unlock
endif
use

* step 2
@ 06, 04 say "Step 2. Create Send File "
do rdtp5a with xbranch, xdate

* step 3
@ 07, 04 say "Step 3. Reset Tab File "

xfil = gdbfpath + "radtrp"
yfil = gdbfpath + "tmp"
set excl on
f_use ("radtrp")
copy stru to &yfil
use
use &yfil
copy stru to &xfil
use
f_use ("radtrp") 
reindex
use
set excl off

* clean tab file
xfil = gdbfpath + "ratab"
yfil = gdbfpath + "tmp"
set excl on
f_use ("ratab")
copy to &yfil for fstatus <> "U" .and. .not. deleted()
use
use &yfil
copy to &xfil
use
f_use ("ratab")
reindex
use
set excl off

* delete audit file
xfil = gdbfpath + "raaudit"
yfil = gdbfpath + "tmp"
set excl on
f_use ("raaudit")
copy stru to &yfil
use
use &yfil
copy stru to &xfil
use
f_use ("raaudit")
reindex
use
set excl off

* step 4
@ 08, 04 say "Step 4. Print End of Day Report "
do rdtp4b with xbranch, xdate

* update post date
f_use ("rasys")
go top
reclock ()
replace fpostdate with xdate
unlock
commit

f_valid (.f., "Process completed...")

close data

************************
function rdtp5b

f_use ("ractr")
locate for fbranch = xbranch
if eof ()
   return .f.
else
   return .t.
endif

