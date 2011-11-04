private yclnval, yclnpict, yclnlgc, yclninq

f_clrscn ("Archive Old Contract")
setcolor (gbluecolor)

yclnval = date () - 180

@ 10, 5 say "Enter Contract Check In Date for contracts to archive " ;
   get yclnval
if f_rd () = 27
   return
endif

if f_confirm ("Are you sure to archive contracts closed before " + ;
      dtoc (yclnval) + "? [Y/N]", "YN") = "N"
   return
endif

f_use ("RAAGRH")
go top
ycnt = 0
yarch = 0
copy to (gstnpath + "RAAGRH") for rsysmaok ()
use

@ 14, 0 clear to 16, 79

select 0
use (gstnpath + "RAAGRH") exclusive

ydiskno = 1
do while .not. eof ()
   do while .t.
      if f_popup ("Insert a blank formatted diskette and hit ENTER...", .t.) = 27
         if f_confirm ("Do you want to quit? [Y/N] ", "YN") = "Y"
            close database
            erase (gstnpath + "RAAGRH.DBF")
            return
         endif
      endif
      if file ("A:RAAGRH.DBF")
         loop
      endif
      copy stru to a:raagrh.dbf
      if .not. file ("a:raagrh.dbf")
         loop
      endif
      exit
   enddo

   @ 14, 5 say "Disk " + str (ydiskno, 2)
   nrecs = int ((diskspace (1) - header ()) / recsize ()) - 2
   ncpy = 0
   copy to a:raagrh while rsysaa1 ()
   ydiskno = ydiskno + 1
   skip
enddo
use

f_popup ("Deleting Archived Records", .f.)
set exclusive on
f_use ("raagrh")
set exclusive off
delete for fdatein < yclnval

f_popup ("Packing history database", .f.)
pack
close databases
erase (gstnpath + "RAAGRH.DBF")

f_popup ("Archive Complete.  Press ENTER...", .t.)


******************************
function rsysaa1

ncpy = ncpy + 1
@ 16, 5 say "Record " + str (ncpy, 6)
return (ncpy < nrecs)


******************************
function rsysmaok

ycnt = ycnt + 1
@ 14, 5 say "Scanning Contracts " + str (ycnt, 6) + " of " + ;
   str (reccount (), 6)
if frastat = "O" .or. fdatein >= yclnval
   return .f.
endif
yarch = yarch + 1
@ 16, 5 say "Contract Archived  " + str (yarch, 6)
return .t.
