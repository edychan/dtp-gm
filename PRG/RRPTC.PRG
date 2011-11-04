*
* print misc form
*
* changes: 08/13/96 reprint form by control #
*          09/23/96 reprint from previous download file
*
f_clrscn ("DTP - PRINT COMPUTER FORMS")

f_use ("rafrmtbl")
go top
ycnt = reccount()
declare yfrm [ycnt], ysw [ycnt]
for i = 1 to ycnt
   if file (fprtfil)
      yfrm [i] = ffrm + chr(16) + chr(16) + chr(16)
   else
      yfrm [i] = ffrm + space(3)
   endif
   skip
next 

yptr = 1
yret = f_pick_a (02, 05, "", "", yfrm, ycnt, yptr)
if yret <= 0
   close data
   return
endif

select rafrmtbl
go yret
yfrmno = upper(alltrim(ffrmno))
yprtfil = fprtfil
yprt = fprinter
ycmd1 = alltrim(fcmd1)
ycmd2 = alltrim(fcmd2)
use

f_box (02, 03, 06, 78, "You have selected to print")
@ 03, 05 say yfrm [yret]

* reprint from previous date
yans = "N"
ydate = date()
do while .t.
   @ 04, 05 say "Reprint from Previous Date ? [Y/N] " get yans pict "!"
   f_rd ()
   if yans = [Y]
      @ 05, 05 say "Enter Date: " get ydate valid ;
        f_valid (f_rrptc(ydate), "Cannot Locate Download File...")
      f_rd ()
   endif
   ykeyin = f_confirm ("[C]ontinue  [E]dit  [Q]uit", "CEQ")
   if ykeyin = "Q"
      close data
      return
   elseif ykeyin = "C"
      exit
   else
      loop
   endif
enddo

* prepare transit
yfil = gstnpath + "transit.dbf"
if .not. file (yfil)
   ytmp = "tmp.dbf"
   create &ytmp
   append blank
   replace field_name with "FIELD"
   replace field_type with "C"
   replace field_len with 132
   replace field_dec with 0 
   create &yfil from &ytmp
   use
   erase &ytmp
endif
yfil = gstnpath + "transit" 
select 0
use &yfil excl alias transit
zap

do rrptc1 with yfrmno        && alignment test

if yans = [Y]
   do rrptc2 with yfrmno, ydate
   select transit
   go top
   if eof ()
      f_valid (.f., "Cannot Locate Form from Download File...")
      close data
      return 
   endif      
else
   if file (yprtfil)
      select transit
      append from &yprtfil sdf
   else
      f_valid (.f., "All Printed...")
      close data
      return 
   endif
endif

* user can input starting control #
yc1 = 0              && start control #
yc2 = 0              && end control #
yrec1 = 0            && start record pointer
yrec2 = 0            && end record pointer
do while .t.
   @ 05, 05 say "Starting / Ending Control # or <Enter> to Begin   "
   @ 05, 55 get yc1 pict "99999" valid f_valid (yc1 >=0)
   @ 05, 61 get yc2 pict "99999" valid f_valid (yc2 >= yc1)
   if f_rd () = 27
      close data
      return
   endif 
   if yc1 = 0                           && start from the top
      select transit
      go top
      yrec1 = recno()
   else
      ys1 = [ ] + str(yc1,4) + [ ]    && start the search 
      select transit
      locate for ys1 $ field
      if eof ()
         f_valid (.f., "Cannot Locate Starting Control #, Please re-enter...")
         loop
      else
         * rewind to page break
         do while .not. eof () .and. substr(field,1,1) <> chr(12)
            skip -1
            loop
         enddo
         yrec1 = recno()
      endif
   endif
   if yc2 = 0 .or. yc2 = 99999                       && print all
      select transit
      go bottom
      yrec2 = recno()
      exit
   else
      ys1 = [ ] + str(yc2,4) + [ ]    && start the search 
      select transit
      locate for ys1 $ field
      if eof ()
         f_valid (.f., "Cannot Locate Ending Control #, Please re-enter...")
         loop
      else
         * advance to next page break
         do while .not. eof () .and. substr(field,1,1) <> chr(12)
            skip 
            loop
         enddo
         yrec2 = recno()
         exit
      endif
   endif
enddo

if f_popup ("Press <Enter> to Begin or <Esc> to Stop Printing...", .t.) = 27
   close data
   return
endif

set console off
set print on
set printer to &yprt
if .not. empty (ycmd1)
   ?? &ycmd1
endif
select transit
go yrec1
do while .not. eof () .and. recno() >= yrec1 .and. recno() <= yrec2
   key = inkey ()
   if key = 27
      set console on
      set print off
      if f_confirm ("Do you want to quit ? ", "YN") = "N"
         set console off
         set print on
      else
         exit
      endif
   endif
   ? trim(field)
   skip
enddo
if .not. empty(ycmd2)
   ?? &ycmd2
endif
eject
set print off
set printer to
set console on
set device to screen
f_valid (.f., "Process completed...")

close data

*******************************************
procedure rrptc1
parameter xfrmno

do while .t.
   SET PRINT OFF
   SET CONSOLE ON
   ytemp = "N"
   @ 04, 05 say "Please select printer: [LPT1/LPT2/LPT3] "
   @ 04, 55 get yprt pict "!!!!" valid  ;
     f_valid (yprt $ "LPT1;LPT2;LPT3")
   @ 05, 05 say "Do you want to print an alignment test ? [Y/N] "
   @ 05, 55 get ytemp pict "!" valid f_valid (ytemp $ "YN")
   f_rd ()
   if ytemp = "N"
      return
   endif
   SET CONSOLE OFF
   SET PRINT ON
   set printer to &yprt
   if .not. empty (ycmd1)
      ?? &ycmd1
   endif
   do case
   case xfrmno = "TR11D"
      ? 
      ? 
      ? 
      ? 
      ? " XX   XXXXXXXXX    XXXX         XXXXXXX   XX   XXXXXXXXXX    XXXX        XXXXXXX"                                                    
      ? 
      ? 
      ? " XXXXXXXXXXXXXXXXX    XXXXX     XXXXXXX   XXXXXXXXXXXXXXXXX    XXXXXX    XXXXXXX"
      ? 
      ? "                               XXXXXXXX                                 XXXXXXXX"
      ? 
      ? 
      ? "                                XXXXXXX                                  XXXXXXX"
      ? 
      ? "           XXXXXXX     XXX      XXXXXXX             XXXXXXX     XXX      XXXXXXX"
      ? 
      ? 
      ? 
      ? 
      ? "                            XXXX  XX/XX"                                                                                             
      ? 
      eject
   case xfrmno = "765A"
      ?   
      ? 
      ? 
      ? "    XXXXXXXXX       XXXXXXXXXXXXXXX                 XXXXXXXXXXXXXXXXX       XXX     XXXXXXXXXXXXX"
      ? 
      ? 
      ? "    XXXXXXXXXXXXXXXXXXXX              XXXXXXXXXXXX                XX     XXXXX       XXXXX"
      ? 
      ? 
      ? 
      ? "    XXXXXXXXXXXXXXXXX               XXXXXXXXXXXX                                  XXX.XX"
      ? 
      ? 
      ? 
      ? "                                                                                        XX  XX/XX"
      eject
   case xfrmno = "T164"
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? "  XXXXXXX                     XXX                                             XXXXXXX"
      ? 
      ? 
      ? " XX    XXXX    XXXXXXXXXXXXXXXXX        XX     XX    XX             X       XX    XXXX    XXXXXXXXXXXXXXXXX       XX-XX-XX"
      eject
   case xfrmno = "2023"
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? "                                       XXXXXXXX"
      ? 
      ? 
      ? "                             XXXXXXXXXXXXXXXXXX                                          XXXXXXXXXX"
      ? "                                                                                            XX  XXXXX"
      ? 
      ? 
      ? 
      ? 
      ? "                                                                         XXXXX"
      ? 
      ? "                    XX         XXXXXXXXXXX                             XXXXXXXXXXXXXXXXX"
      ? 
      ? 
      ? 
      ? 
      ? 
      ? "                                        XXXXXXXXXXXXXXXXXX              XXXXXXXXXXXXXXXXXXXXXXXX"
      ? "                                        XXXXXXXX                     XX  XXXXX"
      ? 
      ? 
      ? 
      ? "            XXXX XX/XX XXX XXXXXXX"
      ? 
      ? "            TAX DEFERMENT CODE: XXXXXX"
      ? 
      eject
   case xfrmno = "51A"
      ? 
      ? 
      ? "                                                                    XX  XXX"
      ? 
      ? "                           X X   X X   X X"
      ? 
      ? "                           X X X X X X X X X X X X X X X X X"
      ? 
      ? "                           X X X X X X X"
      ? 
      ? "                           X X X X X X X                   X"
      ? 
      ? "                           X X X   X X X   X X X X"
      ? 
      ? "                                XX,XXX.XX"
      ? 
      ? 
      ? 
      ? "                                     X"
      ? 
      ? 
      ? 
      ? 
      ? 
      ? "                                     X"
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? "     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
      ? 
      ? 
      ? 
      eject
   case xfrmno = "37100A"
      ? 
      ? 
      ? 
      ? "                                               XXXXX XX    XX  XXXXXXXXXXXXXX    XXXXXXXXXXXXXXXXX XXX"
      ? 
      ? "                                               XXX         X        XXXXXX              XXXXXXX"
      ? "      XXXXXXXXXXXXXX"
      ? "                                                        XXX"
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? "                 XXXXXXXXXXXXXXXXXX"
      ? 
      ? "                 XXXXXXXXXXXXXXXXXX"
      ? "                                                        XXXXX.XX"
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? 
      ? "       XX  XX/XX"
      eject
   case xfrmno = "37100B"  
      ? "PROCESS DATE: XXXX-XX-XX                                 PROGRAM NAME: XXXXXXXX"
      ? "PROCESS TIME: XX:XX:XX           VEHICLE INVOICE                 PAGE:        1"
      ? 
      ? 
      ? 
      ? "VIN: XXXXXXXXXXXXXXXXX  TAG: XXXXXXX  ASSIGNEE NAME: XXXXXXXXXXXXXX"
      ? 
      ? "A-PLAN TOTAL:     XX,XXX.XX   LEASE FEE:    XXXX.XX   INVOICE DATE: XX/XX/XXXX"
      ? "REP CODE: XXXXXX"
      ? 
      ? "SOLD TO:                                SHIP TO:"
      ? " XXXXXXXXXXXXXXXXXXX                     XXXXXXXXXXXXXXXXXXX"
      ? " XXXXXXXXXXXXXXXXXXX                     XXXXXXXXXXXXXXXXXXXXXXXX"
      ? " XXXXXXXXXXXX                XX XXXXX    XXXXXXXX               XX"
      ? 
      ? "                                                    XXXXX XXX    XXXXXX"
      ? 
      ? 
      eject
   endcase 
enddo

********************************
function f_rrptc
parameter xdate
private yfile

yfile = "h:\dtp\ftp\"+jdate(xdate)+".ftp"
if file (yfile)
   return .t.
else
   return .f.
endif

*********************
procedure rrptc2

parameter xfrmno, xdate
private y1, y2, yfile

yfile = "h:\dtp\ftp\"+jdate(xdate)+".ftp"
select transit
append from &yfile sdf
do case
case xfrmno = "765A"      && 06
   do rrptc2a with [06]
case xfrmno = "T164"      && 07
   do rrptc2a with [07]
case xfrmno = "2023"      && 10
   do rrptc2a with [10]
case xfrmno = "51A"       && 11
   do rrptc2a with [11]
case xfrmno = "37100A"    && 05
   do rrptc2a with [05]
case xfrmno = "37100B"    && 16
   do rrptc2a with [16]
endcase 

**********************
procedure rrptc2a
parameter xrectype

f_popup ("Please Wait While Processing...")
select transit
locate for "RECORD "+xrectype $ field
if eof ()
   zap
   return
endif
y1 = recno()
y2 = reccount()
skip
do while .not. eof ()
   if "RECORD " $ field .or. "END OF FILE" $ field
      y2 = recno() 
      exit
   endif
   skip
enddo
select transit
delete for recno() <= y1 .or. recno() >= y2
pack
if reccount() < 5
   zap
elseif xrectype = [07]
   go top
   replace field with substr(field,10) for substr(field,1,1) <> chr(12)
endif

f_popback()
return


