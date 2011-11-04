*
* end of day report
*
* 09/11/96: add 10th digit (model year) to vin chart
* 11.10.98: process recv from other gm location
*           eod report for multiple branches
*           usage: xbranch = 'ALL'
* 11.23.98: output summary page
* 09.07.01: sos format changes fee code length from 2 to 3.

private yret, yans, yi
private loc [6]
loc [1] = [801]
loc [2] = [802]
loc [3] = [803]
loc [4] = [805]
loc [5] = [807]
loc [6] = [809]

f_clrscn ("End of Day Report")
f_box (01, 02, 05, 78)
@ 02, 04 say "Branch #......"
@ 03, 04 say "Date.........."
xbranch = gloc
xdate = date()
f_use ("radtrh")
f_use ("ravin")

do while .t.
   @ 02, 19 get xbranch pict "!!!"
   @ 03, 19 get xdate valid f_valid(.not.empty(xdate).and.f_y2k(@xdate))
   if f_rd () = 27
      close data
      return
   endif
   yret = f_confirm ("[C]ontinue  [E]dit  [Q]uit ","CEQ")
   if yret = "C"
      if xbranch = "ALL"
         yans = " "
         f_box (07, 02, 10, 78)
         @ 08, 04 say "Do you want to process data ? [Y/N/Q] " get yans pict "!" ;
           valid f_valid (yans $ "YNQ")
         if f_rd () = 27 .or. yans = "Q"
            close data
            return
         elseif yans = "N"
            exit
         else
            * process recv data
            do rdtp4a
            exit
         endif
      else
         select radtrh
         set softseek on
         seek xbranch+dtos(xdate)
         set softseek off
         if eof () .or. radtrh->fdate <> xdate
            f_valid (.f., "No transaction to be printed...") 
            loop
         else
            exit
         endif
      endif
   elseif yret = "Q"
      close data
      return
   endif
enddo
if xbranch = "ALL"
   * for debug purpose
   * INKEY(10)
   * set console off
   * set print on
   * set device to print
   * set printer to eod.rpt      && for debug purpose
   * gm branches are 801, 802, 803, 805, 807, 809
   xbranch = [801]
   do rdtp4b with xbranch, xdate
   for yi = 2 to 6
      xbranch = loc [yi]
      select radtrh
      seek xbranch+dtos(xdate)
      if .not. eof ()
         do rdtp4b with xbranch, xdate, ""    && do not pause for continous printing
      endif
   next yi
   * output summary page
   do rdtp4b with "ALL", xdate, ""
   * for debug purpose
   * set printer to
   * set console on
   * set device to screen
   f_valid (.f., "Process Completed... ")
else
   do rdtp4b with xbranch, xdate
endif

close data

******************************
* process recv data
* 05.04.99: sos y2k format changes
procedure rdtp4a
private xxfil, yfil, i, yaction, ytfee, yreg

* set excl on
* f_use ("ratran")
* set excl off
* zap

xxfil = gstnpath + "ratran.dbf"
* if .not. file (xxfil)
   ytmp = "tmp.dbf"
   create &ytmp
   append blank
   replace field_name with "FHDR"
   replace field_type with "C"
   replace field_len with 11
   replace field_dec with 0 
   append blank
   replace field_name with "F04CLERK"
   replace field_type with "C"
   replace field_len with 1
   replace field_dec with 0 
   append blank
   replace field_name with "F05AREA"
   replace field_type with "C"
   replace field_len with 1
   replace field_dec with 0 
   append blank
   replace field_name with "F06RCODE"
   replace field_type with "C"
   replace field_len with 1
   replace field_dec with 0 
   append blank
   replace field_name with "F07TCODE"
   replace field_type with "C"
   replace field_len with 1
   replace field_dec with 0 
   append blank
   replace field_name with "F08JDATE"
   replace field_type with "C"
   replace field_len with 4
   replace field_dec with 0 
   append blank
   replace field_name with "F09BRANCH"
   replace field_type with "C"
   replace field_len with 3
   replace field_dec with 0 
   append blank
   replace field_name with "F10SEQ"
   replace field_type with "C"
   replace field_len with 4
   replace field_dec with 0 
   append blank
   replace field_name with "FSV1"         
   replace field_type with "C"
   replace field_len with 16
   replace field_dec with 0 
   append blank
   replace field_name with "F16TITLE"
   replace field_type with "C"
   replace field_len with 8
   replace field_dec with 0 
   append blank
   replace field_name with "FSV2"       
   replace field_type with "C"
   replace field_len with 16
   replace field_dec with 0 
   append blank
   replace field_name with "F19REG"
   replace field_type with "C"
   replace field_len with 8
   replace field_dec with 0 
   append blank
   replace field_name with "F20ADDED"   && change from 8 to 5
   replace field_type with "C"
   replace field_len with 5
   replace field_dec with 0 
   append blank
   replace field_name with "F25TAB"
   replace field_type with "C"
   replace field_len with 8
   replace field_dec with 0 
   append blank
   replace field_name with "F21PLATE"
   replace field_type with "C"
   replace field_len with 7
   replace field_dec with 0 
   append blank
   replace field_name with "F22PPLATE"
   replace field_type with "C"
   replace field_len with 7
   replace field_dec with 0 
   append blank
   replace field_name with "F23EXP"
   replace field_type with "C"
   replace field_len with 8
   replace field_dec with 0 
   append blank
   replace field_name with "F24MONTH"
   replace field_type with "C"
   replace field_len with 2
   replace field_dec with 0 
   append blank
   replace field_name with "F26FEE"
   replace field_type with "C"
   replace field_len with 3             && 09.07.01
   replace field_dec with 0 
   append blank
   replace field_name with "FSV3"
   replace field_type with "C"
   replace field_len with 20
   replace field_dec with 0 
   append blank
   replace field_name with "F32VIN"
   replace field_type with "C"
   replace field_len with 17
   replace field_dec with 0 
   create &xxfil from &ytmp
   use
   erase &ytmp
* endif

select 0
use &xxfil alias ratran excl
zap

f_use ("raplate")
f_use ("ravm")
f_popup ("Please Wait While Processing ...")

* append recv file, excludes branch 801
for i = 2 to 6
   yfil = gcompath + loc [i] + jdate(xdate) + ".txt"
   if file (yfil)
      select ratran
      append from &yfil sdf
   endif
next 

select ratran
go top
do while .not. eof ()
   * update sos tran file
   select radtrh
   seek ratran->f09branch + dtos(xdate) + ratran->f10seq
   @ 09, 10 say ratran->f09branch + " --> " + ratran->f10seq + "  " + ;
                ratran->f32vin
   if eof ()
      append blank
   else
      reclock ()
   endif
      yaction = " "
      do case
      case ratran->f06rcode + ratran->f07tcode = [PA]
         yaction = [1]
      case ratran->f06rcode + ratran->f07tcode = [AA]
         yaction = [2]
      case ratran->f06rcode + ratran->f07tcode = [GA]
         yaction = [3]
      case ratran->f06rcode + ratran->f07tcode = [HA]
         yaction = [4]
      case ratran->f06rcode + ratran->f07tcode = [A ]
         yaction = [5]
      case ratran->f06rcode + ratran->f07tcode = [B ]
         yaction = [6]
      case ratran->f06rcode + ratran->f07tcode = [G ]
         yaction = [7]
      case ratran->f06rcode + ratran->f07tcode = [H ]
         yaction = [9]
      endcase
      replace faction with yaction, fbranch with ratran->f09branch
      replace fseq with ratran->f10seq, fstate with [MI]
      replace fvin with ratran->f32vin, ftab with ratran->f25tab
      replace fclerk with ratran->f04clerk, fdate with xdate
      replace floc with ratran->f09branch, fmonth with val(ratran->f24month)
      replace fplate with ratran->f21plate, fpplate with ratran->f22pplate
      replace freg with if(yaction $ [3;7], 0, val(ratran->f19reg)/100)
      replace ftfee with if(yaction $ [3;7], 8, 0)
      replace ftitle with val(ratran->f16title)/100
      replace ftotal with freg+ftfee+ftitle, ftr11 with .t.
      commit
      unlock
   * update plate file
   select raplate
   seek radtrh->fplate
   if eof ()
      append blank
   else 
      reclock ()
   endif
   replace fplate with radtrh->fplate
   replace fvin with substr(radtrh->fvin,10,8), fstatus with "U"
   if .not. empty(radtrh->ftab)
      replace ftab with radtrh->ftab
   endif
   if empty (ftype)
      replace ftype with ratran->f26fee
   endif
   if empty (fstate)
      replace fstate with [MI]
   endif
   if empty (fedate)
      replace fedate with xdate
   endif
   if radtrh->faction $ [2;4;5;6;9]
      * 10.27.98: year 2000
      ydate = ctod(substr(ratran->f23exp,1,2)+"/"+  ;
              substr(ratran->f23exp,3,2)+"/"+substr(ratran->f23exp,5))
      f_y2k (@ydate)
      replace frenewal with ydate
   endif
   commit
   unlock
   * update ravm
   select ravm
   seek substr(radtrh->fvin,10,8)
   if eof ()
      append blank
      replace fvin with radtrh->fvin
   else
      reclock ()
   endif
   if radtrh->faction $ [1;2;3;4]
      replace fdate1 with xdate
   endif
   if radtrh->faction $ [3;7;9]
      replace fpplate with ravm->fplate
   endif
   if radtrh->faction $ [2;4;6]
      replace fplate with radtrh->fplate
   endif
   commit
   unlock
   select ratran
   delete
   skip
enddo
select ratran
pack
use
select raplate
use
select ravm
use

f_popback ()

******************************
function f_hdr

private i, yln, ylen, ycol, ydesc

@ 01, 01 say 'DATE: ' + l_date
@ 01, yc1 say gtitle
@ 01, yc3 say 'PAGE: ' + str (l_page, 3)
@ 02, 01 say 'TIME: ' + l_time
@ 02, yc2 say l_ftitle1
@ 03, yc2 say l_ftitle2
@ 05, 01 say l_header1
@ 06, 01 say l_header2
yln = 8
l_page = l_page + 1

return (yln)

