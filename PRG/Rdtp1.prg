* ----------------------------------------------------------------------------
* add dtp transaction
*
* 09/11/96: add 10th digit (model year) to vin chart
* 09/26/96: seq # should be assigned by dtpp.exe from download file
*           so that seq # will be in syn with control #
* 10/14/96: correct calculation for commercial vehicle
* 10/23/96: locate the first available tab for the correct year automatically
* 10/30/96: renewal based on months prorated instead of constant 12
* 08.07.97: new user interface to add to vin chart
* 08.07.97: add plate to raaudit
* 09.17.97: change calc. for company pickup/van to base on msrp not weight
*           (for pickup/van weigh < 5000 lb)
* 10.14.97: multi-user control
* 05.10.98: check if vehicle has been plated
* 09.21.98: GM passenger expires 09/01
*           y2k
* 09.21.98: GM interface GMTRAN
* 10.27.98: year 2000 check on plate expiration date
* ----------------------------------------------------------------------------
* 08.14.00: handle multiple models with the same VIN config.
* 08.14.00: save model in funit
* ----------------------------------------------------------------------------
* 01.14.02: mem file (200?.mem) to validate SOS holidays
* 01.17.02: check plate configuration (see rdtp1b)
* 12.02.02: model year can be current year + 2 (used to be + 1)
* ----------------------------------------------------------------------------
* 10.01.03: title fee and reg. fee increase by $3 (see rdtp1c)
* ----------------------------------------------------------------------------
* 06.10.04: do not prorate for plate renewal (keep month = 12)
* 12.01.04: new 7 digit plate config AAA1234
* ----------------------------------------------------------------------------
* 02.20.06: add registration, title and transfer fee to GMTRAN
* 04.11.06: only allow to process 1 day ahead
* 04.11.06: <F2> to enter odometer reading
*           use fref to store odometer reading
*           funit is used for model info already
* 04.20.06: <F3> to pick lien holder 
*           use fref to store recno in lien table
* fref ::= [0010]+[ ]+[  2]
* 06.08.06: DO NOT use f_use to load lien holder table (RALIEN)
* 08.12.06: DO NOT use RALIEN since currently only 1 lien holder 
* 08.12.06: Determine vehicle ownership
*           if title date < 08/15/2006 ==> GM CORP & SUBSIDIARIES
*           if title date >=08/15/2006 ==> GENERAL MOTORS CORPORATION
* 12.01.06: 
*   1. stop issue new blue [003] and bridge [001] plate type
*   2. existing bridge [001] can be renewed and transfered
*   3. existing blue [003] cannot be renewed or transfered
*   4. new process to transfer/renew blue plate
*   5. issue new white [009] + new tag for blue [003] transactions
*   6. issue new distinctive [010] instead of [001]
* ----------------------------------------------------------------------------
* 02.05.07: add new tran code [B] for GMTRAN
* ----------------------------------------------------------------------------
* 07.05.07: take out edit function (per Theresa)
* ----------------------------------------------------------------------------
* 10.01.07: commercial plate fees are never prorated per SOS
* 12.07.07: bug/ previous plate shoud equal current plate during normal
*           transfer [3;7] 
* ----------------------------------------------------------------------------
* 07.08.09: set default Lien = 0 ==>[NONE]
* 07.08.09: new title name = gcompany::=[GENERAL MOTORS COMPANY]
* 10.01.09: name change to [GENERAL MOTORS LLC]
* 10.22.09: set default Lien = 1 ==>[YES] 
* --------------------------------
* 02.09.11: <f3> returns immediately if default lien holder = NONE
* ----------------------------------------------------------------------------
private xrecno, xmsrp, xadded, xfee, xyr, yfil

restore from (gmempath + "radtr") additive
f_clrscn ("DTP/MI - Add D.T.P. Transaction")
f_box (01, 02, 04, 78)
@ 02, 04 say "Office #......"
@ 02, 41 say "Clerk ID......"
@ 03, 04 say "Date.........."
* @ 04, 04 say "Tab #........."
l_floc = gloc
l_fdate = date()
l_fclerk = gclerk
l_ftr11 = .f.           && print control for GM
*
xmodyr = 3              && 12.02.02: current + 2

* --04.11.06
gmileage = 10                 && default odometer reading set as 10
xmileage = gmileage
* -- 

* --07.08.09
* xlien = 1                    && 08.12.06: 
* xlien = 0                    && 07.08.09: default is lien = 0 ::= [NONE]
* xlien = 1                    && 10.01.09:
xlien = 0                      && 02.09.11: default is lien = NONE
* --

* check last post date
f_use ("rasys")
xdate = fpostdate
use

* 200?.mem must be present
yfil = gmempath + substr(dtos(date()),1,4) + ".mem"
if  file (yfil)
   restore from (yfil) additive
else
   * f_valid (.f., "Missing SOS Holiday Table...")
   * return
   xholiday = ""       && 10.01.03
endif
xplate = space(7)       && current plate
xyear = space(4)        && current exp. year e.g. 1997
xrecno = 0
xmsrp = 0
xadded = 0
xfee = [  ]
xyr = 0
xpdesc = space(3)       && 12.01.06: plate desc code; [001;003;009;010]
xrenewal = ctod("")     && 12.11.06: save previous blue plate renewal
set key 28 to rdtph1
set key -1 to rdtpf2    && 04.11.06
set key -2 to rdtpf3    && 02.09.11: lien set to NONE

do while .t.
   @ 02, 19 get l_floc pict "!!!" valid ;
     f_valid (f_verify("raloc",1,l_floc))
   @ 02, 56 say l_fclerk
   @ 03, 19 get l_fdate valid rdtp1x ()       && 04.11.06
   if f_rd () = 27
      return
   endif
   yret = f_confirm ("[C]ontinue  [E]dit  [Q]uit ","CEQ")
   if yret = "C"
      exit
   elseif yret = "Q"
      return
   endif
enddo

* assign reg. / title fee
f_use ("raloc")
seek l_floc
if eof ()
   f_valid (.f., "Please Setup Branch Office First...")
   close data
   return
else
   xtitle = ftitle
   xreg = freg
   l_fbranch = fbranch
endif
use

* create tmp file for gm transfer
xxfil = gstnpath + "gmtran.dbf"
if .not. file (xxfil)
   ytmp = "tmp.dbf"
   create &ytmp
   append blank
   replace field_name with "FBRANCH"
   replace field_type with "C"
   replace field_len with 3
   replace field_dec with 0 
   append blank
   replace field_name with "FDATE"
   replace field_type with "D"
   replace field_len with 8
   replace field_dec with 0 
   append blank
   replace field_name with "FPLATE"
   replace field_type with "C"
   replace field_len with 7
   replace field_dec with 0 
   append blank
   replace field_name with "FSEQ"
   replace field_type with "C"
   replace field_len with 4
   replace field_dec with 0 
   append blank
   replace field_name with "FTAB"
   replace field_type with "C"
   replace field_len with 7
   replace field_dec with 0 
   append blank
   replace field_name with "FVIN"
   replace field_type with "C"
   replace field_len with 17
   replace field_dec with 0 
   append blank
   replace field_name with "FACTION"
   replace field_type with "C"
   replace field_len with 1
   replace field_dec with 0 
   append blank
   replace field_name with "FTYPE"      && P=Passenger, C=Commercial
   replace field_type with "C"
   replace field_len with 1
   replace field_dec with 0 
   append blank
   replace field_name with "FREG"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 2 
   append blank
   replace field_name with "FTITLE"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 2 
   append blank
   replace field_name with "FTFEE"
   replace field_type with "N"
   replace field_len with 6
   replace field_dec with 2 
   *
   create &xxfil from &ytmp
   use
   erase &ytmp
endif
 
f_use ("raaudit")      && audit file
f_use ("ravm")
f_use ("ravin")
f_use ("raplate")
f_use ("ravalue")
f_use ("radtrh")
f_use ("radtrp")
f_use ("ractr")
f_use ("ratab")
f_use ("rawtfee")
f_use ("radtr")

* 06.08.06: load lien holder table info.
* yfil = gdbfpath + "ralien"
* select 0
* use &yfil alias ralien

select 0
use &xxfil excl alias gmtran
zap

* multi-user control by fclerk...
select radtr
set filter to .not. empty(fvin)
* set filter to .not. empty(fvin) .and. empty(fclerk)
go top
f_box (01, 02, 22, 78)
* 04.11.06: add Mlg heading
@ 02, 04 say "VIN #             Typ/Mlg Plate   Tab #   Title  Reg Fee Transfer  Total"
@ 03, 04 say "컴컴컴컴컴컴컴컴� 컴컴컴� 컴컴컴� 컴컴컴� 컴컴컴 컴컴컴� 컴컴컴컴  컴컴컴"           
setcolor (gredcolor)

* --02.09.11
if empty (glhname)
   @ 24, 01 say "<F2> Odometer Reading"
else
   @ 24, 01 say "<F2> Odometer Reading | <F3> Lien Holder"
endif
* --

setcolor (gbluecolor)
*
yrow = 4
do while .t.
   @ yrow, 04 get l_fvin pict "!!!!!!!!!!!!!!!!!" valid rdtp1a ()
   if f_rd () = 27
      exit 
   endif

   l_ftotal = l_ftitle + l_freg + l_ftfee
   @ yrow, 23 say l_faction pict "!" 
   @ yrow, 30 say l_fplate pict "!!!!!!!"      && 12.01.04
   @ yrow, 38 say l_ftab pict "9999999"
   @ yrow, 46 say l_ftitle pict "999.99"
   @ yrow, 54 say l_freg pict "999.99"
   @ yrow, 62 say l_ftfee pict "999.99" 
   @ yrow, 71 say l_ftotal pict "999.99" 

   yedit = .f.
   xchk = .t.
   do while .t.
      @ yrow, 23 get l_faction pict "!" valid rdtp1e ()
      @ yrow, 30 get l_fplate pict "!!!!!!!" valid rdtp1b ()     && 12.01.04
      @ yrow, 38 get l_ftab pict "9999999" valid rdtp1d ()
      * 07.05.07: cannot edit calculated fees
      * if yedit
      *   @ yrow, 46 get l_ftitle pict "999.99" valid l_ftitle >= 0
      *   @ yrow, 54 get l_freg pict "999.99"   valid l_freg >= 0
      *   @ yrow, 62 get l_ftfee pict "999.99"  valid l_ftfee >= 0
      * endif
      f_rd () 
      l_ftotal = l_ftitle + l_freg + l_ftfee
      @ yrow, 23 say l_faction pict "!" 
      if l_faction $ [1;2;3;4]
        @ yrow, 25 say xmileage pict "9999"           && 04.10.06
      else
        @ yrow, 25 say [    ] 
      endif
      @ yrow, 30 say l_fplate pict "!!!!!!!"        && 12.01.04
      @ yrow, 38 say l_ftab pict "9999999" 
      @ yrow, 46 say l_ftitle pict "999.99"
      @ yrow, 54 say l_freg pict "999.99"
      @ yrow, 62 say l_ftfee pict "999.99" 
      @ yrow, 71 say l_ftotal pict "999.99" 
      if xchk .and. l_ftotal > 0
         xkey = f_confirm ("[C]onfirm  [E]dit  [D]elete  [I]gnore Changes", "CEDI")
      else
         xkey = f_confirm ("[E]dit  [D]elete  [I]gnore Changes", "CEDI")
      endif
      do case
      case xkey = "C"
         * misc check
         if l_ftotal <> l_ftitle + l_freg + l_ftfee 
            f_valid (.f., "Out of Balance....")
            loop
         * 07.05.07: total must be whole number
         elseif l_ftotal - int(l_ftotal) <> 0
            f_valid (.f., "Invalid Total amount...")
            loop
         elseif l_faction $ [1;2;3;4] .and. l_ftitle <=0
            f_valid (.f., "Missing Title Fee...")
            loop
         elseif l_faction $ [2;4;5;6;9] .and. l_freg <=0 
            f_valid (.f., "Missing Registration Fee...")
            loop
         elseif l_faction $ [2;4;5;6;9] .and. empty(l_fplate)
            f_valid (.f., "Missing Plate...")
            loop
         elseif l_faction $ [2;4;5;6;9] .and. empty(l_ftab)
            f_valid (.f., "Missing Tab...")
            loop
         elseif l_faction $ [3;7] .and. l_ftfee <=0
            f_valid (.f., "Missing Transfer Fee...")
            loop
         else
            * 12.11.06: check for duplicates in raaudit (just in case)
            select raaudit
            if .not.empty(l_fplate)
               seek l_fplate
               if .not. eof ()
                  f_valid (.f., "Duplicate transaction found for Plate "+l_fplate)
                  loop
               endif
            elseif .not.empty(l_fpplate)
               seek l_fpplate
               if .not. eof ()
                  f_valid (.f., "Duplicate transaction found for Plate "+l_fpplate)
                  loop
               endif
            endif
            *
         endif
         * 04.20.06: use fref to store odometer reading (funit is used for model info already)
         if l_faction $ [1;2;3;4]      && For title transaction only
            l_fref = str(xmileage,4,0)+[ ]+str(xlien,3,0)   
            l_fowner = gcompany    && 10.01.09
         else
            l_fref = ""
            * 10.01.09: determine proper ownership for vehicle registration
            select ravm
            seek substr(l_fvin,10,8)
            if eof() .or. empty(ravm->fdate1)
               y1 = " "
               yscn = f_box (10, 5, 13, 60)
               @ 11, 7 say "Company...      [1] GENERAL MOTORS LLC"
               @ 12, 7 say "                [2] GENERAL MOTORS COMPANY"
               do while .t.
                  @ 11, 18 get y1 pict "9" valid f_valid(y1 $ "1;2")
                  f_rd ()
                  if f_confirm ("Are you sure ?  [Y/N]","YN") = "Y"
                     if empty(y1) 
                        loop
                     else
                        exit
                     endif
                  endif
               enddo
               f_restbox (yscn)
               if y1 = [1]
                  l_fowner = gcompany
               elseif y1 = [2]
                  l_fowner = goldname
               endif
            else
               if ravm->fdate1 >= ctod('10/01/2009')   
                  l_fowner = gcompany
               else
                  l_fowner = goldname
               endif
            endif
            * --
         endif
         * --

         if empty (l_fseq)
            * get seq. #
            select ractr
            locate for fbranch = l_fbranch
            if eof ()
               f_valid (.f., "Error: System counter out of sequence...")
               close data
               return
            else
               reclock ()
               l_fseq = strtran(str(fseq1,4), " ", "0")     
               replace fseq1 with fseq1 + 1
               commit
               unlock
            endif 
         endif

         select raaudit
         append blank
         replace fvin with substr(l_fvin,10,8)
         commit
         unlock 
         * 08.07.97 audit plate
         if .not. empty(l_fplate)
            select raaudit
            append blank
            replace fvin with l_fplate
            commit
            unlock 
            * 12.11.06: stop from issuing the plate again + update renewal date for transfer
            select raplate
            seek l_fplate
            if .not. eof ()
                reclock ()
                replace fstatus with "U"
                if empty(frenewal)
                   replace frenewal with xrenewal
                endif
                commit
                unlock
            endif
         endif
         * 12.11.06: deactive blue plate
         if .not.empty(l_fpplate) .and. l_fpplate<>l_fplate
            select raplate
            seek l_fpplate
            if .not. eof ()
                reclock ()
                replace fstatus with "I"    
                commit
                unlock
            endif
         endif

         * update gmtran
         select gmtran
         append blank
         replace fbranch with l_fbranch, fdate with l_fdate
         replace fplate with l_fplate, fseq with l_fseq
         replace ftab with l_ftab, fvin with l_fvin
         * 02.05.07: new tran code [B] for blue plate
         if .not.empty(l_fpplate) .and. l_fplate <> l_fpplate
            replace faction with [B]
         else
            replace faction with l_faction
         endif
         ***
         if xfee = [01]
            replace ftype with "P"
         elseif xfee = [03]
            replace ftype with "C"
         endif
         * 02.20.06: additional fields
         replace freg with l_freg, ftitle with l_ftitle, ftfee with l_ftfee

         * update radtrh   
         select radtrh
         append blank
         f_replace ()
         if xrecno > 0
            select radtr
            go xrecno
            select radtrp
            append blank
            f_replace ()
            select radtr 
            f_clrrec ()
         else 
            select radtrp
            append blank
            f_replace ()
            select radtr
            * just in case
            seek l_fbranch+dtos(l_fdate)+l_fclerk+substr(l_fvin,10,8)+l_faction
            if .not. eof ()
               f_clrrec ()
            endif
         endif
         xrecno = 0 
         xplate = space(7)
         exit
      case xkey = "D"
         select raaudit
         seek substr(l_fvin,10,8)
         if .not. eof ()
             reclock ()
             delete
             commit
             unlock
         endif
         * 08.07.97 audit plate
         if .not. empty(l_fplate)
            select raaudit
            seek l_fplate
            if .not. eof ()
                reclock ()
                delete
                commit
                unlock
            endif
         endif
         * 
         if .not. empty(l_ftab)
            select ratab
            seek l_ftab
            if .not. eof ()
               reclock ()
               replace fstatus with "A"
               commit
               unlock 
            endif
         endif  
         if .not. empty(l_fplate) .and. l_faction $ [2;5]
            select raplate
            seek l_fplate
            if .not. eof ()
                reclock ()
                replace fstatus with "A"
                commit
                unlock
            endif
         endif
         select radtr
         if xrecno > 0
            go xrecno 
            f_clrrec ()
         else 
            seek l_fbranch+dtos(l_fdate)+l_fclerk+substr(l_fvin,10,8)+l_faction
            if .not. eof ()
               f_clrrec ()
            endif
         endif
         xrecno = 0 
         xplate = space(7)
         exit
      case xkey = "E"
         if .not. empty(l_ftab)
            select ratab
            seek l_ftab
            if .not. eof ()
               reclock ()
               replace fstatus with "A"
               commit
               unlock 
            endif
         endif  
         yedit = .t.
         loop
      case xkey = "I"
         if xrecno > 0
            select radtr
            go xrecno 
            reclock ()
            replace fclerk with " "
            commit
            unlock 
         endif
         if .not. empty(l_ftab)
            select ratab
            seek l_ftab
            if .not. eof ()
               reclock ()
               replace fstatus with "A"
               commit
               unlock 
            endif
         endif  

         if .not. empty(l_fplate) .and. l_faction $ [2;5]
            select raplate
            seek l_fplate
            if .not. eof ()
                reclock ()
                replace fstatus with "A"
                commit
                unlock
            endif
         endif
         xrecno = 0
         xplate = space(7)
         exit
      endcase
   enddo
   if xkey <> "I"
      if yrow < 20
         yrow = yrow + 1
      else
         scroll (4, 04, 20, 77, 1)
      endif
      @ yrow, 04 clear to yrow, 77
   else
      @ yrow, 04 clear to yrow+1, 77
   endif
   xmileage = gmileage        && 04.11.06

   * --07.08.09
   * xlien = 1                && 08.12.06
   * xlien = 0                  
   * xlien = 1                && 10.01.09
   xlien = 0                  && 02.09.11 
   * --

   l_fvin = space(17)
   l_fcontrol = space(4)
   l_fref = space(15)
   l_fowner = space(25)
   l_funit = space(10)
   l_faction = space(1)
   l_fplate = space(7)
   l_fpplate = space(7)
   l_freg = 0
   l_ftab = space(7)
   l_ftitle = 0 
   l_ftfee = 0
   l_ftotal = 0
   l_fseq = space(4)       && 09/26/96
   xpdesc = space(3)       && 12.01.06: plate desc code; [001;003;009;010]
   xrenewal = ctod("")     && 12.11.06: save previous plate renewal
enddo

* update gm trans gmtran.txt
select gmtran
go top
if .not. eof ()
   * check if gmtran.txt is still out there, if so append
   * per Tom Cyr: gmtran remains local, make sure MD C:\DTP
   yfil = "c:\dtp\gmtran.txt"      
   if file (yfil)
      append from &yfil sdf
   endif
   * 
   select gmtran
   copy to &yfil sdf
endif

set key 28 to
set key -1 to
set key -2 to
set filter to
close database

***************************************************************
* 04.11.06: only allow to process 1 day ahead
function rdtp1x

f_y2k(@l_fdate)
do case
case l_fdate<=xdate
   f_valid (.f.,"Must be later than "+dtoc(xdate)) 
   return .f.
case dow(l_fdate)=1.or.dow(l_fdate)=7
   f_valid (.f.,"Must be Monday thru Friday only...")
   return .f.
case dtos(l_fdate) $ xholiday
   f_valid (.f.,"Date entered is a state holiday...")
   return .f.
case date()>xdate .and. l_fdate>date()
   f_valid (.f.,"Please run End of Day first...")
   return .f.
case l_fdate<>date()
   if dow(date())=6
      if date()+3<>l_fdate
         f_valid (.f.,"Valid date is "+dtoc(date()+3))
         return .f.
      endif
   else
      if date()+1<>l_fdate
         f_valid (.f.,"Valid date is "+dtoc(date()+1))
         return .f.
      endif
   endif
endcase

return .t.

***************************************************************
function rdtp1a

if empty (l_fvin)
   f_valid (.f., "Press <F1> to display ready list...")
   return .f.
endif

if len(alltrim(l_fvin)) = 8    
   select ravm
   seek l_fvin
   if eof ()
      f_valid (.f., "Please enter complete VIN...")
      return .f.
   else
      l_fvin = ravm->fvin
   endif
else
   if f_goodvin (l_fvin)
      select ravm
      seek substr(l_fvin, 10, 8)
      if eof ()
         append blank
         replace fvin with l_fvin
         commit
         unlock
      endif
   else
      return .f.
   endif
endif

* check for duplicate transaction
select raaudit
seek substr(l_fvin,10,8)
if .not. eof ()
   f_valid (.f., "Duplicate Transaction, Please check today's work...")
   select radtr
   if xrecno > 0
      go xrecno 
      f_clrrec ()
   else 
      seek l_fbranch+dtos(l_fdate)+l_fclerk+substr(l_fvin,10,8)+l_faction
      if .not. eof ()
         f_clrrec ()
      endif
   endif
   xrecno = 0 
   return .f.
endif
*
select ravin
seek substr(l_fvin,1,8)+substr(l_fvin,10,1)
if eof () .or. empty(ravin->ffee) .or. ;
   ravin->fmsrp <= 1000 .or. empty(ravin->fyear) .or. empty(ravin->fmake) .or. ;
   (ravin->ffee = [03] .and. ravin->fmsrp <= 5000)
   *
   set cursor on
   private yscn, ycolor, yptr
   private yyvin, yydesc, yyfee, yymake, yymsrp, yystyle, yyyear
   
   ycolor = setcolor (gsubcolor)
   yscn = f_box (09, 25, 16, 60)
   @ 10, 27 say "VIN ......... "
   @ 11, 27 say "Year ........ "
   @ 12, 27 say "Make ........ "
   @ 13, 27 say "Body Style .. "
   @ 14, 27 say "MSRP ........ "
   @ 15, 27 say "Remark ...... "
   
   yyvin = substr(l_fvin,1,8)+substr(l_fvin,10,1)
   
   if eof ()
      yyyear = space(2)
      yymake = space(12)
      yystyle = space(2)
      yymsrp = 0
      yyfee = space(2)
      yydesc = space(15)
   else
      yyyear = ravin->fyear
      yymake = ravin->fmake
      yystyle = ravin->fstyle
      yymsrp = ravin->fmsrp
      yyfee = ravin->ffee
      yydesc = ravin->fdesc
   endif
   
   @ 10, 41 say yyvin
   setcolor (gsubget)
   @ 11, 41 say yyyear pict "99"
   @ 12, 41 say yymake pict "XXXXXXXXXXXX"
   @ 13, 41 say yystyle pict "!!"
   @ 14, 41 say yymsrp pict "999999"
   @ 15, 41 say yydesc pict "XXXXXXXXXXXXXXX"
   
   yptr = 1
   do while .t.
      do case
      case yptr = 1
         f_getfld (@yyyear, 11, 41, "W/N", 0, "99", .t.)
      case yptr = 2
         f_getfld (@yymake, 12, 41, "W/N", 0, replicate ("!", 12), .t.)
      case yptr = 3
         f_getfld (@yystyle, 13, 41, "W/N", 0, "!!", .t.)
      case yptr = 4
         f_getnum (@yymsrp, 14, 41, "", "999999", .t.)
      case yptr = 5
         if .not. (upper(trim(ravin->fdesc)) = "MSRP")
            f_getfld (@yydesc, 15, 41, "W/N", 0, replicate ("!", 15), .t.)
         endif
      endcase
      ykey = lastkey ()
      if (ykey = 24 .or. ykey = 13) .and. yptr < 12
         yptr = yptr + 1
      elseif ykey = 5 .and. yptr > 1
         yptr = yptr - 1
      elseif ykey = 27 .or. ykey = 13 .or. ykey = 3 .or. ykey = 18
         exit
      endif
   enddo
   f_restbox (yscn)
   setcolor (ycolor)
   
   * define fee code
   if f_verify ("rabody", 1, yystyle)
      yyfee = rabody->ftype
   else
      f_valid (.f., "Invalid Body Style ...")   && 09.30.98
      return .f.
   endif
   *
   select ravin
   seek yyvin
   if eof ()
      append blank
   endif
   reclock ()
   replace fvin with yyvin, fyear with yyyear, fmake with yymake
   replace fstyle with yystyle, fmsrp with yymsrp
   replace ffee with yyfee, fdesc with yydesc
   commit
   unlock
endif

select ravin
seek substr(l_fvin,1,8)+substr(l_fvin,10,1)
if eof ()
   f_valid (.f., "Missing Definition in VIN chart...")
   return .f.
else
   * check for multiple models
   select ravin
   skip
   if ravin->fvin = substr(l_fvin,1,8)+substr(l_fvin,10,1)
      skip -1 
      if f_pick_f (05,05,"","MODEL컴YEAR컴MAKE컴컴컴컴컴컴MSRP컴DESC컴�","substr(fmodel,1,3)+[    ]+fyear+[    ]+fmake+[ ]+" + ;
         "str(fmsrp,8)+[ ]+fdesc", "",   ;
         "fvin+substr(fmodel,1,3)", "substr(l_fvin,1,8)+substr(l_fvin,10,1)")
      else
         f_valid (.f., "Please Select the Model...")
         return .f.
      endif
   else
      skip -1     && single model only
   endif
endif
* get msrp, fee code, body style, year, make etc
if empty (ravin->fstyle)
   f_valid (.f., "Missing Fee Code in VIN chart...")
   return .f.
elseif ravin->fmsrp <= 5000       && 09.17.97
   f_valid (.f., "Please check MSRP in VIN chart...")
   return .f.
elseif empty (ravin->fyear) 
   f_valid (.f., "Missing Vehicle Year in VIN chart...")
   return .f.
elseif empty (ravin->fmake)
   f_valid (.f., "Missing Vehicle Make in VIN chart...")
   return .f.
endif

* model year cannot be ahead 2 year, calculation for year past 2000
xyr = val(ravin->fyear)
xyr = if(xyr>=50, xyr+1900, xyr+2000)
y2 = xyr - val(substr(dtos(l_fdate),1,4))
if y2 >= xmodyr      && 12.02.02: change to ahead 3 year (current + 2) 
   f_valid (.f., "Invalid Model Year "+ravin->fyear + "...")
   return (.f.)
endif

l_funit = substr(ravin->fmodel,1,3)      && 08.14.00 save model in funit
xfee = ravin->ffee

* if ravin->ffee = "01"      && passager
* 09.17.97
if ravin->fmsrp > 8000     && 01.09.04:cut off for wt fee calc.
   if ravin->fmsrp > gmaxfee  && fee catagory
      xmsrp = ravin->fmsrp 
   else
      xmsrp = int(ravin->fmsrp/1000) * 1000
   endif
else
   xmsrp = ravin->fmsrp    && commercial    && weight
endif

select radtr 
set order to 2
seek substr(l_fvin,10,8)
set order to 1
if .not. eof ()
   if .not. empty (radtr->fclerk) .and. radtr->fclerk <> l_fclerk     && 10.14.97
      f_valid (.f., "Record in use by Clerk Id: "+radtr->fclerk)
      return .f.
   endif
   reclock ()
   replace fclerk with l_fclerk
   commit 
   unlock
   xrecno = recno ()
   l_fcontrol = fcontrol     && control #
   l_fref = fref             && ref # (garage code)       
   * l_fowner = fowner         && company
   l_faction = faction
   l_fplate = fplate
   l_ftab = ftab
   l_freg = freg
   l_ftitle = ftitle
   l_ftfee = ftfee
   l_fseq = fseq             && 09/26/96
endif

@ yrow, 04 say l_fvin pict "!!!!!!!!!!!!!!!!!" 

return .t.

*************************************************
function rdtp1b
private y1, y2, ymo, ymess
private yplate, ypdesc, yscn, ycolor     && 12.01.06

if lastkey () = 5
   return .t.
endif

* 08.07.97 check daily audit file to make sure plate is not entered again
ypdesc = space(3)          && 12.11.06
if .not. empty (l_fplate)
   select raaudit
   seek l_fplate
   if .not. eof ()
      f_valid (.f., "Plate "+l_fplate+" is in use...")
      xchk = .f.
      return (.f.)
   endif
   * 12.11.06
   select raplate
   seek l_fplate
   if .not. eof ()
      if raplate->fstatus = "I"
         f_valid (.f., "Plate "+l_fplate+" is inactive...")
         xchk = .f.
         return (.f.)
      endif
   else
      f_valid (.f., "Plate "+l_fplate+" not on file...")
      xchk = .f.
      return (.f.)
   endif
   * 01.17.02 check configuration of plate entered
   if .not. chkplate (l_fplate, @ypdesc)        && 12.11.06: get plate desc code
      f_valid (.f., "Invalid Plate Configuration...")
      xchk = .f.
      return (.f.)
   endif
   *  if(empty(xpdesc),ypdesc,xpdesc)    && DO NOT USE 
   if empty(xpdesc)
      xpdesc = ypdesc
   endif
endif

* 12.01.06: stop issue bridge [001] and blue [003] for original plate
do case
case l_faction $ [2;5]
   if ypdesc = [001]
      f_valid (.f., "Cannot issue New Bridge Plate...")
      xchk = .f.
      return (.f.)
   elseif ypdesc = [003]
      f_valid (.f., "Cannot issue New Blue Plate...")
      xchk = .f.
      return (.f.)
   endif
* 12.01.06: handle transfer/renew blue plate here...
case l_faction $ [3;4;6;7;9] .and. xpdesc = [003]
   * l_fpplate = if (empty(l_fpplate),l_fplate,l_fpplate)    && DO NOT USE
   if empty(l_fpplate)
      l_fpplate = l_fplate
   endif
   * issue new white plate to replace old blue plate
   ycolor = setcolor (gsubcolor)
   yscn = f_box (yrow-1, 38, yrow + 2, 60)
   @ yrow, 39 say "Blue Plate : "+l_fpplate
   @ yrow + 1, 39 say "White Plate: "
   yplate = l_fplate
   f_getfld (@yplate, yrow+1, 52, "W/N", 0, "!!!!!!!", .t.)
   f_restbox (yscn)
   setcolor (ycolor)
   l_fplate = yplate
   @ yrow, 30 say l_fplate pict "!!!!!!!"
endcase
*
xadded = 0

do case
case l_faction = [1] 
   if .not. empty (l_fplate)
      f_valid (.f., "Plate # must be blank for this transaction...")
      xchk = .f.
      return (.f.)
   endif
case l_faction $ [2;5]        && original plate
   if empty (l_fplate)
      f_valid (.f., "Plate # CANNOT be blank for this transaction...")
      xchk = .f.
      return (.f.)
   endif
   select raplate
   seek l_fplate
   if eof ()
      f_valid (.f.,"Invalid Plate #...")
      xchk = .f.
      return (.f.)
   elseif raplate->fstatus <> "A" .and. xplate <> l_fplate
      f_valid (.f.,"Plate # is not available...")
      xchk = .f.
      return (.f.) 
   elseif raplate->fstatus = "I" 
      f_valid (.f.,"Plate # is inactive...")
      xchk = .f.
      return (.f.)
   elseif raplate->ftype <> xfee
      ymess = if(xfee="01"," Passenger "," Commercial ")
      f_valid (.f., "Please enter a"+ymess+"Plate...")
      xchk = .f.
      return (.f.)
   endif
   xplate = l_fplate
   select raplate
   reclock ()
   replace fstatus with "U"
   commit
   unlock
   xadded = raplate->fadded
case l_faction $ [3;4;6;7;9]
   if empty (l_fplate)
      f_valid (.f., "Plate # CANNOT be blank for this transaction...")
      xchk = .f.
      return (.f.)
   endif
   select raplate
   seek l_fplate
   if eof ()
      f_valid (.f., "Plate # Not On File...")
      * 12.01.06: must enter plate in raplate table...
      * if f_confirm ("[R]etype  [C]ontinue ","CR") <> "C"
      *   xchk = .f.
      *   return (.f.)
      *endif
      xchk = .f.
      return (.f.)
   elseif raplate->fstatus <> "A" .and. xpdesc = [003]      && 12.11.06: blue plate exchange
      f_valid (.f.,"Plate "+l_fplate+" is in use")
      xchk = .f.
      return (.f.)
   elseif raplate->fstatus = "I"
      f_valid (.f.,"Plate "+l_fplate+" is inactive...")
      xchk = .f.
      return (.f.)
   elseif .not. empty(raplate->ftype) .and. raplate->ftype <> xfee 
      ymess = if(xfee="01"," Passenger "," Commercial ")
      f_valid (.f., "Please enter a"+ymess+"Plate...")
      xchk = .f.
      return (.f.)
   else   && 12.01.06
      ypdesc = space(3)
      if .not. chkplate (l_fplate, @ypdesc)
         f_valid (.f., "Invalid Plate Configuration...")
         xchk = .f.
         return (.f.)
      elseif ypdesc = [003]
         f_valid (.f., "Cannot enter a blue plate...")
         xchk = .f.
         return (.f.)
      endif
   endif
   if l_faction $ [3;7]      && 12.01.06
      if xpdesc = [003]      && check previous plate
         yplate = l_fpplate
      else
         yplate = l_fplate
      endif
      select raplate
      seek yplate
      if eof ()
         f_valid (.f., "Plate "+yplate+" not on file...")
         xchk = .f.
         return (.f.)
      elseif raplate->frenewal <= l_fdate
         f_valid (.f., "Plate "+yplate+"expires "+dtoc(raplate->frenewal))
         xchk = .f.
         return (.f.)
      elseif raplate->fstatus = "I" 
         f_valid (.f.,"Plate "+yplate+" is inactive...")
         xchk = .f.
         return (.f.)
      endif
      xyear = substr(dtos(frenewal),1,4)
      xrenewal = raplate->frenewal          && 12.11.06: get previous blue plate renewal
   endif
endcase

* calculate month poration : GM expires on 09/01

declare xmo1 [12], xmo3 [12]
* for passager fee code = [01] always expires 09/01
xmo1 [1] = 9
xmo1 [2] = 8
xmo1 [3] = 7
xmo1 [4] = 6
xmo1 [5] = 17
xmo1 [6] = 16
xmo1 [7] = 15
xmo1 [8] = 14
xmo1 [9] = 12
xmo1 [10] = 12 
xmo1 [11] = 11
xmo1 [12] = 10

* for commercial fee code = [03]  always expires 02/28
xmo3 [1] = 14
xmo3 [2] = 12
xmo3 [3] = 12
xmo3 [4] = 11
xmo3 [5] = 10
xmo3 [6] = 9
xmo3 [7] = 8
xmo3 [8] = 7
xmo3 [9] = 6
xmo3 [10] = 17
xmo3 [11] = 16
xmo3 [12] = 15

if xfee = [01]
   y1 = val(substr(dtoc(l_fdate),1,2))
   ymo = xmo1 [y1]
   l_fmonth = xmo1 [y1]
   *
   if .not. l_faction $ [2;5]    && 06.16.04: as of 10/2003 no more poration for renewal
      l_fmonth = 12
   endif
else
   y1 = val(substr(dtoc(l_fdate),1,2))
   ymo = xmo3 [y1]

   *--------------------------------------------------------------
   * 10.01.07: commercial plate fees are never prorated per SOS
   * l_fmonth = xmo3 [y1]
   * 06.16.04: as of 10/2003 no more poration for renewal
   * if .not. l_faction $ [2;5]    && 
   *    l_fmonth = 12
   * endif

   * DTP allows 12 month commercial plate ONLY
   * except 12 mo plate at half fee for September ONLY
   if y1 = 9 .and. l_faction $ [2;5]
      l_fmonth = 6
   else
      l_fmonth = 12
   endif
   *----------------------------------------------------------------

endif

* check if plate need to buy new tab
if l_faction $ [2;4;5;6;9]
   xyear = substr(dtos(l_fdate + ymo * 30),1,4)
   if xpdesc = [003]        && 12.11.06
      yplate = l_fpplate
   else
      yplate = l_fplate
   endif
   select raplate
   seek yplate
   if .not. eof ()

      *------------10.01.07-----------------------
      if xfee = [01]
         * lead time is 121 for passenger plate renewal
         if raplate->frenewal - l_fdate > 121    
           f_valid (.f., "Plate "+yplate+" expires "+dtoc(raplate->frenewal))
           xchk = .f.
           return (.f.)
         endif   
      else
         * lead time is 150 for commercial plate renewal
         if raplate->frenewal - l_fdate > 150
           f_valid (.f., "Plate "+yplate+" expires "+dtoc(raplate->frenewal))
           xchk = .f.
           return (.f.)
         endif   
      endif
      *------------------------------------------

   endif
endif

* calc. title/reg/transfer fee
* calculate fee
* passenger renewal expires on 09/01
* commercial renewal expires on 02/28
do case
   case l_faction = "1"            && original title
      f_compute (@l_ftitle, xtitle)
      f_compute (@l_freg,0.00)
      f_compute (@l_ftfee,0.00)
   case l_faction = "2"            && original title/ plate
      f_compute (@l_freg, rdtp1c(xmsrp))
      f_compute (@l_ftitle, xtitle)
      f_compute (@l_ftfee, 0.00)
   case l_faction = "3"            && original title/ transfer plate
      f_compute (@l_ftitle, xtitle)
      f_compute (@l_ftfee, xreg)
      f_compute (@l_freg, 0.00)
   case l_faction = "4"            && original title/ renewal transfer
      f_compute (@l_freg, rdtp1c(xmsrp))
      f_compute (@l_ftitle, xtitle)
      f_compute (@l_ftfee, 0.00)
   case l_faction = "5"            && original plate
      f_compute (@l_freg, rdtp1c(xmsrp))
      f_compute (@l_ftitle, 0.00)
      f_compute (@l_ftfee, 0.00)
   case l_faction = "6"            && renewal plate
      f_compute (@l_freg, rdtp1c(xmsrp))
      f_compute (@l_ftitle, 0.00)
      f_compute (@l_ftfee, 0.00)
   case l_faction = "7"            && transfer plate
      f_compute (@l_ftfee, xreg)
      f_compute (@l_ftitle, 0.00)
      f_compute (@l_freg, 0.00)
   * case l_faction = "8"            && replacement plate/tab
   *    f_compute (@l_freg, 5.00)
   *    f_compute (@l_ftitle, 0.00)
   *    f_compute (@l_ftfee, 0.00)
   case l_faction = "9"            && renewal - transfer plate
      f_compute (@l_ftfee, 0.00)
      f_compute (@l_freg, rdtp1c(xmsrp))
      f_compute (@l_ftitle, 0.00) 
endcase   

* assign tab #
if (l_faction $ [2;4;5;6;9] .and. empty(l_ftab)) .or.  ;    && 12.01.06
   (l_faction $ [3;7] .and. xpdesc = [003] .and. empty(l_ftab))
   * update tab #
   select ratab
   set order to 2
   set softseek on
   seek xyear
   set softseek off
   do while .not. eof () .and. ratab->fyear =xyear
      if ratab->fstatus = "A" 
          reclock ()
          replace fstatus with "X"
          commit
          unlock
          exit
      endif
      skip
   enddo
   f_compute (@l_ftab, ratab->ftab)
   select ratab
   set order to 1
endif

* --------------------------------------
* 12.07.07: previous plate should equal current plate during normal transfer
*   i.e. xpdesc <> [003]
if l_faction $ [3;7] .and. xpdesc <> [003]
   l_fpplate = ""
endif

if empty (l_fpplate) .and. l_faction $ [3;4;6;7;9]
   * f_compute (@l_fpplate, l_fplate)
   l_fpplate = l_fplate
endif
* ---------------------------------------

f_compute (@l_fstate, "MI")

xchk = .t.
return .t.

*****************************************
function rdtp1c
* 10.01.03: increase reg. fee by $3
parameter xmsrp
private y1, y2, y3, yfld, ylv, yfee, yxtra

yxtra = 3
yfee = 0
* if xfee = [01]     && passenger car
if xmsrp > 8000     && cut off for wt fee calc
   * calculate level
   y2 = val(substr(dtos(l_fdate),1,4))    && taken year 2000 into account
   y3 = xyr
   y1 = if(y2 - y3 >= 3, 3, y2 - y3)
   ylv = if(y1 <= 0, [0], str(y1,1))
   if xmsrp > gmaxfee
      do case 
      case ylv = [0]
         yfee = round (xmsrp * .005, 0)           && 12 month base fee
      case ylv = [1]
         yfee = round (xmsrp * .005, 0)
         yfee = round (yfee * .9, 0)
      case ylv = [2]
         yfee = round (xmsrp * .005, 0)
         yfee = round (yfee * .9, 0)
         yfee = round (yfee * .9, 0)
      case ylv = [3]
         yfee = round (xmsrp * .005, 0)
         yfee = round (yfee * .9, 0)
         yfee = round (yfee * .9, 0)
         yfee = round (yfee * .9, 0)
      otherwise                                     && error: just in case
         yfee = 0
      endcase
      yfee = round (yfee / 12 * l_fmonth, 0)   && prorate to l_fmonth
      yfee = yfee + 5                            && final fee
   else
      yfld = "ravalue->f"+strtran(str(l_fmonth,2)," ","")
      select ravalue
      seek str(xmsrp,5)+ylv
      if eof ()
         yfee = 0
      else
         yfee = &yfld
      endif             
   endif                  
   if yfee = 0        && warning:
      f_valid (.f., "Model Year exceeds Limit, Cannot Compute Reg. Fee...") 
   endif
   * for special plate  XXX99
   if l_faction $ [2;5] 
      yfee = yfee + xadded
   endif
   return (yfee + yxtra)
elseif xfee = [03]  && commercial must expire 02/28
   if xmsrp <= 5000
      do case
      case xmsrp > 0 .and. xmsrp <=4000
         yfee = 39.00
      case xmsrp >=4001 .and. xmsrp <=4500
         yfee = 44.00
      case xmsrp >=4501 .and. xmsrp <=5000
         yfee = 49.00
      endcase
      * wt fees are NEVER prorated
      * if l_fmonth <> 12     && applies to new plates only
      *    yfee = f_round ((yfee - 5) / 12 * l_fmonth, 0)   && prorate
      *    yfee = yfee + 5
      * endif
      *
   else
      select rawtfee
      locate for xmsrp >= fwt1 .and. xmsrp <= fwt2
      if .not. eof ()
         yfee = rawtfee->fx12fee
         * wt fees are NEVER prorated
         * if l_fmonth <> 12     && applies to new plates only
         *    yfee = f_round ((yfee - 5) / 12 * l_fmonth, 0)   && prorate
         *    yfee = yfee + 5
         * endif
         *
      endif
   endif
   if yfee = 0      && over 8000 lb
      f_valid (.f., "Weight over 8000lb, Cannot Compute Reg. Fee...")
   endif 
   * for special plate  with added fee (normally added fee=0)
   if l_faction $ [2;5] 
      yfee = yfee + xadded
   endif
   return (yfee + yxtra)
endif

return (0)

***************************************************************
function rdtp1d

if lastkey () = 5
   return .t.
endif

if l_faction $ [2;4;5;6;9] .or. ;
   (l_faction $ [3;7] .and. xpdesc = [003])      && 12.01.06
   if empty (l_ftab)
      f_valid (.f., "Please enter Tab #...")
      xchk = .f.
      return .f.
   else 
      * guard against duplicate tab
      select radtrh
      set order to 4
      seek l_ftab
      set order to 1
      if .not. eof ()
         f_valid (.f., "Duplicate tab, Please Check History Trans...")
         xchk = .f.
         return .f.
      endif 
      *
      select ratab
      seek l_ftab
      if eof () .or. .not. fstatus $ "A;X" 
         f_valid (.f., "Invalid Tab #...")
         xchk = .f.
         return .f.
      elseif fyear <> xyear
         f_valid (.f., "This Tab is Only Valid for "+fyear+" Not "+xyear)
         xchk = .f.
         return .f.
      else
         reclock ()
         replace fstatus with "U"
         commit
         unlock
      endif 
   endif
else
   l_ftab = space(7)
endif

xchk = .t.
return .t.

****************************************************************
function rdtp1e
private y1, y2

* check to see if vehicle has been titled
if l_faction $ [1;2;3;4]
   select ravm
   seek substr(l_fvin,10,8)
   if .not. eof() 
      if .not. empty(ravm->fdate1)
         f_valid (.f., "Vehicle has been titled already...")
         xchk = .f.
         return .f.
      endif
   endif                                                    
endif
* 05.10.98 check to see if vehicle have been plated already
if l_faction $ [2;5]
   select ravm
   seek substr(l_fvin,10,8)
   if .not. eof()
      if .not. empty(ravm->fplate)
         f_valid (.f., "Vehicle has been plated already...")
         xchk = .f.
         return .f.
      endif
   endif  
endif

select radtr
seek l_fbranch+dtos(l_fdate)+l_fclerk+substr(l_fvin,10,8)+l_faction
if eof ()
   if f_verify ("raaction",1,l_faction)
      if l_faction = [1]
         f_compute (@l_fplate, space(7))
         f_compute (@l_fpplate, space(7))
      elseif l_faction $ [2;5]
         f_compute (@l_fpplate, space(7))
      endif
      return .t.
   else
      f_valid (.f., "Press <F1> for help.")
      return .f.
   endif
else
   l_fplate = fplate
   l_fpplate = fpplate
   xchk = .t.
   return .t. 
endif

****************************************************************
procedure rdtph1

private yvar, yscn, ycolor, yname, ystr
yvar = alltrim (upper (readvar ()))
ycolor = setcolor (gsubcolor)
do case
case yvar = "L_FVIN"
   * display data import from download to radtr
   select radtr
   * set softseek on
   * seek l_fbranch+dtos(l_fdate)+l_fclerk+trim(l_fvin)
   * set softseek off
   * if eof ()
   *    go bottom
   * endif
   go top
   if f_pick_f (4, 08, "", "", "faction+[  ]+fvin+[   ]+fclerk+[  ]+" +   ;
             "fplate+[  ]+str(freg,6,2)+[ ]+str(ftitle,6,2)+[ ]+" +   ;
             "str(ftfee,6,2)+[ ]+dtoc(fdate)")
      * f_retrieve ()
      xrecno = recno ()
      l_fvin = fvin
      l_fcontrol = fcontrol     && control #
      l_fref = fref             && ref # 
      * l_fowner = fowner
      l_faction = faction
      l_fplate = fplate
      l_ftab = ftab
      l_freg = freg
      l_ftitle = ftitle
      l_ftfee = ftfee
      l_fseq = fseq             && 09/26/96
      keyboard chr (13)
   endif
case yvar = "L_FACTION"      && 
   f_use ("RAACTION")
   go top
   if f_valid (.not. eof (), "Missing Chart...")
      set softseek on
      seek l_faction
      set softseek off
      if eof ()
         go bottom
      endif
      if f_pick_f (6, 15, "", "", "faction+[ - ]+fdesc")
         &yvar = raaction->faction
         keyboard chr (13)
      endif
   endif
case yvar = "L_FPLATE"
   f_use ("RAPLATE")
   set softseek on
   seek l_fplate
   if eof ()
      go bottom
   endif
   set softseek off
   if f_pick_f (6, 15, "", "", "fplate+[ ]+ftab+[ ]+ftype+[ ]+fstatus")
         &yvar = raplate->fplate
         keyboard chr (13)
   endif 
case yvar = "L_FLOC"
   f_use ("RALOC")
   set softseek on
   seek l_fplate
   if eof ()
      go bottom
   endif
   set softseek off
   if f_pick_f (05, 03, "", "", "trim(floc)+[ -> ]+f_truncate(trim(fname)+" ;
      + "[ ]+fccity,50)")
         &yvar = raloc->floc
         keyboard chr (13)
   endif 
case yvar $ "YTAB;L_FTAB"
   f_valid (.f., "Enter 7 digit tab #...")
otherwise
   * display value fee chart
   f_use ("RAVALUE")
   set softseek on
   seek str(xmsrp, 5)
   set softseek off
   f_pick_f (6,15,"","","str(fval,5)+[ ]+str(flevel,1)+[ ]+str(f6,4)"+  ;
             "+[ ]+str(f7,4)+[ ]+str(f8,4)+[ ]+str(f9,4)+[ ]+str(f10,4)"+  ;
             "+[ ]+str(f11,4)+[ ]+str(f12,4)+[ ]+str(f14,4)+[ ]+str(f15,4)"+  ;
             "+[ ]+str(f16,4)+[ ]+str(f17,4)") 
endcase
setcolor (ycolor)

***************************
* 04.11.06: pop up for enter odometer
procedure rdtpf2
private yvar, yscn, ycolor, yname, ystr

yvar = alltrim (upper (readvar ()))
do case
case yvar = "L_FPLATE"
   ycolor = setcolor (gsubcolor)
   yscn = f_box (yrow-1, 38, yrow + 2, 63)
   @ yrow, 39 say "Previous Plate: "+l_fpplate
   @ yrow + 1, 39 say "Current Plate : "+l_fplate
   inkey(10)
   f_restbox (yscn)
   setcolor (ycolor)
otherwise
   if l_faction $ [1;2;3;4]
      ycolor = setcolor (gsubcolor)
      if yrow < 20
         yscn = f_box (yrow, 15, yrow+2, 35)
         @ yrow+1, 17 say "Odometer: "
         f_getnum (@xmileage, yrow+1, 27, "", "9999", .t.)
         f_restbox (yscn)
      else
         yscn = f_box (19, 15, 21, 35)
         @ 20, 17 say "Odometer: "
         f_getnum (@xmileage, 20, 27, "", "9999", .t.)
         f_restbox (yscn)
      endif
      setcolor (ycolor)                                        
      @ yrow, 25 say xmileage pict "9999"
   endif
endcase

***************************
* 04.20.06: pop up for Lien Holder
* changes:
* 08.12.06: Disable RALIEN for now
*           xlien = 0 ==> no lien holder (default)
*                 = 1 ==> lien holder 
*
procedure rdtpf3
private yscn, ylgc, y1

if .not. l_faction $ [1;2;3;4] .or. empty(glhname)   && 02.09.11
   return
endif

ylgc = if(xlien=1,.t.,.f.)
if yrow < 20
   y1 = if(ylgc,[Y],[N])
   yscn = f_box (yrow, 15, yrow+2, 39)
   @ yrow+1, 17 say "Lien Holder [Y/N]: " 
   ycolor = setcolor (gblueget)
   @ yrow+1, 36 say y1
   f_getlgc (yrow+1, 36, @ylgc)
   f_restbox (yscn)
   setcolor (ycolor)                                        
else
   yscn = f_box (19, 15, 21, 39)
   @ 20, 17 say "Lien Holder [Y/N]: "
   ycolor = setcolor (gblueget)
   @ 20, 36 say y1
   f_getlgc (20, 36, @ylgc)
   f_restbox (yscn)
   setcolor (ycolor)                                        
endif
xlien = if(ylgc,1,0)

* ? xlien        && debug
* inkey(0)

***************************
function f_round
parameter xnum, xdec

return (round(xnum - .01, xdec))

