* ===============================================================
* special option to print title application only
* ===============================================================
f_clrscn ("DTP/MI - Print Title Application Only")
restore from (gmempath + "radtr") additive

f_box (01, 02, 05, 78)
@ 02, 04 say "Office #......"
@ 03, 04 say "Date.........."
@ 04, 04 say "Printer.......            [LPT1/LPT2/LPT3]"

l_floc = space(3)
l_fdate = date()
l_fclerk = gclerk
l_ftr11 = .f.           && print control for GM
l_faction = [2]         && special case: 
xprt = [LPT1]
xtitle = 0
xreg = 0
xyear = space(4)        && current exp. year e.g. 1997
xmsrp = 0
xadded = 0
xfee = [  ]
xyr = 0
f_use ("raloc")
do while .t.
   @ 02, 19 get l_floc pict "!!!" valid rdtp8a()
   @ 03, 19 get l_fdate valid f_y2k(@l_fdate)  
   @ 04, 19 get xprt pict "!!!!" valid f_valid (xprt $ "LPT1;LPT2;LPT3")
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
* load printer macro
if .not. pr_init (xprt, 1)      && load tr11 macros
   close data
  return
endif

f_use ("ravm")
f_use ("ravin")
f_use ("rabody")
f_use ("ravalue")
f_use ("rawtfee")
f_use ("rainvs")        && delphi vehicle record
f_box (01, 02, 22, 78)
@ 02, 04 say "VIN #             Plate     Title  Reg Fee Transfer  Total"
@ 03, 04 say "컴컴컴컴컴컴컴컴� 컴컴컴컴� 컴컴컴 컴컴컴� 컴컴컴컴  컴컴컴"           
ycopy = 1    && print 1 copy
yrow = 4
do while .t.
   @ yrow, 04 get l_fvin pict "!!!!!!!!!!!!!!!!!" valid rdtp8b ()
   if f_rd () = 27
      exit 
   endif

   l_ftotal = l_ftitle + l_freg + l_ftfee
   @ yrow, 24 say l_fplate pict "!!!!!!" 
   @ yrow, 32 say l_ftitle pict "999.99"
   @ yrow, 40 say l_freg pict "999.99"
   @ yrow, 48 say l_ftfee pict "999.99" 
   @ yrow, 57 say l_ftotal pict "999.99" 

   yedit = .f.
   ychk = .t.
   do while .t.
      @ yrow, 24 get l_fplate pict "!!!!!!" valid rdtp8c ()
      if yedit
         @ yrow, 32 get l_ftitle pict "999.99" valid l_ftitle >= 0
         @ yrow, 40 get l_freg pict "999.99"   valid l_freg >= 0
         @ yrow, 48 get l_ftfee pict "999.99"  valid l_ftfee >= 0
      endif
      f_rd () 
      l_ftotal = l_ftitle + l_freg + l_ftfee
      @ yrow, 24 say l_fplate pict "!!!!!!" 
      @ yrow, 32 say l_ftitle pict "999.99"
      @ yrow, 40 say l_freg pict "999.99"
      @ yrow, 48 say l_ftfee pict "999.99" 
      @ yrow, 57 say l_ftotal pict "999.99" 
      if ychk .and. l_ftotal > 0
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
         elseif l_faction $ [3;7] .and. l_ftfee <=0
            f_valid (.f., "Missing Transfer Fee...")
            loop
         endif
         * print tr11
         do rdtp8d with xprt, ycopy
         * update vehicle records
         select rainvs
         seek substr(l_fvin,10,8)
         if eof ()
            append blank
            replace fvin with l_fvin, fdate with l_fdate
            replace ftitle with l_ftitle, freg with l_freg
            replace ftfee with l_ftfee, ftotal with l_ftotal
            commit
            unlock
         endif
         exit
      case xkey = "D"
         xrecno = 0 
         exit
      case xkey = "E"
         yedit = .t.
         loop
      case xkey = "I"
         xrecno = 0
         exit
      endcase
   enddo
   if xkey <> "I"
      if yrow < 20
         yrow = yrow + 1
      else
         scroll (4, 04, 20, 77, 1)
      endif
   else
      @ yrow, 04 clear to yrow, 77
   endif
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
enddo

*******************************************
function rdtp8a
* assign reg. / title fee
select raloc
seek l_floc
if eof ()
   f_valid (.f., "Please Setup Branch Office First...")
   return .f.
else
   @ 02, 30 say raloc->fname
   xtitle = ftitle
   xreg = freg
   l_fbranch = fbranch
   return .t.
endif

************************************************
function rdtp8b

if empty (l_fvin)
   return .f.
endif

if f_goodvin (l_fvin)
   select rainvs
   seek substr(l_fvin, 10, 8)
   if .not. eof ()
      f_valid (.f., "Error: Duplicate Transaction...")
      return .f.
   endif
else
   return .f.
endif

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
         f_getfld (@yydesc, 15, 41, "W/N", 0, replicate ("X", 15), .t.)
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
if y2 >=2 
   f_valid (.f., "Invalid Model Year "+ravin->fyear + "...")
   return (.f.)
endif

xfee = ravin->ffee

* if ravin->ffee = "01"      && passager
* 09.17.97
if ravin->fmsrp > 10000     && fee
   if ravin->fmsrp > gmaxfee  && fee catagory
      xmsrp = ravin->fmsrp 
   else
      xmsrp = int(ravin->fmsrp/1000) * 1000
   endif
else
   xmsrp = ravin->fmsrp    && commercial    && weight
endif

@ yrow, 04 say l_fvin pict "!!!!!!!!!!!!!!!!!" 

return .t.

*******************************
function rdtp8c
private y1, y2, ymo, ymess

l_fmonth = 12
* calculate fee
f_compute (@l_freg, rdtp1c(xmsrp))
f_compute (@l_ftitle, xtitle)
f_compute (@l_ftfee, 0.00)

ychk = .t.
return .t.

*********************************
* print tr11
* note: calling routine open ravin, rabody
procedure rdtp8d
parameter xprinter, xcopy

xplate = space(7)
xdate = l_fdate
xdesc = "Transfer Title/Original Plate"
xexp = space(6)
select ravin
seek substr(l_fvin,1,8)+substr(l_fvin,10,1)
select rabody
seek ravin->fstyle
if ravin->ffee = [01]
   if empty(xexp)
      yy1 = dtoc(xdate + 365)        && 12 month plate
      xexp = substr(yy1,1,2)+[01]+substr(yy1,7,2)
   endif
   if ravin->fmsrp > gmaxfee
      xmsrp = strtran(str(ravin->fmsrp,6,0)," ","0")
   else
      xmsrp = strtran(str(int(ravin->fmsrp/1000),6,0)," ","0")
   endif
else
   if empty(xexp)
      yy1 = val(substr(dtos(xdate),1,4))         && 199X
      if substr(dtoc(xdate),1,2) $ [10,11,12]    && for month = oct,nov,dec
          yy1 = yy1 + 2
      else
          yy1 = yy1 + 1
      endif
      yy1 = str(yy1,4)
      xexp = [0228]+substr(yy1,3,2)
   endif
   if ravin->fmsrp > gmaxfee
      xmsrp = strtran(str(ravin->fmsrp,6,0)," ","0")
   elseif ravin->fmsrp > 10000
      xmsrp = strtran(str(int(ravin->fmsrp/1000),6,0)," ","0")
   else
      xmsrp = strtran(str(ravin->fmsrp,6,0)," ","0")
   endif
endif
xexp1 = substr(xexp,1,2)+"/"+substr(xexp,3,2)+"/"+substr(xexp,5,2)

set console off
set print on
set printer to &xprinter
* set printer to tr11         && debug

if xcopy = 1
   ? [!R! FSET 0p12h12v0s3b0t;CASS 1;COPY 1;CALL VTLE;EXIT,E;]
else
   ? [!R! FSET 0p12h12v0s3b0t;CASS 1;COPY 2;CALL VTLE;EXIT,E;]
endif
?
?
? "  "+f_truncate(xdesc,30)+"   "+xplate+space(10)+xexp1+space(22)+ ;
  str(l_freg,6,2)
?
? space(4)+ravin->fyear+space(7)+f_truncate(ravin->fmake,15)+space(29)+l_fvin+ ;
  space(8)+str(l_ftitle,6,2)
?
?
? "  "+f_truncate(rabody->fdesc,15)+"  "+xmsrp+space(45)+"N"+space(11)
?
? "  "+f_truncate(raloc->fname,35)+space(45)+str(l_ftfee,6,2)
? "  "+raloc->fcaddr
? "  "+f_truncate(raloc->fcaddr1,35)+space(45)+str(l_ftotal,6,2)
? "  "+f_truncate(raloc->fccity,20)+"  "+raloc->fcstate+"  "+raloc->fczip+ ;
  space(15)+space(7)+space(15)+raloc->fcounty
?
?
?
? [  NONE                                 ]+dtoc(xdate)+[    NONE]
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
? [!R! PAGE;EXIT,E;]

set print off
set printer to
set console on
set device to screen
return 


