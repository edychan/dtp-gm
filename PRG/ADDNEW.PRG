*************************************
* To load h:\dtp\dbf\renewal.dbf
*    usage: addnew 806 01  branch 806 passenger plate
*           addnew 806 03  branch 806 commercial plate
*           addnew 811 01  branch 811 passenger plate
*           addnew 811 03  branch 811 commercial plate
parameter xbranch, xtype
set excl off

if pcount() <> 2
   quit
endif

if .not. xtype $ [01;03]
   quit
endif

if empty (gete ("RACPTH"))
   xpath = "\"
else
   xpath = alltrim (upper (gete ("RACPTH")))
endif
gdbfpath = "H:" + xpath + "DTP\DBF\"
yfil = gdbfpath + "raloc"
select 0
use &yfil index &yfil 
seek xbranch
if eof ()
   quit
else
   l_fowner = upper(alltrim(fname))
endif
use

xdate = date()
gexpmo = [09]    && GM
* define renewal
if xtype = [01]
   yy1 = val(substr(dtos(xdate),1,4))         && 199X
   yy1 = str(yy1,4)
   xrenewal = ctod(gexpmo + [/01/] + yy1)
else
   yy1 = val(substr(dtos(xdate),1,4))         && 199X
   if substr(dtoc(xdate),1,2) $ [10,11,12]    && for month = oct,nov,dec
       yy1 = yy1 + 1
   endif
   yy1 = str(yy1,4)
   xrenewal = ctod([02] + [/28/] + yy1)
endif

? xrenewal
inkey(0)


yfil = gdbfpath + "renewal"
select 0
use &yfil excl alias renewal
index on descend(substr(fvin,10,8)) to &yfil

yfil = gdbfpath + "ravm"
yntx = gdbfpath + "ravm1"
select 0
use &yfil index &yntx alias ravm

yfil = gdbfpath + "ravalue"
select 0
use &yfil index &yfil alias ravalue

yfil = gdbfpath + "rawtfee"
select 0
use &yfil alias rawtfee

yfil = gdbfpath + "ravin"
select 0
use &yfil index &yfil alias ravin

yfil = gdbfpath + "raplate"
select 0
use &yfil alias raplate
go top
do while .not. eof ()
   if raplate->ftype <> xtype .or. raplate->frenewal <> xrenewal
      skip
      loop
   endif
   ? "processing -> " + raplate->fplate
   yvin = ""
   ycontrol = ""
   yreg = 0
   select ravm
   seek raplate->fvin
   if eof ()
      ycontrol = [VIN]
   else
      yvin = ravm->fvin
   endif
   xyr = 0
   xfee = ""
   xmsrp = 0
   if .not.empty(yvin)
      select ravin
      seek substr(yvin,1,8)+substr(yvin,10,1)
      xyr = val(ravin->fyear)
      xyr = if(xyr>=50, xyr+1900, xyr+2000)
      xmsrp = ravin->fmsrp
      xfee = ravin->ffee
      yreg = calc_fee (xmsrp)
      yreg = if(yreg>999, 0, yreg)     && just in case
   endif
* ? yvin
* ? yreg
* inkey(0)
   select renewal
   seek descend(substr(yvin, 10, 8))
   if eof ()
      append blank
      replace fvin with yvin, faction with [6]
      replace fplate with raplate->fplate, fpplate with raplate->fplate
      replace fowner with l_fowner, fstate with [MI], fmonth with 12
      replace freg with yreg, ftotal with yreg
      replace fcontrol with ycontrol
      commit
      unlock
   else
      rlock ()
      replace faction with [6]
      replace fplate with raplate->fplate, fpplate with raplate->fplate
      replace fowner with l_fowner, fstate with [MI], fmonth with 12
      replace freg with yreg, ftotal with yreg
      replace fcontrol with ycontrol
      commit
      unlock
   endif
   select raplate
   skip
enddo

close all

*****************************************
function calc_fee

parameter xmsrp
private y1, y2, y3, yfld, ylv, yfee, ymonth, yxtra

yxtra = 3     && 10.01.03: add $3 to reg. fee
ymonth = 12   && for renewal only
yfee = 0
if xmsrp > 8000     && 01.09.04: cut off for wt fee calc
   * calculate level
   y2 = val(substr(dtos(xdate),1,4))    && taken year 2000 into account
   y3 = xyr
   y1 = if(y2 - y3 >= 3, 3, y2 - y3)
   ylv = if(y1 <= 0, [0], str(y1,1))
   if xmsrp > 99999
      do case 
      case ylv = [0]
         yfee = f_round (xmsrp * .005, 0)           && 12 month base fee
      case ylv = [1]
         yfee = f_round (xmsrp * .005, 0)
         yfee = f_round (yfee * .9, 0)
      case ylv = [2]
         yfee = f_round (xmsrp * .005, 0)
         yfee = f_round (yfee * .9, 0)
         yfee = f_round (yfee * .9, 0)
      case ylv = [3]
         yfee = f_round (xmsrp * .005, 0)
         yfee = f_round (yfee * .9, 0)
         yfee = f_round (yfee * .9, 0)
         yfee = f_round (yfee * .9, 0)
      otherwise                                     && error: just in case
         yfee = 0
      endcase
      yfee = f_round (yfee / 12 * ymonth, 0)   && prorate to ymonth
      yfee = yfee + 5                            && final fee
   else
      xmsrp = int(xmsrp/1000) * 1000
      yfld = "ravalue->f"+strtran(str(ymonth,2)," ","")
      select ravalue
      seek str(xmsrp,5)+ylv
      if eof ()
         yfee = 0
      else
         yfee = &yfld
      endif             
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
   else
      select rawtfee
      locate for xmsrp >= fwt1 .and. xmsrp <= fwt2
      if .not. eof ()
         yfee = rawtfee->fx12fee
      endif
   endif
   return (yfee + yxtra)
endif

return (0)

***************************
function f_round
parameter xnum, xdec

return (round(xnum - .01, xdec))


