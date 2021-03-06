*
* setup vin chart
* 
f_clrscn ("DTP/MI - VIN CHART")

f_use ("rabody")
f_use ("rafee")
f_use ("ravin")

f_box (01, 02, 22, 78)
@ 02, 04 say "   VIN    MODEL   YEAR/MAKE       BODY/MSRP FEE/DESCRIPTION"
@ 03, 04 say "ÄÄÄÄÄÄÄÄÄ ÄÄÄÄÄÄ  ÄÄÄÄÄÄÄÄÄÄÄÄÄÄ  ÄÄÄÄÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"

set key 28 to rsyssh3
yrow = 4
do while .t.
   restore from (gmempath + "ravin") additive
   @ yrow, 04 get l_fvin pict "!!!!!!!!!" valid ;
     f_valid (.not. empty(l_fvin), "Invalid VIN #...")
   @ yrow, 14 get l_fmodel pict "!!!"
   if f_rd () = 27
      exit
   endif
   if len(alltrim(l_fvin)) < 9
      select ravin
      set softseek on
      seek l_fvin
      set softseek off
      if eof ()
         go bottom
      endif
      if f_pick_f (04,05,"","","fvin+[ ]+substr(fmodel,1,3)+[ ]+fyear+[ ]+fmake+[ ]+" + ;
         "fstyle+[ ]+str(fmsrp,8)+[ ]+ffee+[ ]+fdesc")
         f_retrieve ()
         l_fmodel = substr(l_fmodel,1,3)
      else
         loop
      endif
   else
      select ravin
      seek l_fvin+substr(l_fmodel,1,3)
      if .not. eof ()
         f_retrieve ()
      endif
   endif
   if len(alltrim(l_fvin)) < 9
      f_valid (.f., "Invalid VIN...")
      loop
   endif 
   @ yrow, 04 say l_fvin    
   @ yrow, 14 say substr(l_fmodel,1,3)
   do while .t.
      @ yrow, 22 get l_fyear pict "99"
      @ yrow, 25 get l_fmake pict "!!!!!!!!!!!!"
      @ yrow, 38 get l_fstyle pict "!!" valid rsyss3a ()
      @ yrow, 41 get l_fmsrp pict "999999" valid f_valid(l_fmsrp > 0)
      @ yrow, 48 get l_ffee pict "!!" valid ;
        f_valid (f_verify("rafee",1,l_ffee),"Press <F1> for help...")
      @ yrow, 51 get l_fdesc pict "!!!!!!!!!!!!!!!" 
      f_rd ()
      xkey = f_confirm ("[C]onfirm  [E]dit  [D]elete  [I]gnore Changes", "CEDI")
      do case
      case xkey = "C"
         @ yrow, 22 say l_fyear pict "99"
         @ yrow, 25 say l_fmake pict "!!!!!!!!!!!!"
         @ yrow, 38 say l_fstyle pict "!!" 
         @ yrow, 41 say l_fmsrp pict "999999" 
         @ yrow, 48 say l_ffee pict "!!" 
         @ yrow, 51 say l_fdesc pict "!!!!!!!!!!!!!!!" 
         select ravin
         seek l_fvin+substr(l_fmodel,1,3)
         if eof ()
            append blank
         endif
         f_replace ()
         exit
      case xkey = "E"
         loop
      case xkey = "D"
         select ravin
         * seek l_fvin      && 12.03.02
         if .not. eof ()
            if f_confirm ("Are you sure you want to delete this " + ;
               "record? [Y/N]", "YN") = "Y"
               reclock ()
               delete
               commit
               unlock
            endif
         endif
         exit 
      case xkey = "I"
         exit
      endcase
   enddo
   if xkey <> "I"
      if yrow <= 20
         yrow = yrow + 1
      else
         scroll (4, 04, 21, 77, 1)
      endif
   else
      @ yrow, 04 clear to yrow, 77
   endif
enddo

set key 28 to
close database

*****************************************
function rsyss3a

if f_verify ("rabody",1,l_fstyle)
   f_compute (@l_ffee, rabody->ftype)
   return .t.
else
   f_valid (.f., "Press <F1> for help...")
   return .f.
endif

****************************************************************
procedure rsyssh3

private yvar, yarray[3], yptr

yvar = alltrim (upper(readvar()))
do case
case yvar = "L_FVIN"
   select ravin
   set softseek on
   seek &yvar
   if eof()
      go bottom
   endif
   if f_pick_f (04,05,"","","fvin+[ ]+substr(fmodel,1,3)+[ ]+fyear+[ ]+fmake+[ ]+" + ;
      "fstyle+[ ]+str(fmsrp,8)+[ ]+ffee+[ ]+fdesc")
      l_fvin = ravin->fvin
      l_fmodel = substr(fmodel,1,3)
      keyboard chr(13)
   endif
   set softseek off
case yvar = "L_FSTYLE"      && pick body style
   select rabody
   set softseek on
   seek l_fstyle
   if eof ()
      go bottom
   endif
   set softseek off
   if f_pick_f (05, 05, "", "", "fcode+[ - ]+fdesc")
      &yvar = rabody->fcode
      f_compute (@l_ffee, rabody->ftype)
      keyboard chr (13)
   endif
   select ravin
case yvar = "L_FFEE"      && pick fee code
   select rafee
   set softseek on
   seek l_ffee
   if eof ()
      go bottom
   endif
   set softseek off
   if f_pick_f (05, 05, "", "", "fcode+[ - ]+fdesc")
      &yvar = rafee->fcode
      keyboard chr (13)
   endif
   select ravin
otherwise
   tone (500,9)
   f_popup ("No Help information for this Field. Press Any Key to Continue...",.t.)
endcase
set cursor on



