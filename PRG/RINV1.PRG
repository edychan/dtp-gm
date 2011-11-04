*
* update vehicle inventory
*
* 12.01.04: new plate config. AAA9999
f_clrscn ("DTP/MI - Update Vehicle Record")
f_use ("ravm")
f_box (01, 02, 22, 78)
@ 02, 04 say "    VIN #          Plate    Prv Plate  Titled   Recv     W Sticker   "
@ 03, 04 say "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ  ÄÄÄÄÄÄÄ  ÄÄÄÄÄÄÄÄÄ  ÄÄÄÄÄÄÄ  ÄÄÄÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄ  "
yrow = 4
set key 28 to rinvh1
do while .t.
   restore from (gmempath + "ravm") additive
   @ yrow, 04 get l_fvin picture "!!!!!!!!!!!!!!!!!"
   if f_rd () = 27
      exit
   endif
   select ravm
   if len(trim(l_fvin)) > 8
      seek substr(l_fvin,10,8)
   elseif len(trim(l_fvin)) = 8
      seek l_fvin
      if eof ()
         f_valid (.f., "Please Check VIN...")
         loop
      endif
      if f_pick_f (04, 05, "", "", "fvin+[ ]+funit+[ ]+fplate+[ ]+dtoc(fdate1)")
         l_fvin = fvin
      else
         loop
      endif
   elseif empty(l_fvin)
      f_valid (.f., "Press <F1> for Help...")
      loop
   endif
   if .not. eof ()
      l_fvin = ravm->fvin
      f_retrieve ()
   endif
   * check vin #
   if .not. f_goodvin (l_fvin)
      loop
   endif
   @ yrow, 04 say l_fvin
   do while .t.
      @ yrow, 23 get l_fplate pict "!!!!!!!"     && 12.01.04
      @ yrow, 32 get l_fpplate pict "!!!!!!!"
      @ yrow, 43 get l_fdate1 valid f_y2k(@l_fdate1) 
      @ yrow, 52 get l_fdate2 valid f_y2k(@l_fdate2)
      @ yrow, 61 get l_funit pict "!!!!!!!!!!"
      f_rd ()
      xkey = f_confirm ("[C]onfirm  [E]dit  [D]elete  [I]gnore Changes", "CEDI")
      do case
      case xkey = "C"
         @ yrow, 23 say l_fplate pict "!!!!!!!"   && 12.01.04
         @ yrow, 32 say l_fpplate pict "!!!!!!!"
         @ yrow, 43 say l_fdate1 
         @ yrow, 52 say l_fdate2
         @ yrow, 61 say l_funit pict "!!!!!!!!!!"
         select ravm
         seek substr(l_fvin,10,8)
         if eof ()
            append blank
         endif
         reclock ()
         f_replace ()
         exit
      case xkey = "E"
         loop
      case xkey = "D"
         select ravm
         seek substr(l_fvin,10,8)
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
close data

**************************
procedure rinvh1

private yvar, yscn, ycolor, yname, ystr
yvar = alltrim (upper (readvar ()))
ycolor = setcolor (gsubcolor)
do case
case yvar = "L_FVIN"
   select ravm
   set softseek on
   seek &yvar
   set softseek off
   if f_valid (.not. eof (), "Invalid VIN...")
      if f_pick_f (04, 05, "", "", "fvin+[ ]+funit+[ ]+fplate+[ ]+dtoc(fdate1)")
         &yvar = ravm->fvin
         keyboard chr (13)
      endif
   endif
otherwise
   tone (500,9)
   f_popup ("No Help information for this Field. Press Any Key to Continue...",.t.)
endcase
setcolor (ycolor)
return
