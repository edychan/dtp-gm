* =======================================
* setup branch offices
* 04.12.06: add lien holder table
* 06.08.06: supply lien holder table manually, 
*           prevent user to change randomly
* ========================================
do while .t.
   xret3 = f_pushmenu (xoption2)
   if xret3 = 0
      exit
   endif
   xoption3 = substr (goption [xret3], 4)
   f_clrscn ("BRANCH OFFICE FILE MAINTENANCE")
   rest from (gmempath+"raloc") additive
   do case
   case xret3 = 1
      f_use ("raloc")
      do while .t.
         @ 05, 02 say "Branch Office..... " get l_floc pict "!!!"
         if f_rd() = 27
            exit
         endif
         set exact on
         seek l_floc
         set exact off
         if .not. found ()
            l_fmoddt = date ()
            f_add ("raloc", "raloc")
            use
            do rsyss1a with l_fbranch
            exit
         else
            f_valid (.f., "Duplicate Branch Office...")
         endif
      enddo
   case xret3 = 2
      f_use ("raloc")
      if .not. eof ()
         if f_pick_f (05, 03, "", "", "trim(floc)+[ -> ]+f_truncate(trim(fname)+" ;
               + "[ ]+fccity,50)")
            f_update ("raloc", "raloc")
            use
            do rsyss1a with l_fbranch 
         endif
      else
         f_valid (.f., "File is Empty...")
      endif
   * 06.08.06
   * enter this table manually so user cannot make changes randomly
   * also take out definition in RAPATH, RAIDX, DBSETUP
   *case xret3 = 3            && 04.12.06
   *   private ydbfld [5], ydbcol [5], ydbvalid [5], ydbpict [5], yedfld [5]
   *   f_clrscn ("DTP/MI - LIEN HOLDER TABLE")
   *   f_use ("ralien")
   *   go top
   *   ydbfld[1]="fname"
   *   ydbfld[2]="faddr"
   *   ydbfld[3]="fcity"
   *   ydbfld[4]="fstate"
   *   ydbfld[5]="fzip"
   *   ydbcol[1]="Name"
   *   ydbcol[2]="Address"
   *   ydbcol[3]="City"
   *   ydbcol[4]="State"
   *   ydbcol[5]="Zip"
   *   ydbvalid[1]="f_valid(.not. f_verify('ralien',1,substr(l_fname,1,15)),'Duplicate Name')"
   *   ydbvalid[2]=".t."
   *   ydbvalid[3]=".t."
   *   ydbvalid[4]=".t."
   *   ydbvalid[5]=".t."
   *   ydbpict[1]=replicate ([!],36)
   *   ydbpict[2]=replicate ([!],20)
   *   ydbpict[3]=replicate ([!],19)
   *   ydbpict[4]=replicate ([!],2)
   *   ydbpict[5]=replicate ([!],5)
   *   yedfld[1]=".t."
   *   yedfld[2]=".t."
   *   yedfld[3]=".f."
   *   yedfld[4]=".f."
   *   yedfld[5]=".f."
   *
   *   f_edit (ydbfld, ydbcol, 0, .t., ydbvalid, ydbpict, yedfld)
   *   use
   endcase
enddo
release all like l_*
close database

**********************
procedure rsyss1a
parameter xbranch

* update system counter
f_use ("ractr")
locate for fbranch = xbranch
if eof ()
   append blank
   replace fbranch with xbranch, fseq1 with 1
endif
use
   
