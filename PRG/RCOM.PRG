*************************************************************************
* DTP - communication program
* Revision:
* 03.01.99: send file thru ftp
*    create mwftp.txt
*    send by upload.bat : ftp -s:mwftp.txt 204.23.253.97 
* --
* 11.01.11: MDOS change passwords for 800 mailbox as follows:
* 
*   801: DOSC801 -> C801DOS11   (801, 802 & 805 combined as 12/09)
*   802: DOSC802 -> C802DOS11   (Not used)
*   807: DOSC807 -> C807DOS11
*   809: DOSC809 -> C809DOS11
*************************************************************************
private ymailbox, ypasswd
f_clrscn ("DTP/MI - Send File")
f_use ("rasys")
xpost = fpostdate                 && last posted date
use

f_box (01, 02, 04, 78)
@ 02, 04 say "Branch #......"
@ 02, 50 say "Last Posted on.... " + dtoc(xpost)
@ 03, 04 say "Date.........."

xbranch = gloc       && branch for qual-effic  804
xdate = date()
do while .t.
   @ 02, 19 get xbranch pict "999" 
   @ 03, 19 get xdate valid f_y2k(@xdate)
   if f_rd () = 27
      return
   endif
   yret = f_confirm ("[C]ontinue  [E]dit  [Q]uit ","CEQ")
   if yret = "C"
      xfil = xbranch + jdate(xdate) + ".txt"
      if file (gcompath+xfil)
         exit
      else
        f_valid (.f., "Missing Send File...")
        loop
      endif    
   elseif yret = "Q"
      return
   endif
enddo

* create script file to send thru procomm
yfil = gcompath + "mwftp.txt"

* --11.01.11: for GM offices only
ymailbox = ""
ypasswd = ""
do case
case xbranch = [801]
   ymailbox = [DOSC801]
   ypasswd = [C801DOS11]
case xbranch = [802]
   ymailbox = [DOSC802]
   ypasswd = [C802DOS11]
case xbranch = [807]
   ymailbox = [DOSC807]
   ypasswd = [C807DOS11]
case xbranch = [809]
   ymailbox = [DOSC809]
   ypasswd = [C809DOS11]
endcase
* --11.01.11

set device to print
set printer to &yfil
setprc (0,0)
yln = 0
@ yln, 0 say ymailbox
yln = yln + 1
@ yln, 0 say ypasswd
yln = yln + 1
@ yln, 0 say [put ]+gcompath+xfil+[ DOS800@DOS800]
yln = yln + 1
@ yln, 0 say [quit]

set printer to
set console on
set print off
set device to screen

*yret = swpruncmd ("upload.bat", 0, gcompath, "f:\dtp")
*
*if .not. yret
*   y1 = swperrmaj ()
*   y2 = swperrmin ()
*   f_valid (.f., "Error: " + y1 + " " + y2)
*endif

f_valid (.f., "Start SOS COMMUNICATION to transmit data to LANSING...")
