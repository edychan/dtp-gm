*
* check radtr for plate that have already been renewal
*
parameter xyear
if pcount() <> 1 
   quit
endif

select 1
use h:\dtp\dbf\raplate index h:\dtp\dbf\raplate alias raplate

select 2
use h:\dtp\dbf\radtr index h:\dtp\dbf\radtr1, h:\dtp\dbf\radtr2 alias radtr
go top 
do while .not. eof ()
   if empty(radtr->fplate)
      skip
      loop
   endif
   select raplate
   seek radtr->fplate
   if .not. eof ()
      if substr(dtos(raplate->frenewal),1,4) = xyear
         select radtr
         delete
      endif
   endif
   select radtr
   skip
enddo

close data  


