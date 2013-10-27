(*1*)
fun is_older(date_1 : int * int * int, date_2 : int * int * int) =
    if (#1 date_1)<(#1 date_2)
    then true
    else if (#1 date_1)=(#1 date_2) andalso (#2 date_1)<(#2 date_2)
    then true
    else if  (#1 date_1)=(#1 date_2) andalso (#2 date_1)=(#2 date_2) andalso (#3 date_1)<(#3 date_2)
    then true
    else false
(*2*)
fun number_in_month(dates:(int * int * int) list,month:int) = 
    if null dates
    then 0
    else if #2 (hd dates) <> month
    then number_in_month(tl dates,month)
    else number_in_month(tl dates,month) + 1
(*3*)
fun number_in_months(dates:(int * int * int) list , months:int list) = 
    if null months
    then 0
    else number_in_month(dates,hd months) + number_in_months(dates,tl months)
(*4*)
fun dates_in_month(dates:(int * int * int)  list,month:int) = 
    if null dates
    then []
    else if #2 (hd dates) <> month
    then dates_in_month(tl dates,month) 
    else hd dates::dates_in_month(tl dates,month)
(*5*)
fun dates_in_months(dates:(int * int * int) list,months:int list) =
    if null months
    then []
    else dates_in_month(dates,hd months) @ dates_in_months(dates,tl months)
(*6*)
fun get_nth(s:string list, n:int) = 
    if n = 1
    then hd s
    else get_nth(tl s,n-1)
(*7*)
fun date_to_string(date:int * int * int) =
    let
	val months = ["January","February","March","April","May","June","July","August","September","October","November","December"]
    in 
	get_nth(months,#2 date) ^" "^ Int.toString(#3 date) ^", "^Int.toString(#1 date)
    end
	
(*8*)
fun number_before_reaching_sum(sum:int,nx:int list) =
    if sum > 0 andalso sum - hd nx <= 0
    then 0
    else number_before_reaching_sum(sum - hd nx,tl nx) + 1
(*9*)
fun what_month(day:int) = 
    let
	val months = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	number_before_reaching_sum(day,months) + 1
    end
(*10*)
fun month_range(day1:int,day2:int) = 
    let
	val months = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	if day1 > day2
	then []
	else what_month(day1)::month_range(day1 + 1,day2)
    end
(*11*)
fun oldest(dates:(int * int * int) list) = 
    if null dates
    then NONE
    else
	let
	    fun oldest_nonempty(xs:(int * int * int) list) = 
		if null (tl xs)
		then hd xs
		else
		    let
			val tl_ans = oldest_nonempty(tl xs)
		    in
			if is_older(hd xs,tl_ans)
			then hd xs
			else tl_ans
		    end
	in
	    SOME(oldest_nonempty dates)
	end


	
(*12*)
fun number_in_months_challenge(dates:(int * int * int) list , months:int list) =
    let
	fun remove(key:int,xs:int list) = 
	    if null xs
	    then []
	    else if hd xs = key
	    then remove(key,tl xs)
	    else hd xs::remove(key,tl xs)
	fun removeDup(xs:int list) = 
	    if null xs
	    then []
	    else hd xs::removeDup(remove(hd xs,tl xs))
    in
	number_in_months(dates,removeDup(months))
    end

fun dates_in_months_challenge(dates:(int * int * int) list , months:int list) =
    let
	fun remove(key:int,xs:int list) = 
	    if null xs
	    then []
	    else if hd xs = key
	    then remove(key,tl xs)
	    else hd xs::remove(key,tl xs)
	fun removeDup(xs:int list) = 
	    if null xs
	    then []
	    else hd xs::removeDup(remove(hd xs,tl xs))
    in
	dates_in_months(dates,removeDup(months))
    end


(*13*)
fun reasonable_date(date:int * int * int) =
    let
	fun isLeap(y:int) =
	    if y mod 4 = 0 andalso y mod 100 <> 0
	    then true
	    else y mod 400 = 0
	val months = [31,28,31,30,31,30,31,31,30,31,30,31]
	fun max_date(month:int,xs:int list) =
	    if month = 1
	    then hd xs
	    else max_date(month - 1,tl xs)
    in
	if #1 date <= 0
	then false
	else if #2 date <= 0 orelse #2 date > 12
	then false
	else 
	    if isLeap(#1 date) andalso #2 date = 2
	    then 
		if #3 date <= 0 orelse #3 date > max_date(#2 date,months) + 1
		then false
		else true
	    else
		if #3 date < 0 orelse #3 date > max_date(#2 date,months)
		then false
		else true
    end


