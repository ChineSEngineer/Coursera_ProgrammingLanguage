val month_day=[31,28,31,30,31,30,31,31,30,31,30,31]
val month_day_leap=[31,29,31,30,31,30,31,31,30,31,30,31]
val month_name = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
fun is_order (d1 : int*int*int, d2 : int*int*int) =
    if #1 d1 < #1 d2
    then true
    else if #1 d1 > #1 d2
    then false
    else if #2 d1 < #2 d2
    then true
    else if #2 d1 > #2 d2
    then false
    else if #3 d1 < #3 d2
    then true
    else false
	     
fun number_in_month (date_list : (int*int*int) list, month : int) =
    let
	fun helper (date_list : (int*int*int) list, month : int, count : int) =
	    if null date_list
	    then count
	    else if #2 (hd date_list) = month
	    then helper(tl date_list, month, count + 1)
	    else helper(tl date_list, month, count)
    in
	helper(date_list, month, 0)
    end



fun number_in_months (date_list : (int*int*int) list, month_list : int list) =
    let
	fun helper(date_list : (int*int*int) list, month_list : int list, count: int) =
	    if null month_list
	    then count
	    else helper(date_list, tl month_list, count + number_in_month(date_list, hd month_list))
    in
	helper(date_list, month_list, 0)
    end
	
fun dates_in_month (date_list : (int*int*int) list, month : int) =
    let
	fun helper(date_list : (int*int*int) list, month : int, res : (int*int*int) list) =
	    if null date_list
	    then res
	    else if #2 (hd date_list) = month
	    then (hd date_list)::helper(tl date_list, month, res)
	    else helper(tl date_list, month, res)
    in
	helper(date_list, month, [])
    end

fun dates_in_months (date_list : (int*int*int) list, month_list : int list) =
    let
	fun helper(date_list : (int*int*int) list, month_list : int list, res : (int*int*int) list) =
	    if null month_list
	    then res
	    else helper(date_list, tl month_list, res @ dates_in_month(date_list,hd month_list))
    in
	helper(date_list, month_list, [])
    end

fun get_nth (str_list : string list, n : int) =
    if n = 1
    then hd str_list
    else get_nth(tl str_list, n - 1)

fun date_to_string(date : int*int*int) =
    get_nth(month_name, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)

fun number_before_reaching_sum(sum : int, int_list : int list) =
    let
	fun helper(int_list : int list, current : int, count : int) =
	    if current + hd int_list >= sum
	    then count
	    else helper(tl int_list, current + hd int_list, count + 1)
    in
	helper(int_list: int list, 0, 0)
    end

fun what_month(day_number : int) =
    number_before_reaching_sum(day_number, month_day) + 1

fun month_range(day1 : int, day2 : int) =
    let
	fun helper(day1 : int, res : int list) =
	if day1 >= day2
	then res@[what_month(day1)]
	else helper(day1 + 1,res@[what_month(day1)])
    in
	helper(day1, [])
    end

fun oldest(dates : (int*int*int) list) =
    if null dates
    then NONE
    else
	let
	    fun helper(dates : (int*int*int) list, res : int*int*int) =
		if null dates
		then SOME res
		else if is_order(res, hd dates)
		then helper(tl dates, res)
		else helper(tl dates, hd dates)
	in
	    helper(tl dates, hd dates)
	end

fun unique(month_list : int list) =
    let
	fun contain(lt : int list, a : int) =
	    if null lt
	    then false
	    else if hd lt = a
	    then true
	    else contain(tl lt, a)
			
	fun helper(month_list : int list, record : int list) =
	    if null month_list
	    then month_list
	    else if contain(record, hd month_list)
	    then helper(tl month_list, record)
	    else (hd month_list)::helper(tl month_list, (hd month_list)::record)
    in
	helper(month_list, [])
    end
	
	    
fun number_in_months_challenge(date_list : (int*int*int) list, month_list : int list) =
    number_in_months(date_list, unique(month_list))

fun dates_in_months_challenge(date_list : (int*int*int) list, month_list : int list) =
    dates_in_months(date_list, unique(month_list))

fun reasonable_date(date : int*int*int) =
    let
	fun get_day(lt : int list, n : int) =
	    if n = 1
	    then hd lt
	    else get_day(tl lt, n-1)
    in
	if #1 date <=0
	then false
	else
	    if #2 date < 1 orelse #2 date > 12
	    then false
	    else
		if (#1 date) mod 400 = 0 orelse ((#1 date) mod 4 = 0 andalso (#1 date) mod 100 <> 0)
		then
		    if #3 date < 1 orelse #3 date > get_day(month_day_leap, #2 date)
		    then false
		    else true
		else
		    if #3 date < 1 orelse #3 date > get_day(month_day, #2 date)
		    then false
		    else true
		 
    end

