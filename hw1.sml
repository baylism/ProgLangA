(* Answers for homework 1, week 2*)


fun is_older (date1 : int*int*int, date2 : int*int*int) =
  if (#1 date1) > (#1 date2)
  then false
  else if (#1 date1) = (#1 date2) andalso (#2 date1) > (#2 date2) 
  then false
  else if (#1 date1) = (#1 date2) andalso (#2 date1) > (#2 date2) andalso (#3 date1) > (#3 date2)
  then false
  else true
	   
fun number_in_month (list_dates : (int*int*int) list, month : int) =
  if null list_dates
  then 0
  else if (#2 (hd(list_dates))) = month
  then 1 + number_in_month(tl list_dates, month)
  else 0 + number_in_month(tl list_dates, month)

fun number_in_months (list_dates : (int*int*int) list, months : int list) =
  if null months
  then 0
  else number_in_month(list_dates, hd months) + number_in_months(list_dates, tl months)

fun dates_in_month (list_dates : (int*int*int) list, month : int) =
  if null list_dates
  then []
  else if (#2 (hd(list_dates))) = month
  then hd list_dates :: dates_in_month (tl list_dates, month)
  else dates_in_month(tl list_dates, month)
		     
fun dates_in_months (list_dates : (int*int*int) list, months : int list) =
  if null months
  then []
  else dates_in_month(list_dates, hd months) @  dates_in_months(list_dates, tl months)

fun get_nth (strings : string list, n : int) =
  if n = 1
  then hd strings
  else get_nth(tl strings, n-1)

fun date_to_string (date : (int*int*int)) =
  let val months_list = ["January ", "February ", "March ", "April ",
			 "May ", "June ", "July ", "August ", "September ",
			 "October ", "November ", "December "]
  in
      get_nth(months_list, #2 date) ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

fun number_before_reaching_sum (sum : int, numbers : int list) =
  if hd numbers >= sum
  then 0
  else 1 + number_before_reaching_sum(sum - hd numbers, tl numbers)

fun what_month (day : int) =
  let val month_lengths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in number_before_reaching_sum(day, month_lengths) + 1
  end

fun month_range (day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest (dates : (int*int*int) list) =
  if null dates
  then NONE
  else let
      fun check_dates (dates : (int*int*int) list) =
	     if null (tl  dates)
	     then hd dates
	     else let val oldest_so_far  = check_dates(tl dates)
		  in
		      if is_older(hd dates, oldest_so_far)
		      then hd dates
		      else oldest_so_far
		  end
       in
	   SOME (check_dates dates)
       end
	   
