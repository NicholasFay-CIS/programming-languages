(* Problem 1 *)
fun is_older (date_1 : (int * int * int), date_2 : (int * int * int)) =
    if (#1 date_1 = #1 date_2 andalso #2 date_1 = #2 date_2 andalso #3 date_1 = #3 date_2)
        then false
    else if (#3 date_1 < #3 date_2)
    	then true
    else if (#3 date_1 > #3 date_2)
        then false
    else if (#2 date_1 < #2 date_2)
        then true
    else if (#2 date_1 > #2 date_2)
        then false
    else if (#1 date_1 < #1 date_2)
        then true
    else 
        false


(* Problem 2 *)
fun number_in_month(dates_l: (int * int * int) list, month: int) =
  if null dates_l 
  	then 0
  else 
    let val month_to_check = #2 (hd dates_l)
    in
        if (month_to_check > month orelse month_to_check < month)
    	   then number_in_month(tl dates_l, month)
        else 1 + number_in_month(tl dates_l, month)
    end


(* Problem 3 *)
fun number_in_months (dates_l: (int * int * int) list, months_l: int list) =
	if null months_l orelse null dates_l
		then 0
	else 
		number_in_months(dates_l, tl months_l) + number_in_month(dates_l, hd months_l)


(* Problem 4 *)
fun dates_in_month (dates_l: (int * int * int) list, m: int) =
    if null dates_l
    	then []
    else 
        let val head_l = #2 (hd dates_l)
        in
            if (head_l = m) 
                then hd dates_l :: dates_in_month(tl dates_l, m)
            else dates_in_month(tl dates_l, m)
        end


(* Problem 5 *)
fun dates_in_months (dates_l: (int * int * int) list, months_l: int list) =
    if null months_l
    	then []
    else dates_in_month(dates_l, hd months_l) @ dates_in_months(dates_l, tl months_l)


(* Problem 6 *)
fun get_nth (strings_l: string list, n: int) =
	if null strings_l
		then ""
    else 
    	if n = 1 
    		then hd strings_l
    	else get_nth(tl strings_l, n - 1)


(* Problem 7 *)
fun date_to_string (date: (int * int * int)) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in 
        get_nth(months, #2 date) ^ "-" ^ Int.toString(#1 date) ^ "-" ^ Int.toString(#3 date)
    end


(* Problem 8 *)
fun number_before_reaching_sum(sum: int, i_list: int list) =
    let val number = hd i_list
    in
        if number >= sum
            then 0
        else
            1 + number_before_reaching_sum(sum - number, tl i_list)
    end


(* Problem 9 *)
fun what_month(day_of_year: int) = 
    let val month_day_counts = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day_of_year, month_day_counts) + 1
    end


(* Problem 10 *)
fun month_range(day_1: int, day_2: int) =
    if(day_2 < day_1)
        then []
    else 
        let val next_day = day_1 + 1
        in 
            what_month(day_1) :: month_range(next_day, day_2)
        end


(* Problem 11 *)
fun oldest(dates_l: (int * int * int) list) =
    if null dates_l
        then NONE
    else
        let fun date_helper(dates_l: (int * int * int) list) =
            if null (tl dates_l)
                then hd dates_l
            else
                let val oldest_date = date_helper(tl dates_l)
                in
                    if is_older(hd dates_l, oldest_date)
                        then hd dates_l
                    else
                        oldest_date
                end
        in
            SOME (date_helper(dates_l))
        end


(* Problem 12  *)
fun cumulative_sum(nums: int list) =
    if null nums 
        then []
    else
        let fun helper(nums: int list, total: int) =
            if null nums
                then []
            else
                (hd nums) + total :: helper(tl nums, (hd nums) + total) 
        in
            hd nums :: helper(tl nums, hd nums)
        end