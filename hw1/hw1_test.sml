use "hw1.sml";

(*Test problem 1*)
val test1a = is_older((1,2,2003),(2,2,2002))
val test1b = is_older((1,2,2003),(2,2,2003))
val test1c = is_older((1,2,2003),(1,2,2004))
val test1d = is_older((2,3,2004),(1,2,2003))
val test1e = is_older((2,2,2003),(1,2,2003))
val test1f = is_older((2,3,2004),(2,3,2004))
val test1g = is_older((2,2,2004),(2,3,2004))

(*Test problem 2*)
val test2a = number_in_month([], 3)
val test2b = number_in_month([(1,3,2021),(4,3,2005)],4)
val test2c = number_in_month([(20,3,1998),(1,3,2009)],3)

(*Test problem 3*)
val test3a = number_in_months([(22,2,2014),(2,3,2006),(2,3,2001),(2,4,2008)],[2,3,4])
val test3b = number_in_months([(2,2,2008),(2,6,2001),(1,3,2001),(2,4,2008)],[2,3,10,4])
val test3c = number_in_months([(20,2,2008),(2,12,2001),(21,3,2001),(2,4,2008)],[])
val test3d = number_in_months([],[1,2,3,4])
val test3e = number_in_months([],[])

(*Test problem 4*)
val test4a = dates_in_month([(2,2,2020),(10,2,2010)],2)
val test4b = dates_in_month([],2)
val test4c = dates_in_month([(2,3,2008),(2,2,2012)],2)

(*Test problem 5*)
val test5a = dates_in_months([(20,2,2007),(3,12,2001),(2,3,2001),(3,5,2008)],[2,3,4])
val test5b = dates_in_months([(2,2,2008),(3,12,2001),(21,1,2012),(15,8,2008)],[])
val test5c = dates_in_months([],[2,3,4])

(*Test problem 6*)
val test6a = get_nth(["Hello!", "this", "is", "a", "test"], 1)
val test6b = get_nth(["Hello!", "this", "is", "a", "test"], 2)
val test6c = get_nth(["Hello!", "this", "is", "a", "test"], 3)
val test6d = get_nth(["Hello!", "this", "is", "a", "test"], 4)
val test6e = get_nth(["Hello!", "this", "is", "a", "test"], 5)

(*Test problem 7*)
val test7a = date_to_string((23, 1, 2002))
val test7b = date_to_string((15, 2, 2003))
val test7c = date_to_string((16, 3, 2004))
val test7d = date_to_string((10, 4, 2005))
val test7e = date_to_string((12, 5, 2006))
val test7f = date_to_string((15, 6, 2007))
val test7g = date_to_string((24, 7, 2008))
val test7h = date_to_string((6, 8, 2009))
val test7i = date_to_string((1, 9, 2010))
val test7j = date_to_string((2, 10, 2011))
val test7k = date_to_string((8, 11, 2012))
val test7l = date_to_string((7, 12, 2013))

(*Test problem 8*)
val test8a = number_before_reaching_sum(20, [1,2,3,4,5,6,7,8,9,10])
val test8b = number_before_reaching_sum(1, [1,2,3,4,5])
val test8c = number_before_reaching_sum(5, [1,2,3,4,5])

(*Test problem 9*)
val test9a = what_month(1)
val test9b = what_month(100)
val test9c = what_month(364)

(* Test problem 10 *)
val test10a = month_range(1, 32)
val test10b = month_range(31, 34)
val test10c = month_range(50, 49)

(* Test Problem 11 *)
val test11a = oldest([(5,2,1998),(8,6,2001), (4, 2, 1998), (4, 2, 1997)])
val test11b = oldest([])
val test11c = oldest([(17,7,1998),(17,7,1998),(17,7,1998)])

(* Test problem 12 *)
val test12a = cumulative_sum([12,27,13])
val test12b = cumulative_sum([1,2,3])
val test12c = cumulative_sum([100,100,100])