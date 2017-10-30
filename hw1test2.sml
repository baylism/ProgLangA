use "q1.sml";

val number_in_month_t1 = number_in_month ([(2012,2,28),(2013,12,1)],1) = 1
val number_in_month_t2 = number_in_month ([(2012,4,28),(2013,12,1)],3) = 0
val number_in_month_t3 = number_in_month ([(2012,2,28),(2013,2,1),(2012,12,22)],2) = 2
val number_in_month_t4 = number_in_month ([(2012,2,28),(2013,2,1)],2) = 0

val number_in_months_t1 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val number_in_months_t2 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[1,1,4]) = 3


val test6_a = get_nth (["hi", "there", "how", "are", "you"], 2) = "you"

val test7 = date_to_string (2013, 6, 1) = "June "
