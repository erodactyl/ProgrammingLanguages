type date = (int * int * int)

fun is_older (date1: date, date2: date) =
  let
    val (year1, month1, day1) = date1
    val (year2, month2, day2) = date2
  in
    year1 < year2
    orelse (year1 = year2
      andalso month1 < month2)
    orelse (year1 = year2
      andalso month1 = month2
      andalso day1 < day2)
  end

fun number_in_month (dates: date list, month) =
  case dates of
    [] => 0
    | date::dates' => if #2 date = month then 1 + number_in_month(dates', month) else number_in_month(dates', month)

fun number_in_months (dates: date list, months) =
  case months of
    [] => 0
    | month::months' => number_in_month(dates, month) + number_in_months(dates, months')

fun dates_in_month (dates: date list, month) =
  case dates of
    [] => []
    | date::dates' => if #2 date = month then date::dates_in_month(dates', month) else dates_in_month(dates', month)

fun dates_in_months (dates: date list, months) =
  case months of
    [] => []
    | month::months' => dates_in_month(dates, month) @ dates_in_months(dates, months')

fun get_nth (list, n) =
  if n = 1
  then hd list
  else get_nth(tl list, n - 1)

fun date_to_string (date: date) =
  let
    val months = [
      "January", "February", "March", "April",
      "May", "June", "July", "August",
      "September", "October", "November", "December"
    ]
  in
    get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

fun number_before_reaching_sum (sum, numbers) =
  let
    fun with_accum (numbers, n, curr) =
      if curr + hd numbers >= sum
      then n
      else with_accum(tl numbers, n + 1, curr + hd numbers)
  in
    with_accum(numbers, 0, 0)
  end

fun what_month (day) =
  let
    val days_in_months = [
      31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
    ]
  in
    number_before_reaching_sum (day, days_in_months) + 1
  end

fun month_range (day1, day2) =
  if day1 > day2
  then []
  else what_month (day1) :: month_range(day1 + 1, day2)

fun oldest (dates: date list) =
  case dates of
    [] => NONE
    | date::dates' =>
      let val oldest_from_rest = oldest(dates')
      in case oldest_from_rest of
        NONE => SOME date
        | SOME oldest_date => if is_older(date, oldest_date) then SOME date else SOME oldest_date
      end
