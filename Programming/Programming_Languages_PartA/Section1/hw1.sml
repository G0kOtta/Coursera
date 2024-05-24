(*problem 1*)
fun is_older(date1:int*int*int,date2:int*int*int)=
	if(#1 date1 <> #1 date2)
	then 
		#1 date1 < #1 date2
	else
		if(#2 date1 <> #2 date2)
		then 
			#2 date1 < #2 date2
		else
			#3 date1 < #3 date2

(*problem 2*)
fun number_in_month(dates:(int*int*int) list,month:int)=
	if null dates
	then 0
	else
		if(#2 (hd dates) = month)
		then 1+number_in_month(tl dates,month)
		else
			number_in_month(tl dates,month)

(*problem 3*)
fun number_in_months(dates:(int*int*int) list,month:int list)=
	if null month
	then 0
	else
		number_in_month(dates,hd month)+number_in_months(dates,tl month)

(*problem 4*)
fun dates_in_month(dates:(int*int*int) list,month:int)=
	if null dates
	then[]
	else
		if (#2 (hd dates) = month)
		then
			(hd dates)::dates_in_month(tl dates,month)
		else
			dates_in_month(tl dates,month)

(*problem 5*)
fun dates_in_months(dates:(int*int*int) list,month:int list)=
	if null month
	then []
	else
		let val month_list=dates_in_month(dates,(hd month))
		in 
			if null month_list
			then
				dates_in_months(dates,(tl month))
			else
				month_list@dates_in_months(dates,(tl month))
		end

(*problem 6*)
fun get_nth(str:string list,index:int)=
	if index=1
	then hd str
	else get_nth(tl str,index-1)

(*problem 7*)
fun date_to_string(date:int*int*int)=
	let val dic=["January", "February", "March", "April",
"May", "June", "July", "August", "September", "October", "November", "December"]
 	in
 		get_nth(dic,#2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
 	end

 (*problem 8*)
 fun number_before_reaching_sum(sum:int,data:int list)=
 	let fun acc(current_sum:int,index:int,current_data:int list)=
 		if (current_sum+(hd current_data)>=sum)
 		then index
 		else
 			acc((current_sum+(hd current_data)),index+1,(tl current_data))
 	in
 		acc(0,0,data)
 	end

(*problem 9*)
fun what_month(day:int)=
	let val month=[31,28,31,30,31,30,31,31,30,31,30,31]
	in
		number_before_reaching_sum(day,month)+1
	end

(*problem 10*)
fun month_range(day1:int,day2:int)=
	if day1>day2
	then []
	else
		let fun countfrom(from:int,to:int)=
			if from=to
			then to::[]
			else
				from::countfrom(from+1,to)
		in
			let fun generate(day:int list)=
				if null day
				then []
				else 
					what_month(hd day)::generate(tl day)
			in
			  generate(countfrom(day1,day2))
			 end
		end

(*problem 11*)
fun oldest(dates:(int*int*int) list)=
	if null dates
	then NONE
	else
		let fun maxDate(dates:(int*int*int) list)=
			if null (tl dates)
			then hd dates
			else
				let val tl_ans=maxDate(tl dates)
				in
					if  is_older(hd dates,tl_ans)
					then
						hd dates
					else
						tl_ans
				end
		in
			SOME(maxDate(dates))
		end
