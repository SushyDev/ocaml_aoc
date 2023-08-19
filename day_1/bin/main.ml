let rec read_lines ?(lines=[]) channel =
    try
        let line = input_line channel in
        read_lines ~lines:(line :: lines) channel
    with End_of_file ->
        close_in channel;
        lines

let rec split_empty_line ?(split_list=[]) lines =
    match lines with 
    | [] -> split_list :: []
    | x :: xs -> 
        if x = "" then
            split_list :: split_empty_line xs
        else
            split_empty_line xs ~split_list:(x :: split_list)

let str_list_to_int_list = List.map int_of_string
let sum_of_list = List.fold_left (+) 0
let biggest_of_list = List.fold_left max 0

let take_biggest_three list = 
    let sorted_list = List.sort compare list in
    let rev_list = List.rev sorted_list in
    match rev_list with
    | x :: y :: z :: _ -> [x; y; z]
    | _ -> []

let sum_of_groups lines = 
    let split_lines = split_empty_line lines in
    let split_lines_int = List.map str_list_to_int_list split_lines in
    List.map sum_of_list split_lines_int

let () = 
    let file = open_in "input.txt" in
    let lines = read_lines file in
    let sum_groups = sum_of_groups lines in
    let biggest_three = take_biggest_three sum_groups in

    let max_biggest_three = biggest_of_list biggest_three in
    print_string "Biggest int: ";
    print_int max_biggest_three;
    print_endline "";

    let sum_biggest_three = sum_of_list biggest_three in
    print_string "Sum of biggest three ints: ";
    print_int sum_biggest_three;
    print_endline ""
