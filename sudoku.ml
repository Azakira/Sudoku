open Printf
(*open Graphics*)
open String
open Array

(* let path = "/mnt/c/Users/nizef/Documents/THIERRY/WorkSpace/L3INFO/Sudoku/grids/grid0.txt" *)
let path = "/Users/ishnuts/Documents/GitHub/Sudoku/grids/grid0.txt"
let initGrille str =
	let mat = make_matrix 9 9 0 in
	for i=0 to 9 do 
		for j=0 to 9 do 
			mat.(i).(j) <- int_of_char str.(j);
                        
		done
	done;
	mat
;;
               
let file_to_string path =
        let file = open_in path in
        try 
                let line = input_line file in
                close_in file;
          line;
        with e->
                close_in_noerr file;
                raise e
;;

let file_string = file_to_string path;;
print_endline " affiche du resulat de file_to_string ";;
print_endline (file_string) ;;

 
