open Printf
(*open Graphics*)
open String
open Array

let path = "/mnt/c/Users/nizef/Documents/THIERRY/WorkSpace/L3INFO/Sudoku/grids/grid0.txt"

let initGrille str =
	let mat = make_matrix 9 9 0 in
	for i=0 to 9 do 
		for j=0 to 9 do 
			mat.(i).(j) <- int_of_char str.(j);
                        
		done
	done;
	mat
;;
(*
let affiche_matrix mat =
        for i=0 to mat.length do
                for j=0 to mat.length do
                        print_int mat.(i).(j);
                done
        done
;;*)
                        
let ()=
	let file = open_in path in
	try
		let line = input_line file in
		print_endline line;
	  close_in file
	
	with e ->                      (* some unexpected exception occurs *)
	  close_in_noerr file;           (* emergency closing *)
	  raise e
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

print_endline " affiche du resulat de file_to_string";;
print_string (file_to_string path) ;;

