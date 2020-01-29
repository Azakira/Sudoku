open Printf
open Graphics
open String
open Array


(*let path = "/mnt/d/Documents/Github/Sudoku/grids/grid0.txt"*)
let path = "/home/tochange/Documents/L3INFO/ocaml/Sudoku/grids/grid0.txt"
let pathS ="/home/tochange/Documents/L3INFO/ocaml/Sudoku/solutions/solution0.txt" 

(*PATH TONY*)
(*let pathS ="/Users/ishnuts/Documents/GitHub/Sudoku/solutions/solution0.txt"*)
(* let path = "/Users/ishnuts/Documents/GitHub/Sudoku/grids/grid0.txt" *)

let initGrille str =
	let mat = make_matrix 9 9 '0' in
	for i=0 to 8 do
		begin
			for j=0 to 8 do
				mat.(i).(j) <- str.[i*9+j]
			done;
		end
	done;
	mat

;;
let affiche_grille mat =
	for i=0 to 8 do
	 begin
		for j=0 to 8 do
			if mat.(i).(j)='0' then print_string "." else print_char mat.(i).(j);
			if j=2 || j=5 || j=8 then print_string "|" else	print_string " ";
		done;
		print_string "\n";
		if i=2 || i=5 || i=8 then print_string "─────────────────\n";
	 end
	done

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
let draw_sudoku  width height  =
        for i=0 to height-90 do
               for j =0 to width-90 do
                       if j mod 90 = 0 then
                               if i mod 90 = 0 then 
                               	    draw_rect i j (width/9)  (height/9);
               done;
        done;

;;


let stroke_that_bitch width height =	

	for i=0 to height do
	     for j=0 to width do
		if j mod 270 =0 then 
			if i mod 270 = 0 then	   		
				draw_rect i j (3*width/9) (3*height/9);
				set_line_width 3;
	     done;
	done;
	draw_rect 0 0 width  height;
;;


let insertValueInMatrix x y valeur mat = 
        mat.(x).(y) <- valeur;
        mat
;;
let removeValueOfMatrix x y mat = 
        mat.(x).(y) <- '0';
        mat
;;

let verifieLigne i matRep =
        (*given an array  of 9 zeros *)
        let hashArray = Array.make 9 0 in 
        (* consider every value in mapRep.(i) as our array's index and do +1 at that index 
         * the number at that index of our array correspond to the occurence of that index as a value
         * in the mapRep.(i) *)
        for j = 0 to 8 do
                let pos = (int_of_char (matRep.(i).(j)) -1)  in
                hashArray.( pos )  <-  hashArray.(pos) + 1
        done;
        (*transform array to list *)
        let arrayToList = to_list hashArray in
        (* check if every number occurs only once *)
        let rec verifyList l = 
                match l with
                []-> true
                |h::t -> if h=0 then verifyList t else h=0 
        in
        verifyList arrayToList


;;
 
               


(* MAIN *)

let file_string = file_to_string path;;
print_endline " affiche du resulat de file_to_string ";;
let grilleReponse= initGrille file_string;;

grilleReponse = insertValueInMatrix 0 0 '8' grilleReponse;;
grilleReponse = insertValueInMatrix 0 1 '3' grilleReponse;;
grilleReponse = insertValueInMatrix 0 2 '5' grilleReponse;;
grilleReponse = insertValueInMatrix 0 3 '9' grilleReponse;;
grilleReponse = insertValueInMatrix 0 4 '1' grilleReponse;;
grilleReponse = insertValueInMatrix 0 5 '7' grilleReponse;;
grilleReponse = insertValueInMatrix 0 6 '2' grilleReponse;;
grilleReponse = insertValueInMatrix 0 7 '4' grilleReponse;;
grilleReponse = insertValueInMatrix 0 8 '6' grilleReponse;;

affiche_grille grilleReponse;;

print_endline " verification de ligne 0 ...";;

if verifieLigne 0 grilleReponse then print_endline "the line is ok ! " else print_endline " tony is that you ?";;

affiche_grille grilleReponse;;

(*
let  rec loop () = 
        loop()
;;
let () = 
        open_graph " 811x811";
        set_window_title " sudoku ";
        draw_sudoku 810 810;
	stroke_that_bitch 810 810;
        sound 10 10 ;
        loop ()
;;
*)
