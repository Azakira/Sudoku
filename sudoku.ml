open Printf
open Graphics
open String
open Array



(************* file paths ******************)
(*PATH ZAK*)
(*let path = "/mnt/d/Documents/Github/Sudoku/grids/grid0.txt"*)

(*PATH*)
let pathReponse = "/home/tochange/Documents/thierry/workSpace/psud/l3Info/s6/pfa/Sudoku/grids/grid0.txt"
let pathSolution = "/home/tochange/Documents/thierry/workSpace/psud/l3Info/s6/pfa/Sudoku/solutions/solution0.txt"
(*let pathS ="/home/tochange/Documents/L3INFO/ocaml/Sudoku/solutions/solution0.txt"*)


(*PATH TONY*)
(*let pathS ="/Users/ishnuts/Documents/GitHub/Sudoku/solutions/solution0.txt"*)
(* let path = "/Users/ishnuts/Documents/GitHub/Sudoku/grids/grid0.txt" *)
(*let path = "/home/tp-home007/tabemon/L3/S6/PFA/Sudoku-master/grids/grid0.txt"*)

(************** end of file paths section *******)



(******** global variables **********************)

(* every cell has 90*90 pixels. this number depends on hight and width of the sudoku*)
let cellSize = 90

(*the height of the whole sudoku windows bottom =0 to top =810*)
let height = 810

(*the width of the whole sudoku windows left=0 to right=810*)
let width = 810

(* all different values that a cordinate in sudoku can have*)
let positionList = [0;90;180;270;360;450;540;630;720;810];;

(*data structure to represent sudoku position *)
type sudoku_position = {mutable abs:int; mutable ord:int};;



(********* end of global variables **************)


(*function initGrille
  given  a string (one line = unidimension) big enough here 9*9 =81 characters, considering 
  a nested loop of i and j where i are lines and j colomns (0<=i<=8 and 0<=j<=8)
  return a matrix where the values at (i,j) is a  couple of the value at
  i*9+j of the string and false if the value is '0', true otherwise
  @param  a string of integers
  @return a 9*9 matrix of couples (int as a char, bool)
*)
let initGrille str =
        let mat = make_matrix 9 9 ('0',true) in
	for i=0 to 8 do
			for j=0 to 8 do
                                if (str.[i*9+j] <> '0') then
                                        mat.(i).(j)<- (str.[i*9+j],false)
                                else
                                        mat.(i).(j) <- (str.[i*9+j],true)
                        done
	done;
	mat

;;

(*procedure affiche_grille  
  @param a 9*9 matrix of couples (int as a char, bool)
  @return none
*)
let affiche_grille mat =
	for i=0 to 8 do
		for j=0 to 8 do
                        begin
                        if (fst (mat.(i).(j))  = '0') then 
                                print_string "."
                        else 
                                print_char (fst (mat.(i).(j)));
	                        if j=2 || j=5 || j=8 then 
                                        print_string "|"
                                else	
                                        print_string " ";
                       end;
		done;
		print_string "\n";
		if i=2 || i=5 || i=8 then print_string "─────────────────\n";
        done;
        if (snd (mat.(0).(0)) ) then print_string "ok"
        else print_string "not ok";

;;

(*function file_to_string 
  given  a file/system/path to a file, if it can not open the file return an error
  otherwise this  function open the file
  grab the content as string with the function input_line and return the string
  @param a file system path
  @return a string
*)
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


(*procedure draw_sudoku
  given a height and a width, considering a nested loop where i are horizental lines ans j
  vertical lines, i and j varying from 0 to (height|width/cellSize)-1=8 as  we  choose height and width  to get 9*9 cells
  and we draw lines every cellSize pixel  (i*cellSize)(j*cellSize)
  @param a width, a height
  @return none
*)
let draw_sudoku  width height  =
        for i=0 to (height/cellSize)-1 do
               for j =0 to (width/cellSize)-1 do
                 draw_rect (i*cellSize) (j*cellSize) (width*10)  (height*10); (*cuz cellSize/9=10*)
               done;
        done;
;;

(*procedure  draw_sudoku_value 
  given a matrix of couples (char,bool), draw its characters to screen 
  !!! ocaml graph uses mathematical represenation of graphs: i or x is from left to light 
      and j from bottom to top  !!!
  @param sudoku_values : matrix of couples (char,bool)
  @return none
*)
let draw_sudoku_value sudoku_values height width=
        for i=0 to (height/cellSize)-1 do
               for j =0 to (width/cellSize)-1 do
                 moveto (i*cellSize+(cellSize/2))  ((((width/cellSize)-1)-j)*cellSize+(cellSize/2)) ;
                 if ( (fst (sudoku_values.(j).(i))) != '0') then
                            Graphics.draw_char (fst (sudoku_values.(j).(i)));
              
               done;
        done;
;;



(*function stroke 
  given a width and height will chossen as we want 9*9 grid
  considering a nested loop i and j from 0 to 3 car height(810)/270 =3
  stroke lines at every 270 pixel stroke on length of 270 (3*width(810)/9)
  @param a width and height
  @return none
*)
let stroke width height =

	for i=0 to height/270 do
	     for j=0 to width/270 do   		
				draw_rect (i*270) (j*270) (3*width/9) (3*height/9);
				set_line_width 3;
	     done;
	done;
	draw_rect 0 0 width  height;
;;

(*function insertValueInMatrix 
  given a x, a y, a value: interger as a char  and a matrix, this fct put the value in the
  position (x,y) of the matrix a,d return the matrix
  @param x, y : integers for the position, and an Interger as char and a matrix of couples (integer as a char, bool)
  @return a matrix of couples (integer as a char, bool)
*)
let insertValueInMatrix x y valeur mat = 
        mat.(x).(y) <- (valeur,true);
;;

(*function removeValueOfMatrix 
  given a x, a y and a matrix of couples (integer as a char, bool)
  put the value at (x,y) of the matrix to '0'(zero as a char)
  @param x, y integers and a matrix of couples (integer as a char, bool)
  @return matrix of couples (integer as a char, bool)
*)
let removeValueOfMatrix x y mat = 
        mat.(x).(y) <- '0';
        mat
;;

(*bool compare
  given 2 lists, this function compare one element of one list to one element of other list 
  that are at the same position 
  @param l1 ,l2: Lists
  @return bool
*)
let rec compare l1 l2 = 
        match l1 with
        |[] -> true
        |h::t-> if h!= List.hd l2 then false else compare t (List.tl l2)
;;

(*function verifGrille
  given 2 matrix, this fct  transforms every matrix into a list, then call
  the function compare on those 2 lists if compare return true this fct 
  return "c bon" return "c pas bon" otherwise
  @param matRep, matSol : matrix of couples (integer as a char, bool)
  @return a string
*)
let verifGrille matRep matSol = 
        let listRep = to_list matRep in
        let listSol = to_list matSol in
        if(compare listSol listRep== true) then print_endline "c bon" else print_endline " c pas bon"
;;

               
(*function sudoku_position 
  given a position given a couple of (x,y) in bounds of our sudoku and
  a list of possible positions, it choose the right position and returns
  a centered coordinate of the sudoku cell
  @param a couple :(int,int)
  @return a couple :(int,int)
*)
let get_sudoku_position pos_couple positionList = 
  let (x,y) = pos_couple in
    let rec matchPos l sudok_pos = 
     match l with
     | [] -> sudok_pos
     | possiPos::restofPos -> if ((sudok_pos.abs) = (-1)) then 
                                  if (possiPos>x) then
                                    sudok_pos.abs <- ((possiPos-90)+(cellSize/2));
                              if ((sudok_pos.ord) = (-1)) then
                                  if(possiPos>y) then 
                                    sudok_pos.ord <- (( possiPos-90)+(cellSize/2));
                              matchPos restofPos sudok_pos
    in matchPos positionList {abs = -1; ord = -1}
;;

(*procedure read_key_and_draw
  listen for numerical keys and draw them in sudoku cell
  under the mouse
  @param none 
  @return none
*)
let read_key_and_draw ()= 
  while true do
    let e = Graphics.wait_next_event [Graphics.Key_pressed] in
    if e.Graphics.keypressed then 
      let key = e.Graphics.key in
      if( (int_of_char key)>=(int_of_char '1') && (int_of_char key)<=(int_of_char '9')) then begin
        let mousePos = (e.mouse_x,e.mouse_y) in
        let sP = get_sudoku_position mousePos positionList in
        Graphics.moveto (sP.abs) (sP.ord); 
        Graphics.draw_char e.key
      end
  done;
;;
(* MAIN *)

(* import grid*)
let file_stringR = file_to_string pathReponse;;
(* import solution*)
let file_stringS = file_to_string pathSolution;;

(*make grille of grid*)
let grilleReponse = initGrille file_stringR;;

(*make grille of solution*)
let grilleSolution = initGrille file_stringS;;


insertValueInMatrix 0 0 '8' grilleReponse;
insertValueInMatrix 0 1 '3' grilleReponse;
insertValueInMatrix 0 2 '5' grilleReponse;
affiche_grille grilleReponse;;

verifGrille grilleSolution grilleSolution;;


(*function loop
  calls its self
  @param unit
  @return none
*)
let  rec loop () = 
        loop()
;;


(*unit
  functions call at  the execution of this file
  @param none 
  @return none
*)

let () = 
        open_graph " 810x810";
        set_window_title " sudoku ";
        draw_sudoku width height;
        stroke 810 810;
        draw_sudoku_value grilleReponse width height;
        read_key_and_draw();
        sound 10 10 ;
        loop ()
;;

(******************end of file*******************)














(*

let verifieLigne i matRep =
        (*given an array  of 9 zeros *)
        let hashArray = Array.make 9 0 in 
        (* consider every value in mapRep.(i) as our array's index and do +1 at that index 
         * the number at that index of our array correspond to the occurence of that index as a value
         * in the mapRep.(i) *)
        for j = 0 to 8 do
                let pos = (int_of_char (matRep.(i).(j)) -1)  in
                hashArray.( pos )  <-  hashArray.(pos) + 1
                (*error in this for-loop*)
        done;
        print_ "ok";
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

*)

