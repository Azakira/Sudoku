open Printf
open Graphics
open String
open Array
open Images



(************* file paths ******************)

(** il n y a plus besoin de spécifier les chemins c'est gérér par les fcts 
 * -get_sol_rep_paths
 * -get_save_path*)

(** return (pathGrid,pathSol):couple*) 
let get_sol_rep_paths lvl = 
        let dir = Sys.getcwd() in
        let stringLvl = string_of_int lvl in
        (dir^"/grids/grid"^stringLvl^".txt",dir^"/solutions/solution"^stringLvl^".txt" )
;;

(** return (pathSave,pathSol):couple*) 
let get_save_path lvl =
        let dir = Sys.getcwd() in
        let stringLvl = string_of_int lvl in
        (dir^"/saveSudoku"^stringLvl^".txt",dir^"/solutions/solution"^stringLvl^".txt" )
;;

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
  @return void
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

let string_to_file path s = 
    let file = open_out path in
    try 
        output_string file s;
        close_out file
with e->
    close_out_noerr file;
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
                 draw_rect (i*cellSize) (j*cellSize) (width/10)  (height/10); (*cuz cellSize/9=10*)
               done;
        done;
;;

(* help_and_commands*)
let help_and_commands xLCorner yLCorner = 
        let setXBack = Graphics.current_x() in
        let setYBack = Graphics.current_x() in
        let decr = yLCorner in 
        Graphics.open_graph " 1200x810";
        let decr = decr-200 in
        Graphics.moveto xLCorner decr;
        
        Graphics.draw_string "Sudoku is played on a grid of 9 X 9 spaces";
        let decr = decr-50 in
        Graphics.moveto xLCorner decr;
        let decr = decr-30 in
        Graphics.draw_string "Within the rows and columns are 9 “squares”";
        Graphics.moveto xLCorner decr;
        Graphics.draw_string "(made up of 3 x 3 spaces Each row, column and";
        let decr = decr-30 in
        Graphics.moveto xLCorner decr;
        Graphics.draw_string "square (9 spaces each) needs to be filled out ";
        let decr = decr-30 in
        Graphics.moveto xLCorner decr;
        Graphics.draw_string "witht he numbers 1-9, without repeating any numbers";
        let decr = decr-30 in
        Graphics.moveto xLCorner decr;
        Graphics.draw_string "within the row, column or square";
        let decr = decr-70 in
        Graphics.moveto xLCorner decr;
        Graphics.draw_string "Commands :";
        let decr = decr-50 in
        Graphics.moveto xLCorner decr;
        Graphics.draw_string " mouse click : select a cell";
        let decr = decr-30 in
        Graphics.moveto xLCorner decr;
        Graphics.draw_string "h key : fill the selected cell with the right valuesi";
        let decr = decr-30 in
        Graphics.moveto xLCorner decr;
        Graphics.draw_string "0 key : delete the number of the selected cell";
        Graphics.moveto (xLCorner-100) 10;
        Graphics.draw_string "show/hide : desc+commandes";
        Graphics.moveto setXBack setYBack
;; 
(*let draw_num num modif i j =
	if (modif = true) then
		dessiner_image (lire_image ("images/img"^num^".png")) i j
	else 
		dessiner_image (lire_image ("images/selectimg"^num^".png")) i j
;;*)
	


(*procedure  draw_sudoku_value 
  given a matrix of couples (char,bool), draw its characters to screen 
  !!! ocaml graph uses mathematical represenation of graphs: i or x is from left to light 
      and j from bottom to top  !!!
  @param sudoku_values : matrix of couples (char,bool)
  @return none*)

let draw_sudoku_value sudoku_values height width=
        for i=0 to (height/cellSize)-1 do
               for j =0 to (width/cellSize)-1 do
                 if ( (snd (sudoku_values.(j).(i))) = false) then 
                 	draw_num (Char.escaped (fst (sudoku_values.(j).(i)))) (false) (i*cellSize+8) ((((width/cellSize)-1)-j)*cellSize+8)
                 else if ( (fst (sudoku_values.(j).(i))) != '0') then
                   		draw_num (Char.escaped (fst (sudoku_values.(j).(i)))) (true) (i*cellSize+8) ((((width/cellSize)-1)-j)*cellSize+8);

              
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
        Graphics.set_line_width 1;
;;

(*function insertValueInMatrix 
  given a x, a y, a value: interger as a char  and a matrix, this fct put the value in the
  position (x,y) of the matrix a,d return the matrix
  @param x, y : integers for the position, and an Interger as char and a matrix of couples (integer as a char, bool)
  @return a matrix of couples (integer as a char, bool)
*)
let insertValueInMatrix mat valeur x y= 
      mat.(x).(y) <- (valeur,true);
      mat
;;


(*function removeValueOfMatrix 
  given x, y and a matrix of couples (integer as a char, bool)
  put the value at (x,y) of the matrix to '0'(zero as a char)
  @param x, y integers and a matrix of couples (integer as a char, bool)
  @return matrix of couples (integer as a char, bool)
*)
let removeValueOfMatrix x y mat = 
        mat.(x).(y) <- ('0',true);
        mat
;;

let matToList mat = 
    let rec rowToList i res = 
        if i<0 then res else rowToList (i-1)  (List.append res (to_list mat.(i))) in
    rowToList ((length mat)-1) []
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
        |h::t-> if (fst h) != fst (List.hd l2) then false else compare t (List.tl l2)
;;

(*function verifGrille
  given 2 matrix, this fct  transforms every matrix into a list, then call
  the function compare on those 2 lists if compare return true this fct 
  return "c bon" return "c pas bon" otherwise
  @param matRep, matSol : matrix of couples (integer as a char, bool)
  @return a string
*)
let verifGrille matRep matSol = 
        let listRep = matToList matRep in
        let listSol = matToList matSol in
        compare listRep listSol
;;

let matToString mat = 
    let resString = ref "" in
    for i = 0 to (length mat)-1 do
        for j = 0 to (length mat)-1 do
            resString := !resString ^ (String.make 1 ( fst mat.(i).(j))) ;
        done ;
    done;
    !resString
;;

let saveSudoku mat path = 
    let sudoku = matToString mat in
    string_to_file path sudoku;
;;

let loadSudoku path = 
        let file = file_to_string path in
        initGrille file
;;
    
(*function insert_value_into_sudoku
  this fct insert a value into the cell at SudokuPosition of coupleMat if 
  the cell is modifiable and return true, just return false otherwise
  @param  a coupleMat, a value : 1<=int<=9 and a sudokuPosition
  @return bool
*)
(*comment recuperer l'element retourner par une fonction ?*)
let insert_value_into_sudoku coupleMat value sudokuPos  =
  if (snd coupleMat.(sudokuPos.abs).(sudokuPos.ord)) then
    insertValueInMatrix coupleMat value sudokuPos.abs sudokuPos.ord 
  else
      coupleMat
;;
let remove_value_from_sudoku coupleMat sudokuPos = 
  if (snd coupleMat.(sudokuPos.abs).(sudokuPos.ord)) then
        removeValueOfMatrix sudokuPos.abs sudokuPos.ord coupleMat
  else
      coupleMat
;;

(*contruct a free position *)
let getListOfFreeSudoPos grilleReponse =
  let listOfFreeMatPos = ref [] in 
  for i = 0 to 8 do 
    for j = 0 to 8 do
      if(snd grilleReponse.(i).(j)) then
        if((fst grilleReponse.(i).(j))='0') then
          listOfFreeMatPos :=(i,j)::!listOfFreeMatPos ;
    done
  done; 
  !listOfFreeMatPos 
;;



let getFreeSudoPos grilleReponse listOfFreeMatPos = 
  let index = Random.int (List.length listOfFreeMatPos) in
  let rec freePos l cpt indCible value = 
      match l with 
      |[] -> (-1,-1)
      |head::tail -> if cpt = indCible then value 
                      else freePos tail (cpt+1) indCible head 
in freePos listOfFreeMatPos 0 index (List.hd listOfFreeMatPos)
;;
    

let insertAVallueOnHelp grilleSolution grilleReponse = 
(*choose one math posi and put a the correct number *)
    let freeMatPos = getListOfFreeSudoPos grilleReponse in
    let pos = getFreeSudoPos grilleReponse freeMatPos in
    let i = fst pos in
    let j = snd pos in 
    let value = fst grilleSolution.(i).(j) in
    if (i <> -1) then
        if (j<> -1 ) then
            insertValueInMatrix grilleReponse value i j
        else grilleReponse
     else
        grilleReponse
;;

(*weirdly giving a bool instead of a char as value doesn't trigger error*)
let insertAVallueOnhelp  grilleSolution grilleReponse sudokuPos =
    let value = fst (grilleSolution.(sudokuPos.abs).(sudokuPos.ord)) in
    if (snd grilleReponse.(sudokuPos.abs).(sudokuPos.ord)) then
        insertValueInMatrix grilleReponse value sudokuPos.abs sudokuPos.ord 
     else
        grilleReponse
;;
    
let checkAllCellsWritten mat = 
    let matAsList = matToList mat in
    let rec checkCell aMatList = 
        match aMatList with
        | [] -> true
        |head::tail -> if (fst head) ='0' then false 
                        else checkCell tail 
    in checkCell matAsList
;; 


let resize_graph widthStd tailleSupp = 
        if Graphics.size_x()>widthStd then
                Graphics.resize_window widthStd (Graphics.size_y())
        else
        Graphics.resize_window (widthStd+tailleSupp) (Graphics.size_y())
;;

(*function sudoku_position 
  given a position given a couple of (x,y) in bounds of our sudoku and
  a list of possible positions, it choose the right position and returns
  a centered coordinate of the sudoku cell
  @param a couple :(int,int)
  @return a couple :(int,int)
*)
let get_sudoku_graph_position pos_couple positionList = 
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

(*function sudoku_position 
  given a position given a couple of (x,y) in bounds of our sudoku and
  a list of possible positions, it choose the right position and returns
  a cordonne of a couple's matrix
  @param a couple :(int,int)
  @return a couple :(int,int)
*)

let get_sudoku_matrice_position pos_couple positionList = 
  let (x,y) = pos_couple in
    let rec matchPos l sudok_pos = 
     match l with
     | [] -> sudok_pos
     | possiPos::restofPos -> if ((sudok_pos.abs) = (-1)) then 
                                  if (possiPos>y) then
                                    sudok_pos.abs <- (((width/cellSize)-1)-(possiPos-90)/90);
                              if ((sudok_pos.ord) = (-1)) then
                                  if(possiPos>x) then 
                                    sudok_pos.ord <- (( possiPos-90)/90);
                              matchPos restofPos sudok_pos
    in matchPos positionList {abs = -1; ord = -1}
;;

let draw_help_rect posX posY = 
      let setXBack = Graphics.current_x() in 
      let setYBack = Graphics.current_y() in 
      Graphics.fill_rect posX (810 -(810-posY)) 40 40;
      Graphics.moveto (posX+10) (posY+15);
      Graphics.set_color Graphics.white;
      Graphics.draw_string "help";
      Graphics.set_color Graphics.black;
      Graphics.moveto setXBack setYBack;
;;

(*procedure read_key_and_draw
  listen for numerical keys and draw them in sudoku cell
  under the mouse
  @param grilleReponse 
  @return none
*)
let read_key_and_draw grilleSolution grilleReponse= 
    let toBeModifiedMousePos = {abs= -1 ; ord = -1} in
    let grilleComplete = ref true in 
  while  !grilleComplete do
    draw_help_rect 770 0;
let e = Graphics.wait_next_event [Graphics.Key_pressed;Graphics.Button_down;Graphics.Mouse_motion] in
    if e.Graphics.button then begin
      Graphics.clear_graph ();
      if(fst (Graphics.mouse_pos()))>770 && (snd (Graphics.mouse_pos()))<40 then
              resize_graph width 400;
      help_and_commands 840 height;
      draw_sudoku width height;
      draw_sudoku_value grilleReponse width height;
      Graphics.set_color Graphics.red;
      toBeModifiedMousePos.abs <- e.mouse_x;
      toBeModifiedMousePos.ord <- e.mouse_y;
      let mousePos = (toBeModifiedMousePos.abs,toBeModifiedMousePos.ord) in
      let sudokuGraphPos = get_sudoku_graph_position mousePos positionList in
      Graphics.fill_rect (sudokuGraphPos.abs-(cellSize/2))  (sudokuGraphPos.ord-(cellSize/2)) (cellSize) (10);
      Graphics.set_color Graphics.black
    end;
    if e.Graphics.keypressed then
        let key = e.Graphics.key in
        let mousePos = (toBeModifiedMousePos.abs,toBeModifiedMousePos.ord) in
        let sudokuMatPos = get_sudoku_matrice_position mousePos positionList in
        let grilleReponse = (if key='h' then
                insertAVallueOnHelp grilleSolution grilleReponse
                (*insertAVallueOnhelp grilleSolution grilleReponse sudokuMatPos *)
             else
               grilleReponse) in
         Graphics.clear_graph ();
         help_and_commands 820 810;
         draw_sudoku width height;
         draw_sudoku_value grilleReponse width height;
        if( (int_of_char key)>=(int_of_char '0') && (int_of_char key)<=(int_of_char '9')) then begin
             let grilleReponse = (if (int_of_char key)=(int_of_char '0') then
                remove_value_from_sudoku   grilleReponse sudokuMatPos
             else
                insert_value_into_sudoku grilleReponse key sudokuMatPos) in
            Graphics.clear_graph ();
            help_and_commands 820 810;
            draw_sudoku width height;
            draw_sudoku_value grilleReponse width height;
        end;
    
    
    grilleComplete :=  not (checkAllCellsWritten grilleReponse);
    
  done;
    grilleReponse;
;;

(*function loop
  calls its self
  @param unit
  @return none
*)
let  rec loop () = 
        loop()
;;


let draw_victory grilleSolution grilleReponse = 
    Graphics.clear_graph();
    if ( verifGrille grilleSolution grilleReponse) then
        let i = ref 0 in
        while !i<500000 do            
            i := !i+1;
            Graphics.moveto 360 400;
            Graphics.draw_string " V I C T O I R E ";
        done;
    else
        let i = ref 0 in
        while !i<500000 do            
            i := !i+1;
            Graphics.moveto 360 400;
            Graphics.draw_string " D E F E A T  "
        done;
;;
(* import grid path and solution path
 * make matrix
 * draw sudoku
 * let play
 * announce victory/defeat*)
let new_match lvl height width=
        let paths = get_sol_rep_paths lvl in
        let file_stringR = file_to_string (fst paths) in
        let file_stringS = file_to_string (snd paths) in
        let grilleReponse = initGrille file_stringR in
        let grilleSolution = initGrille file_stringS in
        draw_sudoku_value grilleReponse height width;
        let grilleReponse = read_key_and_draw  grilleSolution grilleReponse in
        draw_victory grilleSolution grilleReponse

;;

(*import save path and solution path
 * make matrix
 * draw sudoku
 * let play
 * annonce victory/defeat*)
let load_match lvl height width = 
        let paths = get_save_path lvl in
        let file_stringR = file_to_string (fst paths) in
        let file_stringS = file_to_string (snd paths) in
        let grilleReponse = initGrille file_stringR in
        let grilleSolution = initGrille file_stringS in
        draw_sudoku_value grilleReponse height width;
        let grilleReponse = read_key_and_draw  grilleSolution grilleReponse in
        draw_victory grilleSolution grilleReponse
;;





(*
let grilleReponse = insertValueInMatrix 4 0 '8' grilleReponse;;
let grilleReponse = insertValueInMatrix 0 1 '3' grilleReponse;;
let grilleReponse = insertValueInMatrix 0 3 '5' grilleReponse;;
*)
(*
affiche_grille grilleReponse;;

verifGrille grilleSolution grilleSolution;;

*)

    
(*unit
  functions call at  the execution of this file
  @param none 
  @return none
*)

let () =
        help_and_commands 30 30;
        open_graph " 810x810";
        set_window_title " SUDOKU 9X9  click on help to show/hide commands ";
        draw_sudoku width height;
        stroke 810 810;
        new_match 0 810 810;
        sound 10 10 ;
        loop();
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

