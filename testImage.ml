open Graphics
open Images

let () = Graphics.open_graph "";;

let image = lire_image "clamerde.png";;

dessiner_image image 20 20;;


Unix.sleep 5;;
