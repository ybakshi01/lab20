type img = float list list ;;
(* images are lists of lists of floats between 0. (white) and 1. (black) *)
type size = int * int ;;
open Graphics ;;
open List ;;
let mona = Monalisa.image ;;

(* threshold thershold image -- image where pixels above the threshold value are black *)
let threshold (img : img) (threshold : float) : img =
  map (fun row -> map (fun pixel -> if pixel <= threshold then 0. else 1.) row) img

(* dither max image -- dithered image *)
let dither (img : img) : img =
  map (fun row ->
      map (fun pixel -> if pixel > Random.float 1. then 1. else 0.) row) img

(* show the image *)
let depict (img : img) : unit =
  open_graph "";
  clear_graph ();
  let x, y = length (hd img), length img in
  resize_window x y;
  let depict_pix pixel points_done color =
    let lvl = int_of_float (255. *. (1. -. pixel)) in
    set_color (rgb lvl lvl lvl);
    plot color (y - points_done) in
  iteri (fun points_done row -> iteri (fun color pix -> depict_pix pix points_done color) row) img;
  Unix.sleep 2; close_graph () ;;

depict mona ;;
depict (threshold mona 0.75) ;;
depict (dither mona) ;;
