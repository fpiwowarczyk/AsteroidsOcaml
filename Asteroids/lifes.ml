open Utils;;

let render lifes = 
    GlDraw.color (0.5, 1., 1.);
    Utils.drawString 1100. 750. (Printf.sprintf "Lifes: %d" lifes );