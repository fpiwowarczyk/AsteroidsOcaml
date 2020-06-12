open Utils;;


let render score = 
    GlDraw.color (0.5, 1., 1.);
    Utils.drawString 20. 750. (Printf.sprintf "Score: %d" score);;



let renderEnd score = 
    GlDraw.color (0.5, 1., 1.);
    Utils.drawString 400. 400. (Printf.sprintf "Game over! Your Score: %d" score);;