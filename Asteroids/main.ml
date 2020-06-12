(* Lablgl *)


open Game;;

let specialKeyToAction ~key ~x ~y =
    match key with 
    | 27 -> exit 0;  (* ESCAPE*)
    | 119 -> Some(Game.Main_engine(Forward)) (*W*)
    | 97 ->  Some(Game.Side_engines(Left))  (*A*)
    | 115-> Some(Game.Main_engine(Back)) (*S*)
    | 100 -> Some(Game.Side_engines(Right)) (*D*)
    | 32 -> Some(Game.Shoot)   (* SPACE*)
    | _ -> None

let commonKeyToAction ~key ~x ~y = 
    match key with 
    | _->None 

let gameController game keyToAction = fun ~key ~x ~y ->
  match (keyToAction ~key ~x ~y) with
    | Some(action) -> game := Game.controller !game action
    | None -> ()


let rec asteroidTicker game = fun ~value -> 
    game := Game.controller !game Game.AdvanceAsteroids;
    Glut.timerFunc ~ms:10 ~cb:(asteroidTicker game) ~value:0;;


let rec collisionCheckTicker game = fun ~value -> 
    game := Game.controller !game Game.CheckCollisions;
    if not(!game.over)  then Glut.timerFunc ~ms:10 ~cb:(collisionCheckTicker game) ~value:0
    


let rec bulletTicker game = fun ~value ->
    game := Game.controller !game Game.AdvanceBullets;
    Glut.timerFunc ~ms:10 ~cb:(bulletTicker game) ~value:0

let rec asteroidSpawner game = fun ~value ->
    game := Game.controller !game Game.AsteroidSpawn;
    Glut.timerFunc ~ms:5000 ~cb:(asteroidSpawner game) ~value:0

let initDisplay ~w ~h ~title = 
    Glut.initDisplayMode ~double_buffer:true ();
    Glut.initWindowSize ~w ~h;
    Glut.createWindow ~title;
    Glut.idleFunc ~cb:(Some Glut.postRedisplay)

let initView ~w ~h = 
    GlDraw.viewport ~x:0 ~y:0 ~w ~h;
    GlMat.mode `projection;
    GlMat.load_identity();
    GluMat.ortho2d ~x:(0.0, float_of_int(w)) ~y:(0.0, float_of_int(h));
    GlMat.mode `modelview

let initTickers ~game =
    Glut.timerFunc ~ms:10 ~cb:(collisionCheckTicker game) ~value:0;
    Glut.timerFunc ~ms:10 ~cb:(asteroidTicker game) ~value:0;
    Glut.timerFunc ~ms:10 ~cb:(bulletTicker game) ~value:0 ;
    Glut.timerFunc ~ms:5000 ~cb:(asteroidSpawner game) ~value:0

let initInputs ~game =
    Glut.keyboardFunc ~cb:(gameController game specialKeyToAction);
    Glut.specialFunc ~cb:(gameController game commonKeyToAction)

let initEngine ~game ~w ~h = 
    initDisplay ~w ~h ~title:"Ocaml Asteroids";
    initView ~w ~h;
    initTickers ~game;
    initInputs ~game;
    Glut.displayFunc (fun () -> Game.render !game);
    Glut.mainLoop

let _ = 
    ignore (Glut.init Sys.argv);
    let game = ref (Game.init()) in
    let run = initEngine ~game ~w:1200~h:800 in 
        run()
