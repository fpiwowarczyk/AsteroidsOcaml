open Utils;;

type size = 
    | Big 
    | Medium 
    | Small 

type asteroid = {
    mutable x: float;
    mutable y: float; 
    angle:float;
    dir_x: bool;
    dir_y: bool;
    size: size;
}

let pi = 3.14;;

let collisionBounds asteroid =(
    match asteroid.size with 
    | Big ->
    asteroid.x -. 170.,
    asteroid.y -. 80.,
    asteroid.x +. 170.,
    asteroid.y +. 80.
    | Medium -> 
    asteroid.x -. 65.,
    asteroid.y -. 40.,
    asteroid.x +. 65.,
    asteroid.y +. 40.
    | Small ->
    asteroid.x -. 4.,
    asteroid.y -. 7.,
    asteroid.x +. 17.,
    asteroid.y +. 4.
    
)

let isInBoundsAsteroid ~bounds = 
    let check = Utils.isOutOfBounds ~bounds in 
    fun asteroid -> not @@ check(asteroid.x, asteroid.y)




let advance asteroid = 
    let change_x = match asteroid.dir_x with
        | true -> (+.)
        | false -> (-.)
        in 
        asteroid.x <- change_x asteroid.x 1.;
    let change_y = match asteroid.dir_y with 
        | true -> (+.)
        | false -> (-.)
        in 
        asteroid.y <- change_y asteroid.y 1.;
    asteroid 

let outOfBoundsAsteroid asteroid =
    if asteroid.x >1300. then asteroid.x <- -100. else asteroid.x <- asteroid.x;
    if asteroid.x < -100. then asteroid.x <- 1300. else asteroid.x <- asteroid.x;
    if asteroid.y >900. then asteroid.y  <- -100. else asteroid.y <- asteroid.y;
    if asteroid.y < -100. then asteroid.y  <- 900. else asteroid.y <- asteroid.y;
    asteroid


let renderBig () =
    GlDraw.color(1.,1.,1.);
    GlDraw.begins `lines;
    List.iter GlDraw.vertex2 [-40.,-70.; -10.,60.;
                             -10.,60.;80.,0.;
                             80.,0.;130.,70.;
                             130.,70.;140.,0.;
                             140.,0.;170.,-20.;
                             170.,-20.;160.,-80.;
                             160.,-80.;0.,-80.;
                             0.,-80.; 20.,-40.;
                             20.,-40.;-40.,-70.];
    GlDraw.ends()

let renderMedium () = 
    GlDraw.color(1.,1.,1.);
    GlDraw.begins `lines;
    List.iter GlDraw.vertex2 [-20.,-35.; -5.,30.;
                             -5.,30.;40.,0.;
                             40.,0.;65.,35.;
                             65.,35.;70.,0.;
                             70.,0.;85.,-20.;
                             85.,-20.;80.,-40.;
                             80.,-40.;0.,-40.;
                             0.,-40.;-20.,-35.];
    GlDraw.ends()

let renderSmall () = 
    GlDraw.color(1.,1.,1.);
    GlDraw.begins `lines;
    List.iter GlDraw.vertex2 [-4.,-7.; -1.,3.;
                             -1.,3.;8.,0.;
                             8.,0.;13.,7.;
                             13.,7.;14.,0.;
                             14.,0.;17.,-4.;
                             17.,-4.;16.,-8.;
                             16.,-8.;0.,-8.;
                             0.,-8.;-4.,-7.];
    GlDraw.ends()

let render asteroid = 
    GlMat.load_identity();
    GlMat.translate3(asteroid.x , asteroid.y, 0.0);
    GlMat.rotate ~angle: asteroid.angle ~z:1.();
    match asteroid.size with 
        | Big -> renderBig ()
        | Medium -> renderMedium()
        | Small -> renderSmall()
