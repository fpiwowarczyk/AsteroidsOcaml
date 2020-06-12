open Utils;;
type shooter = 
    | Spaceship

type bullet = {
    mutable x: float;
    mutable y: float;
    mutable angle: float;
    shooter: shooter;
}
let pi = 3.14;;
let isInBoundsBullet ~bounds= 
    let check = Utils.isOutOfBounds ~bounds in
    fun bullet -> not @@ check(bullet.x, bullet.y)

let advance bullet =
    let _ = match bullet with
    | { shooter = Spaceship } -> bullet.y <- ((+.) (bullet.y) (8.*.cos((pi/.180.)*.bullet.angle))) in
    let _ = match bullet with 
    | { shooter = Spaceship } -> bullet.x <- ((+.) (bullet.x) (-8.*.sin((pi/.180.)*.bullet.angle))) in 
    bullet

let renderSpaceshipBullet() = 
    GlDraw.color(1.,1.,1.);
    GlDraw.begins `lines;
    List.iter GlDraw.vertex2 [-3., 30.; -3.,36.; 3., 36.; 3., 30.];  
    GlDraw.ends()

let render bullet = 
    GlMat.load_identity();
    GlMat.translate3((bullet.x) , (bullet.y),0.);
    GlMat.rotate ~angle: bullet.angle ~z:1.();
    match bullet.shooter with
    | Spaceship ->renderSpaceshipBullet()
