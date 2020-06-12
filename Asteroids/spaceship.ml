type spaceship = {
    mutable x: float;
    mutable y: float;
    mutable angle: float;
}

let collisionBounds asteroid = (
    asteroid.x -. 10.,
    asteroid.y -. 10.,
    asteroid.x +. 10.,
    asteroid.y +. 30.
)

let renderAt ~x ~y angle= 
    GlMat.load_identity();
    GlMat.translate3(x,y,0.);
    GlDraw.color(1.,1.,1.);
    GlMat.rotate ~angle: angle ~z:1.();
    GlDraw.begins `lines;
    List.iter GlDraw.vertex2[0., 0.; -10., -10.;
                             -10., -10.;0., 30.;
                             0.,30.;10.,-10.;
                             10.,-10.;0.,0.];
    GlDraw.ends ()


let render spaceship= 
    renderAt ~x:spaceship.x ~y:spaceship.y spaceship.angle
