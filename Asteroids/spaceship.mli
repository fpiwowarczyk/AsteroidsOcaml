type spaceship = {
    mutable x: float;
    mutable y: float;
    mutable angle: float;
}

val collisionBounds: spaceship -> float * float * float * float 
val renderAt: x:float-> y:float -> float -> unit
val render: spaceship -> unit