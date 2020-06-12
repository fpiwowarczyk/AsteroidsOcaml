
type shooter = 
    | Spaceship

type bullet = {
    mutable x: float;
    mutable y: float;
    mutable angle: float;
    shooter: shooter;
}


val isInBoundsBullet: bounds:float * float * float * float -> bullet -> bool
val advance: bullet -> bullet
val renderSpaceshipBullet: unit -> unit
val render : bullet ->  unit