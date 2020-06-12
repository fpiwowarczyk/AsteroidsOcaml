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
val collisionBounds: asteroid ->float*float*float*float
val isInBoundsAsteroid: bounds:float*float*float*float -> asteroid -> bool 
val advance: asteroid -> asteroid
val outOfBoundsAsteroid: asteroid -> asteroid
val renderBig: unit -> unit
val renderMedium: unit -> unit
val renderSmall: unit -> unit
val render: asteroid -> unit