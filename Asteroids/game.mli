
type direction_hor = 
    | Left
    | Right

type direction_vert = 
    | Forward
    | Back

type collision ={
    bullet: Bullet.bullet;
    asteroid: Asteroid.asteroid;
}

type action = 
    | Side_engines of direction_hor
    | Main_engine of direction_vert
    | Shoot
    | CheckCollisions
    | AdvanceAsteroids 
    | AdvanceBullets
    | AsteroidSpawn

type game = {
    mutable over: bool;
    mutable score: int;
    mutable lifes: int;
    mutable asteroids: Asteroid.asteroid list;
    mutable spaceship: Spaceship.spaceship;
    mutable bullets: Bullet.bullet list;
    mutable time_of_life: float;
    mutable immune: bool;
    mutable multiplier:int;
}


val controller: game ->action -> game
val render: game -> unit
val init: unit -> game
