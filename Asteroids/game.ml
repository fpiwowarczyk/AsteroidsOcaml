open Score;;
open Lifes;;
open Utils;;
open Spaceship;;
open Bullet;;
open Asteroid;;


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

let pi = 3.14;;

let rec collectAsteroidsCollisions ~asteroids ~bullets = let open Bullet in 
    match asteroids with 
    |[] -> []
    | hd:: tl ->
        let checkHit = isInBoundsBullet ~bounds: (Asteroid.collisionBounds hd) in 
            match List.find_opt checkHit bullets with 
            | None -> collectAsteroidsCollisions ~asteroids: tl ~bullets:bullets
            | Some(bullet) -> {
                asteroid = hd;
                bullet;
            } :: collectAsteroidsCollisions ~asteroids: tl ~bullets: bullets

let collectAsteroidBulletCollisions ~game = let open Bullet in 
    let spaceshipBullets = List.filter (fun bullet -> bullet.shooter == Spaceship) game.bullets in 
    let collisions  = collectAsteroidsCollisions ~asteroids:game.asteroids ~bullets:spaceshipBullets in
    collisions 

let getSpaceshipCollisionAsteroid ~game = let open Asteroid in 
    let checkHit = isInBoundsAsteroid ~bounds:(Spaceship.collisionBounds game.spaceship) in 
    List.find_opt checkHit game.asteroids

    
let controller game = function 
    | Side_engines(direction_hor) ->
        let op = match direction_hor with 
        | Left -> (+.)
        | Right ->  (-.) in 
        let angles =op game.spaceship.angle 6. in 
         game.spaceship.angle <- angles; 
        game
    | Main_engine(direction_vert) ->
        let op = match direction_vert with
        | Forward -> (+.)
        | Back -> (-.) in
        let sp = match direction_vert with 
        | Forward -> 6.
        | Back -> 3. in 
        let coordx =op game.spaceship.x ((-1.)*.sp*.(sin ((pi/.180.)*.game.spaceship.angle))) in 
        let coordy =op game.spaceship.y (sp*.(cos ((pi/.180.)*.game.spaceship.angle))) in 
        game.spaceship.y <- coordy ;
        game.spaceship.x <- coordx ;
        game
    | Shoot ->
        game.bullets <- game.bullets @ [{
            x = game.spaceship.x;
            y = game.spaceship.y;
            angle = game.spaceship.angle;
            shooter = Spaceship
        }];
        game
    | CheckCollisions ->
        let lifes = 
        match getSpaceshipCollisionAsteroid ~game with
            | None -> game.lifes
            | _ -> 
                    game.spaceship.x <- 600.;
                    game.spaceship.y <- 400.;
                    game.lifes -1 in 
        let asteroidCollisions = collectAsteroidBulletCollisions ~game in
            game.asteroids <- List.filter (fun asteroid ->
                match (List.find_opt (fun col -> col.asteroid == asteroid) asteroidCollisions) with
                    | None -> true
                    | _ -> false
            ) game.asteroids;
            game.bullets <- List.filter (fun bullet ->
                match (List.find_opt (fun col -> col.bullet == bullet) asteroidCollisions) with
                    | None -> true
                    | _ -> false
            ) game.bullets;
            game.score <- game.score+1*game.multiplier;
            game.lifes <- lifes;
            game.over <- game.lifes <= 0 ;
        game
    | AdvanceAsteroids -> 
         List.map Asteroid.advance game.asteroids ;
            game.asteroids <- List.map Asteroid.outOfBoundsAsteroid  game.asteroids;
        game 
    | AdvanceBullets -> 
        let bullets = List.map Bullet.advance game.bullets in
            game.bullets <- List.filter (Bullet.isInBoundsBullet ~bounds: (5., 5., 1200., 800.)) bullets;
        game 
    | AsteroidSpawn ->
        game.multiplier <- game.multiplier + 1;
        game.asteroids <- game.asteroids @ [{x=Random.float 1200.; y=Random.float 800.;angle = Random.float 360.;dir_x = Random.bool();dir_y=Random.bool(); size = Small}];
	    game.asteroids <- game.asteroids @ [{x=Random.float 1200.; y=Random.float 800.;angle = Random.float 360.;dir_x = Random.bool();dir_y=Random.bool(); size = Small}];
	    game.asteroids <- game.asteroids @ [{x=Random.float 1200.; y=Random.float 800.;angle = Random.float 360.;dir_x = Random.bool();dir_y=Random.bool(); size = Small}];
        game.asteroids <- game.asteroids @ [{x=Random.float 1200.; y=Random.float 800.;angle = Random.float 360.;dir_x = Random.bool();dir_y=Random.bool(); size = Medium}];
        game.asteroids <- game.asteroids @ [{x=Random.float 1200.; y=Random.float 800.;angle = Random.float 360.;dir_x = Random.bool();dir_y=Random.bool(); size = Medium}];
        game.asteroids <- game.asteroids @ [{x=Random.float 1200.; y=Random.float 800.;angle = Random.float 360.;dir_x = Random.bool();dir_y=Random.bool(); size = Big}];
        game 

let renderGame game = 
    GlClear.clear [`color];
    Score.render game.score;
    Lifes.render game.lifes;
    List.iter Asteroid.render game.asteroids;
    List.iter Bullet.render game.bullets;
    Spaceship.render game.spaceship;
    Glut.swapBuffers()

let renderEnd game = 
    GlClear.clear [`color];
    Score.renderEnd game.score;
    Glut.swapBuffers()

let render game = 
    match game.over with 
    | false -> renderGame game
    | true -> renderEnd game (* Change*)

let asteroids = let open Asteroid in [
    {x=Random.float 1200.; y=Random.float 800.;angle = Random.float 360.;dir_x = Random.bool();dir_y=Random.bool(); size = Small};
    {x=Random.float 1200.; y=Random.float 800.;angle = Random.float 360.;dir_x = Random.bool();dir_y=Random.bool(); size = Small};
    {x=Random.float 1200.; y=Random.float 800.;angle = Random.float 360.;dir_x = Random.bool();dir_y=Random.bool(); size = Small};
    {x=Random.float 1200.; y=Random.float 800.;angle = Random.float 360.;dir_x = Random.bool();dir_y=Random.bool(); size = Small};
    {x=Random.float 1200.; y=Random.float 800.;angle = Random.float 360.;dir_x = Random.bool();dir_y=Random.bool(); size = Medium};
    {x=Random.float 1200.; y=Random.float 800.;angle = Random.float 360.;dir_x = Random.bool();dir_y=Random.bool(); size = Medium};
    {x=Random.float 1200.; y=Random.float 800.;angle = Random.float 360.;dir_x = Random.bool();dir_y=Random.bool(); size = Medium};
    {x=Random.float 1200.; y=Random.float 800.;angle = Random.float 360.;dir_x = Random.bool();dir_y=Random.bool(); size = Medium};
    {x=Random.float 1200.; y=Random.float 800.;angle = Random.float 360.;dir_x = Random.bool();dir_y=Random.bool(); size = Medium};
    {x=Random.float 1200.; y=Random.float 800.;angle = Random.float 360.;dir_x = Random.bool();dir_y=Random.bool(); size = Medium};
    {x=Random.float 1200.;y=Random.float 800.;angle = Random.float 360.;dir_x = Random.bool();dir_y=Random.bool(); size=Big};
    {x=Random.float 1200.;y=Random.float 800.;angle = Random.float 360.;dir_x = Random.bool();dir_y=Random.bool(); size=Big};
    {x=Random.float 1200.;y=Random.float 800.;angle = Random.float 360.;dir_x = Random.bool();dir_y=Random.bool(); size=Big};
    {x=Random.float 1200.;y=Random.float 800.;angle = Random.float 360.;dir_x = Random.bool();dir_y=Random.bool(); size=Big};
]

let spaceship = let  open Spaceship in {
    x=600.;
    y=400.;
    angle=0.;
}

let init () = {
    over = false;
    score = 0;
    lifes = 3;
    asteroids ;
    spaceship;
    time_of_life = Unix.time();
    bullets = [];
    immune = false;
    multiplier = 1
}
