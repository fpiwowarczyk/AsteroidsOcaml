# By skompilowa nalezy skazac sciezke do biblioteki lablgl po -I
 
 
all: main

asteroid.cmi:asteroid.mli 
	ocamlc -c asteroid.mli

asteroid.cmo:asteroid.ml asteroid.cmi utils.cmi utils.cmo
	ocamlopt -c -I /home/filip/.opam/default/lib/lablgl\
	 lablgl.cmxa lablglut.cmxa asteroid.ml	

bullet.cmi: bullet.mli
	ocamlc -c bullet.mli

bullet.cmo:bullet.ml bullet.cmi utils.cmi utils.cmo
	ocamlopt -c -I /home/filip/.opam/default/lib/lablgl\
	 lablgl.cmxa lablglut.cmxa bullet.ml	

spaceship.cmi: spaceship.mli
	ocamlc -c spaceship.mli

spaceship.cmo:spaceship.ml spaceship.cmi
	ocamlopt -c -I /home/filip/.opam/default/lib/lablgl\
	 lablgl.cmxa lablglut.cmxa spaceship.ml
lifes.cmi:lifes.mli
	ocamlc -c lifes.mli

lifes.cmo: lifes.ml lifes.cmi
	ocamlopt -c -I /home/filip/.opam/default/lib/lablgl\
	 lablgl.cmxa lablglut.cmxa lifes.ml

utils.cmi:utils.mli
	ocamlc -c -I /home/filip/.opam/default/lib/lablgl\
	 lablgl.cma lablglut.cma utils.mli

utils.cmo:utils.ml utils.cmi
	ocamlopt -c -I /home/filip/.opam/default/lib/lablgl\
	 lablgl.cmxa lablglut.cmxa utils.ml	

score.cmi:score.mli
	ocamlc -c score.mli 

score.cmo: score.ml score.cmi utils.cmi utils.cmo
	ocamlopt -c -I /home/filip/.opam/default/lib/lablgl\
	 lablgl.cmxa lablglut.cmxa score.ml

game.cmi: game.mli spaceship.cmi spaceship.cmo bullet.cmi bullet.cmo asteroid.cmo asteroid.cmi
	ocamlc -c  game.mli 

game.cmo: game.ml game.cmi score.cmi score.cmo lifes.cmi lifes.cmo bullet.cmi bullet.cmo asteroid.cmo asteroid.cmi
	ocamlopt -c -I /home/filip/.opam/default/lib/lablgl\
	 lablgl.cmxa lablglut.cmxa unix.cmxa game.ml

main: game.cmi game.cmo score.cmi score.cmo utils.cmi utils.cmo
	ocamlopt -I /home/filip/.opam/default/lib/lablgl \
	lablgl.cmxa lablglut.cmxa utils.cmx bullet.cmx asteroid.cmx \
	 spaceship.cmx lifes.cmx score.cmx unix.cmxa game.cmx main.ml -o main 


clean: 
	rm -r *.cmo *.cmi *.cmx *.o main 
