main world undefine = init world step;

init world step =
	let ghosts = (item 2 world) in
  	let state = (ghostmoves ghosts) : (0:0)
  	in (state:step);

item index tuple =
	if index == 0
		then (getelement tuple)
		else (item (index - 1) (cdr tuple));

getelement tuple =
	if (atom tuple)
		then tuple
		else (car tuple);

ghostmoves list =
	ghosthelper 0 0 list;

ghosthelper ghostcount result input =
	if (atom input)
		then result
		else (ghosthelper (ghostcount + 1) ((speed ghostcount) : result) (cdr input));

speed ghostcount = 
	item (modulo ghostcount 4) speedtuple;

speedtuple =
	(130:132:134:136:0);

slowtuple =
	(195:198:201:204:0);

modulo number base =
	number - (base * (number / base));
		
step state world =
	let new = state -- newstate state (item 2 world) (item 0 (item 1 world)) 0 tick
	in (directionhelp new world);

-- TODO berechnen, wann die geister den nächsten move machen
-- newstate state ghosts fright ghostnr tick =
--	if (atom ghosts)
--		then state
--		else cons (nextmove (car state)

directionhelp state world =
	let ghosts = (item 2 world)
	in direction state ghosts (fields world) (item 2 (item 1 world)) (item 0 (item 1 world)) (item 3 world);

fields world =
	let x = (car (item 1 (item 1 world)));
	    y = (cdr (item 1 (item 1 world)));
	    map = (item 0 world);
	    ghosts = (item 2 world)
	in ((field ghosts map x (y - 1) 0) : (field ghosts map (x + 1) y 1) : (field ghosts map x (y + 1) 2) : (field ghosts map (x - 1) y 3) : 0);

field ghosts map x y dir =
	(item x (item y map)) : (isghost map ghosts x y) : dir;

isghost map ghosts x y =
	if (atom ghosts)
		then 0
		else let ghostx = (car (item 1 (car ghosts)));
			 ghosty = (cdr (item 1 (car ghosts)))
		     in if (and (ghostx == x) (ghosty == y))
				then 1
				else if ghostface map (item 2 (car ghosts)) x y ghostx ghosty 
					then 1
					else isghost map (cdr ghosts) x y;

ghostface map front x y ghostx ghosty =
	let face = ghostnext front ghostx ghosty
	in if (item (car face) (item (cdr face) map)) == 0
		then nextto x y ghostx ghosty
		else and ((car face) == x) ((cdr face) == y);
			
nextto x y xx yy =
	if and (x == xx) ((y + 1) == yy)
		then 1
		else if and (x == xx) ((y - 1) == yy)
			then 1
			else if and (y == yy) ((x + 1) == xx)
				then 1
				else if and (y == yy) ((x - 1) == xx)
					then 1
					else 0;

ghostnext front x y =
	if front == 0
		then x:(y - 1)
		else if front == 1
			then (x + 1):y
			else if front == 2
				then x:(y + 1)
				else (x - 1):y;

and first second =
	if first
		then if second
			then 1
			else 0
		else 0;

direction state ghosts nextfields front fright fruit =
	let choices = trans nextfields front state in
	let results = (	  (fruitorlast choices fruit)
			: (eatghosts choices ghosts state fright)
			: (powerpill choices)
			: (pillnoghost choices ghosts state)
			: (noghost choices ghosts state)
			: (pillandghost choices ghosts state)
			: (eatenalive))
	in choose results state;

trans nextfields front state =
	let first = (car (cdr state))
	in if first == 0
		then if front == 0
			then ((item 3 nextfields) : (item 0 nextfields) : (item 1 nextfields) : (item 2 nextfields) : 0)
			else if front == 1
				then nextfields
				else if front == 2
					then ((item 1 nextfields) : (item 2 nextfields) : (item 3 nextfields) : (item 0 nextfields) : 0)
					else ((item 2 nextfields) : (item 3 nextfields) : (item 0 nextfields) : (item 1 nextfields) : 0)
		else if first == 1
			then if front == 0
				then ((item 0 nextfields) : (item 3 nextfields) : (item 1 nextfields) : (item 2 nextfields) : 0)
				else if front == 1
					then ((item 1 nextfields) : (item 0 nextfields) : (item 2 nextfields) : (item 3 nextfields) : 0)
					else if front == 2
						then ((item 2 nextfields) : (item 1 nextfields) : (item 3 nextfields) : (item 0 nextfields) : 0)
						else ((item 3 nextfields) : (item 2 nextfields) : (item 0 nextfields) : (item 1 nextfields) : 0)
			else if front == 0
				then ((item 1 nextfields) : (item 3 nextfields) : (item 0 nextfields) : (item 2 nextfields) : 0)
				else if front == 1
					then ((item 2 nextfields) : (item 0 nextfields) : (item 1 nextfields) : (item 3 nextfields) : 0)
					else if front == 2
						then ((item 3 nextfields) : (item 1 nextfields) : (item 2 nextfields) : (item 0 nextfields) : 0)
						else ((item 0 nextfields) : (item 2 nextfields) : (item 3 nextfields) : (item 1 nextfields) : 0);

-- TODO
choose results state =
	let dir = (car (cdr state));
	    count = (cdr (cdr state));
            res = helpchoose results 0
	in if count > 6
		then ((car state):((modulo (dir + 1) 3):0)):(car res)
		else if (cdr res) > 3
			then ((car state):(dir:(count + 1))):(car res)
			else ((car state):(dir:count)):(car res);

helpchoose results deep =
	if (car results) == 99
		then helpchoose (cdr results) (deep + 1)
		else (car results):deep;

-- TODO check for last pill
fruitorlast choices fruit =
	if (and ((item 0 (car choices)) == 4) fruit)
		then (item 2 (car choices))
		else if (atom (cdr choices))
			then 99
			else fruitorlast (cdr choices) fruit;

eatghosts choices ghosts state fright =
	if fright == 0
		then 99
		else if (item 1 (car choices)) == 1
			then (item 2 (car choices))
			else if (atom (cdr choices))
				then 99
				else eatghosts (cdr choices) ghosts state fright;

powerpill choices =
	if (item 0 (car choices)) == 3
		then (item 2 (car choices))
		else if (atom (cdr choices))
			then 99
			else powerpill (cdr choices);

pillnoghost choices ghosts state =
	if (and ((item 0 (car choices)) == 2) ((item 1 (car choices)) == 0))
		then (item 2 (car choices))
		else if (atom (cdr choices))
			then 99
			else pillnoghost (cdr choices) ghosts state;

noghost choices ghosts state =
	if (and ((item 1 (car choices)) == 0) (((item 0 (car choices)) == 0) == 0))
		then (item 2 (car choices))
		else if (atom (cdr choices))
			then 99
			else noghost (cdr choices) ghosts state;

pillandghost choices ghosts state =
	if (item 0 (car choices)) == 2
		then (item 2 (car choices))
		else if (atom (cdr choices))
			then 99
			else pillandghost (cdr choices) ghosts state;

eatenalive = 0;



