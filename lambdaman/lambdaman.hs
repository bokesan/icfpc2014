main world undefined = init world step;

-- world accessor functions
wMap    world = car world;
wLambda world = car (cdr world);
wGhosts world = car (cdr (cdr world));
wFruit  world = cdr (cdr (cdr world));

-- lambdaMan state accessor functions
lmVitality m  = car m;
lmLocation m  = car (cdr m);
lmDirection m = car (cdr (cdr m));
lmLives m     = car (cdr (cdr (cdr m)));
lmScore m     = cdr (cdr (cdr (cdr m)));

-- type Ghosts = [(GVitality, Location, Direction)]
gVitality g = car g;
gLocation g = car (cdr g);
gDirection g = cdr (cdr g);



-- move into direction from location
move front x y =
  if front == 0 then
    x : y - 1  -- UP
  else if front == 1 then
    x + 1 : y -- RIGHT
  else if front == 2 then
    x : y + 1 -- DOWN
  else
    x - 1 : y; -- LEFT

-- map at location
mapAt    map x y = item x (item y map);
mapAtLoc map loc = item (car loc) (item (cdr loc) map);

-- is a junction?
isjunction world pos =
	junctionhelper (fieldsat world pos) 0;

junctionhelper fields empty =
	if empty > 2
		then 1
		else if atom fields
			then 0
			else if (car (car fields)) == 0
				then junctionhelper (cdr fields) empty
				else junctionhelper (cdr fields) (empty + 1);

nextpos world pos last =
	let near = nearfields world pos last in
	poshelper (wMap world) near pos;

poshelper map near pos =
	if atom near
		then move near (car pos) (cdr pos)
		else let loc = move (car near) (car pos) (cdr pos)
			in if (mapAtLoc map loc) == 0
				then poshelper map (cdr near) pos
				else loc;

-- type State = ( [Int] , (Direction, Int) )
stGhostspeeds state = car state;
stLastMoves   state = cdr state;
stLastMovesDirection state = car (cdr state);
stLastMovesCount     state = cdr (cdr state);

init world step =
	let ghosts = wGhosts world in
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

speedtuple = [130, 132, 134, 136];
slowtuple  = [195, 198, 201, 204];

modulo number base =
	number - (base * (number / base));
		
step state world =
	let new = state
	in (directionhelp new world);

directionhelp state world =
	let ghosts = (wGhosts world)
	in direction world state ghosts (fields world) (lmDirection (wLambda world)) (lmVitality (wLambda world)) (wFruit world);

-- list of adjacent locations
fields world =
	let x = car (lmLocation (wLambda world));
	    y = cdr (lmLocation (wLambda world))
	in fieldsat world (x:y);

fieldsat world pos =
	let x = car pos;
	    y = cdr pos;
	    map = wMap world;
	    ghosts = wGhosts world
	in [field ghosts map x (y - 1) 0,
            field ghosts map (x + 1) y 1,
            field ghosts map x (y + 1) 2,
            field ghosts map (x - 1) y 3];

nearfields world pos last =
	let x = car pos;
	    y = cdr pos;
	    prevx = car last;
	    prevy = cdr last
	in if x > prevx
		then 0:1:2
		else if prevx > x
			then 2:3:0
			else if y > prevy
				then 1:2:3
				else 3:0:1;

field ghosts map x y dir =
	mapAt map x y : isGhost map ghosts x y : dir;

-- ghost on location OR can step on location ?
isGhost map ghosts x y =
  if atom ghosts then
    0 -- false
  else
    let gx = car (gLocation (car ghosts));
        gy = cdr (gLocation (car ghosts))
    in
       (gx == x && gy == y)
       || nextto x y gx gy
       || isGhost map (cdr ghosts) x y;

-- TODO de-duplicate code
-- boolean:richtung
isGhostInField map ghosts x y =
	if atom ghosts then
    		0:0 -- false
  	else
    		let gx = car (gLocation (car ghosts));
        	gy = cdr (gLocation (car ghosts));
        	gd = gDirection (car ghosts)
    		in if (gx == x && gy == y)
			then 1:gd
       		 	else isGhostInField map (cdr ghosts) x y;

-- adjacent cells?
nextto x y xx yy =
  (x == xx && (yy == y + 1 || yy == y - 1)) ||
  (y == yy && (xx == x + 1 || xx == x - 1));

direction world state ghosts nextfields front fright fruit =
	let choices = trans nextfields front state in
	let results = (	  (fruitnear choices fruit)
			: (eatghosts choices fright)
			: (eatdistantghosts world choices fright)
			: (powerpill choices)
			: (pillnoghost choices) 	--works
			: (eatdistant world choices)	--works
			: (noghost world choices)	--works
			: (noimmediateghost choices)	--works
			: (pillandghost choices)	--works
			: (eatenalive choices))		--works
	in choose results state;

-- select 4 items from list by index
order a b c d xs = [item a xs, item b xs, item c xs, item d xs];

trans nextfields front state =
	let first = stLastMovesDirection state
	in if first == 0
		then if front == 0
			then order 3 0 1 2 nextfields
			else if front == 1
				then nextfields
				else if front == 2
					then order 1 2 3 0 nextfields
					else order 2 3 0 1 nextfields
		else if first == 1
			then if front == 0
				then order 0 3 1 2 nextfields
				else if front == 1
					then order 1 0 2 3 nextfields
					else if front == 2
						then order 2 1 3 0 nextfields
						else order 3 2 0 1 nextfields
			else if front == 0
				then order 1 3 0 2 nextfields
				else if front == 1
					then order 2 0 1 3 nextfields
					else if front == 2
						then order 3 1 2 0 nextfields
						else order 0 2 3 1 nextfields;

choose results state =
	let dir   = stLastMovesDirection state;
	    count = stLastMovesCount state;
            res = helpchoose results 0
	in if count > 6
		then ((car state):((modulo (dir + 1) 3):0)):(car res)
		else if (cdr res) > 5
			then ((car state):(dir:(count + 1))):(car res)
			else ((car state):(dir:count)):(car res);

helpchoose results deep =
	if (item 0 results) == 99
		then helpchoose (cdr results) (deep + 1)
		else (item 0 results):deep;

-- return bool:distance
-- TODO auch weglaufende geister essen
distantghost world choice =
	let result = distant world choice 0
	in if (car result) == 3
		then 1:(cdr result)
		else 0:0;

distant world choice iterate =
	let loc = lmLocation (wLambda world)
	in distancehelper iterate world (move (cdr (cdr choice)) (car loc) (cdr loc)) loc 1;
-- 0:nichts, 1:essbar, 2:geist geht, 3:geist kommt evtl, 4:wand

--iterate erstmal nur für essbares
distancehelper iterate world pos last length =
	let map = wMap world in
	let field = mapAtLoc map pos
	in if field == 0
		then 4:length
		else let ghost = isGhostInField map (wGhosts world) (car pos) (cdr pos)
			in if (car ghost) == 1
				then (ghostMoves pos last (cdr ghost)):length
				else if field == 2 || field == 3 || (field == 4 && (wFruit world)) 
					then 1:length
					else if isjunction world pos
						then if iterate
							then distantfood world pos last
							else 0:length
						else distancehelper iterate world (nextpos world pos last) pos (length + 1);

distantfood world pos last =
	let near = nearfields world pos last in
	let next1 = move (car near) (car pos) (cdr pos);
	    next2 = move (car (cdr near)) (car pos) (cdr pos);
	    next3 = move (cdr (cdr near)) (car pos) (cdr pos)
	in if 1 == car (distancehelper 0 world next1 pos 0)
	|| 1 == car (distancehelper 0 world next2 pos 0)
	|| 1 == car (distancehelper 0 world next3 pos 0)
		then 1:0
		else 0:0;

ghostMoves pos last front =
	if equalpair (move front (car last) (cdr last)) pos
		then 2
		else 3;

equalpair first second =
	(car first) == (car second) && (cdr first) == (cdr second);
	
caneat fright distance =
	(2 * (fright / 137)) > distance;

--TODO nicht opfern für frucht
fruitnear choices fruit =
	if ((item 0 (car choices)) == 4) && fruit
		then (item 2 (car choices))
		else if (atom (cdr choices))
			then 99
			else fruitnear (cdr choices) fruit;

eatghosts choices fright =
	if fright == 0
		then 99
		else if (item 1 (car choices)) == 1
			then (item 2 (car choices))
			else if (atom (cdr choices))
				then 99
				else eatghosts (cdr choices) fright;

eatdistantghosts world choices fright =
	if fright == 0
		then 99
		else if isjunction world (lmLocation (wLambda world))
 			then let ghost = distantghost world (car choices)
				in if (car ghost) && (caneat fright (cdr ghost))
					then (item 2 (car choices))
					else if (atom (cdr choices))
						then 99
						else eatdistantghosts world (cdr choices) fright
			else 99;

powerpill choices =
	if (item 0 (car choices)) == 3
		then (item 2 (car choices))
		else if (atom (cdr choices))
			then 99
			else powerpill (cdr choices);

pillnoghost choices =
	if ((item 0 (car choices)) == 2) && ((item 1 (car choices)) == 0)
		then (item 2 (car choices))
		else if (atom (cdr choices))
			then 99
			else pillnoghost (cdr choices);

eatdistant world choices =
	if isjunction world (lmLocation (wLambda world))
		then if (item 1 (car choices)) == 1
			then if (atom (cdr choices))
				then 99
				else eatdistant world (cdr choices)
			else if (car (distant world (car choices) 1)) == 1
				then (item 2 (car choices))
				else if (atom (cdr choices))
					then 99
					else eatdistant world (cdr choices)
		else 99;

noghost world choices  =
	if isjunction world (lmLocation (wLambda world))
		then let dist = (distant world (car choices) 0)
			 in if (car dist == 0 && (item 1 (car choices)) == 0) || (car dist == 2 && cdr dist > 2)
				then (item 2 (car choices))
				else if (atom (cdr choices))
					then 99
					else noghost world (cdr choices)
		else 99;

noimmediateghost choices =	
	if (((item 1 (car choices)) == 0) && (((item 0 (car choices)) == 0) == 0))
		then (item 2 (car choices))
		else if (atom (cdr choices))
			then 99
			else noimmediateghost (cdr choices);

pillandghost choices =
	if (item 0 (car choices)) == 2
		then (item 2 (car choices))
		else if (atom (cdr choices))
			then 99
			else pillandghost (cdr choices);

eatenalive choices =
	if ((item 0 (car choices)) == 0) == 0
		then (item 2 (car choices))
		else eatenalive (cdr choices);


