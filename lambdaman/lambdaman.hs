-- TODO avoid being eaten while eating when ghosts follow, maybe choose empty fields to gain distance
-- TODO maybe wait (back and forth) next to power pills till ghosts are near
-- TODO better tracking of distances (in steps) to do better ghost reach estimation for powers etc

----- MAIN INFRASTRUCTURE SECTION

-- main function
main world enemy = init world step;

-- init so far is just setting initial state
init world step =
  	let state = 1:0:0 in
  	(state:step);

-- the step function making direction decisions
step state world =
	let options = openfields state world in
	if canKill options world then
		goKill state options world
	else
		let ghostfreeoptions = noghosts world options in
		let branches = explore world ghostfreeoptions in	
		stepAndState state world ghostfreeoptions branches;


-- return state and step direction based on branches
stepAndState state world options branches =
	if atom options then
		getState state : 0	-- ghosts on all sides. no powerpills, no fright. we die.
	else let killDir = canKillGhost world options branches in
		-- we can probably kill a ghost
	if 99 > killDir then
		getState state : debug killDir 001

	else let powerpill = powerpill world options in
	if 99 > powerpill then getState state : powerpill
	else let pillnoghost = pillnoghost world options in
	if 99 > pillnoghost then getState state : pillnoghost

	else let fruitDir = canEatFruit world options branches in
		-- we can probably eat the fruit, that is: we reach it in time and faster than ghosts
	if 99 > fruitDir then
		getState state : debug fruitDir 002
	else let powerDir = canEatPowerPill world options branches in
		-- we can probably eat a power pill, that is: faster than ghosts
	if 99 > powerDir then
		getState state : debug powerDir 003
	else let eatSaveDir = canEatSave world options branches in
		-- save means no ghost and also no ghost towards me on other branches when i choose dead end
	if 99 > eatSaveDir then
		getState state : debug eatSaveDir 004
	else let walkSaveDir = canWalkSave world options branches in
		-- save means no ghost also no dead end
	if 99 > walkSaveDir then
		getModifiedState state options : debug walkSaveDir 005 -- this is considered lame, no eat, no kill
	else let eatUnsaveDir = canEatAtLeast world options branches in
		-- we will probably die, so eat at least some points
	if 99 > eatUnsaveDir then
		getState state : debug eatUnsaveDir 006
	else let dieHarderDir = liveLong world options branches in
		-- walk the way where the ghost is far away
	if 99 > dieHarderDir then
		getState state : debug dieHarderDir 007
	else dbug (getState state : 0) 408;	-- FAILURE CANNOT HAPPEN




----- CONFIGURATION SECTION

-- config: 
DEPTH = 2;		-- depth - max depth of recursive way checks, 0 is only until next junction
WRONG_TURNS = 2;	-- number of useless turns before we change default direction
INCREASE = 200;		-- number of steps after which we increase allowed wrong turns, longer loops are less likely
DEBUG = 0;		-- 0: no debug but FAILURE, 1: trace choices

debug return value =
	if DEBUG then dbug return value
	else return;

----- CONSTANTS SECTION

-- field types
WALL 		= 0;	-- #
EMPTY		= 1;	--  
PILL		= 2;	-- .
POWER		= 3;	-- o
FRUIT		= 4;	-- %
LAMBDA_START	= 5;	-- \
GHOST_START	= 6;	-- =

-- gives the movement speed for a given field		
speed field =
	if field == PILL then			137
	else if field == POWER then		137
	else if field == FRUIT then		137	-- we ignore whether the fruit is actually there for now
	else if field == EMPTY then		127
	else if field == LAMBDA_START then	127
	else if field == GHOST_START then	127
	else if field == WALL then		dbug 127 401	-- FAILURE CANNOT HAPPEN
	else					dbug 127 402;	-- FAILURE CANNOT HAPPEN

----- ENVIRONMENT AND TYPE ACCESS SECTION

-- world accessor functions
wMap    world = car world;
wLambda world = car (cdr world);
wGhosts world = car (cdr (cdr world));
wFruit  world = cdr (cdr (cdr world));

-- map at location
mapAt    map x y = item x (item y map);
mapAtLoc map loc = item (car loc) (item (cdr loc) map);

-- lambdaMan state accessor functions
lmVitality	m = car m;
lmLocation	m = car (cdr m);
lmDirection	m = car (cdr (cdr m));
lmLives		m = car (cdr (cdr (cdr m)));
lmScore 	m = cdr (cdr (cdr (cdr m)));

-- type Ghosts = [(GVitality, Location, Direction)]
gVitality	g = car g;
gLocation	g = car (cdr g);
gDirection	g = cdr (cdr g);

-- type State = (DefaultDirection, EventlessTurns, TotalSteps)
stDirection   	state = car state;
stTurns 	state = car (cdr state);
stTotalSteps    state = cdr (cdr state);

-- type branch info = (DeadEnd, PillDistance, FruitDistance, PowerDistance, GhostDistance, GhostDirection, Endpoint)
iDeadEnd	info = (car info);
iPill		info = (car (cdr info));
iFruitDistance	info = (car (cdr (cdr info)));
iPowerDistance	info = (car (cdr (cdr (cdr info))));
iGhostDistance	info = (car (cdr (cdr (cdr (cdr info)))));
iGhostDirection info = (car (cdr (cdr (cdr (cdr (cdr info))))));
iEndpoint	info = (cdr (cdr (cdr (cdr (cdr (cdr info))))));

-- setter for info params
setDeadEnd bool info = replace 0 bool info;
setPill value info = replace 1 value info;
setFruit ticks info = replace 2 ticks info;
setPower ticks info = replace 3 ticks info;
setEndpoint posbase info = iDeadEnd info : iPill info : iFruitDistance info : iPowerDistance info : iGhostDistance info : iGhostDirection info : posbase;
setGhost ticks direction info =  replace 4 ticks (replace 5 direction info);

-- type endpoint = (position, reachedfrom, ticks)
ePos	endpoint = car endpoint;
eFrom	endpoint = car (cdr endpoint);
eTicks	endpoint = cdr (cdr endpoint);


----- UTILITY SECTION
-- TODO sort stuff, functions for ghosts together etc

-- TODO get rid of this wrong thingy, make static access
-- replaces the element at index in a tuple
replace index value tuple =
	if index == 0 then
		if atom tuple then
			value
		else value : cdr tuple
	else
		(car tuple) : (replace (index - 1) value (cdr tuple));

-- checks whether a ghost moves towards me
ghostMovesToMe pos last dir =
	if samepos (posFromDir dir last) pos
	then 0		-- Ghost moves away
	else 1;		-- Ghost may move towards me	

-- get the n item of a list
item index list =
	if index == 0 then
		car list
	else
		item (index - 1) (cdr list);

-- number of elements in list
count list =
	if atom list then 0
	else 1 + count (cdr list);

-- get the minimum of two values
min first second =
	if first > second then
		second
	else
		first;

-- get the minimum of two values, but ignore zero
minnotzero first second =
	if first == 0 then
		second
	else if second == 0 then
		first
	else
		min first second;

-- get the maximum of two values
max first second =
	if first > second then
		first
	else
		second;

-- get a position from direction and location
posFromDir dir base =
	let x = car base;
	    y = cdr base in
  	if dir == 0 then
    		x : y - 1  	-- UP
  	else if dir == 1 then
    		x + 1 : y 	-- RIGHT
  	else if dir == 2 then
    		x : y + 1 	-- DOWN
  	else
    		x - 1 : y; 	-- LEFT

-- get direction to move from chosen option
getDirection world option =
	dirFromPos option (lmLocation (wLambda world));

-- get a direction from position and base location
dirFromPos pos base =
	let xbase = car base;
	    ybase = cdr base;
	    xpos = car pos;
	    ypos = cdr pos in
	if xpos > xbase then
		1		-- RIGHT
	else if xbase > xpos then
		3		-- LEFT
	else if ypos > ybase then
		2		-- DOWN
	else
		0;		-- UP

-- list of current adjacent positions
posAroundMe world =
	let loc = lmLocation (wLambda world) in
	posAround loc;

-- list of adjacent positions
posAround pos =
	let x = car pos;
	    y = cdr pos in
	[x:(y-1), (x+1):y, x:(y+1), (x-1):y];

-- get the positions for non-wall fields around loc
openfieldsat world loc =
	openhelper (wMap world) (posAround loc);

-- get the positions for non-wall fields around me
openfields state world =
	openhelper (wMap world) (sortedoptions state world (posAroundMe world));

openhelper map positions =
	if atom positions then
		positions
	else if mapAtLoc map (car positions) == WALL then
		openhelper map (cdr positions)
	else
		(car positions) : openhelper map (cdr positions);

-- sort options according to default direction -- TODO optimize statements, too tired now
sortedoptions state world options =
	let defdir = (modulo (stDirection state) 3);
	    currdir = lmDirection (wLambda world) in
	if defdir == 0 && currdir == 1 then order 0 1 2 3 options
	else if defdir == 0 && currdir == 2 then order 1 2 3 0 options
	else if defdir == 0 && currdir == 3 then order 2 3 0 1 options
	else if defdir == 0 && currdir == 0 then order 3 0 1 2 options
	else if defdir == 1 && currdir == 2 then order 2 1 3 0 options
	else if defdir == 1 && currdir == 3 then order 3 2 0 1 options
	else if defdir == 1 && currdir == 0 then order 0 3 1 2 options
	else if defdir == 1 && currdir == 1 then order 1 0 3 2 options
	else if currdir == 1 then order 2 1 0 3 options
	else if currdir == 2 then order 3 2 1 0 options
	else if currdir == 3 then order 0 3 2 1 options
	else order 1 0 3 2 options;

order a b c d list = 
	item a list : item b list : item c list : item d list : 0;

item index list =
	if index == 0 then car list
	else  item (index - 1) (cdr list);

-- is pos surrounded by at least 3 empty fields?
isjunction world pos =
	junctionhelper (wMap world) (posAround pos) 0;

junctionhelper map positions empty =
	if empty > 2 then
		1
	else if atom positions then
		0
	else if mapAtLoc map (car positions) == 0 then
		junctionhelper map (cdr positions) empty
	else
		junctionhelper map (cdr positions) (empty + 1);

-- get the  next position following a path from junction to junction
-- either a pos or 0:0 if there is a dead end
nextpos world pos last =
	let near = posAround pos in
	(poshelper (wMap world) (exclude last near));

poshelper map options =
	if atom options then
		0:0
	else if mapAtLoc map (car options) == WALL then
		poshelper map (cdr options)
	else
		car options;

-- removes one pos from a list of positions
exclude remove positions =
	if atom positions then
		positions
	else if samepos remove (car positions) then
		cdr positions
	else (car positions) : (exclude remove (cdr positions));

-- checks if two positions are the same
samepos first second =
	(car first) == (car second) && (cdr first) == (cdr second);

-- returns the modulo: number % base
modulo number base =
	number - (base * (number / base));

-- checks for a ghost, returns bool:direction
isGhost ghosts pos =
 	if atom ghosts then
    		0:0 -- false
  	else
		let ghost = car ghosts in
    		let x = car pos;
		    y = cdr pos;
		    gx = car (gLocation ghost);
        	    gy = cdr (gLocation ghost);
		    dir = gDirection ghost in
		if  (gx == x && gy == y) then
			(1:dir)
		else
		 	isGhost (cdr ghosts) pos;

--any ghosts around the field?
isDangerous world pos =
	car (isGhost (wGhosts world) pos) || dangeroushelper (wGhosts world) (posAround pos);

dangeroushelper ghosts positions =
	if atom positions then 0
	else if car (isGhost ghosts (car positions)) then 1
	else dangeroushelper ghosts (cdr positions); 

-- checks whether any ghost is coming to me
anyTowardsMe branches =
	if atom branches then
		0
	else if iGhostDistance (car branches) && 1 == iGhostDirection (car branches) then
		1
	else
		anyTowardsMe (cdr branches);

-- removes options with a ghost on starting point
noghosts world options =
	if atom options then
		options
	else if isDangerous world (car options) then
		noghosts world (cdr options)
	else (car options) : noghosts world (cdr options);

----- WORLD EXPLORATION SECTION

-- gets one or more branch info for each moving option
explore world options =
	if atom options then
		options
	else makebranch DEPTH world (lmLocation (wLambda world)) (car options) : explore world (cdr options);

-- gives info for the way along pos away from base and checks further if necessary
makebranch iterate world base pos =
	let inf = branchfinder world base pos in
	if iterate && nothingspecial inf then
		moreinfos (iterate - 1) world (iEndpoint inf)
	else
		(inf:0);

-- checks if there is anything worth noting in that info, returns true if all relevant fields are 0
nothingspecial info =
	0 ==	(iDeadEnd info)		+
		(iPill info) 		+
		(iFruitDistance info)	+
		(iPowerDistance info)	+
		(iGhostDistance info);

-- gives infos from branches starting at endpoint as more info
moreinfos iterate world endpoint =
	let options = exclude (eFrom endpoint) (openfieldsat world (ePos endpoint))  in
	addoffset (eTicks endpoint) (cdr (morehelper iterate world options (ePos endpoint)));

morehelper iterate world options base =
	if atom options then
		0
	else (morehelper iterate world (cdr options) base) : (makebranch iterate world base (car options));

addoffset ticks infos =
	if atom infos then infos
	else let info = car infos in
	(
	iDeadEnd info :
	addnonzero ticks (iPill info) :
	addnonzero ticks (iFruitDistance info) :
	addnonzero ticks (iPowerDistance info) :
	addnonzero ticks (iGhostDistance info) :
	iGhostDirection info :
	iEndpoint info)
	: addoffset ticks (cdr infos);

addnonzero ticks value =
	if value == 0 then 0
	else ticks + value;

-- get the info for a specific branch
branchfinder world base pos =
	branchhelper world base pos 1 (0:0:0:0:0:0:((0:0):(0:0):0));

branchhelper world base pos ticks info =
	let field = (mapAtLoc (wMap world) pos) in
	let newinfo = (setInfo field ticks info) in
	if samepos pos (0:0) then		-- dead end has been set by setInfo, its over here
		newinfo				
	else if (isjunction world pos) then	-- end point has to be set, we dont go on
		setEndpoint (pos:base:ticks) newinfo
	else let ghost = isGhost (wGhosts world) pos in
		if car ghost then		-- ghost, wheee! look no further
			setGhost ticks (ghostMovesToMe pos base (cdr ghost)) newinfo
		else
			branchhelper world pos (nextpos world pos base) (ticks + (speed field)) newinfo;

-- modifies the given info object based on field content
setInfo field ticks info =
	if field == PILL then
		setPill (minnotzero ticks (iPill info)) info
	else if field == POWER then
		setPower (minnotzero ticks (iPowerDistance info)) info
	else if field == FRUIT then		-- dont check for fruit status, will be done later
		setFruit (minnotzero ticks (iFruitDistance info)) info
	else if field == WALL then
		setDeadEnd 1 info
	else if field == EMPTY || field == LAMBDA_START || field == GHOST_START then
		info				-- nothing to do
	else
		dbug info 403;			-- FAILURE CANNOT HAPPEN

----- QUICK GHOST KILL SECTION

-- can we immediately kill a ghost?
canKill options world =
	if atom options then 0		-- false
	else if car (isGhost (wGhosts world) (car options)) then
		if lmVitality (wLambda world) > 1 || mapAtLoc (wMap world) (car options) == POWER then
			1		-- true
		else canKill (cdr options) world
	else canKill (cdr options) world;

-- get the kill direction TODO: de-duplicate code
killDirection options world =
	if atom options then dbug 0 417	-- FAILURE CANNOT HAPPEN
	else if car (isGhost (wGhosts world) (car options)) then
		if lmVitality (wLambda world) > 1 || mapAtLoc (wMap world) (car options) == POWER then
			getDirection world (car options)
		else killDirection (cdr options) world
	else killDirection (cdr options) world;

-- kill a ghost, return state and direction
goKill state options world =
	let state = ((stDirection state) : 0 : (1 + (stTotalSteps state)));
	    direction = killDirection options world in
	state:direction;

----- STATE MANAGEMENT SECTION

-- get the state for next step
getState state =
	((stDirection state) : 0 : (1 + (stTotalSteps state)));

-- gets the state with possibly modified standard direction
getModifiedState state options = 
	if (count options) > 2 then
		if stTurns state > ( WRONG_TURNS + (stTotalSteps state) / INCREASE ) then
			(modulo (1 + stDirection state) 4) : 0 : (1 + (stTotalSteps state))
		else
			((stDirection state) : (1 + stTurns state) : (1 + stTotalSteps state))
	else 
		((stDirection state) : (stTurns state) : (1 + stTotalSteps state));

----- HEURISTICS FOR MOVING SECTION

-- we can probably kill a ghost
canKillGhost world options branches =
	if atom branches then 99
	else let branchset = car branches;
		 fright = lmVitality (wLambda world) in
	if anykill fright branchset then
		getDirection world (car options)
	else canKillGhost world (cdr options) (cdr branches);

anykill fright set =
	if atom set then 0
	else let branch = car set in
	if (iGhostDistance branch && fright > 250) then		
			-- ghost going away probably slow enough	
			--((iGhostDirection branch == 0 && 7 * fright > 26 * iGhostDistance branch) ||
			-- ghost coming probably fast enough	
			--(iGhostDirection branch == 1 && 20 * fright / (iGhostDistance branch - fright) > 29))) then
		1
	else anykill fright (cdr set);

-- we can probably eat the fruit, that is we reach it in time and faster than ghosts
canEatFruit world options branches =
	if atom branches then 99
	else let branchset = car branches in
	if anyfruit world branchset then
		getDirection world (car options)
	else canEatFruit world (cdr options) (cdr branches);

anyfruit world set =
	if atom set then 0
	else let branch = car set in
	if wFruit world > iFruitDistance branch &&
			-- no ghost
			(iGhostDistance branch == 0 ||	
			-- ghost going away probably fast enough	
			(iGhostDirection branch == 0 && 14 * iGhostDistance branch > iFruitDistance branch) ||
			-- ghost coming and probably slow enough	
			(iGhostDirection branch == 1 && 25 * iGhostDistance branch > 53 * iFruitDistance branch)) then
		1
	else anyfruit world (cdr set);

-- we can probably eat a power pill, that is faster than ghosts		
canEatPowerPill world options branches =
	if atom branches then 99
	else let branchset = car branches in
	if anypower branchset then
		getDirection world (car options)
	else canEatPowerPill world (cdr options) (cdr branches);

anypower set =
	if atom set then 0
	else let branch = car set in
	if iPowerDistance branch &&
			-- no ghost
			(iGhostDistance branch == 0 ||	
			-- ghost going away probably fast enough	
			(iGhostDirection branch == 0 && 10 * iGhostDistance branch > iPowerDistance branch) ||
			-- ghost coming and probably slow enough	
			(iGhostDirection branch == 1 && 5 * iGhostDistance branch > 11 * iPowerDistance branch)) then
		1
	else anypower (cdr set);		
		
-- save means no ghost and also no ghost towards me on other branches when i choose dead end; take near food first
canEatSave world options branches =
	caneathelper world options branches 99 99999;

caneathelper world options branches dir near =
	if atom branches then dir
	else let branchset = car branches in
	let result = anyeat near branchset in
	if result then
			caneathelper world (cdr options) (cdr branches) (getDirection world (car options)) result
	else caneathelper world (cdr options) (cdr branches) dir near;

anyeat near set =
	if atom set then 0
	else let branch = car set in
	let incoming = anyTowardsMe set in
	if iPill branch && near > iPill branch &&
		(iDeadEnd branch == 0 || incoming == 0) &&
		(iGhostDistance branch == 0 || iGhostDirection branch == 0) then
		iPill branch
	else anyeat near (cdr set);

		
-- save means no ghost also no dead end
canWalkSave world options branches =
	if atom branches then 99
	else let branchset = car branches in
	if anysave branchset then
			getDirection world (car options)
	else canWalkSave world (cdr options) (cdr branches);

anysave branchset =
	if atom branchset then 0
	else let branch = car branchset in
	if 0 == iDeadEnd branch
		&& (0 == iGhostDistance branch || 0 == iGhostDirection branch) then
		1
	else anysave (cdr branchset);
		
-- we will probably die, so eat at least some points
canEatAtLeast world options branches =
	if atom branches then 99
	else let branchset = car branches in 
	if anyatleast branchset then
		getDirection world (car options)
	else canEatAtLeast world (cdr options) (cdr branches);

anyatleast set =
	if atom set then 0
	else let branch = car set in
	if iPill branch then 1
	else anyatleast (cdr set);
		
-- walk the way where the ghost is far away
liveLong world options branches =
	getDirection world (longHelper world options branches (car options) 0);

longHelper world options branches bestoption distance =
	if atom branches then bestoption
	else let branchset = car branches in
	let result = anylong distance branchset in
	if result then
		longHelper world (cdr options) (cdr branches) (car options) result
	else
		 longHelper world (cdr options) (cdr branches) bestoption distance;

anylong distance set =
	if atom set then 0
	else let branch = car set in
	if iGhostDistance branch > distance then iGhostDistance branch
	else anylong distance (cdr set);

powerpill world choices =
	if mapAtLoc (wMap world) (car choices) == POWER
		then getDirection world (car choices)
		else if (atom (cdr choices))
			then 99
			else powerpill world (cdr choices);

pillnoghost world choices =
	if mapAtLoc (wMap world) (car choices) == PILL && car (isGhost (wGhosts world) (car choices)) == 0
		then getDirection world (car choices)
		else if (atom (cdr choices))
			then 99
			else pillnoghost world (cdr choices);

