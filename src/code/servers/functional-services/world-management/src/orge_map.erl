% Copyright (C) 2003-2010 Olivier Boudeville
%
% This file is part of the Orge library.
%
% The Orge library is free software: you can redistribute it and/or modify
% it under the terms of either the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option) 
% any later version.
%
% The Orge library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License and of the GNU General Public License along with the Orge library.
% If not, see <http://www.gnu.org/licenses/>.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
-module(orge_map).

-export([ start/0 ]).


% Line rendering.
-export([ draw_line/3, draw_line/4, draw_lines/2, draw_lines/3,
		draw_cross/2, draw_cross/3 ]).


% Polygon management.
-export([ get_triangle/0, get_triangle/3, 
		  get_upright_square/0, get_upright_square/2, render_polygon/2 ]).


% Camera management.
-export([ set_camera/3, set_current_camera/2 ]).


% Bounding-box management.
% First algorithm to find the smallest enclosing circle of a polygon was based
% on compute_max_overall_distance/2 bud sadly was wrong (ex: failing with a 
% isosceles triangle.
-export([ compute_max_overall_distance/2 ]).

% A 2D point is simply a pair of integers, ex: P = {3,45}.

% Identifiers are generally atoms.


% Describes a 2D polygon, convex or not.
-record( polygon, {

		   % List of points.
		   vertices = [],

           % Color for an edge:
		   edge_color = black,

           % Color of the inside:
           fill_color = grey,

		   % Bounding-box information:

		   % Center of the bounding-box:
		   center,

		   % Radius of the bounding-box:
		   radius

		  } 

	   ).



% Describes a camera in the virtual world.
-record( camera, {

		   location = {0,0},
		   color = red,
		   name

		  }
	   ).



% Describes a virtual world.   
-record( world, {
		   
		   % Overall rectangular dimensions ({width,height}):
		   dimensions,

		   % World elements:
		   polygons = [],

		   % Known cameras ({camera_id,Camera} pairs):
		   cameras = [],

		   % Current camera (camera_id):
		   current_camera,

		   locations

		  } 
	   ).



% Describes the current state of a GUI.
-record( gui_state, {

		   world,

		   main_win,

		   canvas

		  }
	   ).



% Point section


% Returns the square of the distance between the two specified points.
% For comparison purposes, computing the square root is useless.
square_distance( {X1,Y1}, {X2,Y2} ) ->
	XDiff = X2-X1,
	YDiff = Y2-Y1,
	XDiff*XDiff + YDiff*YDiff.


% Returns the cross-product of the two specified 2D points, i.e. the magnitude
% of the vector that would result from a regular 3D cross product of the input
% vectors, taking their Z values implicitly as 0.
cross_product( {X1,Y1}, {X2,Y2} ) ->
	X1*Y2 - Y1*X1.


% Returns a vertex corresponding the middle of the two specified vertices.
get_center( {X1,Y1}, {X2,Y2} ) ->
	{ erlang:round( (X1+X2)/2 ), erlang:round( (Y1+Y2)/2 ) }.



% Line section.

% Draws a line between specified two vertices in specified canvas.
draw_line( V1, V2, Canvas ) ->
	gs:create( line, Canvas, [ {coords, [V1,V2]} ] ).	


% Draws a line between specified two vertices in specied canvas, with specified
% color.
draw_line( V1, V2, Color, Canvas ) ->
	gs:create( line, Canvas, [ {coords, [V1,V2]}, {fg,Color} ] ).



% Draws lines between specified list of vertices, in specified canvas.
draw_lines( Vertices, Canvas ) ->
	gs:create( line, Canvas, [ {coords, Vertices} ] ).	


% Draws lines between specified list of vertices in specied canvas, with specified
% color.
draw_lines( Vertices, Color, Canvas ) ->
	gs:create( line, Canvas, [ {coords, Vertices}, {fg,Color} ] ).



% Draws an upright cross at specified location, with default edge length.
draw_cross( Location, Canvas ) ->
	draw_cross( Location, _DefaultEdgeLength = 4, Canvas ).


% Draws an upright cross at specified location, with specified edge length.
draw_cross( _Location = {X,Y}, EdgeLength, Canvas ) ->
	Offset = EdgeLength div 2,
	% The last pixel of a line is not drawn, hence the +1:
	draw_line( {X-Offset,Y}, {X+Offset+1,Y}, Canvas ),    
	draw_line( {X,Y-Offset}, {X,Y+Offset+1}, Canvas ).	


% Draws an upright cross at specified location, with specified edge length
% and color.
draw_cross( _Location = {X,Y}, EdgeLength, Color, Canvas ) ->
	Offset = EdgeLength div 2,
	% The last pixel of a line is not drawn, hence the +1:
	draw_line( {X-Offset,Y}, {X+Offset+1,Y}, Color, Canvas ),    
	draw_line( {X,Y-Offset}, {X,Y+Offset+1}, Color, Canvas ).




% Bounding-box section.


% Renders specified bounding-box.
render_bounding_box( Center = {XCenter,YCenter}, Radius, Canvas ) ->
	TopLeft     = {XCenter-Radius, YCenter-Radius},
	BottomRight = {XCenter+Radius, YCenter+Radius},
	draw_cross( Center, _EdgeLength = 4, Canvas ),
	gs:create( oval, Canvas, [ {coords,[TopLeft,BottomRight]},
									   {fill,none}, {bw,1} ] ).	
		

% Polygon section.


% Returns an example triangle. 
get_triangle() ->
	get_triangle( {110,110}, {250,155}, {120,335} ).


% Returns a triangle corresponding to the specified three vertices.
get_triangle( V1, V2, V3 ) ->
	update_diameter( #polygon{ vertices = [V1,V2,V3] } ).


% Returns an example upright square.
get_upright_square() -> 
	get_upright_square( _Center = {250,250}, _EdgeLength = 50 ).


% Returns an upright square corresponding to the specified center and edge 
% length.
get_upright_square( _Center = {Xc,Yc}, EdgeLength ) ->
	Offset = erlang:round(EdgeLength / 2),
	X1 = Xc - Offset,
	X2 = Xc + Offset,
	Y1 = Yc - Offset,
	Y2 = Yc + Offset,
	update_diameter( #polygon{ vertices = 
							   [ {X1,Y1}, {X2,Y1}, {X2,Y2}, {X1,Y2} ] } ).



% Returns a polygon diameter, i.e. two points in the polygon which are at the
% maximum distance one of the other.
% Returns {V1,V2,  D = square_distance(V1,V2)} when V1 and V2 are the endpoints
% of a diameter and D is its square length. 
get_diameter( Polygon ) ->
	case Polygon#polygon.vertices of

		[] ->
			throw( no_vertex );

		[_Vertex] ->
			throw( single_vertex );

		ListWithAtLeastTwoVertices ->
			% There are at least two vertices:
			%compute_max_overall_distance( ListWithAtLeastTwoVertices, 
			%							  _Longest = undefined )
			compute_smallest_enclosing_rectangle( ListWithAtLeastTwoVertices, 
												  undefined, undefined )
	
	end.


% Returns {V1,V2,square_distance(V1,V2)} so that (square) distance is maximal.
% We ensure that each edge is examined only once: when the distance between 
% a given vertex V and all other vertices have been computed, V is removed from
% the list and a new maximum is searched within this subset.
% Here there is only one vertex left:
compute_max_overall_distance( [_H], Longest ) ->
	Longest;

% Here we have not compute a distance yet:
compute_max_overall_distance( [H|Others], undefined ) ->
	FirstEntry = compute_max_distance_between( H, Others, undefined ),
	compute_max_overall_distance( Others, FirstEntry );
 
% At least one other vertex remains, and at least one distance was computed:
compute_max_overall_distance( [H|Others], 
							  Best = {_V1,_V2,LongestSquareDistance} ) ->

	case compute_max_distance_between( H, Others, undefined ) of

		{PmaxForH,LongestSquareDistanceFromH} when LongestSquareDistanceFromH >
												   LongestSquareDistance ->
			compute_max_overall_distance( Others, 
			   {H,PmaxForH,LongestSquareDistanceFromH} );
										   
		% Here LongestSquareDistance is not beaten:
		_Other ->							 
			compute_max_overall_distance( Others, Best )
				
	end.



% Computes the maximum distance between a vertex (V1) and a list of other vertices.
% Returns {V1,Vmax,LongestSquareDistance} with LongestSquareDistance being
% the distance between V1 and Vmax, Vmax being chosen so that 
% LongestSquareDistance is maximal.
% As there must have been at least one vertex in the list, Vmax exists here
% (never undefined):
compute_max_distance_between( V1, [], {Vmax,LongestSquareDistance} ) ->
	{V1,Vmax,LongestSquareDistance};

compute_max_distance_between( V1, [V2|OtherVertices], undefined ) ->
	% First vertex examined is at first by construction the first best:
	compute_max_distance_between( V1, OtherVertices, 
								  {V2,square_distance(V1,V2)} );

compute_max_distance_between( V1, [V2|OtherVertices],
							  {Vmax,LongestSquareDistance} ) ->

	case square_distance(V1,V2) of

		SquareDistance when SquareDistance > LongestSquareDistance ->
			% We have a new winner:
			compute_max_distance_between( V1, OtherVertices, 
										  {V2,SquareDistance} );

		_LesserSquareDistance ->
			% Previous best not beaten, let's keep it:
			compute_max_distance_between( V1, OtherVertices, 
										  {Vmax,LongestSquareDistance} )
	end.


% Computes the smallest rectangle that encloses the specified list of points.
% Returns { TopLeft, BottomRight }.
compute_smallest_enclosing_rectangle( [], TopLeft, BottomRight ) ->
	{ TopLeft, BottomRight, square_distance(TopLeft,BottomRight) };

compute_smallest_enclosing_rectangle( [P|Others], undefined, undefined ) ->
	% First found initializes best:
	compute_smallest_enclosing_rectangle( Others, _TopLeft = P, _BottomRight = P );

compute_smallest_enclosing_rectangle( [{X,Y}|Others], {Xt,Yt}, {Xb,Yb} ) ->
	Xmin = erlang:min( X, Xt ),
	Ymin = erlang:min( Y, Yt ),
	Xmax = erlang:max( X, Xb ),
	Ymax = erlang:max( Y, Yb ),											
	compute_smallest_enclosing_rectangle( Others, {Xmin,Ymin}, {Xmax,Ymax} ).


% Updates the precomputed diameter of the specified polygon.
% Returns an updated polygon.
update_diameter( Polygon ) ->
	{V1,V2,D} = get_diameter( Polygon ),
	Polygon#polygon{ center = get_center( V1, V2 ), 
					 radius = erlang:round( math:sqrt(D)/2 ) }.



% Renders specified polygon in specified canvas.
% Throws an exception if the polygon is not valid.
render_polygon( Polygon, Canvas ) ->
	io:format( "Rendering polygon:~n~s.~n", [polygon_to_string(Polygon)] ),
	case Polygon#polygon.vertices of

		[] ->
			throw( null_polygon );

		[_Vertex] ->
			throw( one_vertex_polygon );

		Vertices ->
			gs:create( polygon, Canvas, [ {coords,Vertices},
										  {fg, Polygon#polygon.edge_color},
										  {fill, Polygon#polygon.fill_color} ] 
					 ),

			% Bounding-box is a circle:
			render_bounding_box( Polygon#polygon.center, 
								 Polygon#polygon.radius, Canvas )

%% 		Vertices = [First|_OtherVertices] ->
%% 			EdgeColor = Polygon#polygon.edge_color,
%% 			draw_lines( [First|lists:reverse(Vertices)], EdgeColor, Canvas )
%% 			% TO-DO: fill.

	end.




% Returns a textual description of the specified polygon.
polygon_to_string( Polygon ) ->
	io_lib:format(  "  + vertices: ~w~n", [Polygon#polygon.vertices] )
		++ io_lib:format( "  + edge color: ~w~n", [Polygon#polygon.edge_color] )
		++ io_lib:format( "  + fill color: ~w~n", [Polygon#polygon.fill_color] )
		++ io_lib:format( "  + BB center: ~w~n", [Polygon#polygon.center] )
		++ io_lib:format( "  + BB radius: ~w~n", [Polygon#polygon.radius] ).




% Convex Hull section.


% Finds the pivot, i.e. the leftmost point with the highest ordinate.
% The point list is supposed not having duplicates.
find_pivot( _PointList = [Pivot|Others] ) ->
	find_pivot( Others, Pivot ).


find_pivot( [], Pivot ) ->
	Pivot;

% Higher than the pivot, thus not wanted:
find_pivot( [{_X,Y}|Others], Pivot={_Xp,Yp} ) when Y<Yp ->
	find_pivot( Others, Pivot );

% Lower than the pivot, thus wanted:
find_pivot( [Point={_X,Y}|Others], _Pivot={_Xp,Yp} ) when Y>Yp ->
	find_pivot( Others, Point );

% Same level as the pivot, but at its right, thus not wanted:
find_pivot( [Point={X,Yp}|Others], Pivot={Xp,Yp} ) when X>Xp ->
	find_pivot( Others, Pivot );

% Same level as the pivot, but at its left, thus wanted:
find_pivot( [Point={X,Yp}|Others], Pivot={Xp,Yp} ) when X<Xp ->	
	find_pivot( Others, Point );

% Duplicated pivot, abnormal:
find_pivot( [Pivot|_Others], Pivot ) ->
	throw( {duplicate_pivot,Pivot} ).


sort_by_angle( Pivot, Points ) ->
	% Each element of that list is {Cotangent,P}, where Cotangent is the
	% cotangent of the angle the lines defined by the pivot and P makes with
	% the x axis.
	PointsWithAngles = [ {angle_value(P,Pivot),P} || P <- Points ],
	SortedPoints = lists:keysort(1,PointsWithAngles)


% Returns a vector V made from the specified two points: V=P2-P1.
vectorize( _P1={X1,Y1}, _P2={X2,Y2} ) ->
	{X2-X1,Y2-Y1}.


% Returns the square of the magnitude of the specified vector.
square_magnitude( _V={X,Y} ) ->
	X*X+Y*Y.


% Returns the magnitude of the specified vector.
magnitude( V ) ->
	math:sqrt( square_magnitude(V) ).


% Scales specified vector of specified factor.
scale( _V={X,Y}, Factor ) ->
	{Factor*X,Factor*Y}.


% Returns the specified vector with an unit length (magnitude of 1):
make_unit( {0,0} ) ->
	throw( cannot_make_null_vector_unit );

make_unit( V ) ->
	scale( V, 1/magnitude(V) ).








% Returns a value monotonically increasing with the angle the vector P1P2 
% makes with the abscissa axis, knowing it is in [0,Pi[.
angle_value( P1, P2 ) ->
	V = make_unit( vectorize( P1, P2 ) ),
	% Let a be the angle between [1,0] and V={X,Y}: tan(a)= Y/X

% Camera section.



% Sets the location of specified camera.
% Returns the updated camera.
set_camera_location( NewLocation, Camera ) ->
	Camera#camera{ location = NewLocation }.


% Renders specified camera in specified canvas.
render_camera( Camera, Canvas ) ->
	draw_cross( _Location = Camera#camera.location, _EdgeLength = 4,
				_Color = Camera#camera.color, Canvas ).



% Returns a default camera.
create_default_camera() ->
	#camera{ name = "Default" }.



% Returns the specified camera.
get_camera( GuiState, CameraId ) ->

	World = get_world(GuiState),

	case lists:keyfind( CameraId, _Pos=1, World#world.cameras ) of

		false -> 
			throw( {camera_not_found,CameraId} );

		{CameraId,Camera} ->
			Camera

	end.



% Returns the current camera, if any.
get_current_camera( GuiState ) ->

	World = get_world(GuiState),

	case World#world.current_camera of

		undefined ->
			throw( no_current_camera );

		CurrentCameraId ->
			get_camera(GuiState,CurrentCameraId)

	end.



% Sets specified camera identifier to specified camera state, possibly updating
% a previously existing camera pair.
% Returns an updated GUI state.
set_camera( CameraId, Camera, GuiState ) ->

	World = get_world(GuiState),

	NewCameras = lists:keyreplace( CameraId, _Pos=1,  World#world.cameras, 
								   {CameraId,Camera} ),
	
	GuiState#gui_state{ world = World#world{ cameras = NewCameras } }.



% Sets current camera, possibly updated the previously current camera.
% Returns an updated GUI state.
set_current_camera( Camera, GuiState ) ->

	World = get_world(GuiState),

	case  World#world.current_camera of

		undefined ->
			throw( no_current_camera );

		CurrentCameraId ->
			set_camera( CurrentCameraId, Camera, GuiState )

	end.



% World section.


% Returns the world recorded in the specified GUI state.
get_world( GuiState ) ->
	GuiState#gui_state.world.


% Creates an example world.
create_example_world() ->

	WorldWidth  = 800,
	WorldHeight = 600,

	WorldDimensions = {WorldWidth,WorldHeight},

	% Centers the default camera:
	DefaultCamera = set_camera_location( {WorldWidth div 2,WorldHeight div 2},
								  create_default_camera() ),

	RandomPoints = [ {random:uniform(300)+500,random:uniform(300)+200}
					 || _Count <- lists:seq(1,20) ],

	io:format( "Random points: ~w.~n", [RandomPoints] ),

	#world{ 
			dimensions = WorldDimensions,
			polygons = [ get_triangle(), get_upright_square() ],
			cameras  = [ {default_camera,DefaultCamera} ],
			current_camera = default_camera,
			locations = RandomPoints
		  }.



% Renders specified world in specified canvas.
render_world( World, Canvas ) ->
	[ render_polygon( P, Canvas ) || P <- World#world.polygons ],
	[ render_camera(  Cam, Canvas ) || {_CamId,Cam} <- World#world.cameras ],
	[ draw_cross( Loc, Canvas ) || Loc <- World#world.locations ],

	Pivot = find_pivot( World#world.locations ),

	io:format( "Pivot: ~w.~n", [Pivot] ),
	draw_cross( Pivot, 10, blue, Canvas ).



% Graphical User Interface management section.


	
get_main_window_width() ->
	1024.


get_main_window_height() ->
	768.


get_canvas_width() ->
	800.


get_canvas_height() ->
	600.




% Creates the initial GUI.
% Returns {MainWin,Canvas}:
create_initial_gui( GsId ) ->

	WindowSize = [ {width,get_main_window_width()}, 
				   {height,get_main_window_height()} ],

	MainWin = gs:window( GsId, WindowSize ++ [  
								{title,"Orge Map Viewer"},
								{bg,black} ]),

	CameraCellLength = 50,

	CameraPanelWidth  = 3*CameraCellLength,
	CameraPanelHeight = 4*CameraCellLength,


	% Main window split into four areas:
	%   - first row: empty then canvas
	%   - second row: camera panel then empty
	gs:frame( packer_win, MainWin, [ 
      {bw,1},
	  {packer_x,[ {fixed,CameraPanelWidth}, {stretch,10} ]},
	  {packer_y,[ {stretch,10},{fixed,CameraPanelHeight} ]}
							   ] ),	
	% In the main frame, canvas is at the top right:
	Canvas = gs:create( canvas, packer_win, [ 
		{pack_xy,{2,1}},
		{bg,white}, 
		%{hscroll,bottom}, {vscroll,left},
	    {width,get_canvas_width()},
		{height,get_canvas_height()}
		% Centers canvas:
	    %{x,(get_main_window_width()-get_canvas_width()) / 2},
		%{y,(get_main_window_height()-get_canvas_height()) / 2}
		%{scrollregion, {100,200,30,200}}
										 ] ),
	% In the main frame, the camera frame is at the bottom left:
	CellWidth = {fixed,CameraCellLength},
	CellHeight = CellWidth,
	gs:frame( packer_cam, packer_win, [ 
      {bw,1}, 
	  {pack_xy,{1,2}},
	  {packer_x,[CellWidth,CellWidth,CellWidth]},
	  {packer_y,[CellHeight,CellHeight,CellHeight,CellHeight]},
	  {width,CameraPanelWidth},
	  {height,CameraPanelHeight} ] ),

	gs:create( label, cam_label, packer_cam, [ {label,{text,"No camera set"}},
      {justify,center}, {bw,1}, {pack_xy,{{1,3},1}} ]),

	gs:create( button, cam_left_button, packer_cam, 
      [ {label,{text,"Left"}}, {pack_xy,{1,3}} ]),

	gs:create( button, cam_right_button, packer_cam, 
	  [ {label,{text,"Right"}}, {pack_xy,{3,3}} ]),

	gs:create( button, cam_center_button, packer_cam, 
      [ {label,{text,"Center"}}, {pack_xy,{2,3}} ]),

	gs:create( button, cam_up_button, packer_cam, 
      [ {label,{text,"Up"}}, {pack_xy,{2,2}} ]),

	gs:create( button, cam_down_button, packer_cam, 
      [ {label,{text,"Down"}}, {pack_xy,{2,4}} ]),
	
	gs:config( packer_cam, [ {width,CameraPanelWidth}, 
						 {height,CameraPanelHeight} ] ),

	gs:config( packer_win, WindowSize ),
	{MainWin,Canvas}.



update_camera_panel( GuiState ) ->
	Camera = get_current_camera( GuiState ),
	{X,Y} = Camera#camera.location,
	Text = io_lib:format( "~s camera\nAt [~B;~B]", [Camera#camera.name,X,Y] ),
	gs:config( cam_label, [ {label,{text,Text}} ] ).



% Starts the GUI.
start() ->

	GsId = gs:start(),

	{MainWin,Canvas} = create_initial_gui( GsId ),

	InitialWorld = create_example_world(),

	InitialGuiState = #gui_state{ 
	  world    = InitialWorld,
	  main_win = MainWin,
	  canvas   = Canvas
	 },

	render_world( InitialWorld, Canvas ),
	
	update_camera_panel( InitialGuiState ),

    % Sets the GUI to visible:
	gs:config( MainWin, {map,true} ),

	map_view_loop( InitialGuiState ),

	gs:stop().




% Main loop of the GUI.
map_view_loop( GuiState ) ->
	
	receive
        
		{gs,cam_left_button,click,_Data,_Arg} ->
			io:format( "Click left!~n" ),
			%CurrentCam = GuiState#gui_state.current_camera,
			map_view_loop( GuiState );

		{gs,_Pair,destroy,[],[]} ->
			io:format( "Quitting Orge Map!~n" ),
			erlang:halt();
		
		X ->
            io:format("Got '~w' (ignored).~n",[X]),
			map_view_loop( GuiState )
	
	end.



