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




% A 2D point is simply a pair of integers, ex: P = {3,45}.


% Describes a 2D polygon, convex or not.
-record( polygon, {

		   % List of points.
		   vertices = [],

           % Color for an edge:
		   edge_color = black,

           % Color of the inside:
           fill_color = grey
		  } 

	   ).



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


% Polygon section.


% Returns an example triangle. 
get_triangle() ->
	get_triangle( {10,10}, {50,15}, {40,35} ).


% Returns a triangle corresponding to the specified three vertices.
get_triangle( V1, V2, V3 ) ->
	#polygon{ vertices = [V1,V2,V3] }.


% Returns an example upright square.
get_upright_square() -> 
	get_upright_square( _Center = {50,50}, _EdgeLength = 20 ).


% Returns an upright square corresponding to the specified center and edge length.
get_upright_square( _Center = {Xc,Yc}, EdgeLength ) ->
	Offset = EdgeLength / 2,
	X1 = Xc - Offset,
	X2 = Xc + Offset,
	Y1 = Yc - Offset,
	Y2 = Yc + Offset,
	#polygon{ vertices = [ {X1,Y1}, {X2,Y1}, {X2,Y2}, {X1,Y2} ] }.


% Renders specified polygon in specified canvas.
% Throws an exception if the polygon is not valid.
render_polygon( Polygon, Canvas ) ->
	case Polygon#polygon.vertices of

		[] ->
			throw( null_polygon );

		[_Vertex] ->
			throw( one_vertex_polygon );

		Vertices ->
			gs:create( polygon, Canvas, [ {coords,Vertices},
										  {fg, Polygon#polygon.edge_color},
										  {fill, Polygon#polygon.fill_color} ] )
%% 		Vertices = [First|_OtherVertices] ->
%% 			EdgeColor = Polygon#polygon.edge_color,
%% 			draw_lines( [First|lists:reverse(Vertices)], EdgeColor, Canvas )
%% 			% TO-DO: fill.

	end.



% Camera section.


% Describes a camera in the virtual world.
-record( camera, {

		   location = {0,0},
		   color = red,
		   name

		  }
	   ).


% Sets the location of specified camera.
% Returns the updated camera.
set_camera_location( NewLocation, Camera ) ->
	Camera#camera{ location = NewLocation }.


% Renders specified camera in specified canvas.
render_camera( Camera, Canvas ) ->
	draw_cross( _Location = Camera#camera.location, _EdgeLength = 4,
				_Color = Camera#camera.color, Canvas ).



% Returns a default camera.
get_default_camera() ->
	#camera{ name = "Default camera"}.





% World section.


% Describes a virtual world.   
-record( world, {
		   
		   % Overall rectangular dimensions ({width,height}):
		   dimensions,

		   % World elements:
		   polygons = [],

		   % Known cameras:
		   cameras = []

		  } 
	   ).


% Creates an example world.
create_example_world() ->

	WorldWidth  = 800,
	WorldHeight = 600,

	WorldDimensions = {WorldWidth,WorldHeight},

	% Centers the default camera:
	Camera = set_camera_location( {WorldWidth div 2,WorldHeight div 2},
								  get_default_camera() ),

	#world{ 
			dimensions = WorldDimensions,
			polygons = [ get_triangle(), get_upright_square() ],
			cameras  = [ Camera ] 
		  }.



% Renders specified world in specified canvas.
render_world( World, Canvas ) ->
	[ render_polygon( P, Canvas ) || P <- World#world.polygons ],
	[ render_camera(  C, Canvas ) || C <- World#world.cameras ].




% Graphical User Interface management section.


	
get_main_window_width() ->
	1024.


get_main_window_height() ->
	768.


get_canvas_width() ->
	800.


get_canvas_height() ->
	600.


% Describes the current state of a GUI.
-record( gui_state, {
		   world,
		   main_win,
		   canvas
		  }
	   ).


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

	gs:label( packer_cam, [ {label,{text,"Camera position"}},
						 {pack_xy,{{1,3},1}} ]),

	gs:button( packer_cam, [ {label,{text,"Left"}},   {pack_xy,{1,3}} ]),
	gs:button( packer_cam, [ {label,{text,"Right"}},  {pack_xy,{3,3}} ]),
	gs:button( packer_cam, [ {label,{text,"Center"}}, {pack_xy,{2,3}} ]),
	gs:button( packer_cam, [ {label,{text,"Up"}},     {pack_xy,{2,2}} ]),
	gs:button( packer_cam, [ {label,{text,"Down"}},   {pack_xy,{2,4}} ]),
	
	gs:config( packer_cam, [ {width,CameraPanelWidth}, 
						 {height,CameraPanelHeight} ] ),

	gs:config( packer_win, WindowSize ),
	{MainWin,Canvas}.


%display_camera_panel( MainWin ) ->
	

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

    % Sets the GUI to visible:
	gs:config( MainWin, {map,true} ),

	map_view_loop( InitialGuiState ),

	gs:stop().




% Main loop of the GUI.
map_view_loop( GuiState ) ->
	
	receive
        
		{gs,_Ok,click,_,_} ->
			io:format( "Click!~n" );

		{gs,_Pair,destroy,[],[]} ->
			io:format( "Quitting Orge Map!~n" ),
			erlang:halt();
		
		X ->
            io:format("Got '~w' (ignored).~n",[X]),
			map_view_loop( GuiState )
	
	end.



