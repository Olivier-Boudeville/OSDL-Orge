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



start() ->
	GsId = gs:start(),

	MainWin = gs:window( GsId,[ {width,get_main_window_width()}, 
								{height,get_main_window_height()}, 
								{title,"Orge Map"},
								{bg,black} ]),

	% Centers canvas in main window:
	Canvas = gs:create( canvas, MainWin, 
						[ {bg,white}, 
						  %{hscroll,bottom}, {vscroll,left},
						  {width,get_canvas_width()},
						  {height,get_canvas_height()},
						  {x,(get_main_window_width()-get_canvas_width()) / 2},
						  {y,(get_main_window_height()-get_canvas_height()) / 2}
						  %{scrollregion, {100,200,30,200}}
						] ),

	gs:create( line, Canvas,
          [{coords,[{25,25},{50,50}]}] ),

	gs:config( MainWin, {map,true} ),
	map_view_loop( MainWin),
	gs:stop().



map_view_loop( MainWin ) ->
	
	receive
        
		{gs,_Ok,click,_,_} ->
			io:format( "Click!~n" );

		{gs,_Pair,destroy,[],[]} ->
			io:format( "Quitting Orge Map!~n" ),
			erlang:halt();
		
		X ->
            io:format("Got '~w' (ignored).~n",[X]),
			map_view_loop( MainWin )
	
	end.


	
get_main_window_width() ->
	1024.


get_main_window_height() ->
	768.


get_canvas_width() ->
	800.


get_canvas_height() ->
	600.

