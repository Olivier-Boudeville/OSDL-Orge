% 
% Copyright (C) 2009 Olivier Boudeville
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


% This class allows to manage a language and its variations.
-module(class_LanguageManager).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[ class_TraceEmitter ]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters, LanguageName, LanguageVariations,
	MarkovOrder, LanguageOptions ).



% Static methods:
-export([ get_possible_language_variations/0 ]).

% fixme
-export([ get_sequences_for/2, find_sequence/3 ]).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/4, new_link/4, 
	synchronous_new/4, synchronous_new_link/4,
	synchronous_timed_new/4, synchronous_timed_new_link/4,
	remote_new/5, remote_new_link/5, remote_synchronous_new/5,
	remote_synchronous_new_link/5, remote_synchronous_timed_new/5,
	remote_synchronous_timed_new_link/5, construct/5, delete/1 ).



% Method declarations.
-define(wooper_method_export, learn/1, learn/2, 
	generate/2, evaluate/3 ).




% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Orge.LanguageManager").


% The name of the default directory in which wordsets are supposed to be stored:
-define( default_wordset_dir, "language-wordsets" ).


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Each field corresponds to one of the 28 possible letters, except total_count
% whose role is to precompute the total number of occurences of all letters:
-record( variation_node, { a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p,
	q, r, s, t, u, v, w, x, y, z, dash, eow, total_count } ).


% Each field will contain a pair {PatternCount,VariationNode,Total}.


% Implementation notes:
%
	
	
% Constructs a new language manager:
%  - LanguageName is a string corresponding to the name of this language,
% ex: "ancient-egyptian"
%  - LanguageVariations is a list of atoms describing the variations to
% be supported, ex: [ 'female-names', words ]
%  - MarkovOrder the order of the Markov chains to be used (order 2 is probably 
% the best accuracy vs memory space trade-off)
%  - LanguageOptions is the list of supported options, in:
%    - generate_original_only: the manager will ensure that no generated 
% word will happen to be the same as a word in the variation
construct( State, ?wooper_construct_parameters ) ->

	TraceState = class_TraceEmitter:construct( State,
		"Manager for language " ++ LanguageName ),
	
	?send_info([ TraceState, io_lib:format( 
		"Creating a manager for language '~s' and variation(s) ~w, "
		"with Markov chains of order ~B, and options ~w.", 
		[ LanguageName, LanguageVariations, MarkovOrder, LanguageOptions ] ) ]),
	
	OriginalOnly = lists:member( generate_original_only, LanguageOptions ),
			
	{WordSetDir,Origin} = case os:getenv("ORGE_WORD_SET_DIR") of
	
		false ->
			{ok,CurrentDir} = file:get_cwd(),
			{ file_utils:join( CurrentDir, ?default_wordset_dir ), 
				"determined from current directory" };
			
		SetValue ->
			{SetValue,"determined from the ORGE_WORD_SET_DIR environment "
				"variable"}
			
	end,

	case file_utils:is_existing_directory( WordSetDir ) of
	
		true ->
			?send_info([ TraceState, io_lib:format( 
				"Using word set directory ~s, ~s.", [ WordSetDir, Origin ] ) ]);
				
		false ->
			throw( {wordset_directory_not_found,WordSetDir,Origin} )
	
	end,		
				
	?setAttributes( TraceState, [
		{language_name,LanguageName},
		{variations,LanguageVariations},
		{generate_original_only,OriginalOnly},
		{word_set_dir,WordSetDir},
		{variation_tree,hashtable:new( length(LanguageVariations) )}
	] ).
		
	

% Overridden destructor.
delete(State) ->
	% Class-specific actions:
	?info([ "Deleting language manager." ]),

	?debug([ "Language manager deleted." ]),

	% Then call the direct mother class counterparts and allow chaining:
	class_TraceEmitter:delete(State).
	
	
	
	
% Method section.

	
% Learns all known variations of this language.
% Returns either learning_success or {learning_failure,Reason}.
% (request)
learn( State ) ->
	AllVariations = ?getAttr(variations),
	{LearntState,Result} = learn_all( State, AllVariations),
	?wooper_return_state_result( LearntState, Result ).
	
	

% Learns specified variation of this language.
% Returns either learning_success or {learning_failure,Reason}.
% (request)
learn( State, VariationName ) -> 
	
	case get_variation_filename( VariationName, State ) of
	
		{ok,Filename} ->
			?info([ io_lib:format( "Learning variation ~s, using filename ~s.",
				[ VariationName, Filename] ) ]),
			{ok,BinLines} = file:read_file( Filename ),
			% No closing needed, apparently.
			Line = binary_to_list( BinLines ),
			% io:format( "Line = ~s.~n", [ Line ] ),
			Words = string:tokens( Line, basic_utils:get_whitespaces_list() ),
			%?emit_info([ io_lib:format( "Language words are: ~p.",[Words] ) ]);
			?info([ io_lib:format( 
				"This language variation is made of ~B words.", 
				[ length(Words) ] ) ]),

			VariationTree = build_tree( Words ),
			
			TreeState = ?addKeyValueToAttribute( State, 
				_Attribute = variation_tree, _Key = VariationName, 
				_Value = VariationTree ),
				
			?wooper_return_state_result( TreeState, learning_success );	
			
		{error,Reason} ->
			?error([ io_lib:format( "Learning of variation ~s failed: ~p",
				[ VariationName, Reason ] ) ]),
			?wooper_return_state_result( State, {learning_failure,Reason} )
						
	end.



% Returns a generated word from specified variation, according to language
% options.
% (const request)
generate( State, _Variation ) ->
	?wooper_return_state_result( State, "test" ).



% Returns the probability (as a floting-point number) that the specified 
% word belongs to the specified variation.
% (const request)
evaluate( State, _Word, _Variation ) ->
	?wooper_return_state_result( State, 0.0 ).
	
	
	


% Static section.


% Returns all the possible language variations.
% (static)
get_possible_language_variations() ->
	[ 'female-names', 'male-names', surnames, placenames, names, words ].





% Helper functions.


% Converts an atom designating a language variation into a file element suffix.
% Ex: returns "female-names".
get_variation_suffix( AtomVariation ) ->

	case lists:member( AtomVariation, get_possible_language_variations() ) of 
	
		true ->
			atom_to_list( AtomVariation );
			
		false ->
			throw( {unknown_language_variation,AtomVariation} )
			
	end.



% Returns the filename which would correspond to the one of the word set 
% for the specified language and variation.
get_filename_for( Language, Variation ) ->
	Language ++ "-" ++ get_variation_suffix( Variation ) ++ ".txt".
		
		

% Learns all specified language variations.	
learn_all( State, [] ) ->
	{State,learning_success};
	
learn_all( State, [H|T] ) ->
	% H is an atom, not a string (list):
	case executeRequest( State, learn, H ) of
	
		{NewState,learning_success} ->
			learn_all( NewState, T );
			
		{_AState,{error,Reason}} ->
			{State,{learning_failure,Reason}}
			
	end.			



% Returns either {ok,Filename} or {error,Reason}.
get_variation_filename( VariationName, State ) ->
	case lists:member( VariationName, ?getAttr(variations) ) of
	
		true ->
			VariationFilename = file_utils:join( ?getAttr(word_set_dir),
				get_filename_for( ?getAttr(language_name), VariationName ) ),

			case file_utils:is_existing_file( VariationFilename ) of
	
				true ->
					{ok,VariationFilename};
			
				false ->
					{error,{variation_file_not_found,VariationFilename}}
		
			end;
			
		false ->
			{error,{unknown_variation,VariationName}}
				
	end.
	

% Constructs a variation tree from the specified list of words.
build_tree( _Words ) ->
	fixme.
	


% Returns the list of all sequences found in Word, with respect to the specified
% order.
% Ex: get_sequences_for( "orge", 2 ) should return:
% [ "o", "or", "org", "rge", [ $g, $e, eow] ].
get_sequences_for( Word, Order ) ->
	get_sequences_for( Word ++ [eow], _CurrentIndex = length(Word)+1, 
		Order, _Acc = [] ).
	
	
get_sequences_for( _Word, 0, _Order, Acc ) ->	
	Acc;
	
get_sequences_for( Word, CurrentIndex, Order, Acc  ) ->	
	Seq = find_sequence( Word, CurrentIndex, Order ), 
	get_sequences_for( Word, CurrentIndex-1, Order, [ Seq | Acc ] ).
	
		
		
% Given specified index, returns all sequences ending at that index.
% Ex: find_sequences( [ $o, $r, $g, $e, eow ], _Index = 2, _Order = 2 ) =
% [ $o, $r ]. 
find_sequence( Word, CurrentIndex, Order ) when CurrentIndex > Order ->	
	string:substr( Word, CurrentIndex-Order, Order+1 );
	
find_sequence( Word, CurrentIndex, _Order ) ->	
	string:substr( Word, 1, CurrentIndex ).
	
