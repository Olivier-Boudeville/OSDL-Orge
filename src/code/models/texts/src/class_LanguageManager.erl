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


%Debug:
-export([ build_tree/2, normalize/1, draw_letters/5 ]).


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



% Implementation notes:
%
% Not using the record below, using a basic list instead, as it should use
% less memory (avoids creating subtrees for letters that have no successor
% at all).

% Each field corresponds to one of the 28 possible letters, except total_count
% whose role is to precompute the total number of occurences of all letters:
%-record( variation_node, { a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p,
%	q, r, s, t, u, v, w, x, y, z, dash, eow, total_count } ).
% Each field will contain a pair {PatternCount,VariationNode,Total}.

% Instead we use a series of tuples nested in lists, to represent a tree.
% Each tuple, which is a tree node, respects this form:
% {Letter,OccurrenceCount,{Subtree,SubTreeSum}} where:
%  - Letter designates the letter in the currently explored sequence
%  - OccurrenceCount is the number of times this letter ended a learnt sequence
%  - Subtree is a list of tuples of the current type being described, each 
% tuple corresponding to a possible letter that can be found after Letter
%  - SubTreeSum is the sum of all occurrences of all letters in the direct
% subtree


% Once a tree has been normalized, OccurrenceCount is the maximum number that
% will make the corresponding letter be drawn.
% For example, 'class_LanguageManager:build_tree( [ "a", "a", "b", "c" ], 2 )'
% shall return:
% {[ {99,1,{[{eow,1,{[],0}}],1}},
%    {98,2,{[{eow,1,{[],0}}],1}},
%    {97,4,{[{eow,2,{[],0}}],2}} ],
% 4} = { [ FirstTuple, SecondTuple, ThirdTuple ], Sum }
% Sum=4 is the number of all learnt occurrences of letters at this
% (first) level (i.e. two "a", one "b" and one "c").
% The first tuple references "c" (with 99), whose maximum random value is 1
% (the second element of its tuple), whereas the maximum random value for "b",
% is 2, as specified in the second tuple, and the maximum random value for "a"
% is 4.
% That means that if we draw a random value R in [1,Sum] = [1,4], then
% if R <= 1 (i.e. R=1), then we selected "c", otherwise if R<=2 (i.e. R=2),
% then we selected "b", otherwise (R<=4, i.e. R=3 or R=4), then we selected
% "a".
	
	
	
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
	
	basic_utils:start_random_source( time_based_seed ),
				
	?setAttributes( TraceState, [
		{language_name,LanguageName},
		{variations,LanguageVariations},
		{generate_original_only,OriginalOnly},
		{word_set_dir,WordSetDir},
		{markov_order,MarkovOrder},
		{variation_trees,hashtable:new( length(LanguageVariations) )}
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

			{VariationTree,Sum} = build_tree( Words, ?getAttr(markov_order) ),
			
			TreeState = ?addKeyValueToAttribute( State, 
				_Attribute = variation_trees, _Key = VariationName, 
				_Value = {VariationTree,Sum} ),
				
			?wooper_return_state_result( TreeState, learning_success );	
			
		{error,Reason} ->
			?error([ io_lib:format( "Learning of variation ~s failed: ~p",
				[ VariationName, Reason ] ) ]),
			?wooper_return_state_result( State, {learning_failure,Reason} )
						
	end.



% Returns a generated word from specified variation, according to language
% options.
% (const request)
generate( State, Variation ) ->
	VariationTable = ?getAttr(variation_trees),
	case hashtable:lookupEntry( Variation, VariationTable ) of
	
		undefined ->
			?wooper_return_state_result( State, 
				{ generation_failed, {variation_not_found,Variation} } );
		
		{value, {VariationTree,Sum} } ->
			%io:format( "Generation in tree ~p with sum ~B~n",
			%	[VariationTree,Sum] ),
			GeneratedWord = draw_letters( VariationTree, Sum, 
				?getAttr(markov_order), _CurrentPattern = [], _Acc = [] ),
			?wooper_return_state_result( State, 
				{ generation_success, GeneratedWord }  )

	end.



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
	




% Learning section.


% Constructs a variation tree from the specified list of words.
build_tree( Words, Order ) ->
	IntegratedTree = process_words( Words, Order, _EmptyTree = [] ),
	NormalizedTree = normalize( IntegratedTree ),
	io:format( "Once having learnt words ~p, "
		"initial tree is:~n~w, normalized tree is:~n~w.~n.", 
		[Words,IntegratedTree,NormalizedTree] ),
	NormalizedTree.
	


% Integrates the specified word list into the specified variation tree.
% Returns an updated variation tree.	
process_words( [], _Order, CurrentVariationTree ) ->
	CurrentVariationTree;
	
process_words( [Word|OtherWords], Order, CurrentVariationTree ) ->
	NewVariationTree = integrate_word( Word, Order, CurrentVariationTree ),
	process_words( OtherWords, Order, NewVariationTree ).



% Integrates the specified word into the specified variation tree.
% Returns an updated variation tree.	
integrate_word( Word, Order, CurrentVariationTree ) ->
	Sequences = get_sequences_for( Word, Order ),
	%io:format( "Sequences for word '~s' are: ~w.~n", [Word,Sequences] ),
	integrate_sequences_in_tree( Sequences, Order, CurrentVariationTree ).
	


% Integrates the specified sequences in the variation tree.
% Returns an updated variation tree.	
integrate_sequences_in_tree( [], _Order, CurrentVariationTree ) ->
	CurrentVariationTree ;
	
integrate_sequences_in_tree( [Seq|OtherSeq], Order, CurrentVariationTree ) ->
	UpdatedVariationTree = integrate_sequence_in( Seq, CurrentVariationTree ),
	integrate_sequences_in_tree( OtherSeq, Order, UpdatedVariationTree ).



% Integrates the specified sequence in the variation tree.
% Returns an updated variation tree.	
integrate_sequence_in( [H|[]], VariationTree ) ->
		
	% We arrived to the last letter of the sequence.
	% Here we reached the subtree is which the initial sequence must be
	% recorded, we return an updated tree node:
	% (relies on the fact that sequences have already a correct length)
	Res = register_sequence( H, VariationTree ),
	%io:format( "integrate_sequence_in: inserting final letter ~w in ~p, "
	%	"returning ~w.~n", [[H],VariationTree,Res] ),
	Res;
	
integrate_sequence_in( [H|T], VariationTree ) ->
	% Here we are in the sequence, thus recursing down the relevant subtree:
	%io:format( "1 integrate_sequence_in: handling intermediate letter ~s "
	%	"in ~p.~n",[[H],VariationTree] ),
	
	NewTreeForH = case lists:keyfind( H, _IndexInTuple = 1, VariationTree ) of
		
		false ->
			% This letter was never registered, adding a subtree for it:
			[ { H, 0, {integrate_sequence_in( T, [] ), sum_not_available } }
				| VariationTree ];
						
		{H,Count,{Subtree,sum_not_available}} ->
			% Just iterates and returns an updated subtree:
			lists:keyreplace( _Key = H, _IndexInTuple = 1, VariationTree,
				{H,Count,
					{integrate_sequence_in( T, Subtree ),sum_not_available} } )
		
	end,
	%io:format( "2 integrate_sequence_in: new tree for ~s is ~p.~n",
	%	[[H],NewTreeForH] ),
	
	NewTreeForH.



% Registers the sequence, which is caracterized by the subtree we arrived to
% and its last letter, in that subtree, and returns it.
register_sequence( H, VariationTree ) ->
	case lists:keyfind( _Key = H, _IndexInTuple = 1, VariationTree ) of
		
		false ->
			% This letter was never registered, adding an entry for it:
			[ { H, _Count=1, { _EmptySubtree=[], sum_not_available } } 
				| VariationTree ];
			
		{H,Count,{Subtree,sum_not_available}} ->
			% This letter was already registered, just increments its count:
			lists:keyreplace( _Key = H, _IndexInTuple = 1,
				VariationTree, { H, Count+1, {Subtree,sum_not_available} } )
				
	end.
	


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
	



% Normalization section.
	
	
% Normalizes the specified variation tree, to precompute everything to 
% make the drawing of letters easier.
% When at a given depth we have for example [ {a,1,L1}, {b,5,L2, {c,4,L3} ]
% then we know that the subtree sum is 1+5+4=10, and we can rewrite the tuples
% that way: [ {a,1,L1}, {b,6,L2, {c,10,L3} ].
% This allows to generate a random value R in [1,10].
% If R=1, the we draw a, otherwise if R <7, we draw b, otherwise we draw c.
% 
normalize( VariationTree ) ->
	%io:format( "Normalizing tree ~w.~n", [VariationTree] ),
	% We enclose the first-level letters as if words began with a virtual
	% 'bow' letter (for 'beginning of word'), so that we retrieve the 
	% sum for the first-level letters as well:
	{ [ {bow,0,{NormalizedTree,Sum}} ], 0 } = compute_sums(
		[ {bow,0,{VariationTree,sum_not_available}} ], 
		{ _Acc = [], _Sum = 0 } ),
	{NormalizedTree,Sum}.
	
	

% For each node, computes the sums of all occurrences of next letters and
% dispatches letter offsets so that they range in [1,Sum], covering a range
% proportional to their number of occurrences, so that drawing a number N in
% [1,Sum] allows to pick letters according to their recorded frequency.
%
% Ex: if, during the learning, the current letter was followed X times by 'a'
% and Y times by 'b', returns an updated tree whose sum is X+Y, and all sums
% in the tree are recursively determined.
% If N <= X then 'a' is drawn, otherwise if N <= 3 (always the case), 
% 'b' is drawn, etc.
% Returns a pair {SummedTree,Sum}.
compute_sums( [], {SummedTree,Sum} ) ->
	% We reverse the list so that their offset is ordered from lowest 
	% to highest:
	{lists:reverse(SummedTree),Sum};
	
compute_sums( [ {Letter,Count,{Subtree,sum_not_available}}|T], {Acc,Sum} ) ->
	compute_sums( T, { [ {Letter,Sum+Count,compute_sums(Subtree,{[],0})}|Acc],
		Sum+Count} ) .
		
		

	

% Random generation section.


% Draws letters as long as 'eow' is not drawn:
draw_letters( FullVariationTree, FullSum, Order, CurrentPattern, WordAcc ) ->
	{PatternTree,PatternSum} = get_subtree_for( FullVariationTree, FullSum, 
		CurrentPattern ),
	Value = basic_utils:get_random_value( PatternSum ),
	%io:format( "draw_letters: pattern = ~p, acc = ~p, drawn = ~B~n", 
	%	[CurrentPattern,WordAcc, Value] ), 
	%io:format( "Getting letter for drawn value of ~B/~B in ~p.~n",
	%	[Value,PatternSum,PatternTree] ),
	case get_entry_for( PatternTree, Value ) of
	
		{ eow, _SubtreeEntry } ->
			% _SubtreeEntry must be: {[],0}
			% End of word reached, returning it:
			Res = lists:reverse(WordAcc),
			io:format( "End of word reached, generated word is: '~s'.~n", 
				[Res] ),
			Res;
			
		{ NormalLetter, _SubtreeEntry } ->
			%io:format( "Drawn letter '~s'.~n", [ [NormalLetter] ] ),
			% Adds one more letter:
			draw_letters( FullVariationTree, FullSum, Order, 
				get_new_pattern( CurrentPattern, NormalLetter, Order ), 
				[NormalLetter|WordAcc] )	
	end.

	
	
% Returns the subtree corresponding to specified pattern.	
get_subtree_for( CurrentVariationTree, SubSum, _Pattern = [] ) ->
	{CurrentVariationTree, SubSum};
	
get_subtree_for( CurrentVariationTree, _Sum, [H|T] ) ->
	{H,_Count,{Subtree,SubSum}} = lists:keyfind( _Key = H, _IndexInTuple = 1, 
		CurrentVariationTree ),
	get_subtree_for( Subtree, SubSum, T ).
	
	

% Returns the new pattern to be used.
get_new_pattern( Pattern, NewLetter, Order ) ->
	case length(Pattern) of
	
		Order ->
			% We must add the new letter and remove the most ancient one:
			[_Removed|T] = Pattern,
			T ++ [NewLetter];
		
		_LessThanOrder ->
			% Just add:
			Pattern	++ [NewLetter]
	
	end.
	


% Returns the letter entry that is selected based on the specified value.		
get_entry_for( [ {Letter,Count,{Subtree,Sum}} | _T ], Value ) 
		when Value =< Count ->	
	% We have got a winner!
	{ Letter, {Subtree,Sum} };
	
get_entry_for( [ _H | T ], Value ) ->
	% Here Value > Count, going on:
	get_entry_for( T, Value ).
	
	 
