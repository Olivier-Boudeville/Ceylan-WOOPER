% Copyright (C) 2003-2017 Olivier Boudeville
%
% This file is part of the WOOPER library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


% Modular WOOPER header gathering the primitives (functions) to manage the state
% of an instance.


% This header mostly defines functions, so it should be included late in source
% files, not to prevent them from declaring exports afterwards.



% Voluntary underspecification, to be able to toggle:
-spec is_wooper_debug() -> boolean().


% On debug mode, various additional checkins are enabled:
%
% (put in an header, as different settings might apply to different classes)
%
-ifdef(wooper_debug).


is_wooper_debug() ->
	true.


-else. % wooper_debug


is_wooper_debug() ->
	false.


-endif. % wooper_debug



% These frequent operations must be as fast as possible:
%
% (not recommended functions, i.e. hasAttribute/2 and removeAttribute/2, shall
% not be inlined)
%
-compile( { inline, [ setAttribute/3, setAttributes/2,
					  getAttribute/2, getAttributes/2,
					  addToAttribute/3, subtractFromAttribute/3,
					  incrementAttribute/2, decrementAttribute/2,
					  toggleAttribute/2,
					  appendToAttribute/3, concatToAttribute/3,
					  deleteFromAttribute/3,
					  addKeyValueToAttribute/4, popFromAttribute/2 ] } ).



% Below are listed the correct function-based version (to be inlined), as
% opposed to the faulty macro-based implementations (see
% wooper_state_exports.hrl):



% Sets specified attribute of the instance to the specified value, based from
% specified state.
%
% Returns an updated state.
%
% Always succeeds.
%
% See also: setAttributes/3, to set more than one attribute at a time.
%
-spec setAttribute( wooper:state(), attribute_name(), attribute_value() ) ->
						wooper:state().
setAttribute( State, AttributeName, AttributeValue ) ->
   State#state_holder{
	   attribute_table = ?wooper_hashtable_type:addEntry(
		   AttributeName,
		   AttributeValue,
		   State#state_holder.attribute_table )
   }.



% Sets a list of attribute/value pairs in specified state.
%
% The expected parameter is a list of pairs (2-element tuples), each pair
% containing in first position the attribute name and in second one the
% attribute value.
%
% Returns an updated state.
%
% Always succeeds.
%
% See also: the setAttribute function.
%
-spec setAttributes( wooper:state(), [ attribute_entry() ] ) ->
						wooper:state().
setAttributes( State, ListOfAttributePairs ) ->

   State#state_holder{
	   attribute_table = ?wooper_hashtable_type:addEntries(
		   ListOfAttributePairs,
		   State#state_holder.attribute_table )
   }.



% Tells whether specified attribute exists, returns true or false.
%
% Note: usually the best practise is to set all possible attributes from the
% constructor, either to an appropriate value or to 'undefined', instead of
% having instances with or without a given attribute.
%
% Note: not expected to be ever used, as all attributes should be defined
% directly in the constructor, hence no attribute could appear later, if this
% good practise is respected.
%
-spec hasAttribute( wooper:state(), attribute_name() ) -> boolean().
hasAttribute( State, AttributeName ) ->
	?wooper_hashtable_type:hasEntry( AttributeName,
									 State#state_holder.attribute_table ).



% Returns the value associated to specified named-designated attribute, if
% found, otherwise triggers a case clause error.
%
% Note: not used very frequently, as either the attribute value can be obtained
% with the getAttr/1 macro, using the original state, named as 'State' (as
% externally defined) or the value is already bound to an available variable.
%
% See also: the getAttr/1 shorthand.
%
-spec getAttribute( wooper:state(), attribute_name() ) -> attribute_value().
getAttribute( State, AttributeName ) ->
	?wooper_hashtable_type:getValue( AttributeName,
									 State#state_holder.attribute_table ).



% Returns the value associated to each of the specified named-designated
% attributes (if found, otherwise triggers a case clause error), in the order of
% their specification.
%
% Ex: [ MyCount, MyAge, MyIdeas ] = getAttribute( SomeState,
%                                                 [ count, age, ideas ] )
%
% Note: not used very frequently, as either the attributes can be obtained with
% the getAttr/1 macro, using the original state, named as 'State' (as externally
% defined) or the values are already bound to available variables.
%
% See also: the getAttr/1 shorthand.
%
-spec getAttributes( wooper:state(), [ attribute_name() ] ) ->
						   [ attribute_value() ].
getAttributes( State, AttributeNameList ) ->
	?wooper_hashtable_type:getValues( AttributeNameList,
									  State#state_holder.attribute_table ).



% Returns an updated state not having anymore specified attribute.
%
% No error is triggered if the specified attribute was not existing.
%
% Note: this operation is not recommended, as attributes should always be
% defined. Better keep it defined, but set it to 'undefined'.
%
-spec removeAttribute( wooper:state(), attribute_name() ) -> wooper:state().
removeAttribute( State, AttributeName ) ->

	State#state_holder{
		attribute_table = ?wooper_hashtable_type:removeEntry( AttributeName,
			State#state_holder.attribute_table )
	}.



% Adds specified value to specified attribute, supposed to be a number.
%
% Returns an updated state.
%
% A case clause is triggered if the attribute did not exist, a bad arithm is
% triggered if no addition can be performed on the attribute value.
%
-spec addToAttribute( wooper:state(), attribute_name(), attribute_value() ) ->
		wooper:state().
addToAttribute( State, AttributeName, Value ) ->

	State#state_holder{
		attribute_table = ?wooper_hashtable_type:addToEntry(
			AttributeName,
			Value,
			State#state_holder.attribute_table )
	}.



% Subtracts specified value from specified attribute, supposed to be a number.
%
% Returns an updated state.
%
% A case clause is triggered if the attribute did not exist, a bad arithm is
% triggered if no subtraction can be performed on the attribute value.
%
-spec subtractFromAttribute( wooper:state(), attribute_name(),
					attribute_value() ) -> wooper:state().
subtractFromAttribute( State, AttributeName, Value ) ->

	State#state_holder{
		attribute_table = ?wooper_hashtable_type:subtractFromEntry(
			AttributeName,
			Value,
			State#state_holder.attribute_table )
	}.



% Increments specified attribute, supposed to be a number.
%
% Returns an updated state.
%
% A case clause is triggered if the attribute did not exist, a bad arithm is
% triggered if no addition can be performed on the attribute value.
%
-spec incrementAttribute( wooper:state(), attribute_name() ) ->
		wooper:state().
incrementAttribute( State, AttributeName ) ->

	State#state_holder{
		attribute_table = ?wooper_hashtable_type:addToEntry(
			AttributeName,
			_Value=1,
			State#state_holder.attribute_table )
	}.



% Decrements specified attribute, supposed to be a number.
%
% Returns an updated state.
%
% A case clause is triggered if the attribute did not exist, a bad arithm is
% triggered if no addition can be performed on the attribute value.
%
-spec decrementAttribute( wooper:state(), attribute_name() ) ->
		wooper:state().
decrementAttribute( State, AttributeName ) ->

	State#state_holder{
		attribute_table = ?wooper_hashtable_type:addToEntry(
			AttributeName,
			_Value=-1,
			State#state_holder.attribute_table )
	}.



% Returns an updated state in which specified boolean attribute is toggled: if
% true will be false, if false will be true.
%
% A case clause is triggered if the attribute does not exist or it is not a
% boolean value.
%
-spec toggleAttribute( wooper:state(), attribute_name() ) ->  wooper:state().
toggleAttribute( State, BooleanAttributeName ) ->

	State#state_holder{
		attribute_table = ?wooper_hashtable_type:toggleEntry(
			BooleanAttributeName,
			State#state_holder.attribute_table )
	}.



% Appends specified element to specified attribute, supposed to be a list.
% A case clause is triggered if the attribute did not exist.
%
% Returns an updated state.
%
% Note: no check is performed to ensure the attribute is a list indeed, and the
% operation will not complain if not.
%
-spec appendToAttribute( wooper:state(), attribute_name(),
						attribute_value() ) -> wooper:state().
appendToAttribute( State, AttributeName, Element ) ->

	State#state_holder{
		attribute_table = ?wooper_hashtable_type:appendToEntry(
			AttributeName,
			Element,
			State#state_holder.attribute_table )
	}.



% Concatenes (on the left) specified list to specified attribute, supposed to be
% a list as well. A case clause is triggered if the attribute did not exist.
%
% If that attirbute is not already defined, it will be created and associated to
% the specified list (as if beforehand it was associated to an empty list).

% Returns an updated state.
%
% Note: no check is performed to ensure the attribute is a list indeed, and the
% operation will not complain if not.
%
-spec concatToAttribute( wooper:state(), attribute_name(),
						attribute_value() ) -> wooper:state().
concatToAttribute( State, AttributeName, List ) ->

	State#state_holder{
		attribute_table = ?wooper_hashtable_type:concatToEntry(
			AttributeName,
			List,
			State#state_holder.attribute_table )
	}.



% Deletes the first match of specified element from specified attribute,
% supposed to be a list.
%
% A case clause is triggered if the attribute did not exist.
% If the element is not in the specified list, the list will not be modified.
%
% Returns an updated state.
%
-spec deleteFromAttribute( wooper:state(), attribute_name(),
		  attribute_value() ) -> wooper:state().
deleteFromAttribute( State, AttributeName, Element ) ->

	State#state_holder{
		attribute_table = ?wooper_hashtable_type:deleteFromEntry(
			AttributeName,
			Element,
			State#state_holder.attribute_table )
	}.



% Assumes the specified attribute is a hashtable and adds the specified
% key/value pair to it.
%
% Returns an updated state.
%
% Several lines compacted into a bit impressive one-liner.
%
% Note: to be used with much caution, as a class may use a type of hashtable
% unrelated to the one used by WOOPER (on the other hand we do not want to force
% all classes to define 'hashtable_type').
%
-spec addKeyValueToAttribute( wooper:state(), attribute_name(),
			?wooper_hashtable_type:key(), ?wooper_hashtable_type:value() ) ->
									wooper:state().
addKeyValueToAttribute( State, AttributeName, Key, Value ) ->

	State#state_holder{
		attribute_table = ?wooper_hashtable_type:addEntry(

			AttributeName,

			?wooper_hashtable_type:addEntry( Key, Value,
				?wooper_hashtable_type:getEntry( AttributeName,
					State#state_holder.attribute_table ) ),

			State#state_holder.attribute_table )

	}.



% Removes the head from specified attribute, supposed to be a list, and returns
% a tuple { NewState, PoppedHead }.
%
% For example, if the attribute 'my_list' contains [5,8,3], executing:
% '{ PoppedState, Head } = ?popFromAttribute( State, my_list )'
% returns a state whose my_list attribute is [8,3] and a value Head = 5.
%
% A case clause is triggered if the attribute did not exist.
%
-spec popFromAttribute( wooper:state(), attribute_name() ) ->
							{ wooper:state(), attribute_value() }.
popFromAttribute( State, AttributeName ) ->

	{ Head, PoppedAttributeTable } = ?wooper_hashtable_type:popFromEntry(
				  AttributeName, State#state_holder.attribute_table ),

	{ State#state_holder{ attribute_table = PoppedAttributeTable },
		Head }.



% Helper function for the checkUndefined macro.
%
-spec wooper_check_undefined( wooper:state(), attribute_name() ) ->
							basic_utils:void().
wooper_check_undefined( State, Attribute ) ->

	try

		undefined = ?getAttr(Attribute)

	catch

		exit:{ { badmatch, UnexpectedValue }, Stack } ->

			% Attribute value was not equal to 'undefined':
			throw( { attribute_was_not_undefined,
					 { Attribute, UnexpectedValue },
					 Stack } );

		exit:Error ->
			% Other error (ex: unknown attribute):
			throw( { attribute_error, Attribute, Error } );

		OtherError ->
			throw( { unexpected_attribute_error, Attribute, OtherError } )

	end.
