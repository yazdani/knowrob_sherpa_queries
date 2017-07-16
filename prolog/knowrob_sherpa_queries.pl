/** knowrob_sherpa_queries
Copyright (C) 2017 Fereshta Yazdani
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.
      * Neither the name of the <organization> nor the
        names of its contributors may be used to endorse or promote products
        derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  @author Fereshta Yazdani
  @license BSD
*/

:- module(knowrob_sherpa_queries,
      [
       all_poses/2,
       callDown/2,
       callUp/2,
       clear_marker/0,
       get_all_poses/2,
       get_objects_by_type/2,
       get_object_by_type/2,
       sherpa_interface/0,
       sherpa_interface/1,
       slope/3,
       add_arrow/2,
       add_underscore_arrow/2,
       detected_object/2,
       remove_map_objects/2
  ]).

:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('owl_parser')).
:- use_module(library('owl')).
:- use_module(library('rdfs_computable')).
:- use_module(library('knowrob_owl')).
:- use_module(library('jpl')).

:- rdf_meta sherpa_test(r,r),
    all_poses(r,r),
    callDown(r,r),
    callUp(r,r),
    get_all_poses(r,r),
    get_objects_by_type(r,?),
    get_object_by_type(r,r),
    sherpa_interface(r),
    sherpa_interface2(r),
    slope(r,r,r),
    add_arrow(r,r),
    add_underscore_arrow(r,r),
    detected_object(r,r),
    remove_map_objects(r,r).

:- rdf_db:rdf_register_ns(knowrob, 'http://knowrob.org/kb/knowrob.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(u-map, 'http://knowrob.org/kb/u_map.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(log, 'http://knowrob.org/kb/unreal_log.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(knowrob_u, 'http://knowrob.org/kb/knowrob_u.owl#', [keep(true)]).


sherpa_interface :-  sherpa_interface(_).
 
    
:-assert(sherpa_inter(fail)).

sherpa_interface(SHERPA) :-
sherpa_inter(fail),
jpl_call('com.github.knowrob_sherpa_queries.VisualizationMarker', get, [], SHERPA),
jpl_list_to_array(['com.github.knowrob_sherpa_queries.VisualizationMarker'], Arr),
jpl_call('org.knowrob.utils.ros.RosUtilities', runRosjavaNode, [SHERPA, Arr], _),
retract(sherpa_inter(fail)),
assert(sherpa_inter(SHERPA)),!.

sherpa_interface(DB):-
    sherpa_inter(DB).

add_arrow(Obj,FIN) :-
sherpa_interface(SHERPA),
current_object_pose(Obj,Pose),
jpl_list_to_array(Pose,OA),
map_object_dimensions(Obj,W,D,H),
append([W, D, H],[], L),
jpl_list_to_array(L,LA),
jpl_call(SHERPA,'addArrow',[OA,LA],FIN).

remove_map_objects(A,B):-
sherpa_interface(SHERPA),
jpl_call(SHERPA,'removeMapObject',[A],B).

add_underscore_arrow(A,B):-
sherpa_interface(SHERPA),
jpl_call(SHERPA,'addUnderscoreArrow',[A],B).

clear_marker :-
sherpa_interface(SHERPA),
jpl_call(SHERPA, 'clear', [], _).

detected_object(A,B):-
sherpa_interface(SHERPA),
current_object_pose(A,Pose),
jpl_list_to_array(Pose,OA),
jpl_call(SHERPA,'detectedObject',[OA],B).

slope(A,B,C):-
==(A,'up') -> callUp(B,C);
callDown(B,C).

callDown(A,B):-
sherpa_interface(SHERPA),
get_objects_by_type(A, Names),
jpl_list_to_array(Names,Name),
jpl_call(SHERPA,'slopeDown',[Name],C),
jpl_array_to_list(C,B).

callUp(A,B):-
sherpa_interface(SHERPA),
get_objects_by_type(A, Names),
get_all_poses(Names,Poses),
format("all_poses"),
jpl_list_to_array(Names, N),
format("all_poses123"),
jpl_list_to_array(Poses, P),
format("all_poses456"),
jpl_call(SHERPA,'slopeUp',[N, P],B).

get_objects_by_type(TYPE, Objs) :-
format('get_objects_by_type\n'),
    setof(Obj, get_object_by_type(TYPE, Obj), Objs).

get_object_by_type(TYPE, Obj) :-
format('\nget_object_by_type\n'),
   owl_individual_of(Obj,TYPE).

all_poses(Number,Poses):-
    current_object_pose(Number,P),
jpl_list_to_array(P,S),
Poses = S.

get_all_poses([H|T], [SH|ST]) :-
all_poses(H,P),
     SH = P,
get_all_poses(T, ST).

get_all_poses([],[]).
