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
       add_arrow/2,
       add_traces/1,
       add_stext/2,
       add_name/2,
       all_poses/2,
       callAll/1,
       callSlope/1,
       callVisualizer/1,
       clear_marker/0,
       detected_object/1,
       get_all_poses/2,
       get_objects_by_type/2,
       get_object_by_type/2,
       set_mongodb/1,
       sherpa_interface/0,
       sherpa_interface/1,
       visualize_agent_location/1,
       slope/1,
       visualize_areas/1,
       visualize_bboxes/1
  ]).

:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('owl_parser')).
:- use_module(library('owl')).
:- use_module(library('rdfs_computable')).
:- use_module(library('knowrob_owl')).
:- use_module(library('jpl')).

:- rdf_meta sherpa_test(r,r),
    add_arrow(r,r),
    add_stext(r,r),
    add_name(r,r),
    all_poses(r,r),
    callAll(r),
    callSlope(r),
    callVisualizer(r),
    detected_object(r),
    get_all_poses(r,r),
    get_objects_by_type(r,?),
    get_object_by_type(r,r),
    set_mongodb(r),
    sherpa_interface(r),
    sherpa_interface2(r),
    visualize_agent_location(r),
    slope(r),
    visualize_bboxes(r),
    visualize_areas(r).

:- rdf_db:rdf_register_ns(knowrob, 'http://knowrob.org/kb/knowrob.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(u-map, 'http://knowrob.org/kb/u_map.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(log, 'http://knowrob.org/kb/unreal_log.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(knowrob_u, 'http://knowrob.org/kb/knowrob_u.owl#', [keep(true)]).


sherpa_interface :-  sherpa_interface(_).
 

sherpa_interface(SHERPA) :-
(\+ current_predicate(v_canvas, _)),
jpl_call('com.github.knowrob_sherpa_queries.VisualizationMarker', get, [], SHERPA),
jpl_list_to_array(['com.github.knowrob_sherpa_queries.VisualizationMarker'], Arr),
jpl_call('org.knowrob.utils.ros.RosUtilities', runRosjavaNode, [SHERPA, Arr], _),
 assert(v_canvas(SHERPA)),!.

sherpa_interface(SHERPA):-
current_predicate(v_canvas, _),
v_canvas(SHERPA).


visualize_agent_location(T) :-
sherpa_interface(SHERPA),
jpl_list_to_array(T,TL),
jpl_call(SHERPA,'visualizeLocation',[TL],_).


set_mongodb(COL):-
mongo_interface(Mongo),
jpl_call(Mongo, 'setDatabase', [COL], _).

%% add_arrow(+Individual, B)  is det.
%
% add an arrow ontop of objects
% this is used in order to visualize roofings
%
add_arrow(Obj,B) :-
sherpa_interface(SHERPA),
format("TEST"),
current_object_pose(Obj,Pose),
format("TEST1"),
jpl_list_to_array(Pose,OA),
format("TEST2"),
map_object_dimensions(Obj,W,D,H),
format("TEST3"),
append([W, D, H],[], L),
jpl_list_to_array(L,LA),
jpl_call(SHERPA,'addArrow',[OA,LA],B).

add_stext(Obj,B) :-
sherpa_interface(SHERPA),
current_object_pose(Obj,Pose),
jpl_list_to_array(Pose,OA),
map_object_dimensions(Obj,W,D,H),
append([W, D, H],[], L),
jpl_list_to_array(L,LA),
jpl_call(SHERPA,'addText',[OA,LA,Obj],B).

add_name(N,Tr) :-
sherpa_interface(SHERPA),
jpl_list_to_array(Tr,TL),
jpl_call(SHERPA,'addNameText',[N,TL],_).

%% clear_marker is det.
%
% clears all the markers.
%
clear_marker :-
sherpa_interface(SHERPA),
jpl_call(SHERPA, 'clear', [], _).

%% detected_object(+Individual) is det.
%
% detected_object visualized the object by an arrow.
%
detected_object(A):-
sherpa_interface(SHERPA),
current_object_pose(A,Pose),
jpl_list_to_array(Pose,OA),
jpl_call(SHERPA,'detectedObject',[OA],_).

%% slope(+Checker) is det.
%
% slope is calculating the slopes up and down.
%
slope(B):-
callAll(B).

callAll(A):-
format("callAll"),
get_objects_by_type(A, _),
callSlope(_).

add_traces(A):-
    sherpa_interface(SHERPA),
    jpl_list_to_array(A,B),
    jpl_call(SHERPA,'addRayTracingMarker',[B],_).

callSlope(_):-
format("callslope"),
sherpa_interface(SHERPA),
jpl_call(SHERPA,'Roadneighbours',[], A),
format("callslope3"),
jpl_array_to_list(A, AL),
format("callslope2"),
get_all_poses(AL,Poses),
format("callslope4"),
jpl_list_to_array(AL, N),
format("callslope5"),
jpl_list_to_array(Poses, P),
format("callslope6"),
jpl_call(SHERPA,'slopeUp',[N, P],_).

visualize_areas(A):-
    sherpa_interface(SHERPA),
    jpl_list_to_array(A, L),
    jpl_call(SHERPA,'visualizeAreas',[L],_).

visualize_bboxes(A):-
get_objects_by_type(A, Objs),
    forall(member(X,Objs),callVisualizer(X)).

callVisualizer(A):-
  sherpa_interface(SHERPA),
current_object_pose(A,P),
map_object_dimensions(A,W,D,H),
append([W, D, H],[], L),
    jpl_list_to_array(L,LA), 
    jpl_list_to_array(P,PA),
    jpl_call(SHERPA,'visualizeBBoxes',[PA,LA],_).

get_objects_by_type(TYPE, Objs) :-
    setof(Obj, get_object_by_type(TYPE, Obj), Objs).

get_object_by_type(TYPE, Obj) :-
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
