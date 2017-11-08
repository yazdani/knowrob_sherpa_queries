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
       regions_not_scanned/4,
       goIntoNoScanMethod/5,
       set_keywords_as_markerobjs/1,
       add_arrow/2,
       add_name/2,
       add_tenName/2,
       add_stext/2,
       add_traces/1,
       add_speech/2,
       get_atomlist_from_stringlist/2,
       remove_speech/1,
       add_marker/2,
       all_poses/2,
       all_objects_dimensions/2,
       all_objects_transformations/2,
       all_timepoint_poses/3,
       add_robot/2,
       callAll/1,
       callSlope/1,
       callVisualizer/1,
       cam_pose/1,
       clear_marker/0,
       detected_object/1,
       entering_succeeded/2,
       get_pose_of_transform/2,
       entering_failed/1,
       is_element_in_list/3,
       get_all_detected_regions/4,
       get_all_poses/2,
       get_all_timepoint_poses/3,
       get_all_timepoints/4,
       get_next_timepoint/3,
       get_objects_MinMax/2, 
       get_object_by_type/2,
       get_objects_by_type/2,
       get_points_as_polygon/2,
       get_points_as_array/2,
       get_points/2,
       get_origin/2,
       get_tpose/2,
       get_elements/3,
       get_previous_timepoint/3,
       get_regions/2,
       get_mesh_scale_array/2,
       visualize_meshes/1,
       goIntoOthermethod/4,
       is_part_of/3,
       map_objects_dimensions/2,
       map_objects_transformations/2,
       set_timepoint/2,
       sherpa_interface/0,
       sherpa_interface/1,
       slope/1,
       possible_object_location/2,
       visualize_agent_location/1,
       visualize_areas/1,
       visualize_bboxes/1,
       visualize_bbox/1,
       visualize_mesh/1,
       view_image/1,
       show_gps_location/3
  ]).

:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('owl_parser')).
:- use_module(library('owl')).
:- use_module(library('rdfs_computable')).
:- use_module(library('knowrob_owl')).
:- use_module(library('jpl')).

:- rdf_meta sherpa_test(r,r),
    regions_not_scanned(r,r,r,r),
    goIntoNoScanMethod(r,r,r,r,r),
    set_keywords_as_markerobjs(r),
    get_atomlist_from_stringlist(r,r),
    add_arrow(r,r),
    add_marker(r,r),
    add_name(r,r),
    add_tenName(r,r),
    add_robot(r,r),
    add_stext(r,r),
    get_pose_of_transform(r, r),
    add_speech(r,r),
    remove_speech(r),
    all_objects_dimensions(r,r),
    all_objects_transformations(r,r), 
    all_poses(r,r),
    all_timepoint_poses(r,r,r),
    callAll(r),
    callSlope(r),
    callVisualizer(r),
    cam_pose(r),
    detected_object(r),
    possible_object_location(r,r),
    entering_failed(r),
    entering_succeeded(r,r),
    get_all_detected_regions(r,r,r,r),
    get_all_poses(r,r),
    get_regions(r,r),
    get_mesh_scale_array(r,r),
    visualize_meshes(r),
    get_previous_timepoint(r,r,r),
    get_all_timepoint_poses(r,r,r),
    get_all_timepoints(r,r,r,r),
    get_next_timepoint(r,r,r),
    get_object_by_type(r,r),
    get_objects_by_type(r,?),
    get_objects_MinMax(r,r),
    get_origin(r,r),
    get_points_as_polygon(r,r),
    get_points_as_array(r,r),
    get_points(r,r),
    get_tpose(r,r),
    get_elements(r,r,r),
    goIntoOthermethod(r,r,r,r),
    is_part_of(r,r,r),
    map_objects_dimensions(r,r),
    map_objects_transformations(r,r),
    set_timepoint(r,r),
    sherpa_interface(r),
    sherpa_interface2(r),
    slope(r),
    is_element_in_list(r,r,r),
    visualize_agent_location(r),
    visualize_areas(r),
    visualize_bboxes(r),
    view_image(r),
    visualize_mesh(r),
    visualize_bbox(r),
    show_gps_location(r,r,r).

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

%% visualize_agent_location()
%
%
%
visualize_agent_location(T) :-
sherpa_interface(SHERPA),
get_origin(T,Pose),
jpl_call(SHERPA,'visualizeLocation',[Pose],_).

remove_speech(Cmd):-
marker(sprite_text('SPEECH'),MarkerObj),
format(atom(TextHtml),'<div style="front-size: 18px; font-style: italic; font-family:Oswald,Arial,Helvetica,sans-serif; text-align: center;">~w</div>',[Cmd]), 
marker_text(MarkerObj,TextHtml), 
marker_remove(MarkerObj),
clear_marker.

get_origin(Tr,Pose):-
sherpa_interface(SHERPA),
jpl_list_to_array(Tr,T),
jpl_call(SHERPA,'getOrigin',[T],Pose).

get_tpose(Task,Pose):-
sherpa_interface(SHERPA),
rdf_has(Task, knowrob:'quaternion',literal(type(_X, Q))),
rdf_has(Task, knowrob:'translation',literal(type(_X, TR))),
jpl_call(SHERPA,'getTaskPose',[TR,Q],Pose).

add_marker(Pose, Marker) :-
sherpa_interface(SHERPA),
jpl_list_to_array(Pose,LPose),
jpl_call(SHERPA,'addMarker',[LPose, Marker],_).


%% add_arrow(+Individual, B)  is det.
%
% add an arrow ontop of objects
% this is used in order to visualize roofings
%
add_arrow(Obj,B) :-
sherpa_interface(SHERPA),
format(Obj),
current_object_pose(Obj,Pose),
format(Obj),
jpl_list_to_array(Pose,OA),
map_object_dimensions(Obj,W,D,H),
append([W, D, H],[], L),
jpl_list_to_array(L,LA),
jpl_call(SHERPA,'addArrow',[OA,LA],B).

all_objects_transformations(Obj,Poses):-
    current_object_pose(Obj,Pose),
    jpl_list_to_array(Pose,Sose),
    Poses = Sose.

map_objects_transformations([H|T],[SH|ST]):-
%format('map_objects_transformations'),
%format('\n FIREST \n'),
%format(H),
all_objects_transformations(H,P),
SH = P,
%format('\n  SECOND \n'),
%format(H),
map_objects_transformations(T, ST).

map_objects_transformations([],[]).

add_speech(Cmd,Tr):-
    marker(sprite_text('SPEECH'),MarkerObj),
    marker_color(sprite_text('SPEECH'), [1,1,1]),
    get_origin(Tr,Pose),
jpl_array_to_list(Pose,LPose),
    marker_translation(MarkerObj,LPose),
    format(atom(TextHtml),'<div style="front-size: 18px; font-style: italic; font-family:Oswald,Arial,Helvetica,sans-serif; text-align: center;">~w</div>',[Cmd]),
marker_text(MarkerObj,TextHtml),marker_scale(MarkerObj,[3.0,3.0,3.0]).

all_objects_dimensions(Obj,Poses):-
    map_object_dimensions(Obj,W,D,H),
    append([W, D, H],[], L),
    jpl_list_to_array(L,Pose),
    Poses = Pose.

map_objects_dimensions([H|T],[SH|ST]):-
all_objects_dimensions(H,P),
SH = P,
map_objects_dimensions(T, ST).

map_objects_dimensions([],[]).

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
get_origin(Tr,T),
jpl_call(SHERPA,'addNameText',[N,T],_).

add_tenName(N,Tr) :-
sherpa_interface(SHERPA),
get_origin(Tr,T),
jpl_call(SHERPA,'addName10Text',[N,T],_).


%% clear_marker is det.
%
% clears all the markers.
%
clear_marker :-
sherpa_interface(SHERPA),
jpl_call(SHERPA, 'clear', [], _).

get_all_timepoints(L,T1,T2,LA):-
sherpa_interface(SHERPA),
jpl_call(SHERPA,'getAllTimepoints', [L,T1,T2,3.0],LA).

possible_object_location(Name,Tr):-
sherpa_interface(SHERPA),
jpl_list_to_array(Tr, T),
jpl_call(SHERPA,'posObjLoc',[Name, T],_).

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
get_objects_by_type(A, _),
callSlope(_).

add_traces(A):-
    sherpa_interface(SHERPA),
    jpl_list_to_array(A,B),
    jpl_call(SHERPA,'addTraces',[B],_).

callSlope(_):-
sherpa_interface(SHERPA),
jpl_call(SHERPA,'Roadneighbours',[], A),
jpl_array_to_list(A, AL),
get_all_poses(AL,Poses),
jpl_list_to_array(AL, N),
jpl_list_to_array(Poses, P),
jpl_call(SHERPA,'slopeUp',[N, P],_).

visualize_areas(A):-
    sherpa_interface(SHERPA),
    jpl_list_to_array(A, L),
    jpl_call(SHERPA,'visualizeAreas',[L],_).


visualize_bboxes(A):-
get_objects_by_type(A, Objs),
    forall(member(X,Objs),callVisualizer(X)).

visualize_bbox(A):-
callVisualizer(A).

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

all_timepoint_poses(Link,Timepoint,Poses):-
    ( mng_lookup_transform('/map',Link,Timepoint,Pose)
       -> entering_succeeded(Pose,Poses); entering_failed(_)).


entering_succeeded(Pose,Poses):-
jpl_list_to_array(Pose,Sose),
Poses = Sose.

entering_failed(_):-
format('failed mongodb_lookup\n').

get_all_timepoint_poses(Link,[H|T],[SH|ST]):-
all_timepoint_poses(Link, H,P),
SH = P,
get_all_timepoint_poses(Link,T, ST).

get_all_timepoint_poses(_,[],[]).


all_poses(Number,Poses):-
format(Number),
    current_object_pose(Number,P),
jpl_list_to_array(P,S),
Poses = S.

get_all_poses([H|T], [SH|ST]) :-
all_poses(H,P),
     SH = P,
get_all_poses(T, ST).

get_all_poses([],[]).

regions_not_scanned(Link, T1, T2, A):-
get_all_timepoints(Link, T1, T2, TL),
jpl_array_to_list(TL,LTP),
get_all_timepoint_poses(Link,LTP, TP),
findall(Obj, (owl_individual_of(Obj, knowrob:'Roofing')), Objs),
findall(Rof, (owl_individual_of(Rof, knowrob:'RoofingPart')), Rofs),
map_objects_transformations(Objs, Trans),
jpl_list_to_array(Objs,LObjs),
jpl_list_to_array(Rofs,LRofs),
jpl_list_to_array(Trans,LTrans),
goIntoNoScanMethod(TP,LObjs,LTrans, LRofs, A).

goIntoNoScanMethod(TP, LObjs, E, LRofs, A):-
sherpa_interface(SHERPA),
jpl_list_to_array(TP,TA),
jpl_call(SHERPA, 'getNotDetectedRoofings', [TA, LObjs, E, LRofs], B),
jpl_array_to_list(B,P),
set_keywords_as_markerobjs(P),
=(A,P).


set_keywords_as_markerobjs([H|T]):-
current_object_pose(H, Pose),
add_tenName('Region-Not-Detected',Pose),
marker(object(H), Obj), 
marker_color(Obj, [1.0,0.0,0.0,1.0]),
marker_update(Obj),
set_keywords_as_markerobjs(T).

set_keywords_as_markerobjs([]).

get_regions(Gestures, Elems):-
sherpa_interface(SHERPA),
%format('test\n'),
%findall(Polygon, (owl_individual_of(Polygon, 'http://knowrob.org/kb/knowrob.owl#GISPolygon')),Objs),
map_root_objects('http://knowrob.org/kb/u_map.owl#USemMap_twoY',Objs),
%format('test1011\n'),
map_objects_transformations(Objs, Trans),
%format('test123\n'),
jpl_list_to_array(Objs,LObjs),
%format('test789\n'),
jpl_list_to_array(Trans,LTrans),
jpl_list_to_array(Gestures,LGestures),
%format('test456\n'),
jpl_call(SHERPA,'getRegions',[LGestures, LObjs, LTrans], Elems).


visualize_meshes(Elems):-
forall(member(Elem, Elems), visualize_mesh(Elem)).

visualize_mesh(Elem):-
format('visualize_mesh\n'),
format(Elem),
sherpa_interface(SHERPA),
%map_object_dimensions(Elem,W,D,H),
%append([W, D, H],[], L),
%jpl_list_to_array(L,LDim),
format('\nElem\n'),
rdf_has(Elem, knowrob:pathToCadModel, literal(type(_X,Path))),
format('Path\n'),
current_object_pose(Elem,Pose),
format('Pose\n'),
jpl_list_to_array(Pose, LPose),
jpl_call(SHERPA, 'addObjectMarker',[Path, LPose],_).

get_all_detected_regions(Link, T1, T2, Arr):-
get_all_timepoints(Link, T1, T2, TL),
jpl_array_to_list(TL,TA),
get_all_timepoint_poses(Link,TA, TP),
map_root_objects('http://knowrob.org/kb/u_map.owl#USemMap_twoY',Objs),
map_objects_transformations(Objs, Trans),
%map_objects_dimensions(Objs, Dims),
jpl_list_to_array(Objs,LObjs),
jpl_list_to_array(Trans,LTrans),
%jpl_list_to_array(Dims,LDims),
%jpl_call(SHERPA,'getObjectsMinMax',[LObjs, LTrans, LDims], LANA),
goIntoOthermethod(TP,LObjs,LTrans, Arr).

goIntoOthermethod(TP,LObjs,E, Arr):-
format('goIntoOthermethod'),
sherpa_interface(SHERPA),
jpl_list_to_array(TP,TA),
jpl_call(SHERPA, 'getDetectedObjects', [TA, LObjs, E], Arr).


get_points_as_polygon(Tr,Arr):-
sherpa_interface(SHERPA),
get_points(Tr,T),
jpl_list_to_array(T,R),
jpl_call(SHERPA, 'getPointsAsPolygon', [R], Arr).

get_points_as_array(Point, Arr):-
jpl_list_to_array(Point,Sose),
Arr = Sose.

get_points([H|T],[SH|ST]):-
get_points_as_array(H,P),
SH = P,
get_points(T, ST).

get_points([],[]).

get_objects_MinMax(_,_).

get_next_timepoint(Timepoint, LTimepoint,Result):-
sherpa_interface(SHERPA),
jpl_list_to_array(LTimepoint,LTimes),
jpl_call(SHERPA,'getNextTimepoint',[LTimes,Timepoint],Result).

get_previous_timepoint(Timepoint, LTimepoint,Result):-
sherpa_interface(SHERPA),
jpl_list_to_array(LTimepoint,LTimes),
jpl_call(SHERPA,'getPreviousTimepoint',[LTimes,Timepoint],Result).

set_timepoint(_,_).

view_image(Img):-
atom_concat('Rescue-Mission/scenario_0/episode_0/',Img,Res),
designator_publish_image(Res).

cam_pose(Pose):-
sherpa_interface(SHERPA),
jpl_list_to_array(Pose,Array),
jpl_call(SHERPA,'getPosition',[Array],Origin),
jpl_call(SHERPA,'getOrientation',[Array],Orientation),
jpl_array_to_list(Origin,Lori),
jpl_array_to_list(Orientation,Lorient),
camera_pose(Lori,Lorient).

get_elements(L,String,Result):-
is_element_in_list(L,String,End),
delete(End,'',Result).

is_element_in_list([H|T],String,[S|K]):-
is_part_of(H,String,S),
is_element_in_list(T,String,K).

is_element_in_list([],_,[]).

is_part_of(String,Substring,Result):-
sub_string(String,_,_,_,Substring)
->append(_,String,Result);append(_,'',Result).

get_atomlist_from_stringlist([H|T],[AH|AT]):-
atom_number(H, E),
AH = E,
get_atomlist_from_stringlist(T,AT).

get_atomlist_from_stringlist([],[]).

add_robot(Name,Pose):-
sherpa_interface(SHERPA),
jpl_list_to_array(Pose,P),
jpl_call(SHERPA,'addRobotMarker',[Name,P],_).


show_gps_location(E, Obj, Transform):-
mng_lookup_transform('/map', Obj, E, Transform), !,
possible_object_location('posOf_Kite_Accident', Transform).

get_pose_of_transform(Trans, Pose):-
sherpa_interface(SHERPA),
jpl_list_to_array(Trans,T),
jpl_call(SHERPA,'transformToPose',[T],Pose).

get_mesh_scale_array(Elem,Array):-
rdf_has(Elem, 'srdl2-comp:mesh_scale', literal(type(_X, String))),
split_string(String, " ", "", Array).

% 13 = http://knowrob.org/kb/unreal_log.owl#FrozenLake_150IF
% 14 = http://knowrob.org/kb/unreal_log.owl#FrozenLake_rFdy
