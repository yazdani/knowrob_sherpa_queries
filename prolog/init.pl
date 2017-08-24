/*
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

:- register_ros_package(knowrob_common).
:- register_ros_package(knowrob_sherpa_queries).
:- register_ros_package(knowrob_map_data).
:- register_ros_package(knowrob_objects).

:- use_module(library('knowrob_sherpa_queries')).
:- rdf_db:rdf_register_ns(unreal, 'http://knowrob.org/kb/unreal_log.owl#', [keep(true)]).
:- owl_parser:owl_parse('package://sherpa_world/owl/SemanticMapSherpa.owl').
:- owl_parser:owl_parse('package://sherpa_world/owl/SemanticMapTracks.owl').
:- owl_parser:owl_parse('package://sherpa_world/owl/SemanticMapGIS.owl').
:- owl_parser:owl_parse('package://sherpa_world/owl/SemanticMapRoofings.owl').
