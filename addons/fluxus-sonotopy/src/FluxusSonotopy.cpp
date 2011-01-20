// Copyright (C) 2011 Alexander Berman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include <escheme.h>
#include <iostream>
#include <jack/jack.h>
#include "SonotopyInterface.h"

#undef MZ_GC_DECL_REG
#undef MZ_GC_UNREG
#define MZ_GC_DECL_REG(size) void *__gc_var_stack__[size+2] = { (void*)0, (void*)size };
#define MZ_GC_UNREG() (GC_variable_stack = (void**)__gc_var_stack__[0])

jack_client_t *jackClient = NULL;
bool jackActivated = false;
jack_port_t *jackInputPort;
SonotopyInterface *sonotopyInterface = NULL;

int jackProcess(jack_nframes_t num_frames, void *arg) {
  jack_default_audio_sample_t *buffer =
    (jack_default_audio_sample_t *) jack_port_get_buffer(jackInputPort, num_frames);
  sonotopyInterface->feedAudio((float *)buffer, num_frames);
  return 0;
}

void jackShutdown(void *arg) {
  std::cout << "disconnected from jack" << std::endl;
  if(sonotopyInterface != NULL) {
    delete sonotopyInterface;
    sonotopyInterface = NULL;
  }
}

// StartSectionDoc-en
// fluxus-sonotopy
// This module integrates with Sonotopy, a library for perceptually
// analyzing an acoustic signal in real time, e.g. for visualization
// of music. Sonotopy consists of methods to extract high-level
// features from an acoustic waveform. These numeric feature values
// can be interpreted as e.g. shapes, colors or motion parameters.
// fluxus-sonotopy acts as a layer between Jack, Sonotopy and Fluxus.
// Example:
// (init-sonotopy)
// (require racket/math)
// (define (render)
//  (rotate (vector 90 (* 360 (/ (vane) (* pi 2))) 0))
//  (scale (vector (beat) 0.1 0.1))
//  (translate (vector 0.5 0 0))
//  (draw-cube))
// (every-frame (render))
// EndSectionDoc

// StartFunctionDoc-en
// init-sonotopy jackport-string
// Returns: void
// Description:
// Initializes fluxus-sonotopy by connecting to jack. jackport is an optional name specifying a port to connect to; if unspecified, the user needs to connect manually.
// Example:
// (init-sonotopy "system:capture_1")
// EndFunctionDoc

Scheme_Object *init_sonotopy(int argc, Scheme_Object **argv) {
  bool connect = false;
  char *jackSourcePort = NULL;
  MZ_GC_DECL_REG(1);
  MZ_GC_VAR_IN_REG(0, argv);
  MZ_GC_REG();
  if(argc == 1) {
    if(!SCHEME_CHAR_STRINGP(argv[0])) scheme_wrong_type("start-audio", "string", 0, argc, argv);
    jackSourcePort = scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(argv[0]),SCHEME_CHAR_STRLEN_VAL(argv[0]),NULL,0);
    connect = true;
  }

  if(jackClient == NULL) {
    jackClient = jack_client_open("fluxus", JackNullOption, NULL);
    if(jackClient == NULL) {
      std::cerr << "Failed to connect to jack" << std::endl;
    }
    else {
      jack_on_shutdown(jackClient, jackShutdown, NULL);
      jack_set_process_callback(jackClient, jackProcess, NULL);
      jackInputPort = jack_port_register(jackClient, "in", JACK_DEFAULT_AUDIO_TYPE, JackPortIsInput, 0);
      if(jackInputPort == NULL) {
        std::cerr << "Failed to create jack input port" << std::endl;
      }
    }
  }

  if(jackClient != NULL && sonotopyInterface == NULL) {
    sonotopyInterface = new SonotopyInterface(jack_get_sample_rate(jackClient),
					      jack_get_buffer_size(jackClient));
    if(!jackActivated) {
      if(jack_activate(jackClient) == 0) {
	jackActivated = true;
	if(connect) {
	  if(jack_connect(jackClient, jackSourcePort, jack_port_name(jackInputPort)) != 0)
	    std::cerr << "Failed to connect to jack port " << jackSourcePort << std::endl;
	}
      }
      else
	std::cerr << "Failed to activate jack client" << std::endl;
    }
  }

  MZ_GC_UNREG();
  return scheme_void;
}


// StartFunctionDoc-en
// vane
// Returns: float
// Description:
// Returns an angular value representing the current "direction" of
// the audio input. The value is related to a continously updated
// sonotopic map of recently encountered audio. The output is expected
// to roughly reflect musical and harmonic dynamics. It can be used
// e.g. to control movement. Range: -2*pi to 2*pi.
// Example:
// (rotate (vector 90 (* 360 (/ (vane) (* pi 2))) 0))
// (draw-cube)
// EndFunctionDoc

Scheme_Object *get_vane_angle(int argc, Scheme_Object **argv) {
  float angle = 0.0f;
  if(sonotopyInterface != NULL)
    angle = sonotopyInterface->getVaneAngle();
  return scheme_make_float(angle);
}


// StartFunctionDoc-en
// beat
// Returns: float
// Description:
// Returns a value from 0 to 1 representing the current "beat
// intensity" of the audio input. Rhythmic events such as drum hits
// are expected to yield high values. By contrast, low values are
// yielded by silence and other monotonous sounds.
// Example:
// (scale (vector (beat) 0.1 0.1))
// (draw-cube)
// EndFunctionDoc

Scheme_Object *get_beat_intensity(int argc, Scheme_Object **argv) {
  float beat_intensity = 0.0f;
  if(sonotopyInterface != NULL)
    beat_intensity = sonotopyInterface->getBeatIntensity();
  return scheme_make_float(beat_intensity);
}



// StartFunctionDoc-en
// get-num-spectrum-bins
// Returns: integer
// Description:
// Returns the number of spectrum bins, whose contents can be retrieved by (spectrum-bin).
// Example:
// (get-num-spectrum-bins)
// EndFunctionDoc

Scheme_Object *get_num_spectrum_bins(int argc, Scheme_Object **argv) {
  int num_bins = 0;
  if(sonotopyInterface != NULL)
    num_bins = sonotopyInterface->getNumSpectrumBins();
  return scheme_make_integer_value(num_bins);
}


// StartFunctionDoc-en
// spectrum-bin
// Returns: float
// Description:
// Returns the current total power of frequencies in bin number n, where 0 <= n < (get-num-spectrum-bins). Low n values represent low frequency bands.
// Example:
// (spectrum-bin 1)
// EndFunctionDoc

Scheme_Object *get_spectrum_bin_value(int argc, Scheme_Object **argv) {
  MZ_GC_DECL_REG(1);
  MZ_GC_VAR_IN_REG(0, argv);
  MZ_GC_REG();
  if(!SCHEME_NUMBERP(argv[0])) scheme_wrong_type("spectrum-bin", "number", 0, argc, argv);
  float value = 0.0f;
  if(sonotopyInterface != NULL)
    value = sonotopyInterface->getSpectrumBinValue((int)scheme_real_to_double(argv[0]));
  MZ_GC_UNREG();
  return scheme_make_float(value);
}


/////////////////////

#ifdef STATIC_LINK
Scheme_Object *sonotopy_scheme_reload(Scheme_Env *env)
#else
Scheme_Object *scheme_reload(Scheme_Env *env)
#endif
{
  Scheme_Env *menv=NULL;
  MZ_GC_DECL_REG(2);
  MZ_GC_VAR_IN_REG(0, env);
  MZ_GC_VAR_IN_REG(1, menv);
  MZ_GC_REG();

  menv=scheme_primitive_module(scheme_intern_symbol("fluxus-sonotopy"), env);

  scheme_add_global("init-sonotopy", scheme_make_prim_w_arity(init_sonotopy, "init-sonotopy", 0, 1), menv);
  scheme_add_global("vane", scheme_make_prim_w_arity(get_vane_angle, "vane", 0, 0), menv);
  scheme_add_global("beat", scheme_make_prim_w_arity(get_beat_intensity, "beat", 0, 0), menv);
  scheme_add_global("get-num-spectrum-bins", scheme_make_prim_w_arity(get_num_spectrum_bins, "get-num-spectrum-bins", 0, 0), menv);
  scheme_add_global("spectrum-bin", scheme_make_prim_w_arity(get_spectrum_bin_value, "spectrum-bin", 1, 1), menv);

  scheme_finish_primitive_module(menv);
  MZ_GC_UNREG();

  return scheme_void;
}

#ifndef STATIC_LINK
Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
  return scheme_intern_symbol("fluxus-sonotopy");
}
#endif
