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
#include "SchemeHelper.h"

using namespace std;
using namespace sonotopy;
using namespace SchemeHelper;

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
  string jackSourcePort;
  DECL_ARGV();
  if(argc == 1) {
    ArgCheck("init-sonotopy", "s", argc, argv);
    jackSourcePort = StringFromScheme(argv[0]);
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
	  if(jack_connect(jackClient, jackSourcePort.c_str(), jack_port_name(jackInputPort)) != 0)
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
// Returns the current total power of frequencies in bin number n,
// where 0 <= n < (get-num-spectrum-bins). Low n values represent low
// frequency bands.
// Example:
// (spectrum-bin 1)
// EndFunctionDoc

Scheme_Object *get_spectrum_bin_value(int argc, Scheme_Object **argv) {
  DECL_ARGV();
  ArgCheck("spectrum-bin", "f", argc, argv);
  float value = 0.0f;
  if(sonotopyInterface != NULL)
    value = sonotopyInterface->getSpectrumBinValue(FloatFromScheme(argv[0]));
  MZ_GC_UNREG();
  return scheme_make_float(value);
}


// StartFunctionDoc-en
// set-waveform-window-size secs-float
// Returns: void
// Description:
// Sets the size of the waveform window, measured in seconds. See
// (waveform). Higher values yield a larger window and thus a slower
// movement of the waveform.
// Example:
// (set-waveform-window-size 0.1)
// EndFunctionDoc

Scheme_Object *set_waveform_window_size(int argc, Scheme_Object **argv) {
  DECL_ARGV();
  ArgCheck("set-waveform-window-size", "f", argc, argv);
  if(sonotopyInterface != NULL)
    sonotopyInterface->setWaveformWindowSize(FloatFromScheme(argv[0]));
  MZ_GC_UNREG();
  return scheme_void;
}


// StartFunctionDoc-en
// get-num-waveform-frames
// Returns: integer
// Description:
// Returns the size of the waveform window, measured in audio
// frames. This equals the size of the vector return by (waveform).
// Example:
// (get-num-waveform-frames)
// EndFunctionDoc

Scheme_Object *get_num_waveform_frames(int argc, Scheme_Object **argv) {
  int num_frames = 0;
  if(sonotopyInterface != NULL)
    num_frames = sonotopyInterface->getNumWaveformFrames();
  return scheme_make_integer_value(num_frames);
}


// StartFunctionDoc-en
// waveform
// Returns: audio-buffer-vector
// Description:
// Returns the waveform (sample values) of the most recent audio
// input, as a vector of float values. The amount of time represented
// by this window is set with (set-waveform-window-size).
// Example:
// (set-waveform-window-size 0.1)
// 
// (clear)
// (define p (build-ribbon (get-num-waveform-frames)))
// (with-primitive p
//    (hint-unlit)
//    (pdata-map! (lambda (w) .01) "w"))
// 
// (every-frame
//    (let ([a (waveform)])
//        (with-primitive p
//            (pdata-index-map!
//                (lambda (i p)
//                    (vector (* .005 (- i (/ (pdata-size) 2))) (* 1 (vector-ref a i)) 0))
//                "p"))))
// EndFunctionDoc

Scheme_Object *get_waveform(int argc, Scheme_Object **argv) {
  Scheme_Object *result = NULL;
  Scheme_Object *tmp = NULL;
  MZ_GC_DECL_REG(2);
  MZ_GC_VAR_IN_REG(0, result);
  MZ_GC_VAR_IN_REG(1, tmp);
  MZ_GC_REG();
  
  if(sonotopyInterface != NULL) {
    int num_frames = sonotopyInterface->getNumWaveformFrames();
    result = scheme_make_vector(num_frames, scheme_void);
    const float *p = sonotopyInterface->getWaveformBuffer();
    for (int n = 0; n < num_frames; n++) {
      tmp = scheme_make_float(*p++);
      SCHEME_VEC_ELS(result)[n] = tmp;
    }
  }
  else {
    result = scheme_make_vector(0, scheme_void);
  }

  MZ_GC_UNREG();
  return result;
}


Scheme_Object *get_grid_activation(int argc, Scheme_Object **argv) {
  DECL_ARGV();
  ArgCheck("sonotopic-grid-node", "ii", argc, argv);
  float value = 0.0f;
  if(sonotopyInterface != NULL) {
    unsigned int x = IntFromScheme(argv[0]);
    unsigned int y = IntFromScheme(argv[1]);
    value = sonotopyInterface->getGridMapActivation(x, y);
  }
  MZ_GC_UNREG();
  return scheme_make_float(value);
}

Scheme_Object *get_grid_activation_pattern(int argc, Scheme_Object **argv) {
  Scheme_Object *result = NULL;
  Scheme_Object *tmprow = NULL;
  Scheme_Object *tmpnode = NULL;
  MZ_GC_DECL_REG(3);
  MZ_GC_VAR_IN_REG(0, result);
  MZ_GC_VAR_IN_REG(1, tmprow);
  MZ_GC_VAR_IN_REG(2, tmpnode);
  MZ_GC_REG();

  unsigned int gridMapWidth = sonotopyInterface->getGridMapWidth();
  unsigned int gridMapHeight = sonotopyInterface->getGridMapHeight();

  result = scheme_make_vector(gridMapHeight, scheme_void);

  if(sonotopyInterface != NULL) {
    const SonogramMap::ActivationPattern *activationPattern =
      sonotopyInterface->getGridMapActivationPattern();
    SonogramMap::ActivationPattern::const_iterator activationPatternIterator =
      activationPattern->begin();
    for(unsigned int y = 0; y < gridMapHeight; y++) {
      tmprow = scheme_make_vector(gridMapWidth, scheme_void);
      for(unsigned int x = 0; x < gridMapWidth; x++) {
	tmpnode = scheme_make_float(*activationPatternIterator++);
	SCHEME_VEC_ELS(tmprow)[x] = tmpnode;
      }
      SCHEME_VEC_ELS(result)[y] = tmprow;
    }
  }

  MZ_GC_UNREG();
  return result;
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

  scheme_add_global("init-sonotopy",
		    scheme_make_prim_w_arity(init_sonotopy, "init-sonotopy", 0, 1), menv);
  scheme_add_global("vane",
		    scheme_make_prim_w_arity(get_vane_angle, "vane", 0, 0), menv);
  scheme_add_global("beat",
		    scheme_make_prim_w_arity(get_beat_intensity, "beat", 0, 0), menv);
  scheme_add_global("get-num-spectrum-bins",
		    scheme_make_prim_w_arity(get_num_spectrum_bins, "get-num-spectrum-bins", 0, 0), menv);
  scheme_add_global("spectrum-bin",
		    scheme_make_prim_w_arity(get_spectrum_bin_value, "spectrum-bin", 1, 1), menv);
  scheme_add_global("set-waveform-window-size",
		    scheme_make_prim_w_arity(set_waveform_window_size, "set-waveform-window-size", 1, 1), menv);
  scheme_add_global("get-num-waveform-frames",
		    scheme_make_prim_w_arity(get_num_waveform_frames, "get-num-waveform-frames", 0, 0), menv);
  scheme_add_global("waveform",
		    scheme_make_prim_w_arity(get_waveform, "waveform", 0, 0), menv);
  scheme_add_global("sonotopic-grid",
		    scheme_make_prim_w_arity(get_grid_activation_pattern, "sonotopic-grid", 0, 0), menv);
  scheme_add_global("sonotopic-grid-node",
		    scheme_make_prim_w_arity(get_grid_activation, "sonotopic-grid-node", 2, 2), menv);

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
