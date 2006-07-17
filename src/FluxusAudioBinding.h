// Copyright (C) 2005 Dave Griffiths
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

#include "FluxusBinding.h"

#ifndef FLUXUS_AUDIO_BINDING
#define FLUXUS_AUDIO_BINDING

class FluxusAudioBinding : public FluxusBinding
{
public:
	void RegisterProcs();
	
	static SCM start_audio(SCM s_dev, SCM s_bs, SCM s_sr);
	static SCM get_harmonic(SCM s_harm);
	static SCM process(SCM s_wavname);
	static SCM gain(SCM s_gain);
	static SCM smoothing_bias(SCM s_gain);
	

};
#endif