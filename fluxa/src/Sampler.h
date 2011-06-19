// Copyright (C) 2004 David Griffiths <dave@pawfal.org>
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

#include <map>
#include <string>
#include <vector>
#include "Types.h"
//#include "Event.h"   //not sure we still need events at all? -Kas
#include "Sample.h"
#include "Trace.h"


#ifndef NE_SAMPLER
#define NE_SAMPLER

using namespace spiralcore;
using namespace std;



class Sampler
{
public:
	Sampler(unsigned int samplerate);
	virtual ~Sampler();
	
	virtual void Process(uint32 BufSize, Sample &out, Sample &control);
	virtual void Process(uint32 BufSize, Sample &out, float freq);
	virtual void SetSampleId(int ID);
	virtual void SetStartTime(float time);
	
	
private:
	unsigned int m_SampleRate;
	unsigned int m_SampleId;
	float m_SampleTime;
	float m_ChangePerHz;
	float m_StartTime;
	float m_Position;
};

class Scrubber
{
public:
	Scrubber(unsigned int samplerate);
	virtual ~Scrubber();
	virtual void Process(uint32 BufSize, Sample &out, Sample &control);
	virtual void SetSampleId(int ID);
	
private:
	unsigned int m_SampleRate;
	unsigned int m_SampleId;
	float m_Track;
};

#endif

