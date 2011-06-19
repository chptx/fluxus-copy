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

#include <stdio.h>
#include <math.h>
#include <algorithm>
#include "Sampler.h"
#include "SampleStore.h"
#include "AsyncSampleLoader.h"

static const unsigned int SAFETY_MAX_CHANNELS=30;

Sampler::Sampler(unsigned int samplerate) :
m_SampleRate(samplerate),
m_StartTime(0)
{
m_SampleTime=1.0/(float)m_SampleRate;
m_ChangePerHz=1.0/440.0; //avoid a division per sample period
}

Sampler::~Sampler()
{
}

void Sampler::Process(uint32 BufSize, Sample &out, float freq)
{
	Sample *sample = SampleStore::Get()->GetSample(m_SampleId);

	if (sample != NULL) //avoid lost samples
	{					
		unsigned int length = sample->GetLength();
		if  (length > 0) //avoid not yet fully loaded samples as these currently get things stuck
		{		
			if (m_StartTime < 0) //set playback postion to the end before we start playing for reversed samples
				{
					if (freq < 0) m_Position = length; 
					else m_Position = 0;
				}
			float speed = m_ChangePerHz * freq;
			for (uint32 n=0; n<BufSize; n++)
			{
				if (m_StartTime < 0) //wait for our scheduled start
				{
					m_StartTime += m_SampleTime;
				}
				else
				{
					m_Position += speed;
					if (m_Position<0) m_Position = 0;                 //by keeping things in range and always basing the output on the file
					else if (m_Position>length) m_Position = length;  //instead of returning 0, we make sure a adsr can always avoid clicks caused by
				}													  //any DC offset the file may have. This shouldn't cost more as we need those 
				out[n] = (*sample)[m_Position];						  //conditions anyway.
			}
		}
	}
}

void Sampler::Process(uint32 BufSize, Sample &out, Sample &freq)
{
	Sample *sample = SampleStore::Get()->GetSample(m_SampleId);

	if (sample != NULL) //avoid lost samples
	{					
		unsigned int length = sample->GetLength();
		if  (length > 0) //avoid not yet fully loaded samples as these currently get things stuck
		{		
			for (uint32 n=0; n<BufSize; n++)
			{
				if (m_StartTime < 0) //wait for our scheduled start
				{
					m_StartTime += m_SampleTime;
					if (freq[n] < 0) m_Position = length; //this way we at least start from the correct point, should the control signal be
					else m_Position = 0;				  //unusually erattic
				}
				else
				{
					m_Position += m_ChangePerHz * freq[n];
					if (m_Position<0) m_Position = 0;                 //keeping things in range will help make the most of very heavy modulation
					else if (m_Position>length) m_Position = length;  
				}													   
				out[n] = (*sample)[m_Position];						  
			}
		}
	}
}

void Sampler::SetSampleId(int ID)
{
	m_SampleId = ID;
}

void Sampler::SetStartTime(float time)
{
	m_StartTime = time;
}

Scrubber::Scrubber(unsigned int samplerate) :
m_SampleRate(samplerate),
m_Track(0)
{
}

Scrubber::~Scrubber()
{
}

void Scrubber::Process(uint32 BufSize, Sample &out, Sample &control)
{ 
	Sample *sample = SampleStore::Get()->GetSample(m_SampleId);
	
	if (sample != NULL) //avoid lost samples 
	{
		unsigned int length = sample->GetLength();
		if  (length > 0) //avoid not yet fully loaded samples as these currently get things stuck
		{
			for (uint32 n=0; n<BufSize; n++)
			{
				float input = control[n];
				if (input >= 0 && input <= 1)
				{
					m_Track = input * length;
				}
				out[n] = (*sample)[m_Track];
			}
		}
	} 
}

void Scrubber::SetSampleId(int ID)
{
	m_SampleId = ID;
}
